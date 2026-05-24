open Lwt.Infix

type error =
  | Terminated
      (** Ctrl-D — end of input stream. Operator-initiated clean shutdown. *)
  | Channel_closed of string  (** The input channel was closed underneath us. *)
  | Io_error of Unix.error * string * string
      (** Unix I/O error: (error_code, syscall_name, arg) *)
  | Unexpected of exn
      (** Anything else -- programmer error or unhandled case *)

exception Console_exn of error

let pp_error fmt = function
  | Terminated -> Format.fprintf fmt "stdin closed (EOF)"
  | Channel_closed reason -> Format.fprintf fmt "channel closed: %s" reason
  | Io_error (e, op, arg) ->
      Format.fprintf fmt "I/O error: %s on %s(%s)" (Unix.error_message e) op arg
  | Unexpected e -> Format.fprintf fmt "unexpected: %s" (Printexc.to_string e)

let pp_timestamp fmt ts =
  let tm = Unix.localtime ts in
  let ms = int_of_float ((ts -. floor ts) *. 1000.) in
  Format.fprintf fmt "%04d-%02d-%02d %02d:%02d:%02d.%03d" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec ms

let pp_prompt fmt ts = Format.fprintf fmt "[@[%a@]]" pp_timestamp ts

let pp_console_error fmt err =
  Format.fprintf fmt "%a %a" pp_prompt (Unix.gettimeofday ()) pp_error err

type user_event =
  | Send_msg of bytes
  | Slash_cmd of string
  | User_close
  | User_exit

type t = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  mutable session : Session.t option;
}

let show_help t = Lwt_io.write_line t.oc "Commands: /quit, /help"

(** Console attempts to display exit msg, defensively in case the channel is
    already dead e.g. Channel_closed was the exit reason. This is a beset-effort
    regardless. *)
let fini t =
  try%lwt Lwt_io.write_line t.oc "[Console: shutting down]"
  with _ -> Lwt.return_unit

let maybe_shutdown_session t =
  match t.session with
  | None ->
      Lwt_io.write_line t.oc "You're currently not in any chat, nothing to quit"
  | Some sess -> Session.shutdown sess

let create ?(ic = Lwt_io.stdin) ?(oc = Lwt_io.stdout) () =
  { session = None; ic; oc }

let make_console_callbacks { oc; _ } =
  let on_rx = function
    | (Session.Peer_closed _ | Session.Ack_received _) as ev ->
        let msg = Format.asprintf "%a" Session.pp_display_event ev in
        Lwt_io.write_line oc msg
    | Session.Msg_received rx as ev ->
        let msg = Format.asprintf "%a" Session.pp_display_event ev in
        let%lwt () = Lwt_io.write_line oc msg in
        let len = Bytes.length rx.content in
        let%lwt () = Lwt_io.write_from_exactly oc rx.content 0 len in
        Lwt_io.write_line oc "\n"
  in
  { Session.on_rx }

let bind_session t ~session =
  let s = t |> make_console_callbacks |> Session.set_callbacks session in
  t.session <- Some s;
  t

let unbind_session t =
  (* TODO: [PERF] verify if this cleanup is needed, not sure if will have dangling ptrs *)
  let _ = Option.map Session.unset_callbacks t.session in
  t.session <- None;
  t

let parse_user_input line =
  let l = String.trim line in
  match l with
  | "" -> None
  | "/quit" -> Some User_close
  | "/exit" -> Some User_exit
  | "/help" -> Some (Slash_cmd l)
  | _ -> Some (Send_msg (String.to_bytes line))

let maybe_send_msg t bs =
  match t.session with
  | None ->
      (* TODO: this is where the server announcement type cb will be useful *)
      let msg =
        "[You're not in an active session. Wait for a client to connect before \
         sending messages...]"
      in
      Lwt_io.write_line t.oc msg
  | Some s -> Session.send_message s bs

let run_console t =
  let break_with error =
    let msg = Format.asprintf "%a" pp_console_error error in
    let%lwt () = Lwt_io.eprintf "%s\n" msg in
    Lwt.fail (Console_exn error)
  in
  let rec loop () =
    let%lwt line = Lwt_io.read_line t.ic in
    match parse_user_input line with
    | Some User_close -> maybe_shutdown_session t >>= loop
    | Some User_exit -> Lwt.return_unit
    | Some (Send_msg bs) -> maybe_send_msg t bs >>= loop
    | Some (Slash_cmd "/help") -> show_help t >>= loop
    | None | Some (Slash_cmd _) -> loop ()
  in
  try%lwt loop () with
  | End_of_file -> break_with Terminated
  | Lwt.Canceled -> Lwt.return_unit (* silent, cancellation by harness *)
  | Lwt_io.Channel_closed reason -> break_with (Channel_closed reason)
  | Unix.Unix_error (e, op, arg) -> break_with (Io_error (e, op, arg))
  | e -> break_with (Unexpected e)

(* TODO [REFACTOR] : install signint at cli (top-level) *)
(* let await_sigint { oc; _ } = *)
(*   let c = Lwt_condition.create () in *)

(*   let _handler = *)
(*     let broadcast_sigint _ = *)
(*       Lwt.async (fun () -> *)
(*           Lwt_io.write_line oc "\nReceived <C-c>, time to call it quits"); *)
(*       Lwt_condition.broadcast c () *)
(*     in *)
(*     Lwt_unix.on_signal Sys.sigint broadcast_sigint *)
(*   in *)
(*   Lwt_condition.wait c *)
(* [@@warning "-32"] *)

let run t =
  let%lwt () = Lwt_io.write_line t.oc "Running console..." in
  Lwt.finalize (fun () -> run_console t) (fun () -> fini t)
