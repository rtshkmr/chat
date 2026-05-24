(* TODO: [ERR] capture user errors well -- console needs custom errors *)
open Lwt.Infix

type user_event =
  | Send_msg of bytes
  | Slash_cmd of string
  | User_close
  | User_exit

type t = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  on_fini : unit -> unit Lwt.t;
  mutable session : Session.t option;
}

let show_help t = Lwt_io.write_line t.oc "Commands: /quit, /help"

let fini t =
  let%lwt () = Lwt_io.write_line t.oc "Quitting the app console..." in
  t.on_fini ()

let maybe_shutdown_session t =
  match t.session with
  | None ->
      Lwt_io.write_line t.oc "You're currently not in any chat, nothing to quit"
  | Some sess -> Session.shutdown sess

let create ?(ic = Lwt_io.stdin) ?(oc = Lwt_io.stdout)
    ?(on_fini = fun () -> Lwt.return_unit) () =
  { session = None; ic; oc; on_fini }

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
      let msg =
        "[You're not in an active session. Wait for a client to connect before \
         sending messages...]"
      in
      Lwt_io.write_line t.oc msg
  | Some s -> Session.send_message s bs

let run_console t =
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
  | End_of_file -> Lwt.return_unit
  | Lwt.Canceled -> Lwt.return_unit
  | Lwt_io.Channel_closed _ ->
      Lwt_io.eprintf "[Channel closed: Network or session died]\n"
  | Unix.Unix_error (e, op, arg) ->
      Lwt_io.eprintf "[Console I/O error: %s on %s %s]\n" (Unix.error_message e)
        op arg
  | e -> Lwt_io.eprintf "[Unknown Console error: %s]\n" (Printexc.to_string e)

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
  let thunk () = run_console t in
  Lwt.finalize thunk (fun () -> fini t)
