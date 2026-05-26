open Lwt.Infix
module D = Display
module S = Session
module SMeta = Session_meta

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

let pp_console_error fmt err =
  Format.fprintf fmt "%a %a" D.pp_prompt_now () pp_error err

type user_event =
  | Send_msg of bytes
  | Slash_cmd of string
  | User_close
  | User_exit

type t = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  mutable session : S.t option;
}

let help_text =
  {|
===  ChatTCP  ===

This is a 1:1 chat system over TCP/IP.

COMMANDS:
  /help    Show this message
  /quit    Close chat session (server: waits for next client, client: exits app)
  /exit    Exit the application immediately

MODES:
  Server:  Listens for incoming connections. Use /quit to close current chat, keep waiting.
  Client:  Connects to server. Use /quit or /exit to close and exit.

Just type a message to send it. Both sides auto-acknowledge delivery.
|}

let show_help t = Lwt_io.write_line t.oc help_text

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
  | Some sess -> S.shutdown sess

let create ?(ic = Lwt_io.stdin) ?(oc = Lwt_io.stdout) () =
  { session = None; ic; oc }

let pp_rx_event (meta : SMeta.t) fmt = function
  | S.Msg_received { id; content = _; rcvd_at } ->
      Format.fprintf fmt "◉ %a <%a> <rx:#%ld>" D.pp_prompt rcvd_at D.pp_peer
        meta.them id
  | Session.Ack_received { id; rtt; rcvd_at } ->
      Format.fprintf fmt "◉ %a 🗸 <ack:#%ld> rtt=%.6fs" D.pp_prompt rcvd_at id
        rtt
  | Session.Peer_closed { rcvd_at } ->
      Format.fprintf fmt "◎ %a <%a> disconnected" D.pp_prompt rcvd_at D.pp_peer
        meta.them
  | Session.Spurious_ack { id; rcvd_at } ->
      Format.fprintf fmt "⩼ %a spurious ack for <msg:#%ld>" D.pp_prompt rcvd_at
        id

(* TODO: add in a notify string callback *)
let make_console_callbacks t session =
  let on_rx ev =
    let header =
      match S.meta_of_opt session with
      | None -> Format.asprintf "%a" D.pp_test_dummy_header ()
      | Some meta -> Format.asprintf "%a" (pp_rx_event meta) ev
    in
    match ev with
    | S.Msg_received { content; _ } ->
        let len = Bytes.length content in
        let%lwt () = Lwt_io.write_line t.oc header in
        let%lwt () = Lwt_io.write_line t.oc (Printf.sprintf "[%dB]:" len) in
        let%lwt () = Lwt_io.write_from_exactly t.oc content 0 len in
        Lwt_io.write_line t.oc ""
    | S.Spurious_ack _ | S.Ack_received _ | S.Peer_closed _ ->
        Lwt_io.write_line t.oc header
  in
  { S.on_rx }

let bind_session t ~session =
  session |> make_console_callbacks t |> Session.set_callbacks session;
  t.session <- Some session;
  let display () =
    match S.meta_of_opt session with
    | None -> Lwt.return_unit
    | Some m -> D.write_pp t.oc D.pp_banner m
  in
  Lwt.async display

let unbind_session t =
  let _ = Option.map Session.unset_callbacks t.session in
  t.session <- None

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
      (* TODO: [PP] this is where the server announcement type cb will be useful *)
      let msg =
        "[You're not in an active session. Wait for a client to connect before \
         sending messages...]"
      in
      Lwt_io.write_line t.oc msg
  | Some s -> S.send_message s bs

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
  let%lwt () = show_help t in
  Lwt.finalize (fun () -> run_console t) (fun () -> fini t)
