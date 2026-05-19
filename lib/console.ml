(* TODO: [ERR] capture user errors well *)
open Lwt.Infix

type user_event =
  | Send_msg of bytes
  | Slash_cmd of string
  | User_close
  | User_exit
[@@warning "-37"]
(* slash command not wired in yet, to be wired in at [parse_user_input] *)

type listener = user_event -> unit Lwt.t

type t = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  on_fini : unit -> unit Lwt.t;
  mutable session : Session.t option;
  listeners : listener list ref;
}

let dispatch_event t e =
  Lwt_list.iter_p
    (fun listener ->
      try%lwt listener e
      with e -> Lwt_io.eprintf "[Listener error: %s]\n" (Printexc.to_string e))
    !(t.listeners)

let register_listeners t fs =
  t.listeners := !(t.listeners) @ fs;
  t

let show_help () = Lwt_io.printl "Commands: /quit, /help"
let fini t = Lwt_io.printl "Quitting the app console..." >>= t.on_fini

(* TODO [REFACTOR] on hindsight, this listener pattern doesn't feel right, maybe something more idiomatic that watches some lwt condition or something observer-like may be offerred by lwt actually *)
type session_listener_factory = Session.t -> listener
(** Creates a listener that needs to capture a [Session.t] because it relies on
    that session to do its job.

    An example is sending a message (network io) which is a callback that should
    be fired on the [ SendMsg ] user event.

    Such a factory should be used when binding with session *)

let create_send_msg_listener s : listener = function
  | Send_msg bs -> Session.send_message s bs
  | _ -> Lwt.return ()
[@@warning "-4"]
(* Listener pattern: User events of type [user_event] get dispatched to a collection of
    listeners. These listeners are like observers that ignore cases that don't
    matter to them. Hence the shim. This function only cares about [ SendMsg ] *)

let session_listener_factories : session_listener_factory list =
  [ create_send_msg_listener ]

type user_event_listener_factory = t -> listener
(** Creates a listener that capture a [t] because it relies on the console to do
    its job.

    An example is slash commands listening. Such a factory should be used when
    creating a console*)

let maybe_shutdown_session t =
  match t.session with
  | None -> Lwt_io.printl "You're currently not in any chat, nothing to quit"
  | Some sess -> Session.shutdown sess

(* TODO [CLEAN] consider whether t is needed *)
let create_slash_cmd_listener _t : listener = function
  | Slash_cmd "/help" -> show_help ()
  | Slash_cmd cmd -> Lwt_io.printlf "Unknown slash command: %s\n" cmd
  | _ -> Lwt.return_unit
[@@warning "-4"]
(* Listener pattern: User events of type [user_event] get dispatched to a collection of
    listeners. These listeners are like observers that ignore cases that don't
    matter to them. Hence the shim. This function only cares about [ SlashCmd ] *)

let create_no_session_listener t : listener = function
  | Send_msg _ when Option.is_none t.session ->
      Lwt_io.printl
        "[You're not in an active session. Wait for a client to connect before \
         sending messages...]"
  | _ -> Lwt.return_unit
[@@warning "-4"]
(* Listener pattern: this doesn't care about any other event -- it just ignores*)

let user_event_listener_factories : user_event_listener_factory list =
  [ create_slash_cmd_listener; create_no_session_listener ]

let init_console t =
  let user_event_listeners =
    List.map (fun factory -> factory t) user_event_listener_factories
  in
  register_listeners t user_event_listeners

let create ?(ic = Lwt_io.stdin) ?(oc = Lwt_io.stdout)
    ?(on_fini = fun () -> Lwt.return_unit) () =
  init_console { session = None; listeners = ref []; ic; oc; on_fini }

(** Formats the payload (exact, raw byte segment) for display.*)
let format_msg_rx_bs payload =
  let len = Bytes.length payload in
  let formatted_payload = Bytes.create (len + 1) in
  Bytes.blit payload 0 formatted_payload 0 len;
  Bytes.set formatted_payload len '\n';
  formatted_payload

let make_on_rx_msg_cb oc =
  let on_rx_msg rx_bs =
    try%lwt
      let fmted_rx_payload = format_msg_rx_bs rx_bs in
      let fmted_payload_len = Bytes.length fmted_rx_payload in
      Lwt_io.write_from_exactly oc fmted_rx_payload 0 fmted_payload_len
    with
    | Unix.Unix_error (Unix.EPIPE, _, _) ->
        Lwt_io.eprintf
          "[STDOUT ERROR: Output pipe broken (e.g., redirected to closed file)]\n"
        >>= fun () -> Lwt.fail_with "stdout broken"
    | Unix.Unix_error (Unix.EBADF, _, _) ->
        Lwt_io.eprintf "[STDOUT ERROR: Invalid file descriptor]\n" >>= fun () ->
        Lwt.fail_with "stdout invalid"
  in
  on_rx_msg
[@@warning "-4"]
(* Ignore warning 4: The fragile pattern match on [ Unix.error ] is fine because we only care about some of the error types*)

let make_console_rx_callbacks { oc; _ } =
  let on_rx_msg = make_on_rx_msg_cb oc in
  let on_rx_close () = Lwt_io.printl "Your peer has left the chat" in
  let on_rx_ack id rtt = Lwt_io.printlf "Msg %ld Acked with rtt = %fs" id rtt in
  { Session.on_rx_msg; on_rx_ack; on_rx_close }

let bind_session t ~session =
  let s = t |> make_console_rx_callbacks |> Session.set_callbacks session in
  t.session <- Some s;
  let session_listeners =
    List.map (fun factory -> factory s) session_listener_factories
  in
  register_listeners t session_listeners

let unbind_session t =
  (* TODO: [PERF] verify if this cleanup is needed, not sure if will have dangling ptrs *)
  let _ = Option.map Session.unset_callbacks t.session in
  t.session <- None;
  t.listeners := [];
  init_console t

(* TODO:[STUB] implement this right -- it should end up creating frames if it's a [SendMsg], else slash commands and so on
   TODO: [STUB] slash commands to be parsed here as well, not just messages
*)
let parse_user_input line =
  let l = String.trim line in
  match l with
  | "" -> None
  | "/quit" -> Some User_close
  | "/exit" -> Some User_exit
  | "/help" -> Some (Slash_cmd line)
  | _ -> Some (Send_msg (String.to_bytes line))

let run_console t =
  let rec loop () =
    let%lwt line = Lwt_io.read_line t.ic in
    match parse_user_input line with
    | None -> loop ()
    | Some User_close -> maybe_shutdown_session t >>= loop
    | Some User_exit -> Lwt.return_unit
    | Some user_event -> dispatch_event t user_event >>= loop
      (* It's alright for this to remain exhaustive because the default case is for it to be dispatched as user event*)
      [@@warning "-4"]
  in
  try%lwt loop () with
  | End_of_file -> Lwt.return_unit
  | Lwt.Canceled -> Lwt.return_unit
  | Unix.Unix_error (e, op, arg) ->
      Lwt_io.eprintf "[Console I/O error: %s on %s %s]\n" (Unix.error_message e)
        op arg
  | e -> Lwt_io.eprintf "[Unknown Console error: %s]\n" (Printexc.to_string e)

let handle_sigint =
  let sigint_cond = Lwt_condition.create () in
  let _ =
    Lwt_unix.on_signal Sys.sigint (fun _ ->
        print_endline "\nReceived <C-c>, time to call it quits";
        Lwt_condition.broadcast sigint_cond ())
  in
  Lwt_condition.wait sigint_cond

let run t =
  Lwt_io.printl "Running console..." >>= fun () ->
  let thunk () = Lwt.pick [ run_console t; handle_sigint ] in
  Lwt.finalize thunk (fun () -> fini t)
