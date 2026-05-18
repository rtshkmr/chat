(* TODO: [ERR] capture user errors well *)
open Lwt.Infix

type user_event =
  | SendMsg of bytes
  | SlashCmd of string * string list
  | UserEof
[@@warning "-37"]
(* slash command not wired in yet, to be wired in at [parse_user_input] *)

type listener = user_event -> unit Lwt.t

type t = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  on_fini : unit -> unit Lwt.t;
  (* TODO QQ: does this really need to be mutable?  *)
  mutable session : Session.t option;
  listeners : listener list ref;
}

let dispatch_event t e =
  Lwt_list.iter_p (fun listener -> listener e) !(t.listeners)

let register_listeners t fs =
  t.listeners := !(t.listeners) @ fs;
  t

let show_help () = Lwt_io.printl "Commands: /quit, /help"
let fini t = Lwt_io.printl "Quitting..." >>= t.on_fini

type session_listener_factory = Session.t -> listener
(** Creates a listener that needs to capture a [Session.t] because it relies on
    that session to do its job.

    An example is sending a message (network io) which is a callback that should
    be fired on the [ SendMsg ] user event.

    Such a factory should be used when binding with session *)

let create_send_msg_listener s : listener = function
  | SendMsg bs -> Session.send_message s bs
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

let create_slash_cmd_listener t : listener = function
  | SlashCmd ("/quit", _) -> fini t
  | SlashCmd ("/help", _) -> show_help ()
  | SlashCmd (cmd, _) -> Lwt_io.printlf "Unknown command: %s\n" cmd
  | _ -> Lwt.return ()
[@@warning "-4"]
(* Listener pattern: User events of type [user_event] get dispatched to a collection of
    listeners. These listeners are like observers that ignore cases that don't
    matter to them. Hence the shim. This function only cares about [ SlashCmd ] *)

let user_event_listener_factories : user_event_listener_factory list =
  [ create_slash_cmd_listener ]

let init_console t =
  let user_event_listeners =
    List.map (fun factory -> factory t) user_event_listener_factories
  in
  register_listeners t user_event_listeners

let create ~ic ~oc ~on_fini =
  (* create the skeleton, init state, allow non-session bound listeners to exist *)
  let c = { session = None; listeners = ref []; ic; oc; on_fini } in
  init_console c

(** Formats the payload (exact, raw byte segment) for display.*)
let format_msg_rx_bs payload =
  let len = Bytes.length payload in
  let formatted_payload = Bytes.create (len + 1) in
  Bytes.blit payload 0 formatted_payload 0 len;
  Bytes.set formatted_payload len '\n';
  formatted_payload

let make_console_rx_callbacks { oc; _ } =
  (* TODO [ERR:] stdout write failures to be handled (e.g. piping) -- treat as shutdown signal *)
  let on_rx_msg rx_bs =
    let formatted_rx_payload = format_msg_rx_bs rx_bs in
    Lwt_io.write_from_exactly oc formatted_rx_payload 0 (Bytes.length rx_bs)
  in
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
let parse_user_input line = Some (SendMsg (String.to_bytes line))

(* TODO: [ERR] EOF on stdin (ctrl-D) -- should send up to caller to figure out. Nevertheless --  EOF on stdin in server mode shut down the server *)
let console_loop t =
  let rec loop () =
    let%lwt line = Lwt_io.read_line t.ic in
    (match parse_user_input line with
      | None -> Lwt.return ()
      | Some e -> dispatch_event t e)
    >>= fun () -> loop ()
  in
  try%lwt loop () (* TODO: [ERR] handle custom errors for console here *)
  with End_of_file ->
    let%lwt () = dispatch_event t UserEof in
    Lwt.return ()

let run t =
  Lwt_io.printl "Running console..." >>= fun () ->
  let thunk = fun () -> console_loop t in
  let cleaner_thunk = fun () -> fini t in
  Lwt.finalize thunk cleaner_thunk
