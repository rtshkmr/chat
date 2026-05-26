module D = Display

type network_config = { port : int; bind : string; timeout : int }

type terminal_config = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  log_level : Logs.level;
}

type event =
  | Listening of (string * int)
  | Client_connected
  | Client_disconnected
  | Shutting_down

type error =
  | Port_in_use of int
  | Perms_denied of int
  | Resolve_failed of (string * int)
  | Unexpected of exn

exception Server_error of error

(* TODO [PP] the pp for formatting time tag segment should be universal. It's not currently that's why it's all over the place (within server, within session...) --- this is a cleanup on the usage of formatters and pp-functions -- need to use it better and compose things up *)
let pp_event fmt = function
  | Listening (bind, port) ->
      Format.fprintf fmt "◎ %a <server> listening on %s:%d\n" D.pp_prompt_now ()
        bind port
  | Client_connected ->
      Format.fprintf fmt "◉ %a <server> client connected\n" D.pp_prompt_now ()
  | Client_disconnected ->
      Format.fprintf fmt "◎ %a <server> client disconnected, awaiting next\n"
        D.pp_prompt_now ()
  | Shutting_down ->
      Format.fprintf fmt "◎ %a <server> shutting down\n" D.pp_prompt_now ()

let pp_error fmt = function
  | Port_in_use port ->
      Format.fprintf fmt "◎ %a <server:error> port %d already in use\n"
        D.pp_prompt_now () port
  | Perms_denied port ->
      Format.fprintf fmt
        "◎ %a <server:error> permission denied for port %d (ports <1024 need \
         root\n\
         )"
        D.pp_prompt_now () port
  | Resolve_failed (bind, port) ->
      Format.fprintf fmt
        "◎ %a <server:error> failed to resolve bind address %s:%d\n"
        D.pp_prompt_now () bind port
  | Unexpected e ->
      Format.fprintf fmt "◎ %a <server:error> unexpected: %s\n" D.pp_prompt_now
        () (Printexc.to_string e)

let make_terminal_conf ?(ic = Lwt_io.stdin) ?(oc = Lwt_io.stdout)
    ?(log_level = Logs.Info) () : terminal_config =
  { ic; oc; log_level }

let init_server_socket ~port ~bind =
  let%lwt addr_info =
    Lwt_unix.getaddrinfo bind (string_of_int port) [ Unix.AI_FAMILY PF_INET ]
  in
  match addr_info with
  | [] -> Lwt.fail (Server_error (Resolve_failed (bind, port)))
  | addr :: _ ->
      let server_socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
      let%lwt () = Lwt_unix.bind server_socket addr.ai_addr in
      Lwt_unix.listen server_socket 1;
      Lwt.return server_socket

let run_single_session ~console ~client_sock =
  let net_ic = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input client_sock in
  let net_oc = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output client_sock in
  let conn_sock = Some client_sock in
  let session = Session.create ~ic:net_ic ~oc:net_oc ~conn_sock () in
  Console.bind_session console ~session;
  let fini_chat_session () =
    Console.unbind_session console;
    let%lwt () = try%lwt Lwt_io.close net_oc with _ -> Lwt.return_unit in
    let%lwt () = try%lwt Lwt_io.close net_ic with _ -> Lwt.return_unit in
    try%lwt Lwt_unix.close client_sock with _ -> Lwt.return_unit
  in
  let session_run_task () = Session.run session in
  try%lwt Lwt.finalize session_run_task fini_chat_session with
  | Session.Session_exit reason -> D.eprintf_pp Session.pp_exit_reason reason
  | exn -> Lwt_io.eprintf "[Session unexpected: %s]\n" (Printexc.to_string exn)

let run_with_socket ~sock ?(term = make_terminal_conf ()) ~net:_ () =
  let console = Console.create ~ic:term.ic ~oc:term.oc () in
  let rec accept_chat_loop () =
    try%lwt
      let%lwt client_sock, _ = Lwt_unix.accept sock in
      (* TODO[POLISH, UI] put the hello chat banner here, at bind *)
      let%lwt () = D.write_pp term.oc pp_event Client_connected in
      let%lwt () = run_single_session ~console ~client_sock in
      let%lwt () = D.write_pp term.oc pp_event Client_disconnected in
      accept_chat_loop ()
    with Lwt.Canceled -> Lwt.return_unit
  in
  let run_server () = Lwt.pick [ accept_chat_loop (); Console.run console ] in
  let fini_server () =
    try%lwt
      let%lwt () = D.write_pp term.oc pp_event Shutting_down in
      Lwt_unix.close sock
    with _ -> Lwt.return_unit
  in
  try%lwt Lwt.finalize run_server fini_server
  with Console.Console_exn e -> D.eprintf_pp Console.pp_error e

let run ?(term = make_terminal_conf ()) ~(net : network_config) () =
  let exit_with err = D.eprintf_pp pp_error err in
  try%lwt
    let%lwt sock = init_server_socket ~port:net.port ~bind:net.bind in
    let ev = Listening (net.bind, net.port) in
    let%lwt () = D.write_pp term.oc pp_event ev in
    run_with_socket ~sock ~term ~net ()
  with
  | Unix.Unix_error (Unix.EADDRINUSE, _, _) -> exit_with (Port_in_use net.port)
  | Unix.Unix_error (Unix.EACCES, _, _) -> exit_with (Perms_denied net.port)
  | Server_error e -> exit_with e
  | e -> exit_with (Unexpected e)
[@@warning "-4"]
(* Ignore warning 4: The fragile pattern match on [ Unix.error ] is fine because we only care about some of the error types*)
