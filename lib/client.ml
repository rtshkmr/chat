module D = Display

type network_config = { port : int; host : string; timeout : int }

type terminal_config = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  log_level : Logs.level;
}

type event =
  | Connecting of (string * int)
  | Connected of (string * int)
  | Disconnected

let pp_event fmt = function
  | Connecting (host, port) ->
      Format.fprintf fmt "◎ %a <client> connecting to %s:%d\n" D.pp_prompt_now
        () host port
  | Connected (host, port) ->
      Format.fprintf fmt "◉ %a <client> connected to %s:%d\n" D.pp_prompt_now ()
        host port
  | Disconnected ->
      Format.fprintf fmt "◎ %a <client> disconnected\n" D.pp_prompt_now ()

type error =
  | Conn_refused of (string * int)
  | Perms_denied of int
  | Resolve_failed of (string * int)
  | Unexpected of exn

exception Client_error of error

let pp_error fmt = function
  | Conn_refused (host, port) ->
      Format.fprintf fmt "connection refused at %s:%d — is server running?\n"
        host port
  | Perms_denied port ->
      Format.fprintf fmt "permission denied for port %d\n" port
  | Resolve_failed (host, port) ->
      Format.fprintf fmt "failed to resolve host %s:%d\n" host port
  | Unexpected e -> Format.fprintf fmt "unexpected: %s\n" (Printexc.to_string e)

let make_terminal_conf ?(ic = Lwt_io.stdin) ?(oc = Lwt_io.stdout)
    ?(log_level = Logs.Info) () : terminal_config =
  { ic; oc; log_level }

let init_client_socket ~host ~port =
  let%lwt addr_info =
    Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]
  in
  match addr_info with
  | [] -> Lwt.fail (Client_error (Resolve_failed (host, port)))
  | addr :: _ ->
      let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
      let%lwt () = Lwt_unix.connect socket addr.Unix.ai_addr in
      Lwt.return socket

let run_client ~term ~net =
  let%lwt sock = init_client_socket ~host:net.host ~port:net.port in
  let%lwt () = D.write_pp term.oc pp_event (Connected (net.host, net.port)) in
  let net_ic = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input sock in
  let net_oc = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output sock in
  let conn_sock = Some sock in
  let session = Session.create ~ic:net_ic ~oc:net_oc ~conn_sock () in
  let console = Console.create ~ic:term.ic ~oc:term.oc () in
  Console.bind_session console ~session;
  let thunk () = Lwt.pick [ Session.run session; Console.run console ] in
  let fini () =
    let safe f = try%lwt f () with _ -> Lwt.return_unit in
    let%lwt () = safe (fun () -> Lwt_io.close net_oc) in
    let%lwt () = safe (fun () -> Lwt_io.close net_ic) in
    let%lwt () = safe (fun () -> D.write_pp term.oc pp_event Disconnected) in
    safe (fun () -> Lwt_unix.close sock)
  in
  try%lwt Lwt.finalize thunk fini with
  | Session.Session_exit reason ->
      let msg = Format.asprintf "[Session: %a]" Session.pp_exit_reason reason in
      Lwt_io.eprintf "%s\n" msg
  | Console.Console_exn e ->
      let msg = Format.asprintf "[Console: %a]" Console.pp_error e in
      Lwt_io.eprintf "%s\n" msg

let run ?(term = make_terminal_conf ()) ~net () =
  let%lwt () = D.write_pp term.oc pp_event (Connecting (net.host, net.port)) in
  let exit_with err = D.eprintf_pp pp_error err in
  try%lwt run_client ~term ~net with
  | Unix.Unix_error (Unix.EACCES, _, _) -> exit_with (Perms_denied net.port)
  | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
      exit_with (Conn_refused (net.host, net.port))
  | Client_error e -> exit_with e
  | e -> exit_with (Unexpected e)
[@@warning "-4"]
(* Ignore warning 4: The fragile pattern match on [ Unix.error ] is fine because we only care about some of the error types*)
