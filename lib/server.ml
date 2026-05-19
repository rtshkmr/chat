(* TODO: [ERR] add proper error/exception handling:
   1. state logic errors --
      i) EOF @ read (client disconnection gracefully)
     ii) Input issues e.g. ip addr
   2. expected failures to be handled: (also to be logged)
      i) connection refused @ client by server (server not running)
     ii) bind failure (port already in use)
    iii) read timeout on dead socket
   3. Unexpected faults for bugs -- probably can let it bubble up to the outermost harness
      i) invariant violations -- assertions maybe
     ii) resource exhaustion that can't be handled
*)
open Lwt.Infix

let init_server_socket ~port ~bind =
  let%lwt () =
    Lwt_io.printlf "Starting server, to listen on %s:%d\n" bind port
  in
  let%lwt addr_info =
    Lwt_unix.getaddrinfo bind (string_of_int port) [ Unix.AI_FAMILY PF_INET ]
  in
  match addr_info with
  | [] ->
      let m =
        Printf.sprintf
          "Failed to resolve bind address when starting server at %s:%d" bind
          port
      in
      Lwt.fail_with m
  | addr :: _ ->
      let server_socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
      let%lwt () = Lwt_unix.bind server_socket addr.ai_addr in
      Lwt_unix.listen server_socket 1;
      let%lwt () = Lwt_io.printlf "Server listening on %s:%d\n" bind port in
      Lwt.return server_socket

let init_session client_socket =
  let ic = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input client_socket in
  let oc = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output client_socket in
  let on_fini () =
    Lwt_io.close oc >>= fun () ->
    Lwt_io.close ic >>= fun () -> Lwt_unix.close client_socket
  in
  Session.create ~ic ~oc ~on_fini ()

(* TODO handle custom errors for known cases *)
let run_server port bind =
  let%lwt server_socket = init_server_socket ~port ~bind in
  let console = Console.create () in
  let rec accept_loop () =
    try%lwt
      let%lwt () = Lwt_io.printl "Awaiting chat connections..." in
      let%lwt client_socket, _addr = Lwt_unix.accept server_socket in
      let session = init_session client_socket in
      let console = Console.bind_session console ~session in
      let%lwt () = Session.run session in
      let _ = Console.unbind_session console in
      accept_loop ()
    with Lwt.Canceled -> Lwt.return_unit
  in
  let thunk () = Lwt.pick [ accept_loop (); Console.run console ] in
  let fini () =
    Lwt_unix.close server_socket >>= fun () ->
    Lwt_io.printl "[Server finalised]"
  in
  Lwt.finalize thunk fini

let run ~port ~bind ~timeout ~log_level =
  try%lwt run_server port bind with
  | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Lwt_io.eprintf "Server Error: Port %d is already in use\n" port
      >>= fun () -> Lwt.fail_with "port in use"
  | Unix.Unix_error (Unix.EACCES, _, _) ->
      Lwt_io.eprintf
        "Error: Permission to run on port %d denied (ports < 1024 need root)\n"
        port
      >>= fun () -> Lwt.fail_with "permission denied"
  | Failure msg ->
      Lwt_io.eprintf "Error: %s\n" msg >>= fun () -> Lwt.fail_with msg
  | e ->
      Lwt_io.eprintf "Unexpected error: %s\n" (Printexc.to_string e)
      >>= fun () -> Lwt.fail e
[@@warning "-27-4"]
(* Ignore warning 4: The fragile pattern match on [ Unix.error ] is fine because we only care about some of the error types*)
(*-- TODO: [STUB] wire up log levels and conn timeout *)

(* QQ: what happens if server is shutdown? then will the underlying session and console also be gracefully shut down? *)
