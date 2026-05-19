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
  (* TODO: [ERR] to handle invalid bind addr -- input handling *)
  let inet_addr = Unix.inet_addr_of_string bind in
  (* TODO: [ERR] to handle invalid port number -- Port 0, negative, > 65535, or a privileged port (<1024 without root).
           better @ cli validation though
 *)
  let sockaddr = Unix.(ADDR_INET (inet_addr, port)) in
  let server_socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let%lwt () = Lwt_unix.bind server_socket sockaddr in
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

(* TODO: [ERR] EADDRINUSE @ bind — Server port already in use. *)
(* TODO handle custom errors for known cases *)
(* TODO: consider base listener for server mode console when message is sent while no connection received yet *)
(* TODO start server needs a finalizer, so that console can be killed properly *)
let start_server port bind =
  let%lwt server_socket = init_server_socket ~port ~bind in
  let console = Console.create () in
  let rec accept_loop () =
    let%lwt client_socket, _addr = Lwt_unix.accept server_socket in
    let session = init_session client_socket in
    let console = Console.bind_session console ~session in
    let%lwt () = Session.run session in
    let _ = Console.unbind_session console in
    let%lwt () = Lwt_io.printl "Waiting for next client..." in
    accept_loop ()
  in
  Lwt.pick [ accept_loop (); Console.run console ]

(* TODO: client and server need fini functions as well for graceful shutdowns (where they'll call the session / console finis also) *)
let run ~port ~bind ~timeout ~log_level =
  try%lwt start_server port bind
  with e ->
    let%lwt () = Lwt_io.eprintf "Server error: %s\n" (Printexc.to_string e) in
    Lwt.fail e
[@@warning "-27"]
(*-- TODO: [POLISH] wire up log levels and conn timeout *)
