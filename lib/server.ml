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
(* FIXME: [1-LIFECYCLE] there's an issue here that console run should be running for the
    whole server's lifetime. The problem here is that we're calling Console.run
    here again, it shouldn't be the case because we just need to call
    Console.run once. *)
(* FIXME: [2-LIFECYCLE] Server lifecycle on EOF

   Currently, if the server operator presses Ctrl-D (EOF on stdin), the console
   reads End_of_file, Console.run exits, which calls on_kill_console and closes
   stdin/stdout. This is acceptable for now (interpreted as "server shutdown
   signal"), but we should handle it more explicitly:

   Option 1: Catch EOF in Console.run and return gracefully without calling on_kill
   Option 2: Add a /shutdown command and ignore EOF
   Option 3: Treat EOF as a signal to close all connections and exit cleanly

   For now, we rely on the /quit command for controlled shutdown and defer this problem.
*)
open Lwt.Infix

let close_io_channels ic oc = Lwt_io.close ic >>= fun () -> Lwt_io.close oc

let make_console_on_kill ~console_ic ~console_oc =
 fun () ->
  close_io_channels console_ic console_oc >>= fun () ->
  Lwt_io.printl "[Disconnected from console]"

let make_net_on_kill ~net_ic ~net_oc =
 fun () ->
  close_io_channels net_ic net_oc >>= fun () ->
  Lwt_io.printl "[Disconnected from network]"

let init_server_socket ~port ~bind =
  let%lwt () =
    Lwt_io.printlf "Starting server, to listen on %s:%d\n" bind port
  in
  let inet_addr = Unix.inet_addr_of_string bind in
  let sockaddr = Unix.(ADDR_INET (inet_addr, port)) in
  let server_socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let%lwt () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 1;
  let%lwt () = Lwt_io.printlf "Server listening on %s:%d\n" bind port in
  Lwt.return server_socket

let init_console () =
  let console_ic = Lwt_io.stdin in
  let console_oc = Lwt_io.stdout in
  let on_kill_console = make_console_on_kill ~console_ic ~console_oc in
  Console.create ~ic:console_ic ~oc:console_oc ~on_kill:on_kill_console

let handle_client_connection console client_socket =
  let net_ic = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let net_oc = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
  let on_kill_net = make_net_on_kill ~net_ic ~net_oc in
  let session =
    Session.create ~ic:net_ic ~oc:net_oc ~on_kill:on_kill_net ~callbacks:None
  in
  let console = Console.bind_session console ~session in
  let thunk = fun () -> Lwt.join [ Session.run session; Console.run console ] in
  let session_cleaner_thunk = fun () -> Session.kill session in

  let%lwt () = Lwt.finalize thunk session_cleaner_thunk in
  let console = Console.unbind_session console in
  let%lwt () = Lwt_io.printl "Waiting for next client..." in
  Lwt.return console

(* TODO handle custom errors for known cases *)
(* TODO start server needs a finalizer, so that console can be killed properly *)
let start_server port bind =
  let%lwt server_socket = init_server_socket ~port ~bind in
  let rec accept_loop console =
    let%lwt client_socket, _addr = Lwt_unix.accept server_socket in
    let%lwt console = handle_client_connection console client_socket in
    accept_loop console
  in
  init_console () |> accept_loop

let run ~port ~bind ~timeout ~log_level =
  try%lwt start_server port bind
  with e ->
    let%lwt () = Lwt_io.eprintf "Server error: %s\n" (Printexc.to_string e) in
    Lwt.fail e
[@@warning "-27"]
(*-- TODO: [POLISH] wire up log levels and conn timeout *)
