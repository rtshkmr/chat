(* TODO: add proper error/exception handling:
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
open Lwt
open Lwt.Infix

let handle_client (input, output) =
  let rec echo_loop () =
    let%lwt line_opt = Lwt_io.read_line_opt input in
    match line_opt with
    | None -> Lwt_io.printl "Client disconnected"
    | Some line ->
        let%lwt () = Lwt_io.printl ("Received: " ^ line) in
        let%lwt () = Lwt_io.write_line output line in
        echo_loop ()
  in
  echo_loop ()

let rec accept_connections server_socket =
  let%lwt client_socket, _addr = Lwt_unix.accept server_socket in
  let input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
  Lwt.async (fun () -> handle_client (input, output));
  accept_connections server_socket

let start_server port bind =
  let inet_addr =
    try Unix.inet_addr_of_string bind
    with _ -> failwith (Printf.sprintf "Invalid bind address: %s" bind)
  in
  let sockaddr = Unix.(ADDR_INET (inet_addr, port)) in
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let%lwt () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 10;
  let%lwt () = Lwt_io.printlf "Server started on %s:%d\n" bind port in
  accept_connections server_socket

let run ~port ~bind ~timeout ~log_level =
  let%lwt () = Lwt_io.printlf "Starting server on %s:%d\n" bind port in
  try%lwt start_server port bind
  with e ->
    let%lwt () = Lwt_io.eprintf "Server error: %s\n" (Printexc.to_string e) in
    Lwt.fail e
