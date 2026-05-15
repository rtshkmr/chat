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

let read_stdin_loop queue =
  let rec loop () =
    let%lwt line = Lwt_io.read_line Lwt_io.stdin in
    let%lwt () = Lwt_mvar.put queue line in
    loop ()
  in
  loop ()

let read_socket_loop input =
  let rec loop () =
    let%lwt line_opt = Lwt_io.read_line_opt input in
    match line_opt with
    | None ->
        (* Socket closed *)
        let%lwt () = Lwt_io.printl "[Connection closed by peer]" in
        Lwt.fail End_of_file
    | Some line ->
        let%lwt () = Lwt_io.printlf "<client>: %s\n" line in
        loop ()
  in
  loop ()

let write_socket_loop output queue =
  let rec loop () =
    let%lwt line = Lwt_mvar.take queue in
    let%lwt () = Lwt_io.write_line output line in
    let%lwt () = Lwt_io.flush output in
    loop ()
  in
  loop ()

let handle_client_conc input output =
  let queue = Lwt_mvar.create_empty () in
  try%lwt
    Lwt.join
      [
        read_stdin_loop queue;
        read_socket_loop input;
        write_socket_loop output queue;
      ]
  with
  | End_of_file ->
      let%lwt () = Lwt_io.printl "[EOF: Connection terminated]" in
      Lwt.return ()
  | Unix.Unix_error (err, _, _) ->
      let%lwt () =
        Lwt_io.eprintf "Socket error: %s\n" (Unix.error_message err)
      in
      Lwt.fail (Failure (Unix.error_message err))
  | e ->
      let%lwt () =
        Lwt_io.eprintf "Unexpected error: %s\n" (Printexc.to_string e)
      in
      Lwt.fail e

let rec accept_loop server_socket =
  let%lwt client_socket, _client_addr = Lwt_unix.accept server_socket in

  let input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in

  let%lwt () = Lwt_io.printlf "Client connected\n" in
  let handler_thunk = fun () -> handle_client_conc input output in
  let cleaner_thunk =
   fun () ->
    let%lwt () = Lwt_io.close input in
    let%lwt () = Lwt_io.close output in
    Lwt_io.printl "Client cleaned up"
  in
  Lwt.finalize handler_thunk cleaner_thunk >>= fun () ->
  Lwt_io.printl "Waiting for next client..." >>= fun () ->
  accept_loop server_socket

let start_server port bind =
  let backlog_capacity = 1 in
  let inet_addr =
    try Unix.inet_addr_of_string bind
    with _ -> failwith (Printf.sprintf "Invalid bind address: %s" bind)
  in
  let sockaddr = Unix.(ADDR_INET (inet_addr, port)) in
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let%lwt () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket backlog_capacity;
  let%lwt () = Lwt_io.printlf "Server started on %s:%d\n" bind port in
  accept_loop server_socket

let run ~port ~bind ~timeout ~log_level =
  let%lwt () = Lwt_io.printlf "Starting server on %s:%d\n" bind port in
  try%lwt start_server port bind
  with e ->
    let%lwt () = Lwt_io.eprintf "Server error: %s\n" (Printexc.to_string e) in
    Lwt.fail e
