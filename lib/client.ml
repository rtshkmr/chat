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
        let%lwt () = Lwt_io.printl "[Server closed connection]" in
        Lwt.fail End_of_file
    | Some line ->
        let%lwt () = Lwt_io.printlf "<server>: %s\n" line in
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

let handle_chat_session input output =
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
      let%lwt () = Lwt_io.printl "[Disconnected]" in
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

let start_chat host port =
  let%lwt () = Lwt_io.printlf "Connecting to %s:%d..." host port in
  let%lwt addr_info =
    Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]
  in
  let%lwt addr =
    match addr_info with
    | [] -> Lwt.fail_with "failed to resolve host"
    | addr :: _ -> Lwt.return addr.Unix.ai_addr
  in

  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let%lwt () = Lwt_unix.connect socket addr in

  let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let%lwt () = Lwt_io.printl "Connected successfully!" in

  let handler_thunk = fun () -> handle_chat_session input output in
  let cleaner_thunk =
   fun () ->
    let%lwt () = Lwt_io.close input in
    let%lwt () = Lwt_io.close output in
    Lwt_io.printl "Connection resources cleaned up"
  in
  Lwt.finalize handler_thunk cleaner_thunk

let run ~host ~port ~timeout ~log_level =
  try%lwt start_chat host port
  with e ->
    let%lwt () = Lwt_io.eprintf "Client error: %s\n" (Printexc.to_string e) in
    Lwt.fail e
