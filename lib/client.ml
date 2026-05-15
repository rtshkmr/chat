open Lwt
open Lwt.Infix

let echo_client host port =
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
  let rec send_messages () =
    let%lwt message = Lwt_io.read_line Lwt_io.stdin in
    let%lwt () = Lwt_io.write_line output message in
    let%lwt response = Lwt_io.read_line input in
    let%lwt () = Lwt_io.printlf "Server replied: %s" response in
    send_messages ()
  in
  send_messages ()

let run ~host ~port ~timeout ~log_level =
  let%lwt () = Lwt_io.printlf "Connecting to %s:%d\n" host port in
  try%lwt echo_client host port
  with e ->
    let%lwt () = Lwt_io.eprintf "Client error: %s\n" (Printexc.to_string e) in
    Lwt.fail e
