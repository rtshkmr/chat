open Lwt.Infix

(* TODO: [ERR] ECONNREFUSED @ connect — Client connecting to a server that's not running -- just print helpful message and init fini? *)
(* TODO: [ERR] dns resolution failure @ getaddrinfo -- just print helpful message and init fini? *)
let init_client_socket ~host ~port =
  let%lwt addr_info =
    Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]
  in
  match addr_info with
  | [] -> Lwt.fail_with "Failed to resolve host"
  | addr :: _ ->
      let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
      let%lwt () = Lwt_unix.connect socket addr.Unix.ai_addr in
      Lwt.return socket

let init_session sock =
  let ic = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input sock in
  let oc = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output sock in
  let on_fini () =
    Lwt_io.close oc >>= fun () ->
    Lwt_io.close ic >>= fun () -> Lwt_unix.close sock
  in
  Session.create ~ic ~oc ~on_fini ()

let run_session ~host ~port =
  let%lwt client_socket = init_client_socket ~host ~port in
  let session = init_session client_socket in
  let console = Console.create () |> Console.bind_session ~session in
  Lwt.pick [ Session.run session; Console.run console ]

(* TODO: client and server need fini functions as well for graceful shutdowns (where they'll call the session / console finis also) *)
(* TODO: [STUB] timeout needs to be used so need auto-cancellations and all that *)
let run ~host ~port ~timeout ~log_level =
  let%lwt () = Lwt_io.printlf "Connecting client to %s:%d\n" host port in
  try%lwt run_session ~host ~port
  with e ->
    let%lwt () = Lwt_io.eprintf "Client error: %s\n" (Printexc.to_string e) in
    Lwt.fail e
[@@warning "-27"]
(*-- TODO: [STUB] wire up log levels and conn timeout *)
