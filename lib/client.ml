open Lwt.Infix

type network_config = { port : int; host : string; timeout : int }

type terminal_config = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  log_level : Logs.level;
}

let make_terminal_config ?(ic = Lwt_io.stdin) ?(oc = Lwt_io.stdout)
    ?(log_level = Logs.Info) () : terminal_config =
  { ic; oc; log_level }

let init_client_socket ~host ~port =
  let%lwt addr_info =
    Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]
  in
  match addr_info with
  | [] -> Lwt.fail_with (Printf.sprintf "Failed to resolve host %s" host)
  | addr :: _ ->
      let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
      let%lwt () = Lwt_unix.connect socket addr.Unix.ai_addr in
      Lwt.return socket

let init_session sock =
  let ic = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input sock in
  let oc = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output sock in
  let on_fini () = Lwt_io.close oc >>= fun () -> Lwt_io.close ic in
  Session.create ~ic ~oc ~on_fini ()

let run_client ~host ~port =
  let%lwt sock = init_client_socket ~host ~port in
  let session = init_session sock in
  let console = Console.create () |> Console.bind_session ~session in
  let thunk () = Lwt.pick [ Session.run session; Console.run console ] in
  let fini () =
    Lwt_unix.close sock >>= fun () -> Lwt_io.printl "[Finalised client]"
  in
  Lwt.finalize thunk fini

let run ?(terminal = make_terminal_config ()) ~(net : network_config) () =
  let%lwt () =
    Lwt_io.printlf "Connecting client to %s:%d\n" net.host net.port
  in
  try%lwt run_client ~host:net.host ~port:net.port with
  | Unix.Unix_error (Unix.EACCES, _, _) ->
      Lwt_io.eprintf
        "Error: Permission to run on port %d denied (ports < 1024 need root)\n"
        net.port
      >>= fun () -> Lwt.fail_with "permission denied"
  | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
      Lwt_io.eprintf "Error: Connection refused. Is server running @ %s:%d.\n"
        net.host net.port
      >>= fun () -> Lwt.fail_with "connection refused"
  | Failure msg ->
      Lwt_io.eprintf "Error: %s\n" msg >>= fun () -> Lwt.fail_with msg
  | e ->
      Lwt_io.eprintf "Unexpected error: %s\n" (Printexc.to_string e)
      >>= fun () -> Lwt.fail e
[@@warning "-27-4"]
(* Ignore warning 4: The fragile pattern match on [ Unix.error ] is fine because we only care about some of the error types*)
(*-- TODO: [STUB] wire up log levels and conn timeout. timeout needs to be used so need auto-cancellations and all that *)
