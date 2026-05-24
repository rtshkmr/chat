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

(** TODO: [FINI] client should close the underlying socket her most likely else
    there's 2 cleanup sites for the same resource hierarchy — only the channels.
    But those channels were created with ~close:Lwt.return which means closing
    them doesn't close the fd. So sock never gets properly closed via on_fini.
    The fini () in run_client closes sock, so there's a correct cleanup path,
    but it's only because of Lwt.finalize at the supervisor/harness level *)
let init_session sock =
  let ic = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input sock in
  let oc = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output sock in
  let on_fini () = Lwt_io.close oc >>= fun () -> Lwt_io.close ic in
  Session.create ~ic ~oc ~on_fini ()

let run_client ~terminal ~net =
  let%lwt sock = init_client_socket ~host:net.host ~port:net.port in
  (* TODO [POLISH]: add an endpoint struct with connection info about the chat that can be used for the irc-like promp *)
  let session = init_session sock in
  let ic, oc = (terminal.ic, terminal.oc) in
  let console = Console.create ~ic ~oc () |> Console.bind_session ~session in
  let thunk () = Lwt.pick [ Session.run session; Console.run console ] in

  let fini () =
    let%lwt () = try%lwt Lwt_unix.close sock with _ -> Lwt.return_unit in
    try%lwt Lwt_io.write_line oc "[Finalised client]"
    with _ -> Lwt.return_unit
  in

  Lwt.finalize thunk fini

(* let fini () = *)
(*   ( try%lwt Lwt_unix.close sock with _ -> Lwt.return_unit in *)
(*   try%lwt Lwt_io.write_line oc "[Finalised client]" with _ -> Lwt.return_unit ) in *)
(* Lwt.finalize thunk fini *)

(* Lwt.catch (fun () -> Lwt_unix.close sock) (fun _ -> Lwt.return_unit) *)
(* >>= fun () -> *)
(* Lwt.catch *)
(*   (fun () -> Lwt_io.write_line oc "[Finalised client]") *)
(*   (fun _ -> Lwt.return_unit) *)
(* in *)
(* let fini () = *)
(*   Lwt_unix.close sock >>= fun () -> Lwt_io.write_line oc "[Finalised client]" *)
(* in *)
(* Lwt.finalize thunk fini *)

let run ?(terminal = make_terminal_config ()) ~(net : network_config) () =
  let%lwt () =
    Lwt_io.write_line terminal.oc
      (Printf.sprintf "Connecting client to %s:%d\n" net.host net.port)
  in
  try%lwt run_client ~terminal ~net with
  | Unix.Unix_error (Unix.EACCES, _, _) ->
      let%lwt () =
        Lwt_io.eprintf
          "Error: Permission to run on port %d denied (ports < 1024 need root)\n"
          net.port
      in
      Lwt.fail_with "permission denied"
  | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
      let%lwt () =
        Lwt_io.eprintf "Error: Connection refused. Is server running @ %s:%d.\n"
          net.host net.port
      in
      Lwt.fail_with "connection refused"
  | Failure msg ->
      let%lwt () = Lwt_io.eprintf "Error: %s\n" msg in
      Lwt.fail_with msg
  | e ->
      let%lwt () =
        Lwt_io.eprintf "Unexpected client error: %s\n" (Printexc.to_string e)
      in
      Lwt.fail e
[@@warning "-4"]
(* Ignore warning 4: The fragile pattern match on [ Unix.error ] is fine because we only care about some of the error types*)
(*-- TODO: [STUB] wire up log levels and conn timeout. timeout needs to be used so need auto-cancellations and all that *)
