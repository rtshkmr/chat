open Lwt.Infix

type network_config = { port : int; bind : string; timeout : int }

type terminal_config = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  log_level : Logs.level;
}

let make_terminal_config ?(ic = Lwt_io.stdin) ?(oc = Lwt_io.stdout)
    ?(log_level = Logs.Info) () : terminal_config =
  { ic; oc; log_level }

let init_server_socket ~port ~bind =
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
      Lwt.return server_socket

(* TODO: clean this up, use pp functions everywhere so that the logic is clean *)
let run_with_socket ~sock ?(terminal = make_terminal_config ()) ~net () =
  (* TODO:[POLISH] add hello, use the oc from the terminal *)
  (* let starting_msg = *)
  (*   Printf.sprintf "Server listening on %s:%d\n" net.bind net.port *)
  (* in *)
  let console = Console.create ~ic:terminal.ic ~oc:terminal.oc () in
  let rec accept_loop () =
    try%lwt
      let%lwt client_socket, _addr = Lwt_unix.accept sock in
      let net_ic =
        Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input client_socket
      in
      let net_oc =
        Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output client_socket
      in
      let session = Session.create ~ic:net_ic ~oc:net_oc () in
      let _console = Console.bind_session console ~session in
      let per_session_cleanup () =
        let _ = Console.unbind_session console in
        let%lwt () = try%lwt Lwt_io.close net_oc with _ -> Lwt.return_unit in
        let%lwt () = try%lwt Lwt_io.close net_ic with _ -> Lwt.return_unit in
        try%lwt Lwt_unix.close client_socket with _ -> Lwt.return_unit
      in
      let%lwt () =
        try%lwt
          Lwt.finalize (fun () -> Session.run session) per_session_cleanup
        with
        | Session.Session_exit reason ->
            let msg =
              Format.asprintf "[Session exited: %a]" Session.pp_exit_reason
                reason
            in
            Lwt_io.eprintf "%s\n" msg
        | exn ->
            Lwt_io.eprintf "[Session unexpected: %s]\n" (Printexc.to_string exn)
      in
      accept_loop ()
    with Lwt.Canceled -> Lwt.return_unit
  in
  let thunk () = Lwt.pick [ accept_loop (); Console.run console ] in
  let fini () = try%lwt Lwt_unix.close sock with _ -> Lwt.return_unit in
  try%lwt Lwt.finalize thunk fini
  with Console.Console_exn e ->
    let msg = Format.asprintf "[Console exit: %a]" Console.pp_error e in
    Lwt_io.eprintf "%s\n" msg
[@@warning "-27"]
(* TODO:the ~net is not used. Come back to this when handling the connection meta stuff! *)

(* TODO:custom display *)
let run ?(terminal = make_terminal_config ()) ~(net : network_config) () =
  try%lwt
    let%lwt sock = init_server_socket ~port:net.port ~bind:net.bind in
    run_with_socket ~sock ~terminal ~net ()
  with
  | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Lwt_io.eprintf "Server Error: Port %d is already in use\n" net.port
      >>= fun () -> Lwt.fail_with "port in use"
  | Unix.Unix_error (Unix.EACCES, _, _) ->
      Lwt_io.eprintf
        "Error: Permission to run on port %d denied (ports < 1024 need root)\n"
        net.port
      >>= fun () -> Lwt.fail_with "permission denied"
  | Failure msg ->
      Lwt_io.eprintf "Error: %s\n" msg >>= fun () -> Lwt.fail_with msg
  | e ->
      Lwt_io.eprintf "Unexpected server error: %s\n" (Printexc.to_string e)
      >>= fun () -> Lwt.fail e
[@@warning "-27-4"]
(* Ignore warning 4: The fragile pattern match on [ Unix.error ] is fine because we only care about some of the error types*)
(*-- TODO: [STUB] wire up log levels and conn timeout *)
