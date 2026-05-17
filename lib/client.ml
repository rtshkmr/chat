open Lwt
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

let init_console () =
  let ic = Lwt_io.stdin in
  let oc = Lwt_io.stdout in
  let on_kill = make_console_on_kill ~console_ic:ic ~console_oc:oc in
  Console.create ~ic ~oc ~on_kill

let init_session sock =
  let net_ic = Lwt_io.of_fd ~mode:Lwt_io.input sock in
  let net_oc = Lwt_io.of_fd ~mode:Lwt_io.output sock in
  let on_kill = make_net_on_kill ~net_ic ~net_oc in
  Session.create ~ic:net_ic ~oc:net_oc ~on_kill ~callbacks:None

let run_session ~host ~port =
  let%lwt client_socket = init_client_socket ~host ~port in
  let session = init_session client_socket in
  let console = init_console () |> Console.bind_session ~session in

  let thunk = fun () -> Lwt.join [ Session.run session; Console.run console ] in
  let cleaner_thunk = fun () -> Session.kill session in

  Lwt.finalize thunk cleaner_thunk

(* TODO: timeout needs to be used so need auto-cancellations and all that *)
let run ~host ~port ~timeout ~log_level =
  let%lwt () = Lwt_io.printlf "Connecting client to %s:%d\n" host port in
  try%lwt run_session ~host ~port
  with e ->
    let%lwt () = Lwt_io.eprintf "Client error: %s\n" (Printexc.to_string e) in
    Lwt.fail e
