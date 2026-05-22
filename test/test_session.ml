open Helpers.Test_helpers
module F = Chat.Frame
module S = Chat.Session
module FR = Chat.Frame_reader
module In = Helpers.Test_input
module ALWT = Alcotest_lwt

let test_ignores_spurious_acks =
  ALWT.test_case "On rcving ack for unknown msg_id, warns and continues sesion"
    `Quick (fun switch () ->
      let { rd = net_ic; wr = net_oc; _ } = make_pipe_with_switch switch in
      (* let cap = capture_output switch in *)
      let no_op_cb = fun _ -> Lwt.return_unit in
      let callbacks =
        Some
          {
            S.on_rx_msg = no_op_cb;
            on_rx_ack = (fun _ _ -> Lwt.return_unit);
            on_rx_close = no_op_cb;
          }
      in
      let session =
        S.create ~ic:net_ic ~oc:Lwt_io.null ~callbacks
          ~on_fini:(fun () -> Lwt.return_unit)
          ()
      in

      let%lwt () = ack 999l |> write_frame net_oc in
      let%lwt () = close_frame |> write_frame net_oc in

      (* TODO: [LOGGING] Once logging is integrated (Logs + ?err_oc parameter),
         capture and assert that a warning was logged for spurious ack *)
      let%lwt () = S.run session in
      Lwt.return_unit)

let defensive_tests = [ test_ignores_spurious_acks ]
let suite = [ ("Session/RX-flow/Defensive-cases", defensive_tests) ]
