open Helpers.Test_helpers
module F = Chat.Frame
module S = Chat.Session
module FR = Chat.Frame_reader
module In = Helpers.Test_input
module ALWT = Alcotest_lwt

(** Creates a server-side session with the given callbacks, backed by an
    in-process pipe. Returns (session, client_write_end). Tests drive the
    session by writing frames to [client_oc]. *)
let make_session_complete switch ?(callbacks = None) () =
  let ({ rd = net_ic; wr = net_oc; _ } as p) = make_pipe_with_switch switch in
  let session = S.create ~ic:net_ic ~oc:Lwt_io.null ~callbacks () in
  (session, net_oc, p)

let make_session switch ?(callbacks = None) () =
  let session, net_oc, _pipe = make_session_complete switch ~callbacks () in
  (session, net_oc)

let test_ignores_spurious_acks =
  ALWT.test_case "Session continues on Spurious_ack event" `Quick
    (fun switch () ->
      let spurious_msg_id = 181l in
      let dispatched = ref [] in
      let on_rx ev =
        dispatched := ev :: !dispatched;
        Lwt.return_unit
      in
      let callbacks = Some { S.on_rx } in
      let session, client_oc = make_session switch ~callbacks () in
      let%lwt () = ack spurious_msg_id |> write_frame client_oc in
      let%lwt () = close_frame |> write_frame client_oc in
      let%lwt () = S.run session in
      let was_dispatched =
        List.exists
          (function
            | S.Spurious_ack { id; _ } -> id = spurious_msg_id | _ -> false)
          !dispatched
      in
      Alcotest.(check bool) "Spurious_ack was dispatched" true was_dispatched;
      Lwt.return_unit)
[@@warning "-4"]
(* fragile pattern matching on the exit reasons is fine because the test only asserts a single exit condition*)

let test_invariant_violated_on_missing_callbacks =
  ALWT.test_case
    "Guards against programmer errors if session are run without cbs" `Quick
    (fun switch () ->
      let session, client_oc = make_session switch () in
      let%lwt () = msg "💥" |> write_frame client_oc in
      try%lwt
        let%lwt () = S.run session in
        Alcotest.failf "expected Session_invariant_violated, got unit"
      with
      | S.Session_invariant_violated _ -> Lwt.return_unit
      | e -> Alcotest.failf "unexpected exception: %s" (Printexc.to_string e))

let test_lost_conn_on_eof =
  ALWT.test_case "EOF on read side raises Session_exit (Lost_conn _)" `Quick
    (fun switch () ->
      with_timeout (fun () ->
          let on_rx _ = Lwt.return_unit in

          let session, _client_oc, pipe =
            make_session_complete switch ~callbacks:(Some { S.on_rx }) ()
          in

          try%lwt
            let%lwt () = sim_peer_eof pipe in
            let%lwt () = S.run session in
            Alcotest.failf "expected Session_exit (Lost_conn), got unit"
          with
          | S.Session_exit (S.Lost_conn _) -> Lwt.return_unit
          | S.Session_exit other ->
              Alcotest.failf "wrong exit_reason: %a" S.pp_exit_reason other))
[@@warning "-4"]

let test_protocol_error_on_bad_frame =
  ALWT.test_case "Unknown frame type raises Protocol_error" `Quick
    (fun switch () ->
      let on_rx = fun _ -> Lwt.return_unit in
      let session, client_oc =
        make_session switch ~callbacks:(Some { S.on_rx }) ()
      in
      (* Frame type 0xFF is unknown — valid header size, invalid type *)
      let%lwt () =
        write_raw client_oc (make_raw_frame_bs 0xFF 1l Bytes.empty)
      in
      try%lwt
        let%lwt () = S.run session in
        Alcotest.failf "expected Session_exit (Protocol_error), got unit"
      with
      | S.Session_exit (S.Protocol_error _) -> Lwt.return_unit
      | S.Session_exit other ->
          Alcotest.failf "wrong exit_reason: %a" S.pp_exit_reason other)
[@@warning "-4"]

let test_clean_peer_close =
  ALWT.test_case "Close frame makes session shutdown gracefully" `Quick
    (fun switch () ->
      let on_rx = fun _ -> Lwt.return_unit in
      let session, client_oc =
        make_session switch ~callbacks:(Some { S.on_rx }) ()
      in
      let%lwt () = close_frame |> write_frame client_oc in
      S.run session (* must return unit, not raise *))

let test_msg_received_fires_callback =
  ALWT.test_case "Msg frame fires on_rx with correct Msg_received payload"
    `Quick (fun switch () ->
      let received : S.rx_event list ref = ref [] in
      (* collects emitted events *)
      let on_rx ev =
        received := ev :: !received;
        Lwt.return_unit
      in
      let session, client_oc =
        make_session switch ~callbacks:(Some { S.on_rx }) ()
      in
      let payload = "🔥fireball🔥" in
      let%lwt () = msg ~id:42l payload |> write_frame client_oc in
      let%lwt () = close_frame |> write_frame client_oc in
      let%lwt () = S.run session in
      let found =
        List.find_opt
          (function S.Msg_received _ -> true | _ -> false)
          !received
      in
      match found with
      | None -> Alcotest.failf "Msg_received event never emitted"
      | Some (S.Msg_received r) ->
          Alcotest.(check int32) "id" 42l r.id;
          Alcotest.(check string) "payload" payload (Bytes.to_string r.content);
          Lwt.return_unit
      | Some _ -> assert false)
[@@warning "-4"]

let test_ack_received_fires_callback =
  ALWT.test_case "Ack for pending msg fires on_rx with Ack_received + RTT"
    `Quick (fun switch () ->
      let received : S.rx_event list ref = ref [] in
      let on_rx ev =
        received := ev :: !received;
        Lwt.return_unit
      in
      let session, client_oc =
        make_session switch ~callbacks:(Some { S.on_rx }) ()
      in
      (* NOTE: sends first msg to await a pending ack (will be first msg id=1l);
         gets picked up from mvar by tx-loop. This is cuz of the scheduler being
         cooperative and the mvar being empty. *)
      let _send_task = S.send_message session (Bytes.of_string "💥") in
      let%lwt () = ack 1l |> write_frame client_oc in
      let%lwt () = close_frame |> write_frame client_oc in
      let%lwt () = S.run session in
      let found_ack =
        List.find_opt
          (function S.Ack_received _ -> true | _ -> false)
          !received
      in
      match found_ack with
      | None -> Alcotest.failf "Ack_received never fired"
      | Some (S.Ack_received r) ->
          Alcotest.(check int32) "ack id" 1l r.id;
          Alcotest.(check bool) "rtt is positive" true (r.rtt > 0.0);
          Lwt.return_unit
      | Some _ -> Alcotest.failf "test case logic might be wrong")
[@@warning "-4"]

let defensive_tests =
  [ test_ignores_spurious_acks; test_invariant_violated_on_missing_callbacks ]

let exit_reason_tests =
  [
    test_lost_conn_on_eof;
    test_protocol_error_on_bad_frame;
    test_clean_peer_close;
  ]

let rx_dispatch_tests = [ test_msg_received_fires_callback ]

let suite =
  [
    ("Session/RX-flow/Defensive-cases", defensive_tests);
    ("Session/Exit-reasons", exit_reason_tests);
    ("Session/Rx-dispatch", rx_dispatch_tests);
  ]
