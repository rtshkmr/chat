open Helpers.Test_helpers
open Alcotest
module F = Chat.Frame
module FR = Chat.Frame_reader
module In = Helpers.Test_input
module ALWT = Alcotest_lwt

let streaming_test_case_inputs : In.freader_streaming_input list =
  [
    (* Single-frame: basic reads *)
    {
      name = "single frame: Msg with payload";
      frames = [ msg In.byte_agnostic_torture_string ];
      intent = "Msg frame read correctly";
    };
    {
      name = "single frame: Ack";
      frames = [ ack 99l ];
      intent = "Ack frame read correctly";
    };
    {
      name = "single frame: Close";
      frames = [ close_frame ];
      intent = "Close frame read correctly";
    };
    (* Multi-frame: shows stream-boundary discipline *)
    {
      name = "two frames sequentially";
      frames = [ msg ~id:1l "first"; msg ~id:2l "second" ];
      intent = "sequence preserved, no residual bytes";
    };
    {
      name = "burst of three frames";
      frames = [ msg ~id:10l "msg1"; msg ~id:11l "msg2"; msg ~id:12l "msg3" ];
      intent = "all frames dispatched and received in order";
    };
    {
      name = "mixed frame types: Ack → Msg → Close";
      frames = [ ack 7l; msg ~id:8l "bye"; close_frame ];
      intent = "mixed types demarcated correctly";
    };
  ]

let make_streaming_tc ({ name; frames; intent } : In.freader_streaming_input) =
  ALWT.test_case name `Quick (fun switch () ->
      let { rd = pipe_ic; wr = pipe_oc; _ } = make_pipe_with_switch switch in
      let reader = FR.create pipe_ic in
      let%lwt () = Lwt_list.iter_s (write_frame pipe_oc) frames in
      let%lwt () = Lwt_io.close pipe_oc in

      let%lwt results = Lwt_list.map_s (fun _ -> FR.read_frame reader) frames in

      let rcvd_frames =
        List.filter_map (function Ok f -> Some f | Error _ -> None) results
      in

      if List.length rcvd_frames <> List.length frames then
        Alcotest.failf "%s: expected %d frames, got %d" intent
          (List.length frames) (List.length rcvd_frames);

      check (list frame_testable) intent frames rcvd_frames;
      Lwt.return_unit)

let test_happy_streaming_tests =
  List.map make_streaming_tc streaming_test_case_inputs

let make_edge_case_tc
    ({ name; frame; check_frame } : In.freader_edge_case_input) =
  ALWT.test_case name `Quick (fun switch () ->
      let { rd = pipe_ic; wr = pipe_oc; _ } = make_pipe_with_switch switch in
      let reader = FR.create pipe_ic in
      let%lwt () = write_frame pipe_oc frame in
      let%lwt () = Lwt_io.close pipe_oc in

      let%lwt result = FR.read_frame reader in
      match result with
      | Ok rcvd_frame -> check_frame rcvd_frame
      | Error e ->
          Alcotest.failf "Expected frame, got error: %s" (FR.error_to_string e))

let edge_case_test_inputs : In.freader_edge_case_input list =
  [
    {
      name = "Ack frame (zero payload)";
      frame = ack 12l;
      check_frame =
        (fun result ->
          match result with
          | F.Ack { id } ->
              check int32 "ack id matches" 12l id;
              Lwt.return_unit
          | _ -> Alcotest.fail "Expected Ack");
    };
    {
      name = "Close frame (zero payload)";
      frame = close_frame;
      check_frame =
        (fun result ->
          match result with
          | F.Close -> Lwt.return_unit
          | _ -> Alcotest.fail "Expected Close");
    };
    {
      name = "Msg frame with empty payload";
      frame = msg ~id:15l "";
      check_frame =
        (fun result ->
          match result with
          | F.Msg { payload; _ } ->
              check int "empty payload" 0 (Bytes.length payload);
              Lwt.return_unit
          | _ -> Alcotest.fail "Expected Msg with empty payload");
    };
  ]
[@@warning "-4"]
(* Fragile pattern-matches are alright because our tests extract only one desired test-objective -- single-intent tests*)

let test_edge_cases_zero_payload_tests =
  List.map make_edge_case_tc edge_case_test_inputs

let make_connection_loss_tc
    ({ name; init; check_error } : In.freader_conn_loss_input) =
  ALWT.test_case name `Quick (fun switch () ->
      let { rd = pipe_ic; wr = pipe_oc; fd_wr; _ } =
        make_pipe_with_switch switch
      in
      let reader = FR.create pipe_ic in

      let%lwt () = init pipe_oc in
      let%lwt () = Lwt_io.close pipe_oc in
      let%lwt () = Lwt_unix.close fd_wr in

      let%lwt result = FR.read_frame reader in
      match result with
      | Ok _ ->
          Alcotest.fail
            (Printf.sprintf "Expected error, got Ok. Test intent: %s" name)
      | Error e -> check_error e)

let connection_loss_test_inputs : In.freader_conn_loss_input list =
  [
    {
      name = "EOF before any bytes sent → Connection_lost";
      init = (fun _oc -> Lwt.return_unit);
      check_error =
        (fun error ->
          match error with
          | FR.Connection_lost msg ->
              let intent = "error reason indicates peer closed" in
              check string intent "peer closed" msg;
              Lwt.return_unit
          | _ ->
              Alcotest.failf "Expected Connection_lost, got %s"
                (FR.error_to_string error));
    };
    {
      name = "EOF after 5 of 9 header bytes (partial tx) → Connection_lost";
      init =
        (fun oc ->
          (* msg typ (0) with id (1l) *)
          let partial_header = Bytes.create 5 in
          Bytes.set_uint8 partial_header 0 0;
          Bytes.set_int32_be partial_header 1 1l;
          write_raw oc partial_header);
      check_error =
        (fun error ->
          match error with
          | FR.Connection_lost _ -> Lwt.return_unit
          | _ -> Alcotest.fail "Expected Connection_lost mid-header");
    };
    {
      name = "EOF after header but before claimed payload → Connection_lost";
      init =
        (fun oc ->
          let header = make_raw_frame_bs 0 1l (Bytes.make 100 'x') in
          let header_only = Bytes.sub header 0 F.frame_header_sz in
          let partial_payload = Bytes.make 50 'x' in
          let%lwt () = write_raw oc header_only in
          write_raw oc partial_payload);
      check_error =
        (fun error ->
          match error with
          | FR.Connection_lost _ -> Lwt.return_unit
          | _ -> Alcotest.fail "Expected Connection_lost mid-payload");
    };
  ]
[@@warning "-4"]
(* fragile pattern matches are fine because test cases only care about a singular case (single purpose test cases) *)

let test_conn_loss_eof_tests =
  List.map make_connection_loss_tc connection_loss_test_inputs

let make_protocol_error_tc
    ({ name; frame_bytes; expect_error } : In.freader_protocol_input) =
  ALWT.test_case name `Quick (fun switch () ->
      let { rd = pipe_ic; wr = pipe_oc; _ } = make_pipe_with_switch switch in
      let reader = FR.create pipe_ic in
      let%lwt () = write_raw pipe_oc frame_bytes in

      let%lwt result = FR.read_frame reader in
      match result with
      | Ok _ -> Alcotest.fail "Expected protocol error, got Ok"
      | Error (FR.Protocol_error _e as rcvd_err) -> expect_error rcvd_err
      | Error (FR.Connection_lost msg) ->
          Alcotest.failf "Expected Protocol_error, got Connection_lost: %s" msg)

let protocol_error_test_inputs : In.freader_protocol_input list =
  [
    {
      name = "Unknown frame type (0xFF)";
      frame_bytes = make_raw_frame_bs 0xFF 1l (Bytes.of_string "data");
      expect_error =
        (fun error ->
          match error with
          | FR.Protocol_error (F.Unknown_frame_type 0xFF) -> Lwt.return_unit
          | _ ->
              Alcotest.failf "Expected Unknown_frame_type, got %s"
                (FR.error_to_string error));
    };
    {
      name = "Payload too big (claims max + 1B)";
      frame_bytes =
        (let full_frame =
           make_raw_frame_bs 0 1l (Bytes.make (1 + F.max_payload_sz) 'x')
         in
         (* Only write the header, not the 1MB+ payload *)
         Bytes.sub full_frame 0 F.frame_header_sz);
      expect_error =
        (fun error ->
          match error with
          | FR.Protocol_error (F.Payload_too_big _) -> Lwt.return_unit
          | _ ->
              Alcotest.failf "Expected Payload_too_big, got %s"
                (FR.error_to_string error));
    };
  ]
[@@warning "-4"]
(* Fragile warnings are fine; tcs only test one attribute from variant type *)

let test_protocol_errors_tests =
  List.map make_protocol_error_tc protocol_error_test_inputs

let suite =
  [
    ("Frame_reader/Streaming/Happy-cases", test_happy_streaming_tests);
    ("Frame_reader/Edge-cases/Zero-payload", test_edge_cases_zero_payload_tests);
    ("Frame_reader/Connection-loss/EOF-scenarios", test_conn_loss_eof_tests);
    ("Frame_reader/Protocol-errors", test_protocol_errors_tests);
  ]
