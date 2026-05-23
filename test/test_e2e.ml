open Helpers.E2e_harness
module F = Chat.Frame
module S = Chat.Session
module FR = Chat.Frame_reader
module In = Helpers.Test_input
module ALWT = Alcotest_lwt

let teardown_wait_for = 0.001
let short = 0.5

let test_uni_directional_exchange =
  let tc_name = "[1-dir] server sends → client acks" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let line = "hello from server" in
        let%lwt () = type_as_server env line in
        let%lwt () = verify_line_displayed_by_client env line in
        let%lwt () = verify_line_displayed_by_server env "Acked" in
        let%lwt () = teardown_e2e env in
        Lwt.return_unit
      in
      timed short thunk)

let test_basic_ping_pong =
  let tc_name = "ping-pong test with ACKs" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let c_to_s_msg = "client_to_server" in
        let%lwt () = type_as_client env c_to_s_msg in
        let%lwt () = verify_line_displayed_by_server env c_to_s_msg in
        let%lwt () = verify_line_displayed_by_client env "Acked" in
        let s_to_c_msg = "server_to_client" in
        let%lwt () = type_as_server env s_to_c_msg in
        let%lwt () = verify_line_displayed_by_client env s_to_c_msg in
        let%lwt () = verify_line_displayed_by_server env "Acked" in
        let%lwt () = teardown_e2e env in
        Lwt.return_unit
      in
      timed short thunk)

let test_lifecycle_server_awaits_new_client_after_session_terminates =
  let tc_name =
    "Aft conn is terminated by the client, server waits for another client."
  in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        (* Simulates clean client exit: EOF on stdin causes Console.run to
            exit, which cancels Session.run — TCP socket is closed WITHOUT a
            Close frame (abrupt disconnect from the server's perspective). *)
        let teardown () = teardown_client env in
        let cooldown () = Lwt_unix.sleep teardown_wait_for in
        let%lwt () = Lwt.join [ teardown (); cooldown () ] in
        let%lwt () = Lwt_unix.sleep teardown_wait_for in
        let%lwt () = type_as_server env "/quit" in
        let%lwt () =
          verify_line_displayed_by_server env "currently not in any chat"
        in
        let%lwt () = teardown_e2e env in
        Lwt.return_unit
      in
      timed short thunk)

let test_lifecycle_server_accepts_new_client_after_session_terminates =
  let tc_name =
    "server accepts new conn after an existing chat session terminates"
  in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let teardown () = teardown_client env in
        let cooldown () = Lwt_unix.sleep teardown_wait_for in
        let%lwt () = Lwt.join [ teardown (); cooldown () ] in
        let%lwt () = type_as_server env "/quit" in
        let%lwt () =
          verify_line_displayed_by_server env "currently not in any chat"
        in
        let%lwt client2 = spawn_client switch ~port:env.port in
        let updated_env = { env with client = client2 } in
        let c2_msg = "Hello, I'm client 2" in
        let%lwt () = type_as_client updated_env c2_msg in
        let%lwt () = verify_line_displayed_by_server env c2_msg in

        let%lwt () = teardown_e2e updated_env in
        Lwt.return_unit
      in
      timed short thunk)

let test_lifecycle_server_exit_kills_session_and_server =
  let tc_name =
    "Server's /exit terminates the server (even w existing session)"
  in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        (* Simulates clean client exit: EOF on stdin causes Console.run to
            exit, which cancels Session.run — TCP socket is closed WITHOUT a
            Close frame (abrupt disconnect from the server's perspective). *)
        let initiate_exit () = type_as_server env "/exit" in
        let cooldown () = Lwt_unix.sleep teardown_wait_for in
        let%lwt () = Lwt.join [ initiate_exit (); cooldown () ] in
        let%lwt () = Lwt_unix.sleep teardown_wait_for in
        let%lwt () = verify_line_displayed_by_server env "Quitting" in
        let%lwt () = teardown_e2e env in
        Lwt.return_unit
      in
      timed short thunk)

let test_client_eof_clean_disconnect =
  let tc_name = "client EOF (closed fd) causes clean shutdown" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        (* Close input FD → console gets EOF *)
        let%lwt () = safe_close_fd env.client.terminal_in.fd_wr in
        let%lwt () = Lwt_unix.sleep teardown_wait_for in
        (* TODO: [REFACTOR,TEST] <render cb> needs updating after the displaying callback from session to console can be implemented (add a render callback) *)
        (* Verify client exited cleanly *)
        (* let%lwt () = *)
        (*   verify_line_displayed_by_server env "[Closing connection]" *)
        (* in *)
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed short thunk)

let test_client_initiates_session_termination_gracefully =
  let tc_name = "Client initiates termination gracefully" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt () = type_as_client env "/quit" in
        let%lwt () = Lwt_unix.sleep teardown_wait_for in
        let%lwt () =
          verify_line_displayed_by_server env "peer has left the chat"
        in
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed short thunk)

let test_server_initiates_session_termination_gracefully =
  let tc_name = "Server inits session termination --> continues running" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt () = type_as_server env "/quit" in
        let%lwt () = Lwt_unix.sleep teardown_wait_for in
        let%lwt () =
          verify_line_displayed_by_client env "peer has left the chat"
        in
        let%lwt () = type_as_server env "/quit" in
        let%lwt () =
          verify_line_displayed_by_server env "currently not in any chat"
        in
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed short thunk)

let test_client_channel_closed_abrupt_disconnect =
  let tc_name = "client Channel_closed (network dies) causes shutdown" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        (* Abruptly close channels → console gets Channel_closed *)
        let%lwt () = type_as_client env "/quit" in
        (* let%lwt () = close_client_channel_abruptly env in *)
        let%lwt () = Lwt_unix.sleep teardown_wait_for in
        (* TODO: [TEST] add back logic after implementing the session -> console callbacks AND standardising strings *)
        (* Verify client exited with Channel_closed handling *)
        let%lwt () =
          verify_line_displayed_by_server env "peer has left the chat"
        in
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed short thunk)

(* TODO: [TEST] figure out how to simulate "network connection dropped" *)
let test_client_network_dies =
  ALWT.test_case "client network disconnect handled gracefully" `Quick
    (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        (* Close the session socket → both channels fail *)
        (* let%lwt () = close_client_session_abruptly env in *)
        let%lwt () = Lwt_unix.sleep teardown_wait_for in
        (* Verify graceful shutdown *)
        let%lwt () = verify_line_displayed_by_server env "peer has left" in
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed short thunk)

(* TODO:[TEST] fix this test, most likely the test harness actions are too fast that's why there's all sorts of closes of channels and such *)
let _test_simultaneous_sends_no_deadlock =
  let tc_name =
    "both sides send simultaneously → both messages arrive, no deadlock"
  in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt () = Lwt_io.printl "WALDO 1" in
        let%lwt (), () =
          Lwt.both
            (type_as_client env "concurrent-from-client")
            (type_as_server env "concurrent-from-server")
        in

        let%lwt () = Lwt_io.printl "WALDO 2" in
        (* let%lwt () = Lwt_unix.sleep teardown_wait_for in *)
        let%lwt () =
          verify_line_displayed_by_server env "concurrent-from-client"
        in
        (* let%lwt () = Lwt_unix.sleep teardown_wait_for in *)
        let%lwt () =
          verify_line_displayed_by_client env "concurrent-from-server"
        in
        let%lwt () = Lwt_unix.sleep teardown_wait_for in
        (* Both sides should also have received an ack. *)
        let%lwt () = verify_line_displayed_by_client env "Acked" in
        (* let%lwt () = Lwt_unix.sleep teardown_wait_for in *)
        let%lwt () = verify_line_displayed_by_server env "Acked" in
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed short thunk)

(** NOTE: technically this test logic is redundant because [ assert_connected ]
    within [ setup_e2e ] already does a ping-pong assert *)
let smoke_tests = [ test_uni_directional_exchange; test_basic_ping_pong ]

let app_lifecycle_tests =
  [
    test_client_initiates_session_termination_gracefully;
    test_server_initiates_session_termination_gracefully;
    test_lifecycle_server_awaits_new_client_after_session_terminates;
    test_lifecycle_server_accepts_new_client_after_session_terminates;
    test_lifecycle_server_exit_kills_session_and_server;
  ]

let conn_lifecycle_tests =
  [
    test_client_eof_clean_disconnect;
    (* test_client_network_dies; *)
    test_client_channel_closed_abrupt_disconnect;
  ]

let conc_duplex_tests = [ (* test_simultaneous_sends_no_deadlock *) ]
let robustness_tests = []
let opaque_payload_tests = []
let multi_session_tests = []
let startup_awaiting_tests = []

let suite =
  [
    ("E2E/Basic/Smoke-tests", smoke_tests);
    ("E2E/Lifecycle/Chat-sessions", app_lifecycle_tests);
    ("E2E/Lifecycle/Connection", conn_lifecycle_tests);
    ("E2E/Concurrency/Duplex", conc_duplex_tests);
    ("E2E/Robustness/Connection", robustness_tests);
    ("E2E/Payload/Opaque-payload", opaque_payload_tests);
    ("E2E/Lifecycle/Multi-session", multi_session_tests);
    ("E2E/Startup/Awaiting-states", startup_awaiting_tests);
  ]
