open Helpers.E2e_harness
open Helpers.Test_helpers
module In_typ = Helpers.Input_types
module F = Chat.Frame
module S = Chat.Session
module FR = Chat.Frame_reader
module In = Helpers.Test_input
module ALWT = Alcotest_lwt

let short = 1.5
let medium = 3.0

let wait_for_client_disconnect env =
  let task () =
    verify_line_displayed_by_server env target_client_disconn_substr
  in
  timed medium task

let test_uni_directional_exchange =
  let tc_name = "[1-dir] server sends → client acks" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let line = default_in in
        let%lwt () = type_as_server env line in
        let%lwt () = verify_line_displayed_by_client env line in
        let%lwt () = verify_line_displayed_by_server env target_ack_substr in
        let%lwt () = teardown_e2e env in
        Lwt.return_unit
      in
      timed short thunk)

let test_basic_ping_pong =
  let tc_name = "ping-pong test with ACKs" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let c_to_s_msg = "client:" ^ default_in in
        let%lwt () = type_as_client env c_to_s_msg in
        let%lwt () = verify_line_displayed_by_server env c_to_s_msg in
        let%lwt () = verify_line_displayed_by_client env target_ack_substr in
        let s_to_c_msg = "server:" ^ default_in in
        let%lwt () = type_as_server env s_to_c_msg in
        let%lwt () = verify_line_displayed_by_client env s_to_c_msg in
        let%lwt () = verify_line_displayed_by_server env target_ack_substr in
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
        let%lwt () = sim_peer_eof env.client.terminal_in in
        let%lwt () = wait_for_client_disconnect env in
        let%lwt () = type_as_server env "/quit" in
        let%lwt () =
          verify_line_displayed_by_server env target_quit_on_no_chat
        in
        let%lwt () = teardown_e2e env in
        Lwt.return_unit
      in
      timed medium thunk)

let test_lifecycle_server_accepts_new_client_after_session_terminates =
  let tc_name =
    "server accepts new conn after an existing chat session terminates"
  in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt () = teardown_client env in
        let%lwt () = wait_for_client_disconnect env in
        let%lwt () = type_as_server env "/quit" in
        let%lwt () =
          verify_line_displayed_by_server env target_quit_on_no_chat
        in
        let%lwt client2 = spawn_client switch ~port:env.port in
        let updated_env = { env with client = client2 } in
        let c2_msg = "Hello, I'm client 2" in
        let%lwt () = type_as_client updated_env c2_msg in
        let%lwt () = verify_line_displayed_by_server env c2_msg in

        let%lwt () = teardown_e2e updated_env in
        Lwt.return_unit
      in
      timed medium thunk)

let test_lifecycle_server_exit_kills_session_and_server =
  let tc_name =
    "Server's /exit terminates the server (even w existing session)"
  in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt () = type_as_server env "/exit" in
        let%lwt () =
          verify_line_displayed_by_server env target_server_shutting_down
        in
        let%lwt () = teardown_e2e env in
        Lwt.return_unit
      in
      timed medium thunk)

let test_client_eof_clean_disconnect =
  let tc_name = "client EOF (closed fd) causes clean shutdown" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt () = sim_peer_eof env.client.terminal_in in
        let%lwt () = wait_for_client_disconnect env in
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
        let%lwt () =
          verify_line_displayed_by_server env target_when_client_graceful_term
        in
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed medium thunk)

let test_server_initiates_session_termination_gracefully =
  let tc_name = "Server inits session termination --> continues running" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt () = type_as_server env "/quit" in
        let%lwt () = verify_line_displayed_by_client env "disconnected" in
        let%lwt () = type_as_server env "/quit" in
        let%lwt () =
          verify_line_displayed_by_server env target_quit_on_no_chat
        in
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed medium thunk)

let test_client_channel_closed_abrupt_disconnect =
  let tc_name = "client Channel_closed (network dies) causes shutdown" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt () = sim_peer_eof env.client.terminal_in in
        let%lwt () = wait_for_client_disconnect env in
        let%lwt () =
          verify_line_displayed_by_server env target_when_client_graceful_term
        in
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed medium thunk)

let test_simultaneous_sends_no_deadlock =
  let tc_name =
    "both sides send simultaneously → both messages arrive, no deadlock"
  in
  ALWT.test_case tc_name `Slow (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt (), () =
          Lwt.both
            (type_as_client env "concurrent-from-client")
            (type_as_server env "concurrent-from-server")
        in
        let%lwt () =
          verify_line_displayed_by_server env "concurrent-from-client"
        in
        let%lwt () =
          verify_line_displayed_by_client env "concurrent-from-server"
        in
        let%lwt () = verify_line_displayed_by_client env target_ack_substr in
        let%lwt () = verify_line_displayed_by_server env target_ack_substr in
        let%lwt () = teardown_server env in
        Lwt.return_unit
      in
      timed medium thunk)

let test_multisession_triple_test =
  let tc_name = "server can keep on going one session after another (triple)" in
  ALWT.test_case tc_name `Quick (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let%lwt () = type_as_client env "/quit" in
        let%lwt () = wait_for_client_disconnect env in
        let%lwt () = type_as_server env "/quit" in
        let%lwt () =
          verify_line_displayed_by_server env target_quit_on_no_chat
        in
        let%lwt client2 = spawn_client switch ~port:env.port in
        let updated_env_2 = { env with client = client2 } in
        let c2_msg = "Hello, I'm client 2" in
        let%lwt () = type_as_client updated_env_2 c2_msg in
        let%lwt () = verify_line_displayed_by_server updated_env_2 c2_msg in
        let%lwt () = type_as_client updated_env_2 "/quit" in
        let%lwt () = wait_for_client_disconnect updated_env_2 in

        let%lwt client3 = spawn_client switch ~port:env.port in
        let updated_env_3 = { updated_env_2 with client = client3 } in
        let c3_msg = "Hello, I'm client 3" in
        let%lwt () = type_as_client updated_env_3 c3_msg in
        let%lwt () = verify_line_displayed_by_server updated_env_3 c3_msg in

        let%lwt () = teardown_e2e updated_env_2 in
        Lwt.return_unit
      in
      timed medium thunk)

let test_burst_messages_all_delivered =
  ALWT.test_case "client sends 5 messages in parallel → all arrive, 5 acks"
    `Slow (fun switch () ->
      let thunk () =
        let%lwt env = setup_e2e switch in
        let msgs = List.init 5 (fun i -> Printf.sprintf "burst-msg-%d" i) in
        let%lwt () = Lwt_list.iter_p (type_as_client env) msgs in
        let%lwt () =
          Lwt_list.iter_p (fun m -> verify_line_displayed_by_server env m) msgs
        in
        let%lwt () =
          verify_n_occurrences env.client.terminal_out 5 target_ack_substr
        in
        teardown_e2e env
      in
      timed medium thunk)

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
    test_client_channel_closed_abrupt_disconnect;
  ]

let conc_duplex_tests =
  [
    test_simultaneous_sends_no_deadlock (* NOTE [TEST] POSSIBLE FLAKY TEST*);
    test_burst_messages_all_delivered;
    (* NOTE [TEST] POSSBILE FLAKY *)
  ]

let multi_session_tests = [ test_multisession_triple_test ]

let suite =
  [
    ("E2E/Basic/Smoke-tests", smoke_tests);
    ("E2E/Lifecycle/Chat-sessions", app_lifecycle_tests);
    ("E2E/Lifecycle/Connection", conn_lifecycle_tests);
    ("E2E/Lifecycle/Multi-session", multi_session_tests);
    ("E2E/Concurrency/Duplex", conc_duplex_tests);
  ]
