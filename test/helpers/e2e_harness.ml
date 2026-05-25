open Chat
open Lwt.Infix
open Test_helpers
module S = Server
module C = Client

type peer = {
  terminal_in : pipe;  (** test writes here → simulates typing *)
  terminal_out : capture_spy;  (** test reads from here → captures display *)
  task : unit Lwt.t;  (** the running server/client Lwt task *)
}

type e2e_env = { server : peer; client : peer; port : int }

(** Spawns a test server that will already start running, accepting connections
    as a bg-task.

    NOTES:
    - 1. using [Lwt_unix.socket PF_INET SOCK_STREAM 0] will make the OS give it
      any free port -- good for ephemeral uses. This is important because we
      want to be able to run multiple e2e tests in parallel AND keep them
      independent by making them use different port-pairs. That's also why we
      extract the port that is assigned as [assigned_port]. *)
let spawn_server switch =
  let term_in_pipe = make_pipe_with_switch switch in
  let term_out = make_output_spy switch in
  let server_socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  let%lwt () =
    Lwt_unix.bind server_socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0))
  in
  Lwt_unix.listen server_socket 1;

  let assigned_port =
    match Lwt_unix.getsockname server_socket with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "unexpected address family"
      (* it's alright, it's a test and we only care about this case*)
      [@@warning "-4"]
  in
  let net : S.network_config =
    { port = assigned_port; bind = "localhost"; timeout = 12 }
  in
  let term = S.make_terminal_conf ~ic:term_in_pipe.rd ~oc:term_out.pipe.wr () in
  let task = S.run_with_socket ~sock:server_socket ~term ~net () in
  Lwt.return
    ( { terminal_in = term_in_pipe; terminal_out = term_out; task },
      assigned_port )

let spawn_client switch ~port =
  let term_in_pipe = make_pipe_with_switch switch in
  let term_out = make_output_spy switch in
  let ic = term_in_pipe.rd in
  let oc = term_out.pipe.wr in
  let term = C.make_terminal_conf ~ic ~oc () in
  let net : C.network_config = { host = "localhost"; port; timeout = 12 } in
  let task = C.run ~term ~net () in
  Lwt.return { terminal_in = term_in_pipe; terminal_out = term_out; task }

(** Enforces timing discipline on the test fixture setup, ensures that socket is
    ready to accept before the tests begin. We need to make sure that the server
    is listening by the time the client spawns and to avoid flaky tests (due to
    readiness races), we want to explicitly enforce this. That's why we await
    the spawning of the server then the spawning of the client so that a client
    will always connect to an already listening socket.

    This avoids the use of any condition-based approach or any other sync by
    making the sequencing structural. mechanisms.

    NOTE: timing discipline / readiness testing is key to avoid flaky tests.
    There's another possible source of very rare / low-chance race conditions:
    after the server socket accepts connections and before
    [Console.bind_session] has been called. In this period, if test writes a
    message to the client's terminal and the client sends it, then the server's
    session wouldn't have been bound yet so this is not good. This is
    practically rare / impossible (because accept -> bind is completely
    synchronous region of code), but we want surety. See [wait_for_output]

    NOTE: connectivity assertion: how this should be used for writing e2e tests
    is that we need to be sure that both peers are connected. We can write a
    known sentinel message from the client, wait for it to appear in the
    server's captured output, then proceed with the actual test. This should
    further avoid any flaky tests from being written.*)
let setup_e2e_components switch =
  let%lwt server, port = spawn_server switch in
  let%lwt client = spawn_client switch ~port in
  Lwt.return { server; client; port }

(** A readiness probe that avoids any possible race condition from note at
    [setup_e2e]. It's a poll-with-timeout pattern that avoids busy-waiting also.
*)
let wait_for_output ?(timeout = 2.0) cap predicate =
  let rec poll_loop () =
    (* let%lwt () = Lwt_io.printl "polling loop: wait for output" in *)
    if predicate !(cap.lines) then Lwt.return_unit
    else Lwt_unix.sleep 0.01 >>= poll_loop
  in
  let timeout_task () =
    Lwt_unix.sleep timeout >>= fun () ->
    Alcotest.failf "Timed out waiting for output. Captured lines: %s"
      (String.concat "\n" !(cap.lines))
  in
  Lwt.pick [ poll_loop (); timeout_task () ]

(* TODO [REFACTOR]: make this efficient, the recent msg will be at head of lines, we can just check that *)
let has_line_containing s lines =
  let is_substring ~substring s =
    let pattern = substring |> Re.str |> Re.compile in
    Re.execp pattern s
  in
  List.exists (fun l -> is_substring l ~substring:s) lines

let type_as_client env line = Lwt_io.write_line env.client.terminal_in.wr line

let verify_line_displayed_by_peer peer line =
  wait_for_output peer.terminal_out (has_line_containing line)

let verify_line_displayed_by_server env line =
  verify_line_displayed_by_peer env.server line
(* wait_for_output env.server.terminal_out (has_line_containing line) *)

let type_as_server env line = Lwt_io.write_line env.server.terminal_in.wr line

let verify_line_displayed_by_client env line =
  verify_line_displayed_by_peer env.client line
(* wait_for_output env.client.terminal_out (has_line_containing line) *)

(** Client sends a probe (a sentinel msg) and then waits for server to display
    it. This will confirm that they're connected and tests are ready to begin.

    It's a ping-pong test. *)
let assert_connected env =
  let%lwt () = type_as_client env "ping" in
  let%lwt () = verify_line_displayed_by_server env "ping" in
  let%lwt () = verify_line_displayed_by_client env "Acked" in
  let%lwt () = type_as_server env "pong" in
  let%lwt () = verify_line_displayed_by_client env "pong" in
  let%lwt () = verify_line_displayed_by_server env "Acked" in
  Lwt.return_unit

(** Gives an e2e test environment with correctly running server and client with
    the guarantees:
    - server socket is live
    - client socket is live
    - server and client are connected (guaranteed because sentinel message went
      through) *)
let setup_e2e switch =
  let%lwt env = setup_e2e_components switch in
  let%lwt () = assert_connected env in
  Lwt.return env

(* (\** Signal both peers to close, await their tasks, then turn off the switch. *\) *)
(* let teardown_e2e env = *)
(*   let cooldown = 1.0 in *)
(*   let safe_close_fd fd = *)
(*     try%lwt Lwt_unix.close fd with _ -> Lwt.return_unit *)
(*   in *)
(*   let%lwt () = safe_close_fd env.client.terminal_in.fd_wr in *)
(*   let%lwt () = safe_close_fd env.server.terminal_in.fd_wr in *)
(*   Lwt.pick *)
(*     [ Lwt.join [ env.server.task; env.client.task ]; Lwt_unix.sleep cooldown ] *)

let safe_close_fd fd = try%lwt Lwt_unix.close fd with _ -> Lwt.return_unit

let teardown_server env =
  let%lwt () = safe_close_fd env.server.terminal_in.fd_wr in
  let timeout_task () =
    let cooldown = 2.0 in
    let%lwt () = Lwt_unix.sleep cooldown in
    Alcotest.failf "Teardown timed out"
  in
  Lwt.pick [ env.server.task; timeout_task () ]

let teardown_client env =
  let%lwt () = safe_close_fd env.client.terminal_in.fd_wr in
  let timeout_task () =
    let cooldown = 2.0 in
    let%lwt () = Lwt_unix.sleep cooldown in
    Alcotest.failf "Teardown timed out"
  in
  Lwt.pick [ env.client.task; timeout_task () ]

let teardown_e2e env = Lwt.join [ teardown_client env; teardown_server env ]
(* (\* Signals both peers to shut down via EOF on their terminal input *\) *)
(* let%lwt () = safe_close_fd env.client.terminal_in.fd_wr in *)
(* let%lwt () = safe_close_fd env.server.terminal_in.fd_wr in *)
(* let timeout_task () = *)
(*   let cooldown = 2.0 in *)
(*   let%lwt () = Lwt_unix.sleep cooldown in *)
(*   Alcotest.failf "Teardown timed out" *)
(* in *)

(* Lwt.pick [ Lwt.pick [ env.server.task; env.client.task ]; timeout_task () ] *)

let _teardown_e2e env =
  (* Signals both peers to shut down via EOF on their terminal input *)
  let%lwt () = safe_close_fd env.client.terminal_in.fd_wr in
  let%lwt () = safe_close_fd env.server.terminal_in.fd_wr in
  let timeout_task () =
    let cooldown = 2.0 in
    let%lwt () = Lwt_unix.sleep cooldown in
    Alcotest.failf "Teardown timed out"
  in

  Lwt.pick [ Lwt.pick [ env.server.task; env.client.task ]; timeout_task () ]

let timed timeout thunk =
  try%lwt Lwt_unix.with_timeout timeout thunk
  with Lwt_unix.Timeout -> Alcotest.failf "Test timed out after %.1fs" timeout

(** True when [cap.lines] contains at least [n] entries matching [target]. *)
let has_at_least_n_lines_containing n target lines =
  let count = List.length (List.filter (has_line_containing target) lines) in
  count >= n

(** Tear down a standalone [peer] that has no partner. Sends EOF to its terminal
    input and awaits the task with a guard timeout. *)
let teardown_peer peer =
  let%lwt () = safe_close_fd peer.terminal_in.fd_wr in
  Lwt.pick
    [
      peer.task;
      (Lwt_unix.sleep 2.0 >>= fun () -> Alcotest.failf "Peer teardown timed out");
    ]

let close_client_channel_abruptly env =
  let%lwt () = Lwt_io.close env.client.terminal_in.wr in
  let%lwt () = Lwt_io.close env.client.terminal_out.pipe.wr in
  Lwt.return_unit

(* let close_client_session_abruptly env = *)
(*   (\* Kill the session socket, forcing channel closure *\) *)
(*   let%lwt () = Lwt_unix.close env.client.sock in *)
(*   Lwt.return_unit *)

(* (\** Verify [needle] appears at least [n] times in [cap]'s output. *\) *)
(* let verify_n_occurrences env_cap n needle = *)
(*   wait_for_output env_cap *)
(*     (has_at_least_n_lines_containing n needle) *)
