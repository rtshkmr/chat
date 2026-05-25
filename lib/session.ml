open Lwt.Infix
module D = Display
module F = Frame
module FR = Frame_reader

type exit_reason =
  | Peer_disconnected  (** Received Close frame: clean, expected termination. *)
  | Lost_conn of string  (** EOF/ECONNRESET: peer gone without a Close frame. *)
  | Broken_pipe  (** EPIPE on tx — we tried to write to a dead socket. *)
  | Protocol_error of Frame.error  (** Got a frame that violates protocol. *)
  | Unexpected of exn  (** Programmer error / unhandled exception *)

exception Session_exit of exit_reason
exception Session_invariant_violated of string

(* TODO:[UI,POLISH] add in prompt *)
let pp_exit_reason fmt = function
  | Peer_disconnected -> Format.fprintf fmt "peer disconnected cleanly"
  | Lost_conn reason -> Format.fprintf fmt "connection lost: %s" reason
  | Broken_pipe -> Format.fprintf fmt "broken pipe (peer closed write end)"
  | Protocol_error e -> Format.fprintf fmt "protocol error: %a" Frame.pp_error e
  | Unexpected e -> Format.fprintf fmt "unexpected: %s" (Printexc.to_string e)

type rx_event =
  | Msg_received of { id : F.msg_id; content : bytes; rcvd_at : float }
  | Ack_received of { id : Frame.msg_id; rtt : float; rcvd_at : float }
  | Peer_closed of { rcvd_at : float }
  | Spurious_ack of { id : F.msg_id; rcvd_at : float }

type console_callbacks = { on_rx : rx_event -> unit Lwt.t }

type state = {
  msg_id_counter : int32 ref;
  pending_acks : (F.msg_id, float) Hashtbl.t;
  msg_queue : bytes Lwt_mvar.t;  (** Used for coordinating payloads for tx*)
}

(* TODO[POLISH] consider renaming ic/oc to net_ic/net_oc *)
type t = {
  oc : Lwt_io.output_channel;
  meta : Session_meta.t option;
  callbacks : console_callbacks option ref;
  state : state;
  frame_reader : FR.t;
  shutdown_cond : unit Lwt_condition.t;
}

let create ~ic ~oc ?(callbacks = None) ?(conn_sock = None) () =
  {
    oc;
    meta = Option.map (fun sock -> Session_meta.of_sock sock) conn_sock;
    callbacks = ref callbacks;
    state =
      {
        msg_id_counter = ref 0l;
        pending_acks = Hashtbl.create 32;
        msg_queue = Lwt_mvar.create_empty ();
      };
    frame_reader = FR.create ic;
    shutdown_cond = Lwt_condition.create ();
  }

let meta_of_opt t = t.meta

let set_callbacks t cbs =
  t.callbacks := Some cbs;
  t

let unset_callbacks t =
  t.callbacks := None;
  t

let tx_frame { oc; _ } f =
  let bs = f |> F.to_bytes in
  let bs_len = Bytes.length bs in
  let%lwt () = Lwt_io.write_from_exactly oc bs 0 bs_len in
  Lwt_io.flush oc

(* TODO: [POLISH] better naming, this is only for user-generated msgs *)
let send_message { state = { msg_queue; _ }; _ } payload =
  Lwt_mvar.put msg_queue payload

let next_msg_id t =
  t.state.msg_id_counter := Int32.add !(t.state.msg_id_counter) 1l;
  !(t.state.msg_id_counter)

let track_pending_frame t msg_id =
  let start_time = Unix.gettimeofday () in
  Hashtbl.add t.state.pending_acks msg_id start_time

let resolve_and_get_sent_at { state = { pending_acks; _ }; _ } msg_id =
  match Hashtbl.find_opt pending_acks msg_id with
  | Some ts ->
      Hashtbl.remove pending_acks msg_id;
      Ok ts
  | None -> Error "not awaiting"

(** TODO: [DOCS] document exn raised *)
let get_cbs_exn { callbacks; _ } =
  match !callbacks with
  | None ->
      let exn =
        Session_invariant_violated
          "callbacks not bound: did you forget [Console.bind_session]?"
      in
      raise exn
  | Some cbs -> cbs

(* TODO: [REFACTOR, TEST] <render cb> -- this function is ugly, can avoid materialising and make this faster. Also should likely just be converted to pretty printers *)
let maybe_display_pending_acks { state = { pending_acks; _ }; _ } =
  let num_pending = Hashtbl.length pending_acks in
  if num_pending = 0 then Lwt.return_unit
  else
    let%lwt () =
      Lwt_io.printl
        "Terminating this chat session before all sent msgs have been ack-ed."
    in
    (* print header first *)
    let%lwt () = Lwt_io.printlf "%d Pending Acks for:" num_pending in
    (* print each entry directly *)
    Hashtbl.fold
      (fun msg_id time acc ->
        acc >>= fun () ->
        Lwt_io.printl (Printf.sprintf "Msg %ld sent at %.3f" msg_id time))
      pending_acks Lwt.return_unit
[@@warning "-32"]
(* TODO [REFACTOR, TEST]  <render cb> figure out the displayign callback needs *)

let send_ack t id = F.Ack { id } |> tx_frame t
let send_close t = F.Close |> tx_frame t

let shutdown t =
  let%lwt () = send_close t in
  Lwt_condition.signal t.shutdown_cond ();
  Lwt.return_unit

(* TODO:[POLISH] make this display useful goodbye after chat_meta is up *)
(* let fini _t = *)
(* let now = Unix.gettimeofday () in *)
(* let msg = Format.asprintf "◎ %a <session> shutting down" D.pp_prompt now in *)
(* try%lwt Lwt_io.eprintl msg with _ -> Lwt.return_unit *)
(* TODO [REFACTOR, TEST]  <render cb> figure out the displayign callback needs *)
(* TODO: STUB: wrong channel, pass it via callback instead *)

let on_peer_termination t =
  let rcvd_at = Unix.gettimeofday () in
  let { on_rx; _ } = get_cbs_exn t in
  Peer_closed { rcvd_at } |> on_rx

let on_rcv_ack t msg_id =
  let rcvd_at = Unix.gettimeofday () in
  match resolve_and_get_sent_at t msg_id with
  | Ok sent_at ->
      let rtt = rcvd_at -. sent_at in
      let { on_rx; _ } = get_cbs_exn t in
      Ack_received { id = msg_id; rtt; rcvd_at } |> on_rx
  | Error _ ->
      let rcvd_at = Unix.gettimeofday () in
      let { on_rx; _ } = get_cbs_exn t in
      Spurious_ack { id = msg_id; rcvd_at } |> on_rx

let on_rcv_msg t id payload =
  let rcvd_at = Unix.gettimeofday () in
  let { on_rx; _ } = get_cbs_exn t in
  let%lwt () = Msg_received { id; content = payload; rcvd_at } |> on_rx in
  send_ack t id

let pp_session_exit_reason fmt reason =
  let now = Unix.gettimeofday () in
  Format.fprintf fmt "%a %a" D.pp_prompt now pp_exit_reason reason

let rx_loop ({ frame_reader; _ } as t) =
  let break_with reason =
    let msg = Format.asprintf "%a" pp_session_exit_reason reason in
    let%lwt () = Lwt_io.eprintf "%s\n" msg in
    Lwt.fail (Session_exit reason)
  in
  let rec loop () =
    FR.read_frame frame_reader >>= function
    | Ok (F.Msg { id; payload }) -> on_rcv_msg t id payload >>= loop
    | Ok (F.Ack { id }) -> on_rcv_ack t id >>= loop
    | Ok F.Close -> on_peer_termination t
    | Error (FR.Connection_lost why) -> break_with (Lost_conn why)
    | Error (FR.Protocol_error fe) -> break_with (Protocol_error fe)
  in
  loop ()

let tx_loop ({ state = { msg_queue; _ }; _ } as t) =
  let rec loop () =
    let%lwt payload = Lwt_mvar.take msg_queue in
    let id = next_msg_id t in
    let%lwt () = tx_frame t (F.Msg { id; payload }) in
    track_pending_frame t id;
    loop ()
  in
  loop ()

let await_shutdown_signal { shutdown_cond; _ } =
  Lwt_condition.wait shutdown_cond

let handle_network_io t =
  let exit_with reason =
    let msg = Format.asprintf "%a" pp_session_exit_reason reason in
    let%lwt () = Lwt_io.eprintf "%s\n" msg in
    Lwt.fail (Session_exit reason)
  in
  try%lwt Lwt.pick [ rx_loop t; tx_loop t; await_shutdown_signal t ] with
  | Lwt.Canceled -> Lwt.return_unit (* silent, cancelled by harness *)
  | (Session_exit _ | Session_invariant_violated _) as e -> Lwt.fail e
  | Unix.Unix_error (Unix.ECONNRESET, _, _) -> exit_with (Lost_conn "reset")
  | Unix.Unix_error (Unix.EPIPE, _, _) -> exit_with Broken_pipe
  | e -> exit_with (Unexpected e)
[@@warning "-4"]
(* Ignore warning 4: The fragile pattern match on [ Unix.error ] is fine because we only care about some of the error types*)

let run t = handle_network_io t
(* TODO[CHAT-META,POLISH] after creating chat_meta, use that info to make this meaningful *)
(* let%lwt () = Lwt_io.write_line t.oc "Running session..." in *)
(* let handle_network_thunk () = handle_network_io t in *)
(* let fini_thunk () = fini t in *)
(* Lwt.finalize handle_network_thunk fini_thunk *)
