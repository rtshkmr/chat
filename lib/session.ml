open Lwt.Infix

type console_rx_callbacks = {
  on_rx_msg : bytes -> unit Lwt.t;
  on_rx_ack : Frame.msg_id -> float -> unit Lwt.t;
  on_rx_close : unit -> unit Lwt.t;
}

type state = {
  msg_id_counter : int32 ref;
  pending_acks : (Frame.msg_id, float) Hashtbl.t;
  msg_queue : bytes Lwt_mvar.t;  (** Used for coordinating payloads for tx*)
}

type t = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  on_fini : unit -> unit Lwt.t;
  callbacks : console_rx_callbacks option ref;
      (** TODO: [ERR] the fact that it is an optional now means that we need to
          add in an exception because an invariant to maintain here is that
          there's NEVER a case when we try to invoke any callback when it's
          empty. This is because the console will always outlive a session, so
          it should always be available to hydrate the session *)
  state : state;
  frame_reader : Frame_reader.t;
}
[@@warning "-69"]
(*[ic] is not being used but that's a design things to be relooked at. [ frame_reader ] ends up getting constructed so [ic] is within that, but then [oc] is left alone if we just remove [ic] from t. I guess this is alright? TODO: consider this from a design pov*)

type error =
  | Msg_not_pending_ack of int32
  | Network_error of string
  | Frame_error of Frame.error

let error_to_string = function
  | Msg_not_pending_ack id -> Printf.sprintf "Msg %ld is not pending ack" id
  | Network_error err_str -> Printf.sprintf "Network error: %s" err_str
  | Frame_error e -> Frame.error_to_string e
[@@warning "-32"]
(* TODO: [ERR] wire up session errors*)

exception Rx_callbacks_not_binded of string

let init_state () =
  {
    msg_id_counter = ref 0l;
    pending_acks = Hashtbl.create 32;
    msg_queue = Lwt_mvar.create_empty ();
  }

let create ~ic ~oc ~callbacks ~on_fini =
  let frame_reader = Frame_reader.create ic in
  let state = init_state () in
  { ic; oc; callbacks = ref callbacks; state; on_fini; frame_reader }

let set_callbacks t cbs =
  t.callbacks := Some cbs;
  t

let unset_callbacks t =
  t.callbacks := None;
  t

(* TODO: [ERR] ECONNRESET handling here -- "Connection Lost" *)
(* TODO: [ERR] EPIPE/broken pipe handling here or in handle_network io --- should consider as connection closed *)
let tx_frame { oc; _ } f =
  let bs = f |> Frame.to_bytes in
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

let resolve_and_get_delivery_rtt { state = { pending_acks; _ }; _ } msg_id =
  match Hashtbl.find_opt pending_acks msg_id with
  | Some ts ->
      Hashtbl.remove pending_acks msg_id;
      Ok (Unix.gettimeofday () -. ts)
  | None -> Error (Msg_not_pending_ack msg_id)

(** TODO: [DOCS] document exn raised *)
let get_cbs_exn { callbacks; _ } =
  match !callbacks with
  | None ->
      let expectation =
        "Session's rx callbacks must be binded -- did you forget to call \
         [Console.bind_session]?"
      in
      raise (Rx_callbacks_not_binded expectation)
  | Some cbs -> cbs

let maybe_display_pending_acks { state = { pending_acks; _ }; _ } =
  let num_pending = Hashtbl.length pending_acks in
  if num_pending = 0 then Lwt.return_unit
  else
    (* print header first *)
    Lwt_io.printl
      "Terminating this chat session before all sent msgs have been ack-ed."
    >>= fun () ->
    Lwt_io.printlf "%d Pending Acks for:" num_pending >>= fun () ->
    (* print each entry directly *)
    Hashtbl.fold
      (fun msg_id time acc ->
        acc >>= fun () ->
        Lwt_io.printl (Printf.sprintf "Msg %ld sent at %.3f" msg_id time))
      pending_acks Lwt.return_unit

let fini ({ on_fini; _ } as t) =
  maybe_display_pending_acks t >>= fun () ->
  on_fini () >>= fun () -> Lwt_io.printl "[Session cleaned up]"

let on_peer_termination t =
  let cbs = get_cbs_exn t in
  cbs.on_rx_close ()

let on_rcv_ack t msg_id =
  match resolve_and_get_delivery_rtt t msg_id with
  | Ok rtt ->
      let cbs = get_cbs_exn t in
      cbs.on_rx_ack msg_id rtt
  | Error e ->
      let msg = error_to_string e in
      Lwt_io.eprint ("[WARNING]: " ^ msg ^ ". Ignoring this...")

(* TODO: [ERR] EPIPE/broken pipe handling here *)
let send_ack t id = Frame.Ack { id } |> tx_frame t

let on_rcv_msg t id payload =
  let { on_rx_msg; _ } = get_cbs_exn t in
  let%lwt () = on_rx_msg payload in
  send_ack t id

let rx_loop ({ frame_reader; _ } as t) =
  let handle_fatal_error msg =
    Lwt_io.eprintf "%s\n" msg >>= fun () -> Lwt.fail (Failure msg)
  in
  let rec loop () =
    Frame_reader.read_frame frame_reader >>= function
    | Ok (Frame.Msg { id; payload }) -> on_rcv_msg t id payload >>= loop
    | Ok (Frame.Ack { id }) -> on_rcv_ack t id >>= loop
    | Ok Frame.Close -> on_peer_termination t
    | Error (Connection_lost msg) ->
        handle_fatal_error ("Connection lost @ [ Frame_reader ]: " ^ msg)
    | Error (Protocol_error frame_err) ->
        handle_fatal_error
          ("Frame error @ [ Frame_reader ]: " ^ Frame.error_to_string frame_err)
  in
  loop ()

let tx_loop ({ state = { msg_queue; _ }; _ } as t) =
  let rec loop () =
    let%lwt payload = Lwt_mvar.take msg_queue in
    let id = next_msg_id t in
    let%lwt () = tx_frame t (Frame.Msg { id; payload }) in
    track_pending_frame t id;
    loop ()
  in
  loop ()

(* TODO: [ERR] create custom lwt errors to be handled*)
(* TODO: [ERR] EPIPE/broken pipe handling here or in handle_network io --- should consider as connection closed *)
let handle_network_io t =
  try%lwt Lwt.join [ rx_loop t; tx_loop t ]
  with e ->
    let%lwt () =
      Lwt_io.eprintf "Unexpected session error: %s\n" (Printexc.to_string e)
    in
    Lwt.fail e

let run t =
  Lwt_io.printl "Running session..." >>= fun () ->
  let handle_network_thunk = fun () -> handle_network_io t in
  let fini_thunk = fun () -> fini t in
  Lwt.finalize handle_network_thunk fini_thunk
