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
  on_kill : unit -> unit Lwt.t;
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

let init_state () =
  {
    msg_id_counter = ref 0l;
    pending_acks = Hashtbl.create 32;
    msg_queue = Lwt_mvar.create_empty ();
  }

let create ~ic ~oc ~callbacks ~on_kill =
  let frame_reader = Frame_reader.create ic in
  let state = init_state () in
  { ic; oc; callbacks = ref callbacks; state; on_kill; frame_reader }

let set_callbacks t cbs =
  t.callbacks := Some cbs;
  t

let unset_callbacks t =
  t.callbacks := None;
  t

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

let resolve_and_get_delivery_rtt_exn { state = { pending_acks; _ }; _ } msg_id =
  match Hashtbl.find_opt pending_acks msg_id with
  | Some ts ->
      Hashtbl.remove pending_acks msg_id;
      Unix.gettimeofday () -. ts
  | None ->
      let m =
        Printf.sprintf "resolve_and_get_delivery_rtt: no such entry %ld" msg_id
      in
      failwith m

let get_cbs_exn { callbacks; _ } =
  match !callbacks with
  | None -> failwith "TODO: add custom error for lack of callbacks no there"
  | Some cbs -> cbs

let kill ({ on_kill; _ } as t) =
  let { on_rx_close; _ } = get_cbs_exn t in
  on_rx_close () >>= fun () ->
  on_kill () >>= fun () -> Lwt_io.printl "[Session cleaned up]"

let on_peer_termination t =
  let cbs = get_cbs_exn t in
  cbs.on_rx_close () >>= fun () -> kill t

let on_rcv_ack t msg_id =
  let rtt = resolve_and_get_delivery_rtt_exn t msg_id in
  let cbs = get_cbs_exn t in
  cbs.on_rx_ack msg_id rtt

let send_ack t id = Frame.Ack { id } |> tx_frame t

let on_rcv_msg t id payload =
  let { on_rx_msg; _ } = get_cbs_exn t in
  let%lwt () = on_rx_msg payload in
  send_ack t id

let rx_loop ({ frame_reader; _ } as t) =
  let rec loop () =
    Frame_reader.read_frame frame_reader >>= function
    | Error e ->
        (* TODO:[ERR] improve error handling, likely depends on cases also, not necessarily should propagage error *)
        Lwt_io.eprintf "Frame error: %s\n" (Frame.error_to_string e)
        >>= fun () -> Lwt.fail (Failure "frame parse error")
    | Ok (Frame.Msg { id; payload }) -> on_rcv_msg t id payload >>= loop
    | Ok (Frame.Ack { id }) -> on_rcv_ack t id >>= loop
    | Ok Frame.Close -> on_peer_termination t
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
let handle_network_io t =
  try%lwt Lwt.join [ rx_loop t; tx_loop t ]
  with e ->
    let%lwt () =
      Lwt_io.eprintf "Unexpected session error: %s\n" (Printexc.to_string e)
    in
    Lwt.fail e

let run t =
  Lwt_io.printl "Running session..." >>= fun () ->
  let thunk = fun () -> handle_network_io t in
  let cleaner_thunk = fun () -> kill t in
  Lwt.finalize thunk cleaner_thunk
