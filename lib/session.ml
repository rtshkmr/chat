open Lwt
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
  callbacks : console_rx_callbacks option;
      (** TODO: the fact that it is an optional now means that we need to add in
          an exception because an invariant to maintain here is that there's
          NEVER a case when we try to invoke any callback when it's empty. This
          is because the console will always outlive a session, so it should
          always be available to hydrate the session *)
  state : state;
}

let init_state () =
  {
    msg_id_counter = ref 0l;
    pending_acks = Hashtbl.create 32;
    msg_queue = Lwt_mvar.create_empty ();
  }

let create ~ic ~oc ~callbacks ~on_kill =
  let state = init_state () in
  { ic; oc; callbacks; state; on_kill }

let set_callbacks t cbs = { t with callbacks = Some cbs }
let unset_callbacks t = { t with callbacks = None }

let send_message { state = { msg_queue } } payload =
  Lwt_mvar.put msg_queue payload

(* TODO: wire up frame parsing, frame creation, rx callbacks *)
let rx_loop { ic; _ } =
  let rec loop () =
    (* TODO [STUB]: upgrade to proper rx loop with frame parsing *)
    let%lwt line_opt = Lwt_io.read_line_opt ic in
    match line_opt with
    | None ->
        let%lwt () = Lwt_io.printl "[Connection closed by peer]" in
        Lwt.fail End_of_file
    | Some line_str ->
        (* let bs = Bytes.of_string line_str in *)
        let%lwt () = Lwt_io.printlf "<client>: %s\n" line_str in
        loop ()
  in
  loop ()

let tx_loop { oc; state = { msg_queue; _ }; _ } =
  let rec loop () =
    let%lwt payload = Lwt_mvar.take msg_queue in
    let line = Bytes.to_string payload in
    let%lwt () = Lwt_io.write_line oc line in
    let%lwt () = Lwt_io.flush oc in
    loop ()
  in
  loop ()

(* TODO: create custom lwt errors to be handled*)
let handle_network_io t =
  try%lwt Lwt.join [ rx_loop t; tx_loop t ]
  with e ->
    let%lwt () =
      Lwt_io.eprintf "Unexpected session error: %s\n" (Printexc.to_string e)
    in
    Lwt.fail e

let kill { on_kill; callbacks; _ } =
  let on_rx_close =
    match callbacks with
    | Some cb -> cb.on_rx_close
    | None -> fun () -> Lwt.return_unit
  in
  on_rx_close () >>= fun () ->
  on_kill () >>= fun () -> Lwt_io.printl "[Session cleaned up]"

let run t =
  Lwt_io.printl "Running session..." >>= fun () ->
  let thunk = fun () -> handle_network_io t in
  let cleaner_thunk = fun () -> kill t in
  Lwt.finalize thunk cleaner_thunk
