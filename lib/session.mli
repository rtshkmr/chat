(** Session is a coordinator. It handles network IO by running two concurrent
    loops. One is for rx, and the other for tx -- a natural pair for our duplex
    comms.

    Since a chat-connection is stateful, the session keeps track of [ msg_id ]
    and [ pending_acks ] as well.*)

type console_rx_callbacks = {
  on_rx_msg : bytes -> unit Lwt.t;
      (** Fires when a message frame is received. Session automatically sends an
          ACK msg. *)
  on_rx_ack : Frame.msg_id -> float -> unit Lwt.t;
      (** Fires when an ACK msg is received.

          Args: (msg_id, rtt_seconds)

          Session calculates RTT from pending-ack table. *)
  on_rx_close : unit -> unit Lwt.t;  (** Fires when rx a Close frame. *)
}
(** Callbacks from the console to be fired on different rx events *)

type t

type error =
  | Msg_not_pending_ack of int32
  | Network_error of string
  | Frame_error of Frame.error

val error_to_string : error -> string

exception Rx_callbacks_not_binded of string

val create :
  ic:Lwt_io.input_channel ->
  oc:Lwt_io.output_channel ->
  callbacks:console_rx_callbacks option ->
  on_fini:(unit -> unit Lwt.t) ->
  t
(** Create a session. Does not start I/O until [run] is called. *)

val set_callbacks : t -> console_rx_callbacks -> t
(** Hydrates the session by setting the console-specific callbacks that the
    session needs to have access to.*)

val unset_callbacks : t -> t

val fini : t -> unit Lwt.t
(** Finalises a session gracefully *)

val send_message : t -> bytes -> unit Lwt.t
(** Tx a message. Session assigns [ msg_id ], sends frame, tracks in pending-ack
    table. Awaits frame to be written (backpressure from socket). *)

val run : t -> unit Lwt.t
(** Starts the session: run read/write loops for rx/tx concurrently. Returns
    when the connection closes or an error occurs. *)
