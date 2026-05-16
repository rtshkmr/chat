(** Session is a coordinator. It handles network IO by running two concurrent
    loops. One is for rx, and the other for tx -- a natural pair for our duplex
    comms.

    Since a chat-connection is stateful, the session keeps track of [ msg_id ]
    and [ pending_acks ] as well.*)

type callbacks = {
  on_rx_msg : bytes -> unit Lwt.t;
      (** Fires when a message frame is received. Session automatically sends an
          ACK msg. *)
  on_rx_ack : Frame.msg_id -> float -> unit Lwt.t;
      (** Fires when an ACK msg is received.

          Args: (msg_id, rtt_seconds)

          Session calculates RTT from pending-ack table. *)
  on_rx_close : unit -> unit Lwt.t;  (** Fires when rx a Close frame. *)
}
(** Callbacks for incoming events *)

type t

val create :
  ic:Lwt_io.input_channel ->
  oc:Lwt_io.output_channel ->
  callbacks:callbacks ->
  t
(** Create a session. Does not start I/O until [run] is called. *)

val kill : t -> unit Lwt.t
(** Kills a session gracefully *)

val send_message : t -> bytes -> unit Lwt.t
(** Tx a message. Session assigns [ msg_id ], sends frame, tracks in pending-ack
    table. Awaits frame to be written (backpressure from socket). *)

val run : t -> unit Lwt.t
(** Starts the session: run read/write loops for rx/tx concurrently. Returns
    when the connection closes or an error occurs. *)
