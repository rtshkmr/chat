(** Session is a coordinator. It handles network IO by running two concurrent
    loops. One is for rx, and the other for tx -- a natural pair for our duplex
    comms.

    Since a chat-connection is stateful, the session keeps track of [ msg_id ]
    and [ pending_acks ] as well.*)

type exit_reason =
  | Peer_disconnected  (** Received Close frame: clean, expected termination. *)
  | Lost_conn of string
      (** EOF or ECONNRESET: peer vanished without a Close frame. *)
  | Broken_pipe  (** EPIPE on tx — we tried to write to a dead socket. *)
  | Protocol_error of Frame.error
      (** Received a frame that violates our protocol. *)
  | Unexpected of exn
      (** Programmer error or unhandled exception — should not happen. *)

exception Session_exit of exit_reason
exception Session_invariant_violated of string

val pp_exit_reason : Format.formatter -> exit_reason -> unit

type rx_event =
  | Msg_received of { id : Frame.msg_id; content : bytes; rcvd_at : float }
  | Ack_received of { id : Frame.msg_id; rtt : float; rcvd_at : float }
  | Peer_closed of { rcvd_at : float }
  | Spurious_ack of { id : Frame.msg_id; rcvd_at : float }

type console_callbacks = { on_rx : rx_event -> unit Lwt.t }
type t

(** NOTE: the ic/oc are being passed in separate from the sock for testability.*)
val create :
  ic:Lwt_io.input_channel ->
  oc:Lwt_io.output_channel ->
  ?callbacks:console_callbacks option ->
  (* TODO:[POLISH] rename to conn_sock because it's a better name *)
  ?conn_sock:Lwt_unix.file_descr option ->
  unit ->
  t
(** Create a session. Does not start I/O until [run] is called. *)

val meta_of_opt : t -> Session_meta.t option

val set_callbacks : t -> console_callbacks -> unit
(** Hydrates the session by setting the console-specific callbacks that the
    session needs to have access to. This is entirely effectful.*)

val unset_callbacks : t -> unit

val shutdown : t -> unit Lwt.t
(** Closes a chat session gracefully *)

val send_message : t -> bytes -> unit Lwt.t
(** Tx a message. Session assigns [ msg_id ], sends frame, tracks in pending-ack
    table. Awaits frame to be written (backpressure from socket). *)

val run : t -> unit Lwt.t
(** Starts the session: run read/write loops for rx/tx concurrently. Returns
    when the connection closes or an error occurs. *)
