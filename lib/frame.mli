type msg_id = int32
type error = Header_too_short | Payload_too_short | Unknown_frame_type of int

type t =
  | Msg of { id : msg_id; payload : bytes }
  | Ack of { id : msg_id }
  | Close

type header_meta = { typ : int; id : msg_id; payload_sz : int }

val frame_header_sz : int
val parse_header_bytes : bytes -> (header_meta, error) result

val to_bytes : t -> bytes
(** Serialises a frame to its wire format, ready for tx. Follows network
    byte-order (Big Endian).*)

val make_frame : msg_id -> bytes -> int -> (t, error) result

val of_bytes : bytes -> (t, error) result
(** Deserialises an entire byte-segment representing the whole frame (in network
    byte-order, BE) into a frame. Returns an error if the buffer is incomplete
    or contains invalid data.

    Intended for non-streaming use. TODO: YAGNI? *)

val error_to_string : error -> string
(** Converts an error to a human-readable string.*)
