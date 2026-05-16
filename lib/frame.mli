type msg_id = int32
type error = Header_too_short | Payload_too_short | Unknown_frame_type of int

type t =
  | Msg of { id : msg_id; payload : bytes }
  | Ack of { id : msg_id }
  | Close

val to_bytes : t -> bytes
(** Serialises a frame to its wire format, ready for tx. Follows network
    byte-order (Big Endian).*)

val of_bytes : bytes -> (t, error) result
(** Deserialises a byte-segment (in network byte-order, BE) into a frame.
    Returns an error if the buffer is incomplete or contains invalid data.*)

val error_to_string : error -> string
(** Converts an error to a human-readable string.*)
