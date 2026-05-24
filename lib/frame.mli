type msg_id = int32

type error =
  | Header_too_short
  | Payload_too_short
  | Unknown_frame_type of int
  | Payload_too_big of { sz : int; max : int }

type t =
  | Msg of { id : msg_id; payload : bytes }
  | Ack of { id : msg_id }
  | Close

type header_meta = { typ : int; id : msg_id; payload_sz : int }

val hdr_size : int
(** Frame headers are fixed size for
    {:https://en.wikipedia.org/wiki/Type%E2%80%93length%E2%80%93value}Type-Length-Value
    frames. Frame Structure: [ 9B = <1B type><4B msg_id><4B payload_length> ] *)

val max_payload_size : int
(** A limit on how big the payload can be (in B).*)

val parse_and_validate_header_bytes : bytes -> (header_meta, error) result
(** [parse_and_validate_header_bytes buf] parses the first [hdr_size] bytes of
    [buf] as a frame header and validates its fields.*)

val to_bytes : t -> bytes
(** Serialises a frame to its wire format, ready for tx. Follows network
    byte-order (Big Endian).*)

val make_frame : msg_id -> bytes -> int -> (t, error) result
(** Makes a legal frame using [id] and [payload] and [typ] (with validation).*)

val of_bytes : bytes -> (t, error) result
(** Deserialises an entire byte-segment representing the whole frame (in network
    byte-order, BE) into a frame. Returns an error if the buffer is incomplete
    or contains invalid data.

    Intended for non-streaming use (e.g. testing).*)

val pp : Format.formatter -> t -> unit
(** Pretty-prints a frame. *)

val pp_error : Format.formatter -> error -> unit
(** Pretty-prints a frame-error *)
