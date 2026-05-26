(** [ Frame_reader ] handles reading of discrete frames from the TCP byte-stream
    using the Tag-Length-Value protocol that [Frame] defines.

    - Partial TCP reads are automagically handled because of how
      [Lwt_io.read_into_exactly] handles partial TCP reads internally (it loops
      until the requested bytes are available or EOF)., that's why it doesn't
      need to be manually buffered. *)

type t
type error = Connection_lost of string | Protocol_error of Frame.error

val pp_error : Format.formatter -> error -> unit
val create : Lwt_io.input_channel -> t
val read_frame : t -> (Frame.t, error) result Lwt.t
