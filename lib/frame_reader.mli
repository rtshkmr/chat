type t
type error = Connection_lost of string | Protocol_error of Frame.error

val error_to_string : error -> string
val create : Lwt_io.input_channel -> t
val read_frame : t -> (Frame.t, error) result Lwt.t
