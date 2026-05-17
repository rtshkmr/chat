type t

val create : Lwt_io.input_channel -> t
val read_frame : t -> (Frame.t, Frame.error) result Lwt.t
