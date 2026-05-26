module SMeta = Session_meta

val pp_timestamp : Format.formatter -> float -> unit
val pp_prompt : Format.formatter -> float -> unit
(** Prompts with current time. That's why it's effectful because of current time-read.*)
val pp_prompt_now : Format.formatter -> unit -> unit
val pp_warning : Format.formatter -> string -> unit

val eprintf_pp : (Format.formatter -> 'a -> unit) -> 'a -> unit Lwt.t

val write_pp :
  Lwt_io.output_channel -> (Format.formatter -> 'a -> unit) -> 'a -> unit Lwt.t

val pp_peer : Format.formatter -> SMeta.peer -> unit
val pp_banner : Format.formatter -> SMeta.t -> unit
val pp_dummy_header : Format.formatter -> unit -> unit
