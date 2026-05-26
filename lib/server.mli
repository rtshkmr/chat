type network_config = { port : int; bind : string; timeout : int }

type terminal_config = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  log_level : Logs.level;
}

val make_terminal_conf :
  ?ic:Lwt_io.input_channel ->
  ?oc:Lwt_io.output_channel ->
  ?log_level:Logs.level ->
  unit ->
  terminal_config

val run_with_socket :
  sock:Lwt_unix.file_descr ->
  ?term:terminal_config ->
  net:network_config ->
  unit ->
  unit Lwt.t
(** [run_with_socket] allows the caller to inject the socket [sock], expected to
    be useful when writing tests.*)

val run : ?term:terminal_config -> net:network_config -> unit -> unit Lwt.t
