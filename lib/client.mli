type network_config = { port : int; host : string; timeout : int }

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

val run : ?term:terminal_config -> net:network_config -> unit -> unit Lwt.t
