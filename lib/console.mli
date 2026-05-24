type t

(* TODO: [REFACTOR] the on_fini inverts control for no reason. consider getting rid of the on_fini pattern. Every module should be in charge of its own fini *)
val create :
  ?ic:Lwt_io.input_channel ->
  ?oc:Lwt_io.output_channel ->
  ?on_fini:(unit -> unit Lwt.t) ->
  unit ->
  t
(** Creates a console with the given input/output channels. Caller is
    responsible for cleanup (closing channels) that's why it provides a cleanup
    callback, [on_kill] to the console. *)

val bind_session : t -> session:Session.t -> t
(** Binds a console (possibly longer-lived lifecycle) to a session.*)

val unbind_session : t -> t
(** Unbinds the current session from the console, e.g. when connection is
    terminated.*)

val run : t -> unit Lwt.t
(** Runs the user interaction loop pair: i) reading to an ic (stdin) from ,
    parsing user commands and user inputs ii) writing to an oc (stdout) for
    displaying output*)
