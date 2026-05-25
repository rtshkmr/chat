module D = Display

type t

type error =
  | Terminated
      (** Ctrl-D — end of input stream. Operator-initiated clean shutdown. *)
  | Channel_closed of string  (** The input channel was closed underneath us. *)
  | Io_error of Unix.error * string * string
      (** Unix I/O error: (error_code, syscall_name, arg) *)
  | Unexpected of exn  (** Anything else -- programmer error or unhanded case *)

exception Console_exn of error

val pp_error : Format.formatter -> error -> unit
val create : ?ic:Lwt_io.input_channel -> ?oc:Lwt_io.output_channel -> unit -> t
val bind_session : t -> session:Session.t -> unit

val unbind_session : t -> unit
(** Unbinds the current session from the console, e.g. when connection is
    terminated.*)

val run : t -> unit Lwt.t
(** Runs the user interaction loop pair: i) reading to an ic (stdin) from ,
    parsing user commands and user inputs ii) writing to an oc (stdout) for
    displaying output*)
