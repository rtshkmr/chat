(** Unit tests in this codebase are more of test-generators that take in a
    structured input so that we can massively enumerate many varitions of
    properties. The record types here are "data-structures" that define inputs
    for such test-generators. *)

open Chat
module F = Frame
module FR = Frame_reader

type 'a t = { desc : string; bytes : bytes; err : 'a }

type freader_streaming_input = {
  name : string;
  frames : F.t list;  (** for multiple dispatches *)
  intent : string;
}
(** Allows for multiple frame dispatches to verify frame boundaries at incoming
    byte-stream (multiple frames in sequence)*)

type freader_edge_case_input = {
  name : string;
  frame : F.t;
  check_frame : F.t -> unit Lwt.t;
}
(** Allows for tc generation with single dispatch and custom frame assertions.*)

type freader_conn_loss_input = {
  name : string;
  init : Lwt_io.output_channel -> unit Lwt.t;
      (** What data to write before closing (simulates partial transmission) *)
  check_error : FR.error -> unit Lwt.t;
      (** Verify that we got the right error *)
}

type freader_protocol_input = {
  name : string;
  frame_bytes : bytes; (* Raw bytes, possibly malformed *)
  expect_error : FR.error -> unit Lwt.t;
}
