open Chat
open Lwt.Infix
module F = Frame
module FR = Frame_reader

type pipe = {
  rd : Lwt_io.input_channel;
  wr : Lwt_io.output_channel;
  fd_rd : Lwt_unix.file_descr;
  fd_wr : Lwt_unix.file_descr;
}
(** A named channel pair, write to [wr], read from [rd]. *)

(** NOTE: idempotent closing, TODO[REFACTOR] consider this pattern for main
    logic fd closers *)
let make_pipe_with_switch switch =
  let fd_rd, fd_wr = Lwt_unix.pipe () in
  let rd = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input fd_rd in
  let wr = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output fd_wr in

  let safe_close_ch ch =
    Lwt.catch (fun () -> Lwt_io.close ch) (fun _ -> Lwt.return_unit)
  in
  let safe_close_fd fd =
    Lwt.catch
      (fun () -> Lwt_unix.close fd)
      (function
        | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
        | e -> Lwt.fail e)
      (* Ignore warning 4: The fragile pattern match on [ Unix.error ] is fine because we only care about some of the error types*)
      [@@warning "-4"]
  in

  let fini () =
    safe_close_ch wr >>= fun () ->
    safe_close_ch rd >>= fun () ->
    safe_close_fd fd_wr >>= fun () -> safe_close_fd fd_rd
  in

  Lwt_switch.add_hook (Some switch) fini;
  { rd; wr; fd_rd; fd_wr }

type captured_output = {
  pipe : pipe;
  lines : string list ref;
  task : unit Lwt.t;
}

(** NOTE: switch is threaded in to consolidate cleanup routines *)
let capture_output switch =
  let p = make_pipe_with_switch switch in
  let lines = ref [] in
  let task =
    let rec loop () =
      let%lwt line = Lwt_io.read_line p.rd in
      lines := !lines @ [ line ];
      loop ()
    in
    Lwt.catch loop (function
      | End_of_file -> Lwt.return_unit
      | Lwt.Canceled -> Lwt.return_unit
      | e -> Lwt.fail e)
  in
  { pipe = p; lines; task }

let captured_text cap = String.concat "\n" !(cap.lines)

(** Returns a serialised frame (a byte segment) using [typ] [id] [payload] (msg
    payload) without any validation, so illegal byte segments can be created
    too.*)
let make_raw_frame_bs typ id payload =
  let frame_header_sz = 9 in
  let frame_payload_off = frame_header_sz in
  let frame_type_off = 0 in
  let frame_id_off = 1 in
  let frame_payload_sz_off = 5 in
  let payload_sz = Bytes.length payload in
  let frame_bs = frame_header_sz + payload_sz |> Bytes.create in
  Bytes.set_uint8 frame_bs frame_type_off typ;
  Bytes.set_int32_be frame_bs frame_id_off id;
  Bytes.set_int32_be frame_bs frame_payload_sz_off (Int32.of_int payload_sz);
  Bytes.blit payload 0 frame_bs frame_payload_off payload_sz;
  frame_bs

let write_raw oc bs =
  Lwt_io.write_from_exactly oc bs 0 (Bytes.length bs) >>= fun () ->
  Lwt_io.flush oc

let write_frame oc frame = write_raw oc (F.to_bytes frame)

(** NOTE: id defaults to one for convenient, quick frame construction.*)
let msg ?(id = 1l) payload_str =
  F.Msg { id; payload = Bytes.of_string payload_str }

let ack id = F.Ack { id }
let close_frame = F.Close

let frame_testable =
  let pp fmt f =
    match f with
    | F.Msg { id; payload } ->
        Format.fprintf fmt "Msg{id=%ld, payload=%d bytes}" id
          (Bytes.length payload)
    | F.Ack { id } -> Format.fprintf fmt "Ack{id=%ld}" id
    | F.Close -> Format.fprintf fmt "Close"
  in
  let is_equal a b = F.to_bytes a = F.to_bytes b in
  Alcotest.testable pp is_equal

let frame_error_testable =
  let pp fmt e = Format.fprintf fmt "%s" (F.error_to_string e) in
  Alcotest.testable pp ( = )

let frame_reader_error_testable =
  let pp fmt e = Format.fprintf fmt "%s" (FR.error_to_string e) in
  Alcotest.testable pp ( = )

(** Read exactly one frame from a pipe's read end, fail if error. *)
let read_one_frame _switch pipe =
  let reader = FR.create pipe.rd in
  match%lwt FR.read_frame reader with
  | Ok f -> Lwt.return f
  | Error e -> Alcotest.failf "Expected frame, got: %s" (FR.error_to_string e)
