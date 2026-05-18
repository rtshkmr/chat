type t = { ic : Lwt_io.input_channel }
type error = Connection_lost of string | Protocol_error of Frame.error

let error_to_string = function
  | Connection_lost s -> Printf.sprintf "Connection lost: %s" s
  | Protocol_error frame_e ->
      let reason = Frame.error_to_string frame_e in
      Printf.sprintf "Protocol Error @ [Frame_reader]: %s" reason

let create ic = { ic }

let read_frame t =
  let header_len = Frame.frame_header_sz in
  try%lwt
    let header_buf = Bytes.create header_len in
    let%lwt () = Lwt_io.read_into_exactly t.ic header_buf 0 header_len in
    match Frame.parse_and_validate_header_bytes header_buf with
    | Error e -> Lwt.return (Error (Protocol_error e))
    | Ok { typ; id; payload_sz } ->
        let payload_buf = Bytes.create payload_sz in
        let%lwt () = Lwt_io.read_into_exactly t.ic payload_buf 0 payload_sz in
        let make_frame_res =
          Frame.make_frame id payload_buf typ
          |> Result.map_error (fun e -> Protocol_error e)
        in
        Lwt.return make_frame_res
  with
  | End_of_file -> Lwt.return (Error (Connection_lost "peer closed"))
  | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
      Lwt.return (Error (Connection_lost "peer disconnected, connection reset"))
  | e -> Lwt.return (Error (Connection_lost (Printexc.to_string e)))
[@@warning "-4"]
(* This pattern-matching is fragile but it doesn't matter for us, we only handle
   one case here that we know of, else it's just a protocol error that will
   propagate to the caller as well and the session will just kill itself.
  It will remain exhaustive when constructors are added to type Unix.error*)
