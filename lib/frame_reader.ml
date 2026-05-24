module F = Frame

type t = { ic : Lwt_io.input_channel }
type error = Connection_lost of string | Protocol_error of F.error

let pp_error fmt = function
  | Connection_lost s -> Format.fprintf fmt "Connection lost: %s" s
  | Protocol_error frame_e ->
      Format.fprintf fmt "Protocol Error @ [Frame_reader]: %a" F.pp_error
        frame_e

let create ic = { ic }

let read_frame { ic } =
  let header_len = F.hdr_size in
  try%lwt
    let header_buf = Bytes.create header_len in
    let%lwt () = Lwt_io.read_into_exactly ic header_buf 0 header_len in
    match F.parse_and_validate_header_bytes header_buf with
    | Error e -> Lwt.return (Error (Protocol_error e))
    | Ok { typ; id; payload_sz } ->
        let payload_buf = Bytes.create payload_sz in
        let%lwt () = Lwt_io.read_into_exactly ic payload_buf 0 payload_sz in
        let make_frame_res =
          F.make_frame id payload_buf typ
          |> Result.map_error (fun e -> Protocol_error e)
        in
        Lwt.return make_frame_res
  with
  | End_of_file -> Lwt.return (Error (Connection_lost "peer closed"))
  | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
      Lwt.return (Error (Connection_lost "peer disconnected, connection reset"))
  | e -> Lwt.return (Error (Connection_lost (Printexc.to_string e)))
[@@warning "-4"]
(* Fragile pattern-matching on Unix.error here is fine, we care about a subset of errors and how to handle them. If there's a need to lift new errors from the catch-all case to specialised handling then they will be done with test-additions *)
