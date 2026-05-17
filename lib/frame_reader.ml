type t = { ic : Lwt_io.input_channel }

let create ic = { ic }

let read_frame t =
  let header_len = Frame.frame_header_sz in
  try%lwt
    let header_buf = Bytes.create header_len in
    let%lwt () = Lwt_io.read_into_exactly t.ic header_buf 0 header_len in
    match Frame.parse_header_bytes header_buf with
    | Error e -> Lwt.return (Error e)
    | Ok { typ; id; payload_sz } ->
        let payload_buf = Bytes.create payload_sz in
        let%lwt () = Lwt_io.read_into_exactly t.ic payload_buf 0 payload_sz in
        Lwt.return (Frame.make_frame id payload_buf typ)
  with e ->
    let%lwt () =
      Lwt_io.eprintf "TODO: manage frame reader error: %s\n"
        (Printexc.to_string e)
    in
    Lwt.fail e
