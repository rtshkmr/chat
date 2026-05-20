type msg_id = int32

type t =
  | Msg of { id : msg_id; payload : bytes }
  | Ack of { id : msg_id }
  | Close

type frame_format_info = {
  frame_header_sz : int;
  frame_payload_off : int;
  frame_type_off : int;
  frame_id_off : int;
  frame_payload_sz_off : int;
}

(* Frame Headers are fixed size,
    [ 9B = <1B type><4B msg_id><4B payload_length> ] *)
let frame_header_sz = 9
let max_payload_sz = 1_000_000 (* 1MB max*)

let frame_format =
  {
    frame_header_sz;
    frame_payload_off = frame_header_sz;
    frame_type_off = 0;
    frame_id_off = 1;
    frame_payload_sz_off = 5;
  }

let fmt = frame_format

type error =
  | Header_too_short
  | Payload_too_short
  | Unknown_frame_type of int
  | Payload_too_big of { sz : int; max : int }

let error_to_string = function
  | Header_too_short -> "Frame header too short"
  | Payload_too_short -> "Frame payload incomplete"
  | Unknown_frame_type tag -> Printf.sprintf "Unknown frame type: %d" tag
  | Payload_too_big { sz; max } ->
      Printf.sprintf "Payload is to big (%dB). Max %dB allowed." sz max

let type_of = function Msg _ -> 0 | Ack _ -> 1 | Close -> 2

let payload_of = function
  | Msg { payload; _ } -> payload
  | Ack _ | Close -> Bytes.empty

type header_meta = {
  typ : int; (* 1-byte type tag *)
  id : msg_id; (* 4-byte message id *)
  payload_sz : int; (* variable, needs calculation *)
}

let header_meta_of t =
  match t with
  | Msg { id; payload } ->
      let payload_sz = payload |> Bytes.length in
      { typ = type_of t; id; payload_sz }
  | Ack { id } -> { typ = type_of t; id; payload_sz = 0 }
  | Close -> { typ = type_of t; id = 0l; payload_sz = 0 }

let validate_payload_sz payload_sz =
  if payload_sz < 0 then
    Error (Payload_too_big { sz = payload_sz; max = max_payload_sz })
  else if payload_sz > max_payload_sz then
    Error (Payload_too_big { sz = payload_sz; max = max_payload_sz })
  else Ok ()

let validate_header_meta { typ; payload_sz; _ } =
  match typ with
  | 0 | 1 | 2 -> validate_payload_sz payload_sz
  | _ -> Error (Unknown_frame_type typ)

let parse_header_bytes bs =
  if Bytes.length bs < fmt.frame_header_sz then Error Header_too_short
  else
    Ok
      {
        typ = Bytes.get_uint8 bs fmt.frame_type_off;
        id = Bytes.get_int32_be bs fmt.frame_id_off;
        payload_sz =
          Bytes.get_int32_be bs fmt.frame_payload_sz_off |> Int32.to_int;
      }

let parse_and_validate_header_bytes buf =
  match parse_header_bytes buf with
  | Error e -> Error e
  | Ok header -> (
      match validate_header_meta header with
      | Ok () -> Ok header
      | Error e -> Error e)

let to_bytes t =
  let { typ; id; payload_sz } = header_meta_of t in
  let frame = fmt.frame_header_sz + payload_sz |> Bytes.create in
  Bytes.set_uint8 frame fmt.frame_type_off typ;
  Bytes.set_int32_be frame fmt.frame_id_off id;
  Bytes.set_int32_be frame fmt.frame_payload_sz_off (Int32.of_int payload_sz);
  Bytes.blit (payload_of t) 0 frame fmt.frame_payload_off payload_sz;
  frame

let make_frame id payload typ =
  let payload_sz = Bytes.length payload in
  match { id; typ; payload_sz } |> validate_header_meta with
  | Error e -> Error e
  | Ok _ -> (
      match typ with
      | 0 -> Ok (Msg { id; payload })
      | 1 -> Ok (Ack { id })
      | 2 -> Ok Close
      | tag -> Error (Unknown_frame_type tag))

let of_bytes bs =
  match parse_header_bytes bs with
  | Error e -> Error e
  | Ok { id; typ; payload_sz } ->
      if Bytes.length bs < fmt.frame_header_sz + payload_sz then
        Error Payload_too_short
      else
        let payload =
          if payload_sz = 0 then Bytes.empty
          else Bytes.sub bs fmt.frame_header_sz payload_sz
        in
        make_frame id payload typ
