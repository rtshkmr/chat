open Helpers.Test_helpers
open Alcotest
module F = Chat.Frame
module In = Helpers.Test_input
module In_typ = Helpers.Input_types
module ALWT = Alcotest_lwt

let make_legal_payload_test ({ desc; bytes; _ } : F.error option In_typ.t) =
  let test_name = Printf.sprintf "supports %s payload" desc in
  ALWT.test_case_sync test_name `Quick (fun () ->
      match F.make_frame 1l bytes 0 with
      | Ok frame ->
          let expected = F.Msg { id = 1l; payload = bytes } in
          check frame_testable
            (Printf.sprintf "Payload should be allowed to make frame: %s" desc)
            frame expected
      | Error e -> failf "make_frame failed for %s: %a" desc F.pp_error e)

let make_test_codec_case ({ desc; bytes; err } : F.error In_typ.t) =
  let test_name = Printf.sprintf "rejects raw frame when %s" desc in
  ALWT.test_case_sync test_name `Quick (fun () ->
      match F.of_bytes bytes with
      | Ok _ -> failf "This is supposed to be negative test for %s" test_name
      | Error e ->
          let intent =
            Printf.sprintf
              "Illegal byte segment for frame should be rejected @ \
               deserialisation: %s"
              desc
          in
          let expected = err in
          let actual = e in
          check frame_error_testable intent expected actual)

let test_byte_opaque_cases = List.map make_legal_payload_test In.legal_payloads

let test_codec_illegal_byte_segment_cases =
  List.map make_test_codec_case In.illegal_serialised_frames

let test_tlv_structure_test_cases =
  [
    ALWT.test_case_sync "TLV frame has fixed header and value only" `Quick
      (fun () ->
        let payload_string = "my_payload" in
        let payload_bs_len =
          payload_string |> Bytes.of_string |> Bytes.length
        in
        let actual = msg payload_string |> F.to_bytes |> Bytes.length in
        let expected = F.hdr_size + payload_bs_len in
        let desc =
          "Length of byte-segment is fixed header + length of payload"
        in
        check int desc expected actual);
    ALWT.test_case_sync "Close and Ack frames have empty payloads" `Quick
      (fun () ->
        let check_test_case frame =
          let frame_len = frame |> F.to_bytes |> Bytes.length in
          let desc =
            Printf.sprintf
              "length of byte-segment for frame with empty payload should be %d"
              F.hdr_size
          in
          check int desc F.hdr_size frame_len
        in
        check_test_case close_frame;
        check_test_case (ack 1l));
    ALWT.test_case_sync "Serialisation follows network-byte-order (BE)" `Quick
      (fun () ->
        let bs = "H4X0r" |> msg ~id:0xCAFEBABEl |> F.to_bytes in
        let byte_idx_1 = Bytes.get_uint8 bs 1 in
        let byte_idx_2 = Bytes.get_uint8 bs 2 in
        check int "bytes @ idx 1 is 0xCA" 0xCA byte_idx_1;
        check int "bytes @ idx 2 is 0xFE" 0xFE byte_idx_2);
  ]

let suite =
  [
    ("Frame/Codec/TLV-framing-invariants", test_tlv_structure_test_cases);
    ("Frame/Codec/Payload/Byte-opaque", test_byte_opaque_cases);
    ("Frame/Codec/Invalid-byte-segments", test_codec_illegal_byte_segment_cases);
  ]
