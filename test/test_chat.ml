let () =
  Alcotest_lwt.run "chat"
    (List.concat [ Test_frame.suite; Test_frame_reader.suite ])
  |> Lwt_main.run
