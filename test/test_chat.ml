let () =
  Alcotest_lwt.run "chat" (List.concat [ Test_frame.suite ]) |> Lwt_main.run
