let () =
  Alcotest_lwt.run "chat"
    (List.concat
       [
         Test_frame.suite;
         Test_frame_reader.suite;
         Test_session.suite;
         Test_e2e.suite;
       ])
  |> Lwt_main.run
