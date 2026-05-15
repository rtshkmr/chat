open Cmdliner

let cmd =
  Cmd.group
    (Cmd.info "chat" ~version:"0.1.0" ~doc:"Simple one-on-one chat application")
    [ Cmd_server.cmd; Cmd_client.cmd ]

let run () = exit (Cmd.eval cmd)
