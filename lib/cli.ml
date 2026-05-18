(* TODO: [ERR] add arg validation:
   - e.g. port should be safe, use regex for hostname/ip addr (ref RFC) *)
(* TODO: [ERR] consider better error handling *)

[@@@warning "-44"]
(* case 1: [open Cmdliner] makes [ Arg ] avail which is fine because we use it multiple times for this cli setup *)

open Cmdliner

let timeout_arg =
  let default = 30 in
  let doc =
    "Connection timeout in seconds (default: " ^ Int.to_string default ^ ")"
  in
  Arg.(value & opt int 30 & info [ "timeout" ] ~docv:"SECONDS" ~doc)

let log_level_arg =
  let open Logs in
  let levels =
    [ Debug; Info; Warning; Error ]
    |> List.map (fun level -> (Some level |> level_to_string, level))
  in
  let default_level = Info in
  let levels_str = levels |> List.map fst |> String.concat ", " in
  let default_str = Some default_level |> level_to_string in
  let doc =
    Printf.sprintf "Logging level: %s (default: %s)" levels_str default_str
  in
  Arg.(
    value
    & opt (enum levels) default_level
    & info [ "log-level" ] ~docv:"LEVEL" ~doc)
[@@warning "-45-44"]

let port_server_arg =
  let default = 4242 in
  let doc =
    "Server port to listen on (default " ^ Int.to_string default ^ ")"
  in
  Arg.(value & opt int default & info [ "port"; "p" ] ~docv:"PORT" ~doc)

let bind_arg =
  let default = "127.0.0.1" in
  let doc = "Bind address (default: " ^ default ^ ")" in
  Arg.(value & opt string default & info [ "bind" ] ~docv:"ADDR" ~doc)

let port_client_arg =
  let default = 4242 in
  let doc =
    "Server port to connect to (default: " ^ Int.to_string default ^ ")"
  in
  Arg.(value & opt int default & info [ "port"; "p" ] ~docv:"PORT" ~doc)

let host_arg =
  let default = "127.0.0.1" in
  let doc = "Server hostname or IP (default: " ^ default ^ ")" in
  Arg.(value & opt string default & info [ "host" ] ~docv:"HOST" ~doc)

let run_server port bind timeout log_level =
  try Lwt_main.run (Server.run ~port ~bind ~timeout ~log_level)
  with e ->
    Lwt_io.eprintf "Server error: %s\n%!" (Printexc.to_string e) |> Lwt_main.run;
    exit 1

let run_client host port timeout log_level =
  try Lwt_main.run (Client.run ~host ~port ~timeout ~log_level)
  with e ->
    Lwt_io.eprintf "Client error: %s\n%!" (Printexc.to_string e) |> Lwt_main.run;
    exit 1

let cmd =
  Cmd.group
    (Cmd.info "chat" ~version:"0.1.0" ~doc:"Simple one-on-one chat application")
    [
      Cmd.make
        (Cmd.info "server" ~doc:"Start a chat server")
        Term.(
          const run_server $ port_server_arg $ bind_arg $ timeout_arg
          $ log_level_arg);
      Cmd.make
        (Cmd.info "client" ~doc:"Connect to a chat server")
        Term.(
          const run_client $ host_arg $ port_client_arg $ timeout_arg
          $ log_level_arg);
    ]

let run () = exit (Cmd.eval cmd)
