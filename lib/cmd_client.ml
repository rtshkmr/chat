(* TODO: add arg validation:
   - e.g. port should be safe, use regex for hostname/ip addr (ref RFC) *)
(* TODO: consider better error handling *)

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
    [ ("debug", Debug); ("info", Info); ("warn", Warning); ("error", Error) ]
  in
  let default, default_str = (Info, "info") in
  let doc =
    "Logging level: debug, info, warn, error (default: " ^ default_str ^ ")"
  in
  Arg.(
    value & opt (enum levels) default & info [ "log-level" ] ~docv:"LEVEL" ~doc)

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

let run_client host port timeout log_level =
  try Lwt_main.run (Client.run ~host ~port ~timeout ~log_level)
  with e ->
    Lwt_io.eprintf "Client error: %s\n%!" (Printexc.to_string e) |> Lwt_main.run;
    exit 1

let cmd =
  Cmd.v
    (Cmd.info "client" ~doc:"Connect to a chat server")
    Term.(
      const run_client $ host_arg $ port_client_arg $ timeout_arg
      $ log_level_arg)
