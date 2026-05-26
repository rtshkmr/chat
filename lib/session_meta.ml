type peer = { handle : string; mutable addr : string; port : int }

let peer_of_sockaddr ~handle = function
  | Unix.ADDR_INET (inet_addr, port) ->
      { handle; addr = Unix.string_of_inet_addr inet_addr; port }
  | Unix.ADDR_UNIX _ -> { handle; addr = "local"; port = 0 }

let maybe_resolve_hostname_bg peer sockaddr =
  let resolve_task () =
    let lookup () =
      try%lwt
        let%lwt info = Lwt_unix.getnameinfo sockaddr [ Unix.NI_NOFQDN ] in
        peer.addr <- info.Unix.ni_hostname;
        Lwt.return_unit
      with _ -> Lwt.return_unit
    in
    let time = 0.3 in
    (* if DNS hasn't answered, the IP string stays *)
    let timeout () = Lwt_unix.sleep time in
    Lwt.pick [ lookup (); timeout () ]
  in
  Lwt.async resolve_task

type t = { me : peer; them : peer; connected_at : float }

(** Tries a best-effort approach of using the socket to do DNS resolution to
    resolve hostnames involved in the chat session.

    NOTE: [of_sock] is expected to be called immediately after connection
    establishment, before the connecting socket, [sock], is handed to Session.
    At this point, [sock] is guaranteed to be open. If [getsockname] or
    [getpeername] raises EBADF, it indicates a bug in the Session creation logic
    (e.g., premature socket closure). This shall be left defenceless. *)
let of_sock sock =
  let me_addr = Unix.getsockname (Lwt_unix.unix_file_descr sock) in
  let them_addr = Unix.getpeername (Lwt_unix.unix_file_descr sock) in
  let me = peer_of_sockaddr ~handle:"me" me_addr in
  let them = peer_of_sockaddr ~handle:"remote" them_addr in
  maybe_resolve_hostname_bg me me_addr;
  maybe_resolve_hostname_bg them them_addr;
  { me; them; connected_at = Unix.gettimeofday () }
