type peer = { handle : string; mutable addr : string; port : int }
type t = { me : peer; them : peer; connected_at : float }

val of_sock : Lwt_unix.file_descr -> t
(** Synchronously creates meta from a connected socket — derives both addresses
    via getsockname/getpeername, fires background DNS. *)
