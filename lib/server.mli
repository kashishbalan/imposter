(** Imposter game server. *)

val run : port:int -> unit
(** [run ~port] starts the server on the given TCP port. Blocks forever (or
    until the listening socket is closed). The first connecting client is
    treated as host and may issue a [Start] to begin the game. *)
