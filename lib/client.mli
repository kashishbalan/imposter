(** Imposter game client. *)

val run : host:string -> port:int -> name:string -> unit
(** [run ~host ~port ~name] connects to an Imposter server and plays as the
    given player name. Blocks until the server shuts down or the connection
    drops. *)
