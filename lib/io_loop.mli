(* lib/io_loop.mli *)

(** [run ()] starts the Imposter game. Loads categories and words from
    data files, randomly selects a category and secret answer, and enters
    the main game loop. The player is the imposter and must guess the
    secret word from one-word hints. After each round, the player is
    prompted to play again. *)
val run : unit -> unit