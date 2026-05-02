(* lib/io_loop.mli *)

val run : unit -> unit
(** [run ()] starts the Imposter game. Loads categories and words from data
    files, randomly selects a category and secret answer, and enters the main
    game loop. The player is the imposter and must guess the secret word from
    one-word hints. After each round, the player is prompted to play again. *)

module Test : sig
  val bold : string -> string
  val cyan : string -> string
  val green : string -> string
  val red : string -> string
  val yellow : string -> string
  val dim : string -> string
  val clear_screen : unit -> unit
  val print_header : string -> unit
  val print_hint_row : int -> string -> unit
  val print_prompt : unit -> unit
  val print_correct : int -> unit
  val print_wrong : string -> unit
  val print_gave_up : string -> unit
  val print_no_hints : string -> unit
  val get_category : unit -> string
  val get_answer : unit -> string
  val game_loop : string list -> string list -> string -> int -> unit
end
