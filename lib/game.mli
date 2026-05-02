type words_map = (string, string list) Hashtbl.t
(** A mapping from category names to their possible secret words. *)

val load_categories : unit -> string list
(** [load_categories ()] reads [data/category.txt] and returns the category
    names in file order. *)

val load_words : unit -> words_map
(** [load_words ()] reads [data/words.txt] and returns a mapping from category
    names to their associated words. *)

val get_hints : words_map -> string -> string -> string list
(** [get_hints words_map category answer] returns all words in [words_map] under
    [category] except [answer], in a random order. *)
