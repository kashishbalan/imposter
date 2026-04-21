Random.self_init ();;

let load_categories () =
  let ic = open_in "data/category.txt" in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (String.trim line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []

let load_words () =
  let ic = open_in "data/words.txt" in
  let rec loop acc =
    try
      let line = input_line ic in
      let colon_pos = String.index line ':' in
      let category = String.sub line 0 colon_pos |> String.trim in
      let words_str =
        String.sub line (colon_pos + 1) (String.length line - colon_pos - 1)
        |> String.trim
      in
      let words = String.split_on_char ',' words_str |> List.map String.trim in
      loop ((category, words) :: acc)
    with End_of_file ->
      close_in ic;
      acc
  in
  let pairs = loop [] in
  let map = Hashtbl.create (List.length pairs) in
  List.iter (fun (cat, ws) -> Hashtbl.add map cat ws) pairs;
  map

let categories = load_categories ()
let words_map = load_words ()
let current_category = ref ""
let current_answer = ref ""

let get_category () =
  let len = List.length categories in
  let idx = Random.int len in
  let cat = List.nth categories idx in
  current_category := cat;
  cat

let get_answer () =
  let words = Hashtbl.find words_map !current_category in
  let len = List.length words in
  let idx = Random.int len in
  let ans = List.nth words idx in
  current_answer := ans;
  ans

let get_hints () =
  let words = Hashtbl.find words_map !current_category in
  let other_words = List.filter (fun w -> w <> !current_answer) words in
  other_words

(* Main game loop *)
let rec game_loop possible_hints previous_guesses answer attempts =
  let available_hints =
    List.filter (fun h -> not (List.mem h previous_guesses)) possible_hints
  in
  match available_hints with
  | [] ->
      print_endline ("No more hints! The word was: " ^ answer);
      print_endline "Better luck next time!"
  | _ ->
      let idx = Random.int (List.length available_hints) in
      let hint = List.nth available_hints idx in
      Printf.printf "Hint: %s\n" hint;
      print_string "Your guess (or type 'give up'): ";
      let input = String.lowercase_ascii (String.trim (read_line ())) in
      if input = "give up" then begin
        Printf.printf "The word was: %s\n" answer;
        print_endline "Thanks for playing!"
      end
      else if input = String.lowercase_ascii answer then begin
        Printf.printf "Correct! You got it in %d hint(s)!\n" (attempts + 1)
      end
      else begin
        print_endline "Not quite, here's another hint...";
        let new_possible_hints =
          List.filter
            (fun h -> String.lowercase_ascii h <> input && h <> hint)
            possible_hints
        in
        let new_previous_guesses = input :: previous_guesses in
        game_loop new_possible_hints new_previous_guesses answer (attempts + 1)
      end

let run () =
  let category = get_category () in
  let answer = get_answer () in
  let possible_hints = get_hints () in
  Printf.printf "Category: %s\n" category;
  print_endline "Try to guess the secret word!";
  print_endline "----------------------------";
  game_loop possible_hints [] answer 0
