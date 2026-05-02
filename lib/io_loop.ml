let () = Random.self_init ()

(*ANSI Color Coding for the Terminal Interface*)
let bold s = "\027[1m" ^ s ^ "\027[0m"
let cyan s = "\027[36m" ^ s ^ "\027[0m"
let green s = "\027[32m" ^ s ^ "\027[0m"
let red s = "\027[31m" ^ s ^ "\027[0m"
let yellow s = "\027[33m" ^ s ^ "\027[0m"
let dim s = "\027[2m" ^ s ^ "\027[0m"

(*Layout*)
let line () = print_endline (dim "────────────────────────────────────────")

let blank () = print_newline ()

(* State *)
let categories = Game.load_categories ()
let words_map = Game.load_words ()
let current_category = ref ""
let current_answer = ref ""

(* Helpers *)
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

(* Display *)
let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout

let print_title () =
  blank ();
  line ();
  Printf.printf "  %s\n" (bold (red "             IMPOSTER      "));
  line ();
  blank ()

let print_header category =
  Printf.printf "  %s  %s\n" (bold "Category:") (cyan (bold category));
  Printf.printf "  %s\n"
    (dim "You are the imposter. Guess the secret word using the hints.");
  blank ();
  line ()

let print_hint_row n word =
  blank ();
  Printf.printf "  %s  %s\n" (yellow (Printf.sprintf "[Hint %d]" n)) (bold word)
  
let print_prompt () =
  blank ();
  print_string (dim "  Your guess (or 'give up'): ");
  flush stdout

let print_correct attempts =
  blank ();
  line ();
  Printf.printf "  %s  Solved in %s hint(s)!\n"
    (green (bold "✓ Correct!"))
    (bold (string_of_int attempts));
  line ();
  blank ()

let print_wrong guess =
  blank ();
  Printf.printf "  %s  \"%s\" is not the word.\n" (red "X") (dim guess)

let print_gave_up answer =
  blank ();
  line ();
  Printf.printf "  You gave up. The word was: %s\n" (cyan (bold answer));
  line ();
  blank ()

let print_no_hints answer =
  blank ();
  line ();
  Printf.printf "  Out of hints! The word was: %s\n" (cyan (bold answer));
  line ();
  blank ()

let print_play_again () =
  blank ();
  print_string (dim "  Play again? (y/n): ");
  flush stdout

(* Main game loop *)
let rec game_loop possible_hints previous_guesses answer attempts =
  let available_hints =
    List.filter (fun h -> not (List.mem h previous_guesses)) possible_hints
  in
  match available_hints with
  | [] -> print_no_hints answer
  | _ ->
      let idx = Random.int (List.length available_hints) in
      let hint = List.nth available_hints idx in
      print_hint_row (attempts + 1) hint;
      print_prompt ();
      let input = String.lowercase_ascii (String.trim (read_line ())) in
      if input = "give up" then print_gave_up answer
      else if input = String.lowercase_ascii answer then
        print_correct (attempts + 1)
      else begin
        print_wrong input;
        let new_possible_hints =
          List.filter
            (fun h -> String.lowercase_ascii h <> input && h <> hint)
            possible_hints
        in
        let new_previous_guesses = input :: previous_guesses in
        game_loop new_possible_hints new_previous_guesses answer (attempts + 1)
      end

let rec run () =
  clear_screen ();
  print_title ();
  let category = get_category () in
  let answer = get_answer () in
  let possible_hints = Game.get_hints words_map category answer in
  print_header category;
  game_loop possible_hints [] answer 0;
  print_play_again ();
  let again = String.lowercase_ascii (String.trim (read_line ())) in
  if again = "y" then begin
    run ()
  end
