(* Placeholder - Hannah's module will provide this *)
let get_hints () : string list =
  [ "it's sweet"; "it's a fruit"; "it's tropical"; "it's yellow" ]

(* Placeholder - Hannah's module will provide this *)
let get_answer () : string = "mango"

(* Placeholder - Hannah's module will provide this *)
let get_category () : string = "Fruits"

(* Main game loop *)
let rec game_loop hints answer attempts =
  match hints with
  | [] ->
      print_endline ("No more hints! The word was: " ^ answer);
      print_endline "Better luck next time!"
  | hint :: remaining_hints ->
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
        game_loop remaining_hints answer (attempts + 1)
      end

let run () =
  let category = get_category () in
  let hints = get_hints () in
  let answer = get_answer () in
  Printf.printf "Category: %s\n" category;
  print_endline "Try to guess the secret word!";
  print_endline "----------------------------";
  game_loop hints answer 0
