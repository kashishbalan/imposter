open OUnit2

let contains_substring ~substr s =
  let len = String.length substr in
  let rec loop i =
    if i + len > String.length s then false
    else if String.sub s i len = substr then true
    else loop (i + 1)
  in
  loop 0

let assert_contains ~ctxt:_ substr s =
  assert_bool
    ("Expected output to contain: " ^ substr)
    (contains_substring ~substr s)

let with_redirected_io input f =
  let open Unix in
  let stdin_r, stdin_w = pipe () in
  let stdout_r, stdout_w = pipe () in
  let saved_stdin = dup stdin in
  let saved_stdout = dup stdout in
  try
    dup2 stdin_r stdin;
    dup2 stdout_w stdout;
    close stdin_r;
    close stdout_w;
    let oc_in = out_channel_of_descr stdin_w in
    output_string oc_in input;
    close_out oc_in;
    let result = f () in
    flush Stdlib.stdout;
    dup2 saved_stdin stdin;
    dup2 saved_stdout stdout;
    close saved_stdin;
    close saved_stdout;
    let ic_out = in_channel_of_descr stdout_r in
    let buffer = Buffer.create 256 in
    (try
       while true do
         Buffer.add_string buffer (input_line ic_out);
         Buffer.add_char buffer '\n'
       done
     with End_of_file -> ());
    close_in ic_out;
    (result, Buffer.contents buffer)
  with exn ->
    dup2 saved_stdin stdin;
    dup2 saved_stdout stdout;
    close saved_stdin;
    close saved_stdout;
    raise exn

let test_load_categories _ =
  let categories = Imposter.Game.load_categories () in
  assert_equal "Animals" (List.hd categories);
  assert_equal 50 (List.length categories)

let test_load_words _ =
  let words_map = Imposter.Game.load_words () in
  let animals = Hashtbl.find words_map "Animals" in
  assert_bool "lion present" (List.exists (( = ) "lion") animals);
  assert_bool "shark present" (List.exists (( = ) "shark") animals);
  assert_equal 9 (List.length animals)

let test_get_hints _ =
  let words_map = Imposter.Game.load_words () in
  Random.init 0;
  let hints = Imposter.Game.get_hints words_map "Animals" "lion" in
  assert_equal 8 (List.length hints);
  assert_bool "answer excluded" (not (List.exists (( = ) "lion") hints));
  assert_equal
    (List.sort String.compare
       [
         "cobra";
         "dolphin";
         "elephant";
         "giraffe";
         "kangaroo";
         "penguin";
         "shark";
         "tiger";
       ])
    (List.sort String.compare hints)

let test_format_helpers _ =
  assert_equal "\027[1mhi\027[0m" (Imposter.Io_loop.Test.bold "hi");
  assert_equal "\027[36mhi\027[0m" (Imposter.Io_loop.Test.cyan "hi");
  assert_equal "\027[32mhi\027[0m" (Imposter.Io_loop.Test.green "hi");
  assert_equal "\027[31mhi\027[0m" (Imposter.Io_loop.Test.red "hi");
  assert_equal "\027[33mhi\027[0m" (Imposter.Io_loop.Test.yellow "hi");
  assert_equal "\027[2mhi\027[0m" (Imposter.Io_loop.Test.dim "hi")

let test_print_header _ =
  let _, out =
    with_redirected_io "" (fun () ->
        Imposter.Io_loop.Test.print_header "Fruits")
  in
  assert_contains ~ctxt:() "Category:" out;
  assert_contains ~ctxt:() "Fruits" out

let test_print_hint_row _ =
  let _, out =
    with_redirected_io "" (fun () ->
        Imposter.Io_loop.Test.print_hint_row 2 "apple")
  in
  assert_contains ~ctxt:() "[Hint 2]" out;
  assert_contains ~ctxt:() "apple" out

let test_game_loop_give_up _ =
  Random.init 0;
  let _, out =
    with_redirected_io "give up\n" (fun () ->
        Imposter.Io_loop.Test.game_loop [ "apple"; "banana" ] [] "orange" 0)
  in
  assert_contains ~ctxt:() "Your guess" out;
  assert_contains ~ctxt:() "gave up" out

let test_game_loop_no_hints_after_wrong _ =
  Random.init 1;
  let _, out =
    with_redirected_io "kiwi\n" (fun () ->
        Imposter.Io_loop.Test.game_loop [ "apple" ] [] "orange" 0)
  in
  assert_contains ~ctxt:() "Out of hints!" out;
  assert_contains ~ctxt:() "is not the word" out

let test_get_category_answer _ =
  Random.init 42;
  let category = Imposter.Io_loop.Test.get_category () in
  assert_bool "valid category"
    (List.mem category (Imposter.Game.load_categories ()));
  let answer = Imposter.Io_loop.Test.get_answer () in
  let words = Hashtbl.find (Imposter.Game.load_words ()) category in
  assert_bool "valid answer" (List.mem answer words)

let suite =
  "Imposter Tests"
  >::: [
         "load_categories" >:: test_load_categories;
         "load_words" >:: test_load_words;
         "get_hints" >:: test_get_hints;
         "format_helpers" >:: test_format_helpers;
         "print_header" >:: test_print_header;
         "print_hint_row" >:: test_print_hint_row;
         "game_loop_give_up" >:: test_game_loop_give_up;
         "game_loop_no_hints_after_wrong"
         >:: test_game_loop_no_hints_after_wrong;
         "get_category_answer" >:: test_get_category_answer;
       ]

let () = run_test_tt_main suite
