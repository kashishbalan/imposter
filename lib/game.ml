type words_map = (string, string list) Hashtbl.t

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

let get_hints words_map category answer =
  let words = Hashtbl.find words_map category in
  let other_words = List.filter (fun w -> w <> answer) words in
  other_words
  |> List.map (fun x -> (Random.bits (), x))
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd
