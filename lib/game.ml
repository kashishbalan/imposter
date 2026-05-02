type words_map = (string, string list) Hashtbl.t

let project_root () =
  let start =
    if Filename.is_relative Sys.executable_name then Sys.getcwd ()
    else Filename.dirname Sys.executable_name
  in
  let rec find dir =
    let candidate = Filename.concat dir "data/category.txt" in
    if Sys.file_exists candidate then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then failwith "Could not locate data/category.txt"
      else find parent
  in
  find start

let data_file path = Filename.concat (project_root ()) path

let load_categories () =
  let ic = open_in (data_file "data/category.txt") in
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
  let ic = open_in (data_file "data/words.txt") in
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
