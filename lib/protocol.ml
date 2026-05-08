(* Minimal line-oriented protocol. Each message is one JSON object on one line.

   We avoid pulling in yojson by hand-rolling a tiny encoder/decoder. To keep
   that tractable, all string values are sanitized: we strip control chars,
   double-quotes, and backslashes before they ever hit the wire. This is
   enforced by [sanitize] below, which is called on every outgoing string. *)

type client_msg =
  | Join of string
  | Start
  | Clue of string
  | Vote of string
  | ImposterGuess of string
  | PlayAgain of bool

type server_msg =
  | Welcome of string
  | LobbyUpdate of string list
  | Error of string
  | RoundStart of {
      category : string;
      role : [ `Imposter | `Crew ];
      word : string option;
      players : string list;
      clue_order : string list;
    }
  | YourTurnClue
  | CluePosted of {
      player : string;
      clue : string;
    }
  | YourTurnVote of { candidates : string list }
  | VotePosted of {
      voter : string;
      voted_for : string;
    }
  | Accused of {
      player : string;
      was_imposter : bool;
    }
  | YourTurnGuess of { hint : string }
  | RoundEnd of {
      winner : [ `Crew | `Imposter ];
      imposter : string;
      word : string;
      reason : string;
    }
  | YourTurnPlayAgain
  | ServerShutdown of string

(* ---------- Sanitization & primitives ---------- *)

let sanitize s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' | '\\' -> Buffer.add_char buf '_'
      | c when Char.code c < 0x20 -> Buffer.add_char buf ' '
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let q s = "\"" ^ sanitize s ^ "\""
let kv k v = q k ^ ":" ^ v
let kv_str k v = kv k (q v)
let kv_bool k v = kv k (if v then "true" else "false")
let kv_list k items = kv k ("[" ^ String.concat "," (List.map q items) ^ "]")
let obj fields = "{" ^ String.concat "," fields ^ "}"

(* ---------- Encoding ---------- *)

let encode_client = function
  | Join name -> obj [ kv_str "type" "join"; kv_str "name" name ]
  | Start -> obj [ kv_str "type" "start" ]
  | Clue c -> obj [ kv_str "type" "clue"; kv_str "clue" c ]
  | Vote v -> obj [ kv_str "type" "vote"; kv_str "voted_for" v ]
  | ImposterGuess g -> obj [ kv_str "type" "imposter_guess"; kv_str "guess" g ]
  | PlayAgain b -> obj [ kv_str "type" "play_again"; kv_bool "yes" b ]

let role_str = function
  | `Imposter -> "imposter"
  | `Crew -> "crew"

let winner_str = function
  | `Imposter -> "imposter"
  | `Crew -> "crew"

let encode_server = function
  | Welcome name -> obj [ kv_str "type" "welcome"; kv_str "name" name ]
  | LobbyUpdate players ->
      obj [ kv_str "type" "lobby"; kv_list "players" players ]
  | Error m -> obj [ kv_str "type" "error"; kv_str "message" m ]
  | RoundStart { category; role; word; players; clue_order } ->
      let word_field =
        match word with
        | Some w -> kv_str "word" w
        | None -> kv "word" "null"
      in
      obj
        [
          kv_str "type" "round_start";
          kv_str "category" category;
          kv_str "role" (role_str role);
          word_field;
          kv_list "players" players;
          kv_list "clue_order" clue_order;
        ]
  | YourTurnClue -> obj [ kv_str "type" "your_turn_clue" ]
  | CluePosted { player; clue } ->
      obj
        [
          kv_str "type" "clue_posted";
          kv_str "player" player;
          kv_str "clue" clue;
        ]
  | YourTurnVote { candidates } ->
      obj [ kv_str "type" "your_turn_vote"; kv_list "candidates" candidates ]
  | VotePosted { voter; voted_for } ->
      obj
        [
          kv_str "type" "vote_posted";
          kv_str "voter" voter;
          kv_str "voted_for" voted_for;
        ]
  | Accused { player; was_imposter } ->
      obj
        [
          kv_str "type" "accused";
          kv_str "player" player;
          kv_bool "was_imposter" was_imposter;
        ]
  | YourTurnGuess { hint } ->
      obj [ kv_str "type" "your_turn_guess"; kv_str "hint" hint ]
  | RoundEnd { winner; imposter; word; reason } ->
      obj
        [
          kv_str "type" "round_end";
          kv_str "winner" (winner_str winner);
          kv_str "imposter" imposter;
          kv_str "word" word;
          kv_str "reason" reason;
        ]
  | YourTurnPlayAgain -> obj [ kv_str "type" "your_turn_play_again" ]
  | ServerShutdown m -> obj [ kv_str "type" "shutdown"; kv_str "message" m ]

(* ---------- Decoding ----------

   Tiny recursive-descent parser. Only handles strings, bools, null, lists of
   strings, and flat objects — exactly what the encoder produces. *)

exception Parse_error of string

let parse_error msg = raise (Parse_error msg)

type parser_state = {
  src : string;
  mutable pos : int;
}

let peek p = if p.pos >= String.length p.src then None else Some p.src.[p.pos]
let advance p = p.pos <- p.pos + 1

let skip_ws p =
  while
    p.pos < String.length p.src
    &&
    let c = p.src.[p.pos] in
    c = ' ' || c = '\t'
  do
    advance p
  done

let expect p c =
  skip_ws p;
  match peek p with
  | Some x when x = c -> advance p
  | Some x ->
      parse_error (Printf.sprintf "expected '%c' at %d, got '%c'" c p.pos x)
  | None -> parse_error (Printf.sprintf "expected '%c', got EOF" c)

let parse_string p =
  skip_ws p;
  expect p '"';
  let start = p.pos in
  while p.pos < String.length p.src && p.src.[p.pos] <> '"' do
    advance p
  done;
  if p.pos >= String.length p.src then parse_error "unterminated string";
  let s = String.sub p.src start (p.pos - start) in
  advance p;
  (* consume closing quote *)
  s

let parse_literal p lit =
  skip_ws p;
  let n = String.length lit in
  if p.pos + n > String.length p.src then parse_error ("expected " ^ lit);
  if String.sub p.src p.pos n <> lit then parse_error ("expected " ^ lit);
  p.pos <- p.pos + n

let parse_bool p =
  skip_ws p;
  match peek p with
  | Some 't' ->
      parse_literal p "true";
      true
  | Some 'f' ->
      parse_literal p "false";
      false
  | _ -> parse_error "expected bool"

(* Returns string list. The encoder only ever emits string lists. *)
let parse_string_list p =
  expect p '[';
  skip_ws p;
  let items = ref [] in
  (match peek p with
  | Some ']' -> advance p
  | _ ->
      items := [ parse_string p ];
      let continue = ref true in
      while !continue do
        skip_ws p;
        match peek p with
        | Some ',' ->
            advance p;
            items := parse_string p :: !items
        | Some ']' ->
            advance p;
            continue := false
        | _ -> parse_error "expected ',' or ']' in list"
      done);
  List.rev !items

(* Parse one field. Returns (key, value) where value is one of: `Str s | `Bool b
   | `Null | `List xs *)
type field_value =
  [ `Str of string
  | `Bool of bool
  | `Null
  | `List of string list
  ]

let parse_field p : string * field_value =
  let key = parse_string p in
  expect p ':';
  skip_ws p;
  let value : field_value =
    match peek p with
    | Some '"' -> `Str (parse_string p)
    | Some 't' | Some 'f' -> `Bool (parse_bool p)
    | Some 'n' ->
        parse_literal p "null";
        `Null
    | Some '[' -> `List (parse_string_list p)
    | Some c -> parse_error (Printf.sprintf "unexpected char '%c' in value" c)
    | None -> parse_error "EOF in value"
  in
  (key, value)

let parse_object p =
  expect p '{';
  skip_ws p;
  let fields = ref [] in
  (match peek p with
  | Some '}' -> advance p
  | _ ->
      fields := [ parse_field p ];
      let continue = ref true in
      while !continue do
        skip_ws p;
        match peek p with
        | Some ',' ->
            advance p;
            skip_ws p;
            fields := parse_field p :: !fields
        | Some '}' ->
            advance p;
            continue := false
        | _ -> parse_error "expected ',' or '}' in object"
      done);
  !fields

let get_str fields k =
  match List.assoc_opt k fields with
  | Some (`Str s) -> s
  | _ -> parse_error ("missing or non-string field: " ^ k)

let get_bool fields k =
  match List.assoc_opt k fields with
  | Some (`Bool b) -> b
  | _ -> parse_error ("missing or non-bool field: " ^ k)

let get_list fields k =
  match List.assoc_opt k fields with
  | Some (`List xs) -> xs
  | _ -> parse_error ("missing or non-list field: " ^ k)

let get_str_or_null fields k =
  match List.assoc_opt k fields with
  | Some (`Str s) -> Some s
  | Some `Null -> None
  | _ -> parse_error ("missing string-or-null field: " ^ k)

let decode_client line =
  try
    let p = { src = line; pos = 0 } in
    let fields = parse_object p in
    match get_str fields "type" with
    | "join" -> Ok (Join (get_str fields "name"))
    | "start" -> Ok Start
    | "clue" -> Ok (Clue (get_str fields "clue"))
    | "vote" -> Ok (Vote (get_str fields "voted_for"))
    | "imposter_guess" -> Ok (ImposterGuess (get_str fields "guess"))
    | "play_again" -> Ok (PlayAgain (get_bool fields "yes"))
    | t -> Error ("unknown client message type: " ^ t)
  with Parse_error m -> Error m

let role_of_string = function
  | "imposter" -> `Imposter
  | "crew" -> `Crew
  | s -> parse_error ("bad role: " ^ s)

let winner_of_string = function
  | "imposter" -> `Imposter
  | "crew" -> `Crew
  | s -> parse_error ("bad winner: " ^ s)

let decode_server line =
  try
    let p = { src = line; pos = 0 } in
    let fields = parse_object p in
    match get_str fields "type" with
    | "welcome" -> Ok (Welcome (get_str fields "name"))
    | "lobby" -> Ok (LobbyUpdate (get_list fields "players"))
    | "error" -> Ok (Error (get_str fields "message"))
    | "round_start" ->
        Ok
          (RoundStart
             {
               category = get_str fields "category";
               role = role_of_string (get_str fields "role");
               word = get_str_or_null fields "word";
               players = get_list fields "players";
               clue_order = get_list fields "clue_order";
             })
    | "your_turn_clue" -> Ok YourTurnClue
    | "clue_posted" ->
        Ok
          (CluePosted
             { player = get_str fields "player"; clue = get_str fields "clue" })
    | "your_turn_vote" ->
        Ok (YourTurnVote { candidates = get_list fields "candidates" })
    | "vote_posted" ->
        Ok
          (VotePosted
             {
               voter = get_str fields "voter";
               voted_for = get_str fields "voted_for";
             })
    | "accused" ->
        Ok
          (Accused
             {
               player = get_str fields "player";
               was_imposter = get_bool fields "was_imposter";
             })
    | "your_turn_guess" -> Ok (YourTurnGuess { hint = get_str fields "hint" })
    | "round_end" ->
        Ok
          (RoundEnd
             {
               winner = winner_of_string (get_str fields "winner");
               imposter = get_str fields "imposter";
               word = get_str fields "word";
               reason = get_str fields "reason";
             })
    | "your_turn_play_again" -> Ok YourTurnPlayAgain
    | "shutdown" -> Ok (ServerShutdown (get_str fields "message"))
    | t -> Error ("unknown server message type: " ^ t)
  with Parse_error m -> Error m
