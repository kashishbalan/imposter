(* Imposter networked client.

   Two threads: the main thread reads stdin and sends to the server; a reader
   thread pulls server messages and renders them. We use a flag to track whether
   we're currently expecting input — when not, stdin is ignored (read but
   discarded with a hint). *)

(* ---------- Display helpers (mirrors the original io_loop.ml style)
   ---------- *)

let bold s = "\027[1m" ^ s ^ "\027[0m"
let cyan s = "\027[36m" ^ s ^ "\027[0m"
let green s = "\027[32m" ^ s ^ "\027[0m"
let red s = "\027[31m" ^ s ^ "\027[0m"
let yellow s = "\027[33m" ^ s ^ "\027[0m"
let dim s = "\027[2m" ^ s ^ "\027[0m"

let print_line () =
  print_endline (dim "────────────────────────────────────────")

(* ---------- State the reader thread needs ---------- *)

(* What the client is waiting for from the user. The reader thread sets this
   when the server tells us it's our turn; the main thread clears it after
   sending. *)
type prompt =
  | None_pending
  | Clue_pending
  | Vote_pending of string list
  | Guess_pending
  | PlayAgain_pending

let prompt_state = ref None_pending
let prompt_mu = Mutex.create ()

let set_prompt p =
  Mutex.lock prompt_mu;
  prompt_state := p;
  Mutex.unlock prompt_mu

(* My own name — set on Welcome. Currently informational only; could be used to
   special-case rendering of "your turn" prompts vs other players. *)
let my_name = ref ""

(* Track the in-round event log so we can show clues in order if we ever want to
   redisplay them (e.g. before voting). *)
let clue_log : (string * string) list ref = ref []
let reset_round_state () = clue_log := []

(* ---------- Renderers ---------- *)

let render_round_start ~category ~role ~word ~players ~clue_order =
  print_string "\027[2J\027[H";
  flush stdout;
  print_newline ();
  print_line ();
  Printf.printf "  %s\n" (bold (red "             IMPOSTER      "));
  print_line ();
  print_newline ();
  Printf.printf "  %s  %s\n" (bold "Category:") (cyan (bold category));
  (match role with
  | `Crew ->
      let w =
        match word with
        | Some w -> w
        | None -> "?"
      in
      Printf.printf "  %s  You are %s. The secret word is %s\n" (bold "Role:")
        (green (bold "CREW"))
        (cyan (bold w));
      print_endline
        (dim "  Give a one-word clue when it's your turn. Don't be too obvious.")
  | `Imposter ->
      Printf.printf "  %s  You are the %s. You don't know the word.\n"
        (bold "Role:")
        (red (bold "IMPOSTER"));
      print_endline
        (dim
           "  Bluff a clue from context. Survive the vote — or guess if caught."));
  print_newline ();
  Printf.printf "  %s %s\n" (bold "Players:") (String.concat ", " players);
  Printf.printf "  %s %s\n" (bold "Clue order:")
    (String.concat " → " clue_order);
  print_line ();
  print_newline ()

let render_clue_posted ~player ~clue =
  Printf.printf "  %s  %s: %s\n" (yellow "[clue]") (bold player) (cyan clue);
  flush stdout

let render_vote_posted ~voter ~voted_for =
  Printf.printf "  %s  %s voted for %s\n" (yellow "[vote]") (bold voter)
    (bold voted_for);
  flush stdout

let render_accused ~player ~was_imposter =
  print_newline ();
  print_line ();
  if was_imposter then
    Printf.printf "  %s %s was accused — and they %s the imposter!\n" (red "▶")
      (bold player) (green "WERE")
  else
    Printf.printf "  %s %s was accused — but they %s the imposter.\n" (red "▶")
      (bold player) (red "WERE NOT");
  print_line ()

let render_round_end ~winner ~imposter ~word ~reason =
  print_newline ();
  print_line ();
  let winner_str =
    match winner with
    | `Crew -> green (bold "CREW WINS")
    | `Imposter -> red (bold "IMPOSTER WINS")
  in
  Printf.printf "  %s\n" winner_str;
  Printf.printf "  Imposter was: %s\n" (bold imposter);
  Printf.printf "  The word was: %s\n" (cyan (bold word));
  Printf.printf "  %s\n" (dim reason);
  print_line ();
  print_newline ()

let render_lobby players =
  Printf.printf "  %s %s (%d)\n" (dim "Lobby:")
    (String.concat ", " players)
    (List.length players);
  flush stdout

(* ---------- Reader thread ---------- *)

let reader_loop in_chan =
  let rec loop () =
    match
      try Some (input_line in_chan) with End_of_file | Sys_error _ -> None
    with
    | None ->
        print_newline ();
        print_endline (red "  ✗ disconnected from server");
        exit 0
    | Some line -> (
        match Protocol.decode_server line with
        | Error e -> Printf.printf "  %s bad server message: %s\n" (red "!") e
        | Ok msg ->
            (match msg with
            | Protocol.Welcome name ->
                my_name := name;
                Printf.printf "  %s connected as %s\n" (green "✓") (bold name)
            | Protocol.LobbyUpdate players -> render_lobby players
            | Protocol.Error m -> Printf.printf "  %s %s\n" (red "!") m
            | Protocol.RoundStart { category; role; word; players; clue_order }
              ->
                reset_round_state ();
                render_round_start ~category ~role ~word ~players ~clue_order
            | Protocol.YourTurnClue ->
                set_prompt Clue_pending;
                print_string (dim "  Your one-word clue: ");
                flush stdout
            | Protocol.CluePosted { player; clue } ->
                clue_log := !clue_log @ [ (player, clue) ];
                render_clue_posted ~player ~clue
            | Protocol.YourTurnVote { candidates } ->
                set_prompt (Vote_pending candidates);
                print_newline ();
                Printf.printf "  Vote for the imposter (%s): "
                  (dim (String.concat ", " candidates));
                flush stdout
            | Protocol.VotePosted { voter; voted_for } ->
                render_vote_posted ~voter ~voted_for
            | Protocol.Accused { player; was_imposter } ->
                render_accused ~player ~was_imposter
            | Protocol.YourTurnGuess _ ->
                set_prompt Guess_pending;
                print_newline ();
                print_string
                  (dim
                     "  You're the imposter and you've been caught. Guess the \
                      word: ");
                flush stdout
            | Protocol.RoundEnd { winner; imposter; word; reason } ->
                render_round_end ~winner ~imposter ~word ~reason
            | Protocol.YourTurnPlayAgain ->
                set_prompt PlayAgain_pending;
                print_string (dim "  Play again? (y/n): ");
                flush stdout
            | Protocol.ServerShutdown m ->
                print_newline ();
                Printf.printf "  %s server: %s\n" (cyan "•") m;
                exit 0);
            loop ())
  in
  loop ()

(* ---------- Main thread: stdin → server ---------- *)

let send out_chan msg =
  let line = Protocol.encode_client msg in
  output_string out_chan line;
  output_char out_chan '\n';
  flush out_chan

let main_loop out_chan =
  let rec loop () =
    let line = try Some (input_line stdin) with End_of_file -> None in
    match line with
    | None -> ()
    | Some raw ->
        let trimmed = String.trim raw in
        let current =
          Mutex.lock prompt_mu;
          let p = !prompt_state in
          Mutex.unlock prompt_mu;
          p
        in
        (match current with
        | None_pending ->
            (* Special case: lobby. Allow "start" to start the game. *)
            if String.lowercase_ascii trimmed = "start" then
              send out_chan Protocol.Start
            else if trimmed = "" then ()
            else
              Printf.printf
                "  %s waiting on the server (type 'start' if you're host)\n"
                (dim "·")
        | Clue_pending ->
            if trimmed = "" then begin
              print_string (dim "  Your one-word clue: ");
              flush stdout
            end
            else begin
              let one_word =
                match String.split_on_char ' ' trimmed with
                | w :: _ -> w
                | [] -> trimmed
              in
              send out_chan (Protocol.Clue one_word);
              set_prompt None_pending
            end
        | Vote_pending candidates -> (
            let target =
              List.find_opt
                (fun c ->
                  String.lowercase_ascii c = String.lowercase_ascii trimmed)
                candidates
            in
            match target with
            | Some t ->
                send out_chan (Protocol.Vote t);
                set_prompt None_pending
            | None ->
                Printf.printf "  %s pick one of: %s\n  Vote: " (red "!")
                  (String.concat ", " candidates);
                flush stdout)
        | Guess_pending ->
            if trimmed = "" then begin
              print_string (dim "  Guess the word: ");
              flush stdout
            end
            else begin
              send out_chan (Protocol.ImposterGuess trimmed);
              set_prompt None_pending
            end
        | PlayAgain_pending -> (
            match String.lowercase_ascii trimmed with
            | "y" | "yes" ->
                send out_chan (Protocol.PlayAgain true);
                set_prompt None_pending
            | "n" | "no" ->
                send out_chan (Protocol.PlayAgain false);
                set_prompt None_pending
            | _ ->
                print_string (dim "  Play again? (y/n): ");
                flush stdout));
        loop ()
  in
  loop ()

let run ~host ~port ~name =
  let addr =
    try (Unix.gethostbyname host).Unix.h_addr_list.(0)
    with Not_found ->
      Printf.eprintf "could not resolve host: %s\n" host;
      exit 1
  in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (try Unix.connect sock (Unix.ADDR_INET (addr, port))
   with Unix.Unix_error (e, _, _) ->
     Printf.eprintf "connect failed: %s\n" (Unix.error_message e);
     exit 1);
  let in_chan = Unix.in_channel_of_descr sock in
  let out_chan = Unix.out_channel_of_descr sock in
  send out_chan (Protocol.Join name);
  let _reader : Thread.t = Thread.create reader_loop in_chan in
  print_endline (dim "  (Once everyone has joined, the host types 'start'.)");
  main_loop out_chan
