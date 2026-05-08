(* Networked Imposter server.

   Architecture: - One acceptor thread takes new TCP connections. While the game
   is in the Lobby phase, new connections become players. Once a round starts,
   new connections are rejected. - Each connected client has a dedicated reader
   thread that parses line-delimited messages and pushes them onto a single
   shared event queue. - One main game thread consumes events from that queue
   and runs the game state machine. All game-state mutation happens here, so we
   don't need locks around the state itself — only around the client table
   (added to by the acceptor, read by the game thread). - Writes go directly
   through each client's out_channel, guarded by a per-client mutex (so a
   broadcast from one thread doesn't interleave bytes with a private message
   from another).

   This keeps the game logic itself purely sequential and easy to read, which
   matches the existing single-player code's style. *)

(* ---------- Client table ---------- *)

type client = {
  id : int;
  name : string;
  in_chan : in_channel;
  out_chan : out_channel;
  write_mu : Mutex.t;
  mutable alive : bool;
}

(* Events flowing to the game thread. *)
type event =
  | Msg of int * Protocol.client_msg  (** client id, message *)
  | Disconnected of int  (** client id dropped *)

(* ---------- Shared state ---------- *)

let clients : (int, client) Hashtbl.t = Hashtbl.create 16
let clients_mu = Mutex.create ()
let next_id = ref 0

(* Single event queue feeding the game thread. *)
let events : event Queue.t = Queue.create ()
let events_mu = Mutex.create ()
let events_cv = Condition.create ()
let lobby_open = ref true (* whether new connections become players *)

(* ---------- Helpers ---------- *)

let with_mutex mu f =
  Mutex.lock mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock mu) f

let push_event ev =
  with_mutex events_mu (fun () ->
      Queue.push ev events;
      Condition.signal events_cv)

let pop_event () =
  Mutex.lock events_mu;
  while Queue.is_empty events do
    Condition.wait events_cv events_mu
  done;
  let ev = Queue.pop events in
  Mutex.unlock events_mu;
  ev

let send_to client msg =
  if not client.alive then ()
  else begin
    let line = Protocol.encode_server msg in
    Mutex.lock client.write_mu;
    (try
       output_string client.out_chan line;
       output_char client.out_chan '\n';
       flush client.out_chan
     with Sys_error _ | End_of_file -> client.alive <- false);
    Mutex.unlock client.write_mu
  end

let send_to_id id msg =
  match with_mutex clients_mu (fun () -> Hashtbl.find_opt clients id) with
  | Some c -> send_to c msg
  | None -> ()

let all_clients () =
  with_mutex clients_mu (fun () ->
      Hashtbl.fold (fun _ c acc -> c :: acc) clients [])

let broadcast msg = List.iter (fun c -> send_to c msg) (all_clients ())

let close_client c =
  c.alive <- false;
  (* Closing both channels closes the underlying fd. Wrap in try to avoid
     double-close issues if the peer already disconnected. *)
  (try close_in c.in_chan with _ -> ());
  try close_out c.out_chan with _ -> ()

let drop_client id =
  with_mutex clients_mu (fun () ->
      match Hashtbl.find_opt clients id with
      | Some c ->
          close_client c;
          Hashtbl.remove clients id
      | None -> ())

let player_names () =
  let cs = all_clients () in
  let cs = List.sort (fun a b -> compare a.id b.id) cs in
  List.map (fun c -> c.name) cs

let find_by_name name =
  List.find_opt
    (fun c -> String.lowercase_ascii c.name = String.lowercase_ascii name)
    (all_clients ())

(* ---------- Reader thread ---------- *)

let reader_loop client =
  let rec loop () =
    match
      try Some (input_line client.in_chan)
      with End_of_file | Sys_error _ -> None
    with
    | None -> push_event (Disconnected client.id)
    | Some line -> (
        match Protocol.decode_client line with
        | Ok msg ->
            push_event (Msg (client.id, msg));
            loop ()
        | Error e ->
            send_to client (Protocol.Error ("bad message: " ^ e));
            loop ())
  in
  loop ()

(* ---------- Acceptor thread ----------

   Reads only the [Join] message synchronously, then registers the client and
   spawns a reader thread. *)

let sanitize_name raw =
  let raw = String.trim raw in
  let buf = Buffer.create (String.length raw) in
  String.iter
    (fun c ->
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '_' || c = '-' || c = ' '
      then Buffer.add_char buf c)
    raw;
  let s = Buffer.contents buf |> String.trim in
  if s = "" then None else Some s

let unique_name desired =
  (* Append _2, _3 etc if collision. *)
  let exists n =
    List.exists
      (fun c -> String.lowercase_ascii c.name = String.lowercase_ascii n)
      (all_clients ())
  in
  if not (exists desired) then desired
  else
    let rec try_n i =
      let cand = Printf.sprintf "%s_%d" desired i in
      if exists cand then try_n (i + 1) else cand
    in
    try_n 2

let handshake sock =
  (* Reads one line; expects [Join name]. Returns the chosen client record. *)
  let in_chan = Unix.in_channel_of_descr sock in
  let out_chan = Unix.out_channel_of_descr sock in
  let cleanup () =
    (try close_in in_chan with _ -> ());
    try close_out out_chan with _ -> ()
  in
  let send_err_and_close msg =
    let err = Protocol.encode_server (Protocol.Error msg) in
    (try
       output_string out_chan (err ^ "\n");
       flush out_chan
     with _ -> ());
    cleanup ()
  in
  match
    try Some (input_line in_chan) with End_of_file | Sys_error _ -> None
  with
  | None ->
      cleanup ();
      None
  | Some line -> (
      match Protocol.decode_client line with
      | Ok (Protocol.Join raw_name) -> (
          match sanitize_name raw_name with
          | None ->
              send_err_and_close "name must contain at least one letter/digit";
              None
          | Some name ->
              if not !lobby_open then begin
                send_err_and_close "game already in progress";
                None
              end
              else begin
                let final_name = unique_name name in
                let id =
                  with_mutex clients_mu (fun () ->
                      let i = !next_id in
                      incr next_id;
                      i)
                in
                let client =
                  {
                    id;
                    name = final_name;
                    in_chan;
                    out_chan;
                    write_mu = Mutex.create ();
                    alive = true;
                  }
                in
                with_mutex clients_mu (fun () -> Hashtbl.add clients id client);
                send_to client (Protocol.Welcome final_name);
                Some client
              end)
      | Ok _ ->
          send_err_and_close "first message must be 'join'";
          None
      | Error e ->
          send_err_and_close e;
          None)

let acceptor_loop listen_sock =
  while true do
    match
      try Some (Unix.accept listen_sock) with Unix.Unix_error _ -> None
    with
    | None -> Thread.delay 0.1
    | Some (sock, _addr) -> (
        match handshake sock with
        | None -> ()
        | Some client ->
            broadcast (Protocol.LobbyUpdate (player_names ()));
            let _ : Thread.t = Thread.create reader_loop client in
            ())
  done

(* ---------- Game state machine ----------

   Run on a dedicated thread. Pulls events from the queue and walks through
   phases. On any disconnect during an active round, aborts the round. *)

let words_map = lazy (Game.load_words ())

let pick_category () =
  let cats = Game.load_categories () in
  List.nth cats (Random.int (List.length cats))

let pick_word category =
  let ws = Hashtbl.find (Lazy.force words_map) category in
  List.nth ws (Random.int (List.length ws))

let shuffle xs =
  xs
  |> List.map (fun x -> (Random.bits (), x))
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd

(* Drain any events that arrived during a previous phase but weren't handled.
   Used after a phase ends so stale messages don't poison the next phase. *)
let drain_events () = with_mutex events_mu (fun () -> Queue.clear events)

(* Wait for next event from a specific client. Other events: disconnect of the
   watched client aborts; disconnect of any other player also aborts the round;
   messages from other clients during a single-player turn are silently dropped
   (we send them an Error to clarify). *)
exception Round_aborted of string

let rec wait_for_msg_from id =
  match pop_event () with
  | Disconnected i when i = id ->
      raise (Round_aborted (Printf.sprintf "player (id %d) disconnected" id))
  | Disconnected _i -> raise (Round_aborted "a player disconnected")
  | Msg (i, msg) when i = id -> msg
  | Msg (other_id, _msg) ->
      send_to_id other_id (Protocol.Error "not your turn — please wait");
      wait_for_msg_from id

(* (Earlier draft had a generic [collect_from] helper here; removed because the
   vote phase ended up inlining its own loop with retry logic.) *)

(* Find host's id (lowest id among connected clients). *)
let host_id () =
  let cs = all_clients () in
  match List.sort (fun a b -> compare a.id b.id) cs with
  | [] -> None
  | c :: _ -> Some c.id

let play_round () =
  let players = all_clients () |> List.sort (fun a b -> compare a.id b.id) in
  if List.length players < 3 then begin
    broadcast
      (Protocol.Error
         "need at least 3 players — game cancelled, kicking back to lobby");
    raise (Round_aborted "not enough players")
  end;
  let category = pick_category () in
  let word = pick_word category in
  let names = List.map (fun c -> c.name) players in
  let imposter = List.nth players (Random.int (List.length players)) in
  let clue_order = shuffle names in

  (* Send role messages individually. *)
  List.iter
    (fun c ->
      let role, w =
        if c.id = imposter.id then (`Imposter, None) else (`Crew, Some word)
      in
      send_to c
        (Protocol.RoundStart
           { category; role; word = w; players = names; clue_order }))
    players;

  (* ----- Clue phase ----- *)
  List.iter
    (fun clue_giver_name ->
      match find_by_name clue_giver_name with
      | None -> raise (Round_aborted "player vanished mid-round")
      | Some giver ->
          send_to giver Protocol.YourTurnClue;
          let rec await () =
            match wait_for_msg_from giver.id with
            | Protocol.Clue raw ->
                let trimmed = String.trim raw in
                if trimmed = "" then begin
                  send_to giver (Protocol.Error "clue cannot be empty");
                  send_to giver Protocol.YourTurnClue;
                  await ()
                end
                else begin
                  (* Force one-word clue: take first whitespace-separated
                     token. *)
                  let one_word =
                    match String.split_on_char ' ' trimmed with
                    | w :: _ -> w
                    | [] -> trimmed
                  in
                  broadcast
                    (Protocol.CluePosted
                       { player = giver.name; clue = one_word })
                end
            | _ ->
                send_to giver
                  (Protocol.Error "expected a clue — please send one word");
                send_to giver Protocol.YourTurnClue;
                await ()
          in
          await ())
    clue_order;

  (* ----- Vote phase ----- *)
  List.iter
    (fun c -> send_to c (Protocol.YourTurnVote { candidates = names }))
    players;
  let voter_ids = List.map (fun c -> c.id) players in
  let votes = ref [] in
  let needed = ref voter_ids in
  while !needed <> [] do
    match pop_event () with
    | Disconnected _ -> raise (Round_aborted "a player disconnected")
    | Msg (i, msg) -> (
        if not (List.mem i !needed) then
          send_to_id i (Protocol.Error "you have already voted")
        else
          match msg with
          | Protocol.Vote target -> (
              match find_by_name target with
              | None ->
                  send_to_id i
                    (Protocol.Error
                       ("no such player: " ^ target ^ " — try again"));
                  send_to_id i (Protocol.YourTurnVote { candidates = names })
              | Some t ->
                  needed := List.filter (fun x -> x <> i) !needed;
                  let voter_name =
                    (List.find (fun c -> c.id = i) players).name
                  in
                  votes := (voter_name, t.name) :: !votes;
                  broadcast
                    (Protocol.VotePosted
                       { voter = voter_name; voted_for = t.name }))
          | _ ->
              send_to_id i (Protocol.Error "expected a vote");
              send_to_id i (Protocol.YourTurnVote { candidates = names }))
  done;
  let votes = List.rev !votes in

  (* Tally. Tie => imposter wins (defenders couldn't agree). *)
  let counts = Hashtbl.create 8 in
  List.iter
    (fun (_voter, target) ->
      let n = try Hashtbl.find counts target with Not_found -> 0 in
      Hashtbl.replace counts target (n + 1))
    votes;
  let max_votes = Hashtbl.fold (fun _ v acc -> max v acc) counts 0 in
  let top =
    Hashtbl.fold
      (fun k v acc -> if v = max_votes then k :: acc else acc)
      counts []
  in
  let accused_name, tied =
    match top with
    | [ x ] -> (x, false)
    | xs ->
        (* Tie: pick first by clue_order for determinism, mark as tie. *)
        let pick = List.find (fun n -> List.mem n xs) clue_order in
        (pick, true)
  in

  let was_imposter =
    String.lowercase_ascii accused_name = String.lowercase_ascii imposter.name
  in
  broadcast (Protocol.Accused { player = accused_name; was_imposter });

  if tied && not was_imposter then begin
    broadcast
      (Protocol.RoundEnd
         {
           winner = `Imposter;
           imposter = imposter.name;
           word;
           reason = "vote was tied — imposter slips away";
         });
    ()
  end
  else if not was_imposter then
    broadcast
      (Protocol.RoundEnd
         {
           winner = `Imposter;
           imposter = imposter.name;
           word;
           reason = "crew accused the wrong player";
         })
  else begin
    (* Imposter accused — give them one guess. *)
    let collected_clues =
      (* This is a UX nicety: we already broadcast all clues, so we don't need
         to resend. But the imposter prompt includes "the clues so far"
         conceptually — clients have them already. We just pass an empty hint
         string; clients can show their own log. *)
      ""
    in
    send_to imposter (Protocol.YourTurnGuess { hint = collected_clues });
    let rec await_guess () =
      match wait_for_msg_from imposter.id with
      | Protocol.ImposterGuess g -> g
      | _ ->
          send_to imposter (Protocol.Error "expected your guess");
          send_to imposter (Protocol.YourTurnGuess { hint = collected_clues });
          await_guess ()
    in
    let guess = await_guess () in
    let correct =
      String.lowercase_ascii (String.trim guess) = String.lowercase_ascii word
    in
    if correct then
      broadcast
        (Protocol.RoundEnd
           {
             winner = `Imposter;
             imposter = imposter.name;
             word;
             reason = "imposter was caught but guessed the word";
           })
    else
      broadcast
        (Protocol.RoundEnd
           {
             winner = `Crew;
             imposter = imposter.name;
             word;
             reason = "imposter caught and failed to guess the word";
           })
  end

(* Returns true if all surviving players want to play again. *)
let play_again_phase () =
  let players = all_clients () in
  if players = [] then false
  else begin
    List.iter (fun c -> send_to c Protocol.YourTurnPlayAgain) players;
    let voter_ids = List.map (fun c -> c.id) players in
    let yeses = ref 0 in
    let nos = ref 0 in
    let needed = ref voter_ids in
    let aborted = ref false in
    (try
       while !needed <> [] do
         match pop_event () with
         | Disconnected _ ->
             aborted := true;
             needed := []
         | Msg (i, msg) -> (
             if not (List.mem i !needed) then ()
             else
               match msg with
               | Protocol.PlayAgain b ->
                   needed := List.filter (fun x -> x <> i) !needed;
                   if b then incr yeses else incr nos
               | _ ->
                   send_to_id i (Protocol.Error "expected play_again response");
                   send_to_id i Protocol.YourTurnPlayAgain)
       done
     with _ -> aborted := true);
    if !aborted then false else !nos = 0 && !yeses > 0
  end

(* Wait for host to send Start. Lobby updates flow naturally as joins happen in
   the acceptor thread. *)
let wait_for_start () =
  let rec loop () =
    match pop_event () with
    | Disconnected i ->
        drop_client i;
        broadcast (Protocol.LobbyUpdate (player_names ()));
        loop ()
    | Msg (i, Protocol.Start) ->
        if Some i = host_id () then ()
        else begin
          send_to_id i
            (Protocol.Error "only the host (first to join) can start");
          loop ()
        end
    | Msg (_i, Protocol.Join _) ->
        (* Shouldn't happen post-handshake, but ignore gracefully. *)
        loop ()
    | Msg (i, _other) ->
        send_to_id i (Protocol.Error "game has not started yet");
        loop ()
  in
  loop ()

let game_loop () =
  let rec outer () =
    lobby_open := true;
    broadcast (Protocol.LobbyUpdate (player_names ()));
    wait_for_start ();
    if List.length (all_clients ()) < 3 then begin
      broadcast (Protocol.Error "need at least 3 players to start");
      outer ()
    end
    else begin
      lobby_open := false;
      drain_events ();
      (try play_round ()
       with Round_aborted msg ->
         broadcast (Protocol.Error ("round aborted: " ^ msg)));
      drain_events ();
      let again = play_again_phase () in
      if again then begin
        drain_events ();
        outer ()
      end
      else begin
        broadcast (Protocol.ServerShutdown "thanks for playing!");
        List.iter (fun c -> close_client c) (all_clients ())
      end
    end
  in
  outer ()

(* ---------- Entry point ---------- *)

let run ~port =
  Random.self_init ();
  let listen_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listen_sock Unix.SO_REUSEADDR true;
  Unix.bind listen_sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen listen_sock 16;
  Printf.printf "Imposter server listening on port %d\n%!" port;
  let _acceptor : Thread.t = Thread.create acceptor_loop listen_sock in
  game_loop ();
  try Unix.close listen_sock with _ -> ()
