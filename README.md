# cs3110-project
Team 99: The Sorcerer Camels
Hannah Jacob - htj7
Sanaa Bhorkar - sb2759
Kashish Balan - kdb89
Prisha Rai - pr482

---

# Imposter

A word-guessing social deduction game. Two versions: solo and multiplayer.

---

## Solo Mode

You play as the imposter. The game picks a secret word from a random category and gives you one hint at a time. Your job is to figure out the word.

```bash
dune exec bin/main.exe
```

**How to play:**
1. A category is shown (e.g. *Animals*).
2. You get one hint word at a time from the same category.
3. Type your guess after each hint.
4. Type `give up` to reveal the word and quit the round.
5. At the end, type `y` to play again or `n` to quit.

---

## Multiplayer Mode

You need at least 3 players. One person runs the server, everyone connects as a client.

### Start the server
```bash
dune exec bin/server_main.exe -- 4000
```

### Everyone connects
```bash
dune exec bin/client_main.exe -- HOST PORT YOUR_NAME
```

Example:
```bash
dune exec bin/client_main.exe -- localhost 4000 prisha
```

### Start the game

The **first person to connect** is the host (shown as 👑 in the lobby). Once everyone has joined, the host types:

start

### How a round works

One random player is secretly chosen as the **imposter**. Everyone else is **crew** and knows the secret word.

- **Clue phase:** Players take turns giving one word related to the secret word. The imposter has to fake a believable clue.
- **Vote phase:** Everyone votes on who they think the imposter is. Type the player's name (case doesn't matter).
- **Guess phase:** If the imposter gets voted out, they get one last chance — guess the secret word to still win.

### Winning
- **Crew wins** if the imposter is voted out and fails to guess the word.
- **Imposter wins** if they survive the vote, or get caught but correctly guess the word.
- **Draw** if the vote ties.

After each round, everyone votes `y` or `n` to play again.
