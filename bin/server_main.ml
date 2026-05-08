(* bin/server_main.ml — usage: imposter-server [PORT] (default: 4000) *)

let () =
  let port =
    if Array.length Sys.argv >= 2 then (
      try int_of_string Sys.argv.(1)
      with Failure _ ->
        prerr_endline "usage: imposter-server [PORT]";
        exit 1)
    else 4000
  in
  Imposter.Server.run ~port
