(* bin/client_main.ml — usage: imposter-client HOST PORT NAME *)

let () =
  if Array.length Sys.argv < 4 then begin
    prerr_endline "usage: imposter-client HOST PORT NAME";
    exit 1
  end;
  let host = Sys.argv.(1) in
  let port =
    try int_of_string Sys.argv.(2)
    with Failure _ ->
      prerr_endline "PORT must be an integer";
      exit 1
  in
  let name = Sys.argv.(3) in
  Imposter.Client.run ~host ~port ~name
