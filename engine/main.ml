(* ----------------------------------------------------------------------- *)
(* Entry point *)

let in_file = ref ""
let out_file = ref ""

let anonymous s =
  if !in_file = "" then in_file := s else out_file := s

let speclist = []

let usage =
  Printf.sprintf
    "Usage: %s [options] <in_filename> <out_filename> \nOptions are:"
    (Filename.basename Sys.argv.(0))

let main _ =
  Arg.parse speclist anonymous usage;
  if !in_file = "" then failwith "in_filename required";
  let (ast_lists,tables,ua,fns) = Parse_cocci.process !in_file None false in
  Ctltotex.totex !out_file
    ast_lists
    (List.map2
       (function (ast,ft) -> function (ex,ua) ->
	 Asttoctl.asttoctl ast ft ex ua)
       (List.combine ast_lists tables) (List.combine fns ua))

let _ = main ()
