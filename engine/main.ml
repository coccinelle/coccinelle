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
  let o = open_out !out_file in
  Ctltotex.make_prelude o;
  let ast_lists = Parse_cocci.process_for_ctl !in_file None false in
  List.iter
    (function ast_list ->
      let ctls = Asttoctl.asttoctl ast_list in
      Ctltotex.ctltotex ast_list Asttoctl.pred2c (function x -> x) ctls o)
    ast_lists;
  Ctltotex.make_postlude o;
  close_out o


let _ = main ()
