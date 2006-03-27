(* ----------------------------------------------------------------------- *)
(* Entry point *)

let file = ref ""
let verbose = ref true

let anonymous s = file := s

let speclist = [("-v", Arg.Set verbose, "print parse result")]

let usage =
  Printf.sprintf "Usage: %s [options] <filename> \nOptions are:"
    (Filename.basename Sys.argv.(0))

let main _ =
  begin
  Arg.parse speclist anonymous usage;
  (* Parse_cocci.parse_and_merge !file; *)
  if !file = "" then failwith "filename required";
  Parse_cocci.process !file !verbose
  (* if !verbose then Unparse_cocci.unparse minus; *)
  end

let _ = main ()
