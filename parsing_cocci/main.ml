(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

(* ----------------------------------------------------------------------- *)
(* Entry point *)

let file = ref ""
let isofile = ref None
let verbose = ref true

let anonymous s = if !file = "" then file := s else isofile := Some s

let speclist = [("-v", Arg.Set verbose, "print parse result")]

let usage =
  Printf.sprintf "Usage: %s [options] <filename> \nOptions are:"
    (Filename.basename Sys.argv.(0))

let main _ =
  begin
  Arg.parse speclist anonymous usage;
  (* Parse_cocci.parse_and_merge !file; *)
  if !file = "" then failwith "filename required";
  Parse_cocci.process !file !isofile !verbose
  end

let _ = main ()
