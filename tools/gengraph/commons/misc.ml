open Lexing

(* From Devil, thank you Laurent *)
(* **************************************************************** *)

let print_pos pos =
  Printf.sprintf("File \"%s\", line %d, characters %d-%d:\n")
    pos.Ast.file pos.Ast.line pos.Ast.colfr pos.Ast.colto;;

let report_error pos str =
  prerr_endline ((print_pos pos) ^ str);
  exit 2

(* for the parser *)

(* helper function which fills out a pos structure *)
let getpos n =
  let start_pos = Parsing.rhs_end_pos 0 in
  let end_no    = Parsing.rhs_end     n in
  {
    Ast.file  = start_pos.pos_fname;
    Ast.line  = start_pos.pos_lnum;
    Ast.colfr = start_pos.pos_cnum - start_pos.pos_bol;
    Ast.colto = end_no - start_pos.pos_bol
  }

let init filename lexbuf =
  let curp = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos_fname = filename;
   			 pos_lnum  = curp.pos_lnum;
   			 pos_bol   = curp.pos_bol;
   			 pos_cnum  = curp.pos_cnum}

