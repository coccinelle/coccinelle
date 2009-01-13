open Lexing

let parse_bugs file : Ast_bugs.bugs =
  let in_ch = open_in file in
  let lexbuf = Lexing.from_channel in_ch  in
  try
    (* ignore(Parsing.set_trace true); *)
    Misc.init file lexbuf;
    let ast = Bugs_parser.main Bugs_lexer.token lexbuf in
      close_in in_ch;
      ast
  with
      (Bugs_lexer.Lexical msg) ->
	let pos = lexbuf.lex_curr_p in
	  Misc.report_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Bugs Lexer Error: " ^ msg);
    | Bugs_parser.Error ->
	let pos = lexbuf.lex_curr_p in
	  Misc.report_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Bugs Parser Error: unexpected token '" ^ (Lexing.lexeme lexbuf) ^"'")


let compute_bug vmax (bug, c, vs, _) =
  let min_b = List.fold_left min vmax vs in
  let max_b = List.fold_left max 0 vs in
    (bug, c, min_b, max_b)

let show_bug vmax bug =
  let (fb, c, bmin, bmax) = compute_bug vmax bug in
  let min_b = string_of_int bmin in
  let max_b = string_of_int bmax in
    prerr_endline ("Bugs in " ^ fb ^ " from " ^ min_b ^ " to " ^ max_b)
