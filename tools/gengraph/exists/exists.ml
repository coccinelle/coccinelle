open Lexing

let parse_exist file =
  let in_ch = open_in file in
  let lexbuf = Lexing.from_channel in_ch  in
  try
    (* ignore(Parsing.set_trace true); *)
    Misc.init file lexbuf;
    let ast = Exist_parser.main Exist_lexer.token lexbuf in
      close_in in_ch;
      ast
  with
      (Exist_lexer.Lexical msg) ->
	let pos = lexbuf.lex_curr_p in
	  Misc.report_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Exist Lexer Error: " ^ msg);
    | Exist_parser.Error ->
	let pos = lexbuf.lex_curr_p in
	  Misc.report_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Exist Parser Error: unexpected token '" ^ (Lexing.lexeme lexbuf) ^"'")

let min_list l = List.fold_left min 0 l
let max_list l = List.fold_left max 0 l

let min_exist l : (int * bool) =
  List.fold_left
    (fun (i,e) s ->
       if (e || (not e && s)) then
	 (i, true)
       else
	 (i+1, false)
    ) (0, false) l

let max_exist l i : (int * bool) =
  List.fold_right
    (fun s (i,e) ->
       if (e || (not e && s)) then
	 (i, true)
       else
	 (i-1, false)
    ) l (i, false)

let show_ver vs =
  print_string "Analysis from version ";
  print_int (min_list vs);
  print_string " to version ";
  print_int (max_list vs);
  print_newline ()

let convert_state s =
  match s with
      Ast_exist.True _ -> true
    | Ast_exist.False _ -> false

let compute_entry i e =
  let (f, sl, _) = e in
    (f,
     (fst (min_exist (List.map convert_state sl))),
     (fst (max_exist (List.map convert_state sl) i))
    )

let show_entry (f, min, max) =
  print_string "From ";
  print_int min;
  print_string " to ";
  print_int max;
  print_string " ";
  print_string f;
  print_string " exists";
  print_newline ()

let compute_exist ast =
  let (versions, entries) = ast in
  let max = max_list versions in
    (max, List.map (compute_entry max) entries)
