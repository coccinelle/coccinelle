open Lexing

exception Unrecoverable

let parse_org file : Ast_org.orgs =
  let in_ch = open_in file in
  let lexbuf = Lexing.from_channel in_ch  in
  try
    Misc.init file lexbuf;
    let ast = Org_parser.main Org_lexer.token lexbuf in
      close_in in_ch;
      ast
  with
      (Org_lexer.Lexical msg) ->
	let pos = lexbuf.lex_curr_p in
	  Misc.report_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Org Lexer Error: " ^ msg);
    | Org_parser.Error ->
	let pos = lexbuf.lex_curr_p in
	  Misc.report_error
	    { Ast.file  = file;
	      Ast.line  = pos.pos_lnum;
	      Ast.colfr = pos.pos_cnum - pos.pos_bol;
	      Ast.colto = (Lexing.lexeme_end lexbuf) - pos.pos_bol + 1}
	    ("Org Parser Error: unexpected token '" ^ (Lexing.lexeme lexbuf) ^"'")

let line_of ops =
  match List.find (fun x ->
		     match x with
			 Ast_org.Line _  -> true
		       | _ -> false) ops
  with
      Ast_org.Line l -> l
    | _ -> raise Unrecoverable

let start_of ops =
  match List.find (fun x ->
		     match x with
			 Ast_org.ColB _ -> true
		       | _ -> false) ops
  with
      Ast_org.ColB b -> b
    | _ -> raise Unrecoverable

let end_of ops =
  match List.find (fun x ->
				    match x with
					Ast_org.ColE _ -> true
				      | _ -> false) ops
  with
      Ast_org.ColE e -> e
    | _ -> raise Unrecoverable

let position ops =
  (line_of ops, start_of ops, end_of ops)

let compute_org prefix orgs =
  let flat_orgs =
    List.map (fun (s, (p, ops, (t,l))) ->
		let p2 = Str.replace_first (Str.regexp_string prefix) "" p in
		let re = Str.regexp "^\\([0-9]+\\)/\\(.*\\)$" in
		  ignore(Str.string_match re p2 0);
		  let ver = int_of_string (Str.matched_group 1 p2) in
		  let file = Str.matched_group 2 p2 in
		  let pos = position ops in
		    (s, file, ver, pos, t))
      orgs
  in
  let file_list = List.map (fun (s, file, ver, pos, t) -> file) flat_orgs in
  let file_list_unique =
    List.fold_left (fun u file ->
		      if List.mem file u then
			u
		      else
			file :: u
		   ) [] file_list
  in
    List.map (fun file ->
		(file,
		 let subs =
		   List.find_all
		     (fun (s, file2, ver, pos, t) -> file = file2)
		     flat_orgs
		 in
		   List.sort
		     (fun (_, _, ver1, _, _) (_, _, ver2, _, _) -> ver1 - ver2)
		     subs
		)
	     )
      file_list_unique

let show_org prefix orgs =
  let orgs2 = compute_org prefix orgs in
    print_endline ("Prefix used: " ^ prefix);
    List.iter (fun (file, orgs) ->
		 print_string file;
		 print_string " ";
		 List.iter (fun (s, f, ver, pos, t) ->
			      print_int ver;
			      print_string " ")
		   orgs;
		 print_newline ())
      orgs2
