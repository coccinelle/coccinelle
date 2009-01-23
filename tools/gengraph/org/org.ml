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

let unique_file_list flat_orgs =
  let file_list = List.map (fun (s, file, ver, pos, t, _) -> file) flat_orgs in
    List.fold_left (fun u file ->
		      if List.mem file u then
			u
		      else
			file :: u
		   ) [] file_list

let find_bug bug bugs =
  List.find (fun (s, f, v, pos, t, _) ->
	       let (sb, fb, vb, posb, tb, _) = bug in
		 f = fb && v = vb && pos = posb
	    ) bugs

let show_pos (line, cb, ce) =
  let sline = string_of_int line in
  let scb = string_of_int cb in
  let sce = string_of_int ce in
  let snpos = "("^sline  ^"@"^ scb ^"-"^ sce ^ ")" in
    print_string snpos

let show_bug verbose bug =
  let (_, _, ver, pos, _, n) = bug in
    print_int ver;
    if verbose then show_pos pos

let get_bug bug bugs =
  List.find (fun (s, f, v, pos, t, _) ->
	       let (sb, fb, vb, posb, tb, _) = bug in
		 f = fb && v = vb && pos = posb
	    ) bugs

let rm_bug_chain bugs chain =
  List.filter (fun bug -> not (List.mem bug chain)) bugs

let rec compute_bug_next diffs bugs bug =
  let (s, f, v, pos, t, r) = bug in
  let check_pos = Diff.compute_new_pos diffs f v pos in
  let check = (s, f, v+1, check_pos, t, {Ast_org.def=None}) in
    (try
       let checked = get_bug check bugs in
       let next = compute_bug_next diffs bugs checked in
	 r.Ast_org.def <- Some next
     with Not_found -> ()
    );
    bug

let rec compute_bug_chain diffs bugs =
  let res = List.map (compute_bug_next diffs bugs) bugs in
    if res = bugs then
      res
    else
      compute_bug_chain diffs res

let rec get_chain bugs bug =
  let (s, f, v, pos, t, r) = bug in
    match r.Ast_org.def with
	None -> [bug]
      | Some nbug -> bug::get_chain bugs nbug

let sort_ver subs =
  List.sort
    (fun (_, _, ver1, _, _, _) (_, _, ver2, _, _, _) -> ver1 - ver2)
    subs

let rec sort_bugs diffs bugs =
  match bugs with
      [] -> []
    | hd::tail ->
	let chain = get_chain bugs hd in
	let ntail = rm_bug_chain tail chain in
	  chain :: sort_bugs diffs ntail

let compute_org prefix diffs orgs : (Ast_diff.path * Ast_org.bugs list) list =
  let flat_orgs =
    List.map (fun (s, (p, ops, (t,l))) ->
		let (ver, file) = Misc.strip_prefix prefix p in
		let pos = position ops in
		  (s, file, ver, pos, t, {Ast_org.def=None}))
      orgs
  in
    List.map (fun file ->
		(file,
		 let subs =
		   List.find_all
		     (fun (s, file2, ver, pos, t, _) -> file = file2)
		     flat_orgs
		 in
		 let ordered = sort_ver subs in
		 let bugs = compute_bug_chain diffs ordered in
		 let sorted = sort_bugs diffs bugs in
		   sorted
		)
	     )
      (unique_file_list flat_orgs)

let count = ref 0

let show_org verbose prefix diffs p_orgs =
  count := 0;
  let orgs2 = compute_org prefix diffs p_orgs in
    if verbose then
      begin
	prerr_endline ("Prefix used: " ^ prefix);
	List.iter (fun (file, bugslist) ->
		     print_string file;
		     print_newline ();
		     List.iter (fun bugs ->
				  print_string "#";
				  print_int !count;
				  count := !count + 1;
				  print_string " in vers. ";
				  List.iter
				    (fun  bug ->
				       show_bug verbose bug;
				       print_string " -> "
				    )
				    bugs;
		   		  print_newline ()
			       )
		       bugslist;
		     print_newline ()
		  )
	  orgs2
      end

let print_orglinkbug prefix bug =
  let (_, file, ver, pos, t, _) = bug in
  let (line, cb, ce) = pos in
    print_string "[[view:";
    print_string prefix;
    print_int ver;
    print_string "/";
    print_string file;
    print_string "::face=ovl-face1";
    print_string "::linb=";
    print_int line;
    print_string "::colb=";
    print_int cb;
    print_string "::cole=";
    print_int ce;
    print_string "][";
    print_string prefix;
    print_int ver;
    print_string "/";
    print_string file;
    print_string "::";
    print_int line;
    print_string "]]"

let print_org prefix diffs p_orgs =
  count := 0;
  let orgs2 = compute_org prefix diffs p_orgs in
    List.iter (fun (file, bugslist) ->
		 List.iter (fun bugs ->
			      print_string ("* BUG "^file);
			      print_string " - ";
			      print_int !count;
			      count := !count + 1;
			      print_newline ();
			      List.iter
				(fun  bug ->
				   print_string "** ";
				   print_orglinkbug prefix bug;
				   print_newline ()
				)
				bugs;
		   	      print_newline ()
			   )
		   bugslist;
		 print_newline ()
	      )
      orgs2
