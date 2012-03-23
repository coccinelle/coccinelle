(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

{

  open Lexing
  open Parser
  open Positions

  (* This wrapper saves the current lexeme start, invokes its argument,
     and restores it. This allows transmitting better positions to the
     parser. *)

  let savestart lexbuf f =
    let startp = lexbuf.lex_start_p in
    let token = f lexbuf in
    lexbuf.lex_start_p <- startp;
    token
  
  (* Updates the line counter, which is used in some error messages. *)

  let update_loc lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  (* Extracts a chunk out of the source file. *)    

  let chunk ofs1 ofs2 =
    let contents = Error.get_file_contents() in
    let len = ofs2 - ofs1 in
    String.sub contents ofs1 len

  (* Extracts a chunk out of the source file, delimited by
     one position and extending to the end of the file. *)

  let echunk ofs1 =
    let contents = Error.get_file_contents() in
    let len = String.length contents - ofs1 in
    String.sub contents ofs1 len

  (* Overwrites an old character with a new one at a specified
     offset in a string. *)

  let overwrite content offset c1 c2 =
    assert (content.[offset] = c1);
    content.[offset] <- c2

  (* Creates a stretch. *)

  let mk_stretch parenthesize pos1 pos2 pkeywords = 
    let ofs1 = pos1.pos_cnum
    and ofs2 = pos2.pos_cnum in
    let raw_content = chunk ofs1 ofs2 in
    let content = String.copy raw_content in
    (* Turn our keywords into valid Objective Caml identifiers
       by replacing '$', '(', and ')' with '_'. Bloody. *)
    List.iter (function { value = keyword; position = pos } ->
      let pos = start_of_position pos in
      let ofs = pos.pos_cnum - ofs1 in
      overwrite content ofs '$' '_';
      match keyword with
      | Keyword.Dollar _
      | Keyword.Position (Keyword.Left, _, _)
      | Keyword.PreviousError ->
	  ()
      | Keyword.SyntaxError ->
	  (* $syntaxerror is replaced with
	     (raise _eRR) *)
          let source = "(raise _eRR)" in
          String.blit source 0 content ofs (String.length source)
      | Keyword.Position (subject, where, _) ->
	  let ofslpar =
	    match where with
	    | Keyword.WhereStart ->
		ofs + 9
	    | Keyword.WhereEnd ->
		ofs + 7
	  in
	  overwrite content ofslpar '(' '_';
	  match subject with
	  | Keyword.Left ->
	      assert false
	  | Keyword.RightDollar i ->
	      overwrite content (ofslpar + 1) '$' '_';
	      overwrite content (ofslpar + 2 + String.length (string_of_int i)) ')' '_'
	  | Keyword.RightNamed id ->
	      overwrite content (ofslpar + 1 + String.length id) ')' '_'
    ) pkeywords;
    (* Add whitespace so that the column numbers match those of the source file.
       If requested, add parentheses so that the semantic action can be inserted
       into other code without ambiguity. *)
    let content =
      if parenthesize then
	  (String.make (pos1.pos_cnum - pos1.pos_bol - 1) ' ') ^ "(" ^ content ^ ")"
      else
	(String.make (pos1.pos_cnum - pos1.pos_bol) ' ') ^ content
    in
    {
      Stretch.stretch_filename = Error.get_filename();
      Stretch.stretch_linenum = pos1.pos_lnum;
      Stretch.stretch_linecount = pos2.pos_lnum - pos1.pos_lnum;
      Stretch.stretch_content = content;
      Stretch.stretch_raw_content = raw_content;
      Stretch.stretch_keywords = pkeywords
    } 

  (* Translates the family of position-related keywords to abstract
     syntax. *)

  let mk_keyword lexbuf w f n id =
    let where =
      match w with
      | Some _ ->
	  Keyword.WhereStart
      | None ->
	  Keyword.WhereEnd
    and flavor =
      match f with
      | Some _ ->
	  Keyword.FlavorPosition
      | None ->
	  Keyword.FlavorOffset
    and subject =
      match n, id with
      | Some n, None ->
	  Keyword.RightDollar (int_of_string n)
      | None, Some id ->
	  Keyword.RightNamed id
      | None, None ->
	  Keyword.Left
      | Some _, Some _ ->
          assert false
    in
    let keyword = Keyword.Position (subject, where, flavor) in
    with_cpos lexbuf keyword

  (* Objective Caml's reserved words. *)

  let reserved =
    let table = Hashtbl.create 149 in
    List.iter (fun word -> Hashtbl.add table word ()) [
      "and";
      "as";
      "assert";
      "begin";
      "class";
      "constraint";
      "do";
      "done";
      "downto";
      "else";
      "end";
      "exception";
      "external";
      "false";
      "for";
      "fun";
      "function";
      "functor";
      "if";
      "in";
      "include";
      "inherit";
      "initializer";
      "lazy";
      "let";
      "match";
      "method";
      "module";
      "mutable";
      "new";
      "object";
      "of";
      "open";
      "or";
      "parser";
      "private";
      "rec";
      "sig";
      "struct";
      "then";
      "to";
      "true";
      "try";
      "type";
      "val";
      "virtual";
      "when";
      "while";
      "with";
      "mod";
      "land";
      "lor";
      "lxor";
      "lsl";
      "lsr";
      "asr";
    ];
    table

  (* A short-hand. *)

  let error1 pos msg =
    Error.error (Positions.one pos) msg

}

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ';' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let poskeyword = 
  '$'
  (("start" as w) | "end")
  (("pos" as f) | "ofs")
  ( '(' ( '$' (['0'-'9']+ as n) | ((lowercase identchar*) as id)) ')')?

let previouserror =
  "$previouserror"

let syntaxerror =
  "$syntaxerror"

rule main = parse
| "%token"
    { TOKEN }
| "%type"
    { TYPE }
| "%left"
    { LEFT }
| "%right"
    { RIGHT }
| "%nonassoc"
    { NONASSOC }
| "%start"
    { START }
| "%prec"
    { PREC }
| "%public"
    { PUBLIC }
| "%parameter"
    { PARAMETER }
| "%inline"
    { INLINE }
| "%%"
    { let ofs = lexeme_end lexbuf in
      PERCENTPERCENT (lazy (echunk ofs)) }
| ":"
    { COLON }
| ","
    { COMMA }
| "="
    { EQUAL }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| "|"
    { BAR }
| "?"
    { QUESTION }
| "*"
    { STAR }
| "+"
    { PLUS }
| (lowercase identchar *) as id
    { if Hashtbl.mem reserved id then
	Error.errorp
	  (Positions.with_poss (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) ())
	  "this is an Objective Caml reserved word."
      else
	LID (with_pos (cpos lexbuf) id)
    }
| (uppercase identchar *) as id
    { UID (with_pos (cpos lexbuf) id) }
| "//" [^ '\010' '\013']* newline (* skip C++ style comment *)
| newline
    { update_loc lexbuf; main lexbuf }
| whitespace+
    { main lexbuf }
| "/*"
    { comment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| "<"
    { savestart lexbuf (ocamltype (lexeme_end_p lexbuf)) }
| "%{"
    { savestart lexbuf (fun lexbuf ->
        let openingpos = lexeme_end_p lexbuf in
        let closingpos, _ = action true openingpos [] lexbuf in
	(* TEMPORARY if keyword list nonempty, issue an error *)
        HEADER (mk_stretch false openingpos closingpos [])
      ) }
| "{"
    { savestart lexbuf (fun lexbuf ->
        let openingpos = lexeme_end_p lexbuf in
        let closingpos, pkeywords = action false openingpos [] lexbuf in
	let stretch = mk_stretch true openingpos closingpos pkeywords in
        ACTION (Action.from_stretch stretch)
      ) }
(* TEMPORARY comprendre si la différence entre header et action est bien
             justifiée et si certains choix comme le parenthésage et le
             traitement des keywords ne pourraient pas être effectués
             plus loin. *)
| eof
    { EOF }
| _
    { error1 (lexeme_start_p lexbuf) "unexpected character(s)." }

(* Skip C style comments. *)

and comment openingpos = parse
| newline
    { update_loc lexbuf; comment openingpos lexbuf }
| "*/"
    { () }
| eof
    { error1 openingpos "unterminated comment." }
| _
    { comment openingpos lexbuf }

(* Collect an O'Caml type delimited by angle brackets. Angle brackets can
   appear as part of O'Caml function types. They might also appear as part
   of O'Caml variant types, but we ignore that possibility for the moment. *)

and ocamltype openingpos = parse
| "->"
    { ocamltype openingpos lexbuf }
| '>'
    { OCAMLTYPE (Stretch.Declared (mk_stretch true openingpos (lexeme_start_p lexbuf) [])) }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; ocamltype openingpos lexbuf }
| newline
    { update_loc lexbuf; ocamltype openingpos lexbuf }
| eof
    { error1 openingpos "unterminated Objective Caml type." }
| _
    { ocamltype openingpos lexbuf }

(* Collect O'Caml code delimited by curly brackets. Any occurrences of
   the special ``$i'' identifiers are recorded in the accumulating
   parameter [pkeywords]. Nested curly brackets must be properly
   counted. Nested parentheses are also kept track of, so as to better
   report errors when they are not balanced. *)

and action percent openingpos pkeywords = parse
| '{'
    { let _, pkeywords = action false (lexeme_end_p lexbuf) pkeywords lexbuf in
      action percent openingpos pkeywords lexbuf }
| ("}" | "%}") as delimiter
    { match percent, delimiter with
      | true, "%}"
      | false, "}" ->
	  (* This is the delimiter we were instructed to look for. *)
	  lexeme_start_p lexbuf, pkeywords
      | _, _ ->
	  (* This is not it. *)
	  error1 openingpos "unbalanced opening brace."
    }
| '('
    { let _, pkeywords = parentheses (lexeme_end_p lexbuf) pkeywords lexbuf in
      action percent openingpos pkeywords lexbuf }
| '$' (['0'-'9']+ as n)
    { let pkeyword = with_cpos lexbuf (Keyword.Dollar (int_of_string n)) in
      action percent openingpos (pkeyword :: pkeywords) lexbuf }
| poskeyword
    { let pkeyword = mk_keyword lexbuf w f n id in
      action percent openingpos (pkeyword :: pkeywords) lexbuf }
| previouserror
    { let pkeyword = with_cpos lexbuf Keyword.PreviousError in
      action percent openingpos (pkeyword :: pkeywords) lexbuf }
| syntaxerror
    { let pkeyword = with_cpos lexbuf Keyword.SyntaxError in
      action percent openingpos (pkeyword :: pkeywords) lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf;
      action percent openingpos pkeywords lexbuf }
| "'"
    { char lexbuf;
      action percent openingpos pkeywords lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf;
      action percent openingpos pkeywords lexbuf }
| newline
    { update_loc lexbuf;
      action percent openingpos pkeywords lexbuf }
| ')'
| eof
    { error1 openingpos "unbalanced opening brace." }
| _
    { action percent openingpos pkeywords lexbuf }

and parentheses openingpos pkeywords = parse
| '('
    { let _, pkeywords = parentheses (lexeme_end_p lexbuf) pkeywords lexbuf in
      parentheses openingpos pkeywords lexbuf }
| ')'
    { lexeme_start_p lexbuf, pkeywords }
| '{'
    { let _, pkeywords = action false (lexeme_end_p lexbuf) pkeywords lexbuf in
      parentheses openingpos pkeywords lexbuf }
| '$' (['0'-'9']+ as n)
    { let pkeyword = with_cpos lexbuf (Keyword.Dollar (int_of_string n)) in
      parentheses openingpos (pkeyword :: pkeywords) lexbuf }
| poskeyword
    { let pkeyword = mk_keyword lexbuf w f n id in
      parentheses openingpos (pkeyword :: pkeywords) lexbuf }
| previouserror
    { let pkeyword = with_cpos lexbuf Keyword.PreviousError in
      parentheses openingpos (pkeyword :: pkeywords) lexbuf }
| syntaxerror
    { let pkeyword = with_cpos lexbuf Keyword.SyntaxError in
      parentheses openingpos (pkeyword :: pkeywords) lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; parentheses openingpos pkeywords lexbuf }
| "'"
    { char lexbuf; parentheses openingpos pkeywords lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; parentheses openingpos pkeywords lexbuf }
| newline
    { update_loc lexbuf; parentheses openingpos pkeywords lexbuf }
| '}'
| eof
    { error1 openingpos "unbalanced opening parenthesis." }
| _
    { parentheses openingpos pkeywords lexbuf }

(* Skip O'Caml comments. Comments can be nested and can contain
   strings or characters, which must be correctly analyzed. (A string
   could contain begin-of-comment or end-of-comment sequences, which
   must be ignored; a character could contain a begin-of-string
   sequence.) *)

and ocamlcomment openingpos = parse
| "*)"
    { () }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; ocamlcomment openingpos lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; ocamlcomment openingpos lexbuf }
| "'"
    { char lexbuf; ocamlcomment openingpos lexbuf }
| newline
    { update_loc lexbuf; ocamlcomment openingpos lexbuf }
| eof
    { error1 openingpos "unterminated Objective Caml comment." }
| _
    { ocamlcomment openingpos lexbuf }

(* Skip O'Caml strings. *)

and string openingpos = parse
| '"' 
   { () }
| '\\' newline
| newline
   { update_loc lexbuf; string openingpos lexbuf }
| '\\' _
   (* Upon finding a backslash, skip the character that follows,
      unless it is a newline. Pretty crude, but should work. *)
   { string openingpos lexbuf }
| eof 
   { error1 openingpos "unterminated Objective Caml string." }
| _
   { string openingpos lexbuf }

(* Skip O'Caml characters. A lone quote character is legal inside
   a comment, so if we don't recognize the matching closing quote,
   we simply abandon. *)

and char = parse
| '\\'? newline "'"
   { update_loc lexbuf }
| [^ '\\' '\''] "'"
| '\\' _ "'"
| '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
| '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
| ""
   { () } 

