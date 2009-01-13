
{

  open Lexing
  module P=Org_parser
  module D=Data

  exception Lexical of string
  let lexerr s1 s2 = raise (Lexical (Printf.sprintf "%s%s" s1 s2))

  let id_tokens lexbuf =
    let in_org = !D.in_org in
      match lexbuf with
	| "view"     when in_org -> P.TVIEW
	| "face"     when in_org -> P.TFACE
	| "linb"     when in_org -> P.TLINB
	| "colb"     when in_org -> P.TCOLB
	| "cole"     when in_org -> P.TCOLE
	| "org"      when in_org -> P.TORG
	| "config"   when in_org -> P.TCONFIG
	| "TODO"     when in_org -> P.TTODO
	| "SEQ_TODO" when in_org -> P.TSTODO
	| _                      -> P.TId(lexbuf)

}

(* ---------------------------------------------------------------------- *)
(* tokens *)

let special = ['-' '.']
let letter = ['A'-'Z' 'a'-'z' '_']
let digit  = ['0'-'9']

let alphanum = (letter | digit)
let salphanum = (letter | digit | special)

let dec = ['0'-'9']

let decimal = ('0' | (['1'-'9'] dec*))

rule token = parse
  | ['\n' '\r' '\011' '\012']
      {
	let curp = lexbuf.lex_curr_p in
	  lexbuf.lex_curr_p <- { pos_fname = curp.pos_fname;
   				 pos_lnum  = curp.pos_lnum + 1;
   				 pos_bol   = curp.pos_cnum;
   				 pos_cnum  = curp.pos_cnum};
	  P.EOL
      }
  | ' '                       { token lexbuf }
  | '*'                       { P.TSTAR }
  | '/'                       { P.TSLASH }
  | ':'                       { P.TCOLON }
  | '='                       { P.TEQUAL }
  | '+'                       { P.TPLUS }
  | '|'                       { P.TVERT }
  | '#'                       { P.TDASH }
  | '['                       { P.TLAB }
  | ']'                       { P.TRAB }
  | decimal                   { P.TInt(int_of_string(Lexing.lexeme lexbuf)) }
  | salphanum*                { id_tokens (Lexing.lexeme lexbuf) }
 (* | letter (letter | digit | special)*  { id_tokens (Lexing.lexeme lexbuf) }*)
  | eof            { P.EOF }
  | _ { lexerr "unrecognised symbol, in token rule: " (Lexing.lexeme lexbuf) }
