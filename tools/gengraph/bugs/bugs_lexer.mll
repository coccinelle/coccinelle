
{

  open Lexing
  module P=Bugs_parser


  exception Lexical of string
  let lexerr s1 s2 = raise (Lexical (Printf.sprintf "%s%s" s1 s2))

}

(* ---------------------------------------------------------------------- *)
(* tokens *)

let letter = ['A'-'Z' 'a'-'z' '_' '/' '-' '.']
let digit  = ['0'-'9']

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
  | '-'                       { P.TMINUS }
  | decimal                   { P.TInt(Lexing.lexeme lexbuf) }
  | letter (letter | digit)*  { P.TId(Lexing.lexeme lexbuf) }

  | eof            { P.EOF }
  | _ { lexerr "unrecognised symbol, in token rule: " (Lexing.lexeme lexbuf) }
