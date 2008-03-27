{
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
exception Lexical of string
let tok = Lexing.lexeme
}
(* ---------------------------------------------------------------------- *)
(* tokens *)

let myrule = [^'@']+

rule token = parse
  | myrule		{ TScriptData (tok lexbuf) }
  | "@@" { TArobArob }
  | "@"  { TArob }
  | eof  { EOF }
  | _ { raise (Lexical ("unrecognised symbol, in token rule:"^tok lexbuf)) }


