{
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
exception Lexical of string
let tok = Lexing.lexeme
}
(* ---------------------------------------------------------------------- *)
(* tokens *)

let myrule = [^'"''@']+

rule token = parse
  | myrule		{ TScriptData (tok lexbuf) }
  | "@@" { TArobArob }
  | "@"  { TArob }
  | "//" [^ '\n']* { token lexbuf } (* skip SmPL comments *)
  | '"'  { TScriptData (Printf.sprintf "\"%s\"" (string lexbuf)) }
  | eof  { EOF }
  | _ { raise (Lexical ("unrecognised symbol, in token rule:"^tok lexbuf)) }

(* These are C strings.  Perhaps they require some adjustment. *)
and string  = parse
  | '"'                 { "" }
  | (_ as x)            { Common.string_of_char x ^ string lexbuf }
  | ("\\" _) as x       { x ^ string lexbuf }
