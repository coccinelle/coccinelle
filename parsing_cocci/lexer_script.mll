{
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
exception Lexical of string
let tok = Lexing.lexeme
let inc_line _ = Lexer_cocci.line := !Lexer_cocci.line + 1
}
(* ---------------------------------------------------------------------- *)
(* tokens *)

let oct = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let myrule = [^'\'''"''@''/''\n''\r''\011''\012']+

rule token = parse
  | myrule { TScriptData (tok lexbuf) }
  | ['\n' '\r' '\011' '\012'] { inc_line(); TScriptData (tok lexbuf) }
  | "@@" { TArobArob }
  | "@"  { TArob }
  | "/"  { TScriptData (tok lexbuf) }
  | "//" [^ '\n']* { token lexbuf } (* skip SmPL comments *)
  | '"'  { TScriptData (Printf.sprintf "\"%s\"" (string lexbuf)) }
  | "'"  { TScriptData (Printf.sprintf "'%s'" (char lexbuf)) }
  | eof  { EOF }
  | _ { raise (Lexical ("unrecognised symbol, in token rule:"^tok lexbuf)) }

(* These are C strings.  Perhaps they require some adjustment. *)
and string  = parse
  | '"'                 { "" }
  | (_ as x)            { Common.string_of_char x ^ string lexbuf }
  | ("\\" _) as x       { x ^ string lexbuf }

and char = parse
  | (_ as x) "'"                                     { String.make 1 x }
  | (("\\" (oct | oct oct | oct oct oct)) as x  "'") { x }
  | (("\\x" (hex | hex hex)) as x  "'")       { x }
  | (("\\" _ ) as x "'") { x }
