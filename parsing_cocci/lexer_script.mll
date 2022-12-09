(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

{
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
exception Lexical of string
let tok = Lexing.lexeme
let language = ref ""
let inc_line _ = Lexer_cocci.line := !Lexer_cocci.line + 1

let in_comment = ref false
}
(* ---------------------------------------------------------------------- *)
(* tokens *)

let oct = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let myrule = [^'\'''"''@''/''\n''\r''\011''\012''('')']+

rule token = parse
  | myrule { TScriptData (tok lexbuf) }
  | '(' { TScriptData (tok lexbuf) }
  | ')' { TScriptData (tok lexbuf) }
  | ['\n' '\r' '\011' '\012']
      { inc_line();
	let text = tok lexbuf in
	let text =
	  if !language = "ocaml"
	  then
	    Printf.sprintf "%s# %d \"%s\"%s"
	      text !Lexer_cocci.line !Lexer_cocci.file text
	  else text in
	TScriptData text }
  | "@@" { TArobArob }
  | "@"  { TArob }
  | "/"  { TScriptData (tok lexbuf) }
  | "//" [^ '\n']* { token lexbuf } (* skip SmPL comments *)
  | "#"  [^ '\n']* { token lexbuf } (* skip python comments *)
  (* detect ocaml comments *)
  | "(*" { in_comment := true; TScriptData (tok lexbuf) }
  | "*)" { in_comment := false; TScriptData (tok lexbuf) }
  | '"'  { TScriptData (Printf.sprintf "\"%s\"" (string lexbuf)) }
  | "'"  { if !in_comment
           then TScriptData (tok lexbuf)
           else TScriptData (Printf.sprintf "'%s'" (cstring lexbuf)) }
  | eof  { EOF }
  | _ { raise (Lexical ("unrecognised symbol, in token rule:"^tok lexbuf)) }

(* These are C strings.  Perhaps they require some adjustment. *)
and string  = parse
  | '"'                 { "" }
  | ['\n' '\r' '\011' '\012'] as x
    { inc_line(); (Printf.sprintf "%c" x) ^ string lexbuf }
  | "\\\""              { "\\\"" ^ string lexbuf }
  | (_ as x)            { (String.make 1 x) ^ string lexbuf }

and cstring  = parse
  | "'"                 { "" }
  | ['\n' '\r' '\011' '\012'] as x
    { inc_line(); (Printf.sprintf "%c" x) ^ cstring lexbuf }
  | (_ as x)            { (String.make 1 x) ^ cstring lexbuf }
