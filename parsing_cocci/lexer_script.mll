(*
 * Copyright 2012-2014, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./lexer_script.mll"
{
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
exception Lexical of string
let tok = Lexing.lexeme
let file = ref ""
let language = ref ""
let inc_line _ = Lexer_cocci.line := !Lexer_cocci.line + 1
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
	      text !Lexer_cocci.line !file text
	  else text in
	TScriptData text }
  | "@@" { TArobArob }
  | "@"  { TArob }
  | "/"  { TScriptData (tok lexbuf) }
  | "//" [^ '\n']* { token lexbuf } (* skip SmPL comments *)
  | "#"  [^ '\n']* { token lexbuf } (* skip python comments *)
  | '"'  { TScriptData (Printf.sprintf "\"%s\"" (string lexbuf)) }
  | "'"  { TScriptData (Printf.sprintf "'%s'" (cstring lexbuf)) }
  | eof  { EOF }
  | _ { raise (Lexical ("unrecognised symbol, in token rule:"^tok lexbuf)) }

(* These are C strings.  Perhaps they require some adjustment. *)
and string  = parse
  | '"'                 { "" }
  | (_ as x)            { (String.make 1 x) ^ string lexbuf }

and cstring  = parse
  | "'"                 { "" }
  | (_ as x)            { (String.make 1 x) ^ cstring lexbuf }
