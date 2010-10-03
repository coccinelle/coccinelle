(*
 * Copyright 2005-2010, Ecole des Mines de Nantes, University of Copenhagen
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


(*
 * Copyright 2005-2010, Ecole des Mines de Nantes, University of Copenhagen
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

let myrule = [^'"''@''/''\n''\r''\011''\012']+

rule token = parse
  | myrule { TScriptData (tok lexbuf) }
  | ['\n' '\r' '\011' '\012'] { inc_line(); TScriptData (tok lexbuf) }
  | "@@" { TArobArob }
  | "@"  { TArob }
  | "/"  { TScriptData (tok lexbuf) }
  | "//" [^ '\n']* { token lexbuf } (* skip SmPL comments *)
  | '"'  { TScriptData (Printf.sprintf "\"%s\"" (string lexbuf)) }
  | eof  { EOF }
  | _ { raise (Lexical ("unrecognised symbol, in token rule:"^tok lexbuf)) }

(* These are C strings.  Perhaps they require some adjustment. *)
and string  = parse
  | '"'                 { "" }
  | (_ as x)            { Common.string_of_char x ^ string lexbuf }
  | ("\\" _) as x       { x ^ string lexbuf }
