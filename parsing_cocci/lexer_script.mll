(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
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


