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


# 0 "./lexer_cli.mll"
(* Lexer for the command line mode *)

{
  exception Lexical of string

  let tok = Lexing.lexeme

  type cli_tok =
      Id of string
    | NotEq
    | EqEq
    | Other of string
    | EOF

  let pretty_print tok =
    match tok with
	Id s    -> s
      | NotEq   -> "when !="
      | EqEq    -> "when =="
      | Other s -> s
      | EOF     -> ""
}

let special = ':'
let letter  = ['A'-'Z' 'a'-'z' '_']
let dec     = ['0'-'9']

let alphanum = (letter | dec)
let id = letter (alphanum | special)*

rule token = parse
  | "when" [' ' '\t']* "!=" [' ' '\t']* { NotEq }
  | "when" [' ' '\t']* "==" [' ' '\t']* { EqEq  }
  | [' ' '\t']+ { Other(" ") }
  | id   { Id(tok lexbuf)    }
  | eof  { EOF               }
  | _    { Other(tok lexbuf) }

