(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

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
