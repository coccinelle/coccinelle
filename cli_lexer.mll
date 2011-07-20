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
      | NotEq   -> "!="
      | EqEq    -> "=="
      | Other s -> s
      | EOF     -> ""
}

let special = ':'
let letter  = ['A'-'Z' 'a'-'z' '_']
let dec     = ['0'-'9']

let alphanum = (letter | dec)
let id = letter (alphanum | special)*

rule token = parse
  | " "  { token lexbuf      }
  | "!=" { NotEq             }
  | "==" { EqEq              }
  | id   { Id(tok lexbuf)    }
  | eof  { EOF               }
  | _    { Other(tok lexbuf) }

