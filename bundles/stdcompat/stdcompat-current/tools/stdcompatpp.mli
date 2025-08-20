type options

type lexer_state

type lexer =
  options -> lexer_state -> Lexing.lexbuf -> unit

val ocaml_lexer : lexer

val c_lexer : lexer
