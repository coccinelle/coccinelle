type token =
  | COLON
  | EOF
  | EOL
  | TERMINAL of (
# 29 "menhir-20181113/src/sentenceParser.mly"
       Grammar.Terminal.t * Lexing.position * Lexing.position
# 9 "menhir-20181113/src/sentenceParser.mli"
)
  | NONTERMINAL of (
# 30 "menhir-20181113/src/sentenceParser.mly"
       Grammar.Nonterminal.t * Lexing.position * Lexing.position
# 14 "menhir-20181113/src/sentenceParser.mli"
)
  | COMMENT of (
# 31 "menhir-20181113/src/sentenceParser.mly"
       string
# 19 "menhir-20181113/src/sentenceParser.mli"
)

val optional_sentence :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> SentenceParserAux.sentence option
val entry :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> SentenceParserAux.located_sentence SentenceParserAux.or_comment list
