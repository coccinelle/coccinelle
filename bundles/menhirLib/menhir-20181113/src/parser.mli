type token =
  | TOKEN
  | TYPE
  | LEFT
  | RIGHT
  | NONASSOC
  | START
  | PREC
  | PUBLIC
  | COLON
  | BAR
  | EOF
  | EQUAL
  | INLINE
  | LPAREN
  | RPAREN
  | COMMA
  | QUESTION
  | STAR
  | PLUS
  | PARAMETER
  | ON_ERROR_REDUCE
  | PERCENTATTRIBUTE
  | SEMI
  | LID of (
# 33 "menhir-20181113/src/parser.mly"
        string Positions.located
# 29 "menhir-20181113/src/parser.mli"
)
  | UID of (
# 33 "menhir-20181113/src/parser.mly"
        string Positions.located
# 34 "menhir-20181113/src/parser.mli"
)
  | QID of (
# 33 "menhir-20181113/src/parser.mly"
        string Positions.located
# 39 "menhir-20181113/src/parser.mli"
)
  | HEADER of (
# 34 "menhir-20181113/src/parser.mly"
        Stretch.t
# 44 "menhir-20181113/src/parser.mli"
)
  | OCAMLTYPE of (
# 35 "menhir-20181113/src/parser.mly"
        Stretch.ocamltype
# 49 "menhir-20181113/src/parser.mli"
)
  | PERCENTPERCENT of (
# 36 "menhir-20181113/src/parser.mly"
        Stretch.t Lazy.t
# 54 "menhir-20181113/src/parser.mli"
)
  | ACTION of (
# 37 "menhir-20181113/src/parser.mly"
        Syntax.raw_action
# 59 "menhir-20181113/src/parser.mli"
)
  | ATTRIBUTE of (
# 38 "menhir-20181113/src/parser.mly"
        Syntax.attribute
# 64 "menhir-20181113/src/parser.mli"
)
  | GRAMMARATTRIBUTE of (
# 38 "menhir-20181113/src/parser.mly"
        Syntax.attribute
# 69 "menhir-20181113/src/parser.mli"
)
  | LET
  | TILDE
  | UNDERSCORE
  | COLONEQUAL
  | EQUALEQUAL

val grammar :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.partial_grammar
