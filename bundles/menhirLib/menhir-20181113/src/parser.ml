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
# 29 "menhir-20181113/src/parser.ml"
)
  | UID of (
# 33 "menhir-20181113/src/parser.mly"
        string Positions.located
# 34 "menhir-20181113/src/parser.ml"
)
  | QID of (
# 33 "menhir-20181113/src/parser.mly"
        string Positions.located
# 39 "menhir-20181113/src/parser.ml"
)
  | HEADER of (
# 34 "menhir-20181113/src/parser.mly"
        Stretch.t
# 44 "menhir-20181113/src/parser.ml"
)
  | OCAMLTYPE of (
# 35 "menhir-20181113/src/parser.mly"
        Stretch.ocamltype
# 49 "menhir-20181113/src/parser.ml"
)
  | PERCENTPERCENT of (
# 36 "menhir-20181113/src/parser.mly"
        Stretch.t Lazy.t
# 54 "menhir-20181113/src/parser.ml"
)
  | ACTION of (
# 37 "menhir-20181113/src/parser.mly"
        Syntax.raw_action
# 59 "menhir-20181113/src/parser.ml"
)
  | ATTRIBUTE of (
# 38 "menhir-20181113/src/parser.mly"
        Syntax.attribute
# 64 "menhir-20181113/src/parser.ml"
)
  | GRAMMARATTRIBUTE of (
# 38 "menhir-20181113/src/parser.mly"
        Syntax.attribute
# 69 "menhir-20181113/src/parser.ml"
)
  | LET
  | TILDE
  | UNDERSCORE
  | COLONEQUAL
  | EQUALEQUAL

open Parsing
let _ = parse_error;;
# 24 "menhir-20181113/src/parser.mly"

open Syntax
open Positions

# 84 "menhir-20181113/src/parser.ml"
let yytransl_const = [|
  257 (* TOKEN *);
  258 (* TYPE *);
  259 (* LEFT *);
  260 (* RIGHT *);
  261 (* NONASSOC *);
  262 (* START *);
  263 (* PREC *);
  264 (* PUBLIC *);
  265 (* COLON *);
  266 (* BAR *);
    0 (* EOF *);
  267 (* EQUAL *);
  268 (* INLINE *);
  269 (* LPAREN *);
  270 (* RPAREN *);
  271 (* COMMA *);
  272 (* QUESTION *);
  273 (* STAR *);
  274 (* PLUS *);
  275 (* PARAMETER *);
  276 (* ON_ERROR_REDUCE *);
  277 (* PERCENTATTRIBUTE *);
  278 (* SEMI *);
  288 (* LET *);
  289 (* TILDE *);
  290 (* UNDERSCORE *);
  291 (* COLONEQUAL *);
  292 (* EQUALEQUAL *);
    0|]

let yytransl_block = [|
  279 (* LID *);
  280 (* UID *);
  281 (* QID *);
  282 (* HEADER *);
  283 (* OCAMLTYPE *);
  284 (* PERCENTPERCENT *);
  285 (* ACTION *);
  286 (* ATTRIBUTE *);
  287 (* GRAMMARATTRIBUTE *);
    0|]

let yylhs = "\255\255\
\001\000\006\000\006\000\004\000\004\000\004\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\008\000\008\000\012\000\012\000\012\000\013\000\013\000\016\000\
\016\000\016\000\015\000\015\000\014\000\014\000\009\000\009\000\
\010\000\010\000\017\000\017\000\005\000\005\000\005\000\018\000\
\019\000\019\000\019\000\019\000\019\000\020\000\020\000\024\000\
\024\000\025\000\025\000\026\000\026\000\027\000\027\000\011\000\
\011\000\021\000\021\000\028\000\028\000\028\000\023\000\023\000\
\022\000\030\000\030\000\029\000\029\000\031\000\031\000\003\000\
\032\000\032\000\002\000\002\000\033\000\033\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\000\000\002\000\002\000\001\000\003\000\
\002\000\003\000\003\000\002\000\002\000\001\000\003\000\002\000\
\000\000\001\000\001\000\001\000\001\000\000\000\003\000\001\000\
\001\000\001\000\000\000\001\000\000\000\002\000\000\000\005\000\
\000\000\002\000\000\000\001\000\000\000\002\000\002\000\008\000\
\000\000\001\000\001\000\002\000\002\000\000\000\003\000\001\000\
\003\000\000\000\003\000\001\000\003\000\002\000\002\000\000\000\
\003\000\000\000\001\000\001\000\001\000\001\000\000\000\003\000\
\003\000\000\000\002\000\001\000\002\000\002\000\003\000\002\000\
\000\000\002\000\003\000\005\000\000\000\002\000\002\000"

let yydefred = "\000\000\
\004\000\000\000\079\000\000\000\000\000\000\000\019\000\020\000\
\021\000\000\000\000\000\056\000\056\000\006\000\007\000\037\000\
\014\000\005\000\022\000\018\000\031\000\056\000\033\000\000\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\000\028\000\000\000\000\000\015\000\000\000\002\000\000\000\
\039\000\003\000\001\000\038\000\000\000\000\000\000\000\024\000\
\025\000\026\000\000\000\000\000\030\000\044\000\045\000\000\000\
\023\000\000\000\000\000\054\000\060\000\062\000\061\000\055\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\032\000\
\051\000\000\000\000\000\000\000\000\000\053\000\000\000\047\000\
\059\000\073\000\049\000\000\000\063\000\000\000\000\000\073\000\
\069\000\000\000\000\000\000\000\000\000\074\000\000\000\072\000\
\000\000\073\000\065\000\067\000\000\000\077\000\071\000\064\000\
\000\000\000\000\077\000\078\000\000\000"

let yydgoto = "\002\000\
\003\000\094\000\084\000\004\000\028\000\043\000\018\000\021\000\
\030\000\024\000\026\000\019\000\029\000\037\000\035\000\051\000\
\067\000\044\000\045\000\071\000\082\000\085\000\090\000\076\000\
\060\000\068\000\069\000\064\000\086\000\096\000\089\000\087\000\
\106\000"

let yysindex = "\011\000\
\000\000\000\000\000\000\191\000\009\255\011\255\000\000\000\000\
\000\000\025\255\026\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\255\
\000\000\045\255\004\255\006\000\045\255\045\255\045\255\024\255\
\000\000\000\000\033\255\071\255\000\000\090\255\000\000\099\255\
\000\000\000\000\000\000\000\000\033\255\033\255\084\255\000\000\
\000\000\000\000\105\255\082\255\000\000\000\000\000\000\071\255\
\000\000\095\255\033\255\000\000\000\000\000\000\000\000\000\000\
\123\255\000\000\071\255\126\255\000\255\033\255\154\255\000\000\
\000\000\033\255\120\255\155\255\158\255\000\000\033\255\000\000\
\000\000\000\000\000\000\160\255\000\000\162\255\048\255\000\000\
\000\000\172\255\185\255\033\255\182\255\000\000\121\255\000\000\
\160\255\000\000\000\000\000\000\033\255\000\000\000\000\000\000\
\121\255\177\255\000\000\000\000\177\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\067\000\000\000\000\000\000\000\
\000\000\129\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\222\000\
\000\000\122\255\153\255\106\255\184\255\160\000\215\255\253\000\
\000\000\000\000\000\000\020\255\000\000\109\255\000\000\137\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\060\255\091\255\000\000\000\000\000\000\254\254\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\000\000\
\186\255\000\000\098\000\000\000\180\255\000\000\000\000\000\000\
\000\000\000\000\187\255\000\000\080\255\000\000\000\000\000\000\
\000\000\000\000\000\000\171\255\000\000\000\000\003\255\000\000\
\000\000\008\000\001\000\000\000\014\001\000\000\142\255\000\000\
\193\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\142\255\173\255\000\000\000\000\204\255"

let yygindex = "\000\000\
\000\000\000\000\125\000\000\000\000\000\000\000\000\000\000\000\
\000\000\200\000\248\255\000\000\000\000\220\255\236\255\214\255\
\000\000\000\000\000\000\000\000\000\000\126\000\000\000\146\000\
\000\000\152\000\223\255\000\000\000\000\139\000\134\000\000\000\
\135\000"

let yytablesize = 556
let yytable = "\053\000\
\066\000\052\000\056\000\057\000\027\000\039\000\029\000\040\000\
\046\000\047\000\029\000\001\000\066\000\031\000\074\000\061\000\
\062\000\063\000\034\000\065\000\029\000\029\000\029\000\029\000\
\029\000\029\000\029\000\075\000\029\000\029\000\072\000\066\000\
\029\000\036\000\029\000\020\000\075\000\022\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\029\000\033\000\029\000\
\029\000\100\000\029\000\023\000\025\000\095\000\092\000\048\000\
\049\000\050\000\102\000\034\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\105\000\107\000\050\000\093\000\049\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\058\000\050\000\
\050\000\050\000\050\000\057\000\057\000\057\000\057\000\057\000\
\057\000\061\000\062\000\063\000\036\000\054\000\058\000\058\000\
\058\000\057\000\055\000\058\000\058\000\057\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\059\000\057\000\066\000\
\057\000\057\000\016\000\016\000\016\000\016\000\016\000\016\000\
\041\000\041\000\041\000\042\000\042\000\042\000\079\000\070\000\
\061\000\062\000\063\000\073\000\016\000\016\000\016\000\016\000\
\027\000\027\000\027\000\016\000\029\000\016\000\036\000\029\000\
\016\000\029\000\029\000\029\000\029\000\029\000\029\000\043\000\
\043\000\043\000\077\000\029\000\029\000\029\000\029\000\081\000\
\080\000\088\000\029\000\029\000\029\000\029\000\029\000\027\000\
\027\000\027\000\029\000\075\000\029\000\098\000\075\000\029\000\
\012\000\012\000\012\000\012\000\012\000\012\000\091\000\092\000\
\101\000\052\000\046\000\075\000\075\000\075\000\108\000\068\000\
\048\000\075\000\012\000\012\000\012\000\012\000\027\000\027\000\
\027\000\012\000\076\000\012\000\097\000\076\000\012\000\010\000\
\010\000\010\000\010\000\010\000\010\000\070\000\032\000\104\000\
\083\000\078\000\076\000\076\000\076\000\099\000\103\000\000\000\
\076\000\010\000\010\000\010\000\010\000\027\000\027\000\027\000\
\010\000\109\000\010\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\066\000\000\000\066\000\000\000\066\000\038\000\000\000\040\000\
\000\000\040\000\000\000\040\000\000\000\000\000\066\000\066\000\
\066\000\066\000\000\000\041\000\066\000\040\000\040\000\040\000\
\040\000\042\000\000\000\040\000\035\000\035\000\035\000\035\000\
\035\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\000\000\000\000\000\000\035\000\035\000\
\035\000\035\000\000\000\035\000\000\000\035\000\000\000\035\000\
\000\000\035\000\035\000\017\000\017\000\017\000\017\000\017\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\000\000\000\000\000\000\000\017\000\017\000\017\000\
\017\000\000\000\017\000\000\000\017\000\000\000\017\000\000\000\
\000\000\017\000\029\000\029\000\029\000\029\000\029\000\029\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\029\000\000\000\000\000\000\000\029\000\029\000\029\000\029\000\
\000\000\029\000\000\000\029\000\000\000\029\000\000\000\000\000\
\029\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\000\033\000\033\000\033\000\033\000\
\000\000\000\000\033\000\000\000\033\000\000\000\000\000\033\000\
\008\000\008\000\008\000\008\000\008\000\008\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\008\000\008\000\008\000\000\000\027\000\
\000\000\008\000\000\000\008\000\000\000\000\000\008\000\005\000\
\006\000\007\000\008\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\012\000\013\000\014\000\000\000\000\000\000\000\
\015\000\000\000\016\000\000\000\000\000\017\000\009\000\009\000\
\009\000\009\000\009\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\009\000\009\000\009\000\009\000\000\000\000\000\000\000\009\000\
\000\000\009\000\000\000\000\000\009\000\011\000\011\000\011\000\
\011\000\011\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\011\000\011\000\011\000\000\000\024\000\000\000\011\000\024\000\
\011\000\000\000\024\000\011\000\000\000\024\000\024\000\024\000\
\000\000\000\000\000\000\024\000\024\000\024\000\024\000\000\000\
\000\000\000\000\024\000\024\000"

let yycheck = "\036\000\
\000\000\035\000\045\000\046\000\013\000\000\000\009\001\000\000\
\029\000\030\000\013\001\001\000\010\001\022\000\015\001\016\001\
\017\001\018\001\015\001\056\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\070\000\009\001\010\001\067\000\029\001\
\013\001\030\001\015\001\027\001\079\000\027\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\023\001\028\001\
\029\001\092\000\031\001\027\001\027\001\087\000\007\001\023\001\
\024\001\025\001\095\000\015\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\101\000\105\000\010\001\023\001\024\001\
\025\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\007\001\028\001\
\029\001\030\001\031\001\001\001\002\001\003\001\004\001\005\001\
\006\001\016\001\017\001\018\001\030\001\012\001\023\001\024\001\
\025\001\015\001\008\001\024\001\029\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\013\001\028\001\025\001\
\030\001\031\001\001\001\002\001\003\001\004\001\005\001\006\001\
\023\001\024\001\025\001\023\001\024\001\025\001\015\001\013\001\
\016\001\017\001\018\001\014\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\007\001\028\001\030\001\010\001\
\031\001\001\001\002\001\003\001\004\001\005\001\006\001\023\001\
\024\001\025\001\009\001\022\001\023\001\024\001\025\001\010\001\
\014\001\010\001\029\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\007\001\028\001\010\001\010\001\031\001\
\001\001\002\001\003\001\004\001\005\001\006\001\029\001\007\001\
\011\001\014\001\009\001\023\001\024\001\025\001\022\001\029\001\
\014\001\029\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\007\001\028\001\088\000\010\001\031\001\001\001\
\002\001\003\001\004\001\005\001\006\001\029\001\023\000\098\000\
\079\000\074\000\023\001\024\001\025\001\091\000\097\000\255\255\
\029\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\107\000\028\001\255\255\255\255\031\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\008\001\255\255\010\001\255\255\012\001\008\001\255\255\008\001\
\255\255\012\001\255\255\012\001\255\255\255\255\022\001\023\001\
\024\001\025\001\255\255\022\001\028\001\022\001\023\001\024\001\
\025\001\028\001\255\255\028\001\001\001\002\001\003\001\004\001\
\005\001\006\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\015\001\255\255\255\255\255\255\019\001\020\001\
\021\001\022\001\255\255\024\001\255\255\026\001\255\255\028\001\
\255\255\030\001\031\001\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\015\001\255\255\255\255\255\255\019\001\020\001\021\001\
\022\001\255\255\024\001\255\255\026\001\255\255\028\001\255\255\
\255\255\031\001\001\001\002\001\003\001\004\001\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\015\001\255\255\255\255\255\255\019\001\020\001\021\001\022\001\
\255\255\024\001\255\255\026\001\255\255\028\001\255\255\255\255\
\031\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\019\001\020\001\021\001\022\001\023\001\
\255\255\255\255\026\001\255\255\028\001\255\255\255\255\031\001\
\001\001\002\001\003\001\004\001\005\001\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\019\001\020\001\021\001\022\001\255\255\024\001\
\255\255\026\001\255\255\028\001\255\255\255\255\031\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\026\001\255\255\028\001\255\255\255\255\031\001\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\026\001\
\255\255\028\001\255\255\255\255\031\001\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001\255\255\007\001\255\255\026\001\010\001\
\028\001\255\255\013\001\031\001\255\255\016\001\017\001\018\001\
\255\255\255\255\255\255\022\001\023\001\024\001\025\001\255\255\
\255\255\255\255\029\001\030\001"

let yynames_const = "\
  TOKEN\000\
  TYPE\000\
  LEFT\000\
  RIGHT\000\
  NONASSOC\000\
  START\000\
  PREC\000\
  PUBLIC\000\
  COLON\000\
  BAR\000\
  EOF\000\
  EQUAL\000\
  INLINE\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  QUESTION\000\
  STAR\000\
  PLUS\000\
  PARAMETER\000\
  ON_ERROR_REDUCE\000\
  PERCENTATTRIBUTE\000\
  SEMI\000\
  LET\000\
  TILDE\000\
  UNDERSCORE\000\
  COLONEQUAL\000\
  EQUALEQUAL\000\
  "

let yynames_block = "\
  LID\000\
  UID\000\
  QID\000\
  HEADER\000\
  OCAMLTYPE\000\
  PERCENTPERCENT\000\
  ACTION\000\
  ATTRIBUTE\000\
  GRAMMARATTRIBUTE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'declarations) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Stretch.t Lazy.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'rules) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'postlude) in
    Obj.repr(
# 64 "menhir-20181113/src/parser.mly"
    (
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_declarations      = List.rev _1;
        pg_rules             = _3;
        pg_postlude          = _4
      }
    )
# 419 "menhir-20181113/src/parser.ml"
               : Syntax.partial_grammar))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "menhir-20181113/src/parser.mly"
    ( None )
# 425 "menhir-20181113/src/parser.ml"
               : 'postlude))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Stretch.t Lazy.t) in
    Obj.repr(
# 77 "menhir-20181113/src/parser.mly"
    ( Some (Lazy.force _1) )
# 432 "menhir-20181113/src/parser.ml"
               : 'postlude))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "menhir-20181113/src/parser.mly"
    ( [] )
# 438 "menhir-20181113/src/parser.ml"
               : 'declarations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declarations) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 87 "menhir-20181113/src/parser.mly"
    ( _2 @ _1 )
# 446 "menhir-20181113/src/parser.ml"
               : 'declarations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declarations) in
    Obj.repr(
# 89 "menhir-20181113/src/parser.mly"
    ( _1 )
# 453 "menhir-20181113/src/parser.ml"
               : 'declarations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Stretch.t) in
    Obj.repr(
# 93 "menhir-20181113/src/parser.mly"
    ( [ unknown_pos (DCode _1) ] )
# 460 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_ocamltype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'terminals) in
    Obj.repr(
# 96 "menhir-20181113/src/parser.mly"
    ( let ty, ts = _2, _3 in
      List.map (Positions.map (fun (terminal, alias, attrs) ->
        DToken (ty, terminal, alias, attrs)
      )) ts )
# 471 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nonterminals) in
    Obj.repr(
# 102 "menhir-20181113/src/parser.mly"
    ( List.map (Positions.map (fun nonterminal -> DStart nonterminal)) _2 )
# 478 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Stretch.ocamltype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 105 "menhir-20181113/src/parser.mly"
    ( List.map (Positions.map (fun nt -> DType (_2, nt)))
        (List.map Parameters.with_pos _3) )
# 487 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Stretch.ocamltype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nonterminals) in
    Obj.repr(
# 110 "menhir-20181113/src/parser.mly"
    ( Misc.mapd (fun ntloc ->
        Positions.mapd (fun nt -> DStart nt, DType (_2, ParameterVar ntloc)) ntloc) _3 )
# 496 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'priority_keyword) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'symbols) in
    Obj.repr(
# 114 "menhir-20181113/src/parser.mly"
    ( let prec = ParserAux.new_precedence_level (rhs_start_pos 1, rhs_end_pos 1) in
      List.map (Positions.map (fun symbol -> DTokenProperties (symbol, _1, prec))) _2 )
# 505 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Stretch.ocamltype) in
    Obj.repr(
# 118 "menhir-20181113/src/parser.mly"
    ( [ unknown_pos (DParameter _2) ] )
# 512 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.attribute) in
    Obj.repr(
# 121 "menhir-20181113/src/parser.mly"
    ( [ unknown_pos (DGrammarAttribute _1) ] )
# 519 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'actuals) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributes) in
    Obj.repr(
# 124 "menhir-20181113/src/parser.mly"
    ( [ unknown_pos (DSymbolAttributes (_2, _3)) ] )
# 527 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 127 "menhir-20181113/src/parser.mly"
    ( let prec = ParserAux.new_on_error_reduce_level() in
      List.map (Positions.map (fun nt -> DOnErrorReduce (nt, prec)))
        (List.map Parameters.with_pos _2) )
# 536 "menhir-20181113/src/parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "menhir-20181113/src/parser.mly"
    ( None )
# 542 "menhir-20181113/src/parser.ml"
               : 'optional_ocamltype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Stretch.ocamltype) in
    Obj.repr(
# 135 "menhir-20181113/src/parser.mly"
    ( Some _1 )
# 549 "menhir-20181113/src/parser.ml"
               : 'optional_ocamltype))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "menhir-20181113/src/parser.mly"
    ( LeftAssoc )
# 555 "menhir-20181113/src/parser.ml"
               : 'priority_keyword))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "menhir-20181113/src/parser.mly"
    ( RightAssoc )
# 561 "menhir-20181113/src/parser.ml"
               : 'priority_keyword))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "menhir-20181113/src/parser.mly"
    ( NonAssoc )
# 567 "menhir-20181113/src/parser.ml"
               : 'priority_keyword))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "menhir-20181113/src/parser.mly"
    ( [] )
# 573 "menhir-20181113/src/parser.ml"
               : 'symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'symbols) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_comma) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 161 "menhir-20181113/src/parser.mly"
    ( _3 :: _1 )
# 582 "menhir-20181113/src/parser.ml"
               : 'symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Positions.located) in
    Obj.repr(
# 165 "menhir-20181113/src/parser.mly"
    ( _1 )
# 589 "menhir-20181113/src/parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Positions.located) in
    Obj.repr(
# 167 "menhir-20181113/src/parser.mly"
    ( _1 )
# 596 "menhir-20181113/src/parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Positions.located) in
    Obj.repr(
# 169 "menhir-20181113/src/parser.mly"
    ( _1 )
# 603 "menhir-20181113/src/parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 173 "menhir-20181113/src/parser.mly"
    ( () )
# 609 "menhir-20181113/src/parser.ml"
               : 'optional_comma))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "menhir-20181113/src/parser.mly"
    ( () )
# 615 "menhir-20181113/src/parser.ml"
               : 'optional_comma))
; (fun __caml_parser_env ->
    Obj.repr(
# 179 "menhir-20181113/src/parser.mly"
    ( [] )
# 621 "menhir-20181113/src/parser.ml"
               : 'attributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.attribute) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attributes) in
    Obj.repr(
# 180 "menhir-20181113/src/parser.mly"
                       ( _1 :: _2 )
# 629 "menhir-20181113/src/parser.ml"
               : 'attributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 188 "menhir-20181113/src/parser.mly"
    ( [] )
# 635 "menhir-20181113/src/parser.ml"
               : 'terminals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'terminals) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'optional_comma) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string Positions.located) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'optional_alias) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'attributes) in
    Obj.repr(
# 190 "menhir-20181113/src/parser.mly"
    ( let ts, uid, alias, attrs = _1, _3, _4, _5 in
      let alias = Option.map Positions.value alias in
      Positions.map (fun uid -> uid, alias, attrs) uid :: ts )
# 648 "menhir-20181113/src/parser.ml"
               : 'terminals))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "menhir-20181113/src/parser.mly"
    ( [] )
# 654 "menhir-20181113/src/parser.ml"
               : 'nonterminals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nonterminals) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string Positions.located) in
    Obj.repr(
# 198 "menhir-20181113/src/parser.mly"
    ( _2 :: _1 )
# 662 "menhir-20181113/src/parser.ml"
               : 'nonterminals))
; (fun __caml_parser_env ->
    Obj.repr(
# 202 "menhir-20181113/src/parser.mly"
    ( None )
# 668 "menhir-20181113/src/parser.ml"
               : 'optional_alias))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Positions.located) in
    Obj.repr(
# 204 "menhir-20181113/src/parser.mly"
    ( Some _1 )
# 675 "menhir-20181113/src/parser.ml"
               : 'optional_alias))
; (fun __caml_parser_env ->
    Obj.repr(
# 213 "menhir-20181113/src/parser.mly"
    ( [] )
# 681 "menhir-20181113/src/parser.ml"
               : 'rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rules) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 215 "menhir-20181113/src/parser.mly"
    ( _2 :: _1 )
# 689 "menhir-20181113/src/parser.ml"
               : 'rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rules) in
    Obj.repr(
# 217 "menhir-20181113/src/parser.mly"
    ( _1 )
# 696 "menhir-20181113/src/parser.ml"
               : 'rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'flags) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'attributes) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'optional_formal_parameters) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'optional_bar) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'production_group) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'production_groups) in
    Obj.repr(
# 227 "menhir-20181113/src/parser.mly"
    (
      let public, inline = _1 in
      { pr_public_flag = public;
        pr_inline_flag = inline;
        pr_nt          = Positions.value _2;
        pr_positions   = [ Positions.position _2 ];
        pr_attributes  = _3;
        pr_parameters  = _4;
        pr_branches    = List.flatten (_7 :: List.rev _8)
      }
    )
# 719 "menhir-20181113/src/parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 241 "menhir-20181113/src/parser.mly"
    ( false, false )
# 725 "menhir-20181113/src/parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 243 "menhir-20181113/src/parser.mly"
    ( true, false )
# 731 "menhir-20181113/src/parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 245 "menhir-20181113/src/parser.mly"
    ( false, true )
# 737 "menhir-20181113/src/parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 247 "menhir-20181113/src/parser.mly"
    ( true, true )
# 743 "menhir-20181113/src/parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 249 "menhir-20181113/src/parser.mly"
    ( true, true )
# 749 "menhir-20181113/src/parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 259 "menhir-20181113/src/parser.mly"
    ( [] )
# 755 "menhir-20181113/src/parser.ml"
               : 'optional_formal_parameters))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_parameters) in
    Obj.repr(
# 261 "menhir-20181113/src/parser.mly"
    ( _2 )
# 762 "menhir-20181113/src/parser.ml"
               : 'optional_formal_parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 265 "menhir-20181113/src/parser.mly"
    ( [ Positions.value _1 ] )
# 769 "menhir-20181113/src/parser.ml"
               : 'formal_parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_parameters) in
    Obj.repr(
# 267 "menhir-20181113/src/parser.mly"
    ( Positions.value _1 :: _3 )
# 777 "menhir-20181113/src/parser.ml"
               : 'formal_parameters))
; (fun __caml_parser_env ->
    Obj.repr(
# 271 "menhir-20181113/src/parser.mly"
    ( [] )
# 783 "menhir-20181113/src/parser.ml"
               : 'optional_actuals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_comma) in
    Obj.repr(
# 273 "menhir-20181113/src/parser.mly"
    ( _2 )
# 790 "menhir-20181113/src/parser.ml"
               : 'optional_actuals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actual) in
    Obj.repr(
# 277 "menhir-20181113/src/parser.mly"
    ( [ _1 ] )
# 797 "menhir-20181113/src/parser.ml"
               : 'actuals_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actual) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_comma) in
    Obj.repr(
# 279 "menhir-20181113/src/parser.mly"
    ( _1 :: _3 )
# 805 "menhir-20181113/src/parser.ml"
               : 'actuals_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'symbol) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_actuals) in
    Obj.repr(
# 283 "menhir-20181113/src/parser.mly"
    ( Parameters.app _1 _2 )
# 813 "menhir-20181113/src/parser.ml"
               : 'actual))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'actual) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'modifier) in
    Obj.repr(
# 285 "menhir-20181113/src/parser.mly"
    ( ParameterApp (_2, [ _1 ]) )
# 821 "menhir-20181113/src/parser.ml"
               : 'actual))
; (fun __caml_parser_env ->
    Obj.repr(
# 289 "menhir-20181113/src/parser.mly"
    ( [] )
# 827 "menhir-20181113/src/parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_comma) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'actual) in
    Obj.repr(
# 291 "menhir-20181113/src/parser.mly"
    ( _3::_1 )
# 836 "menhir-20181113/src/parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    Obj.repr(
# 295 "menhir-20181113/src/parser.mly"
    ( () )
# 842 "menhir-20181113/src/parser.ml"
               : 'optional_bar))
; (fun __caml_parser_env ->
    Obj.repr(
# 297 "menhir-20181113/src/parser.mly"
    ( () )
# 848 "menhir-20181113/src/parser.ml"
               : 'optional_bar))
; (fun __caml_parser_env ->
    Obj.repr(
# 305 "menhir-20181113/src/parser.mly"
    ( unknown_pos "option" )
# 854 "menhir-20181113/src/parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 307 "menhir-20181113/src/parser.mly"
    ( unknown_pos "nonempty_list" )
# 860 "menhir-20181113/src/parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 309 "menhir-20181113/src/parser.mly"
    ( unknown_pos "list" )
# 866 "menhir-20181113/src/parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 317 "menhir-20181113/src/parser.mly"
    ( [] )
# 872 "menhir-20181113/src/parser.ml"
               : 'production_groups))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'production_groups) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'production_group) in
    Obj.repr(
# 319 "menhir-20181113/src/parser.mly"
    ( _3 :: _1 )
# 880 "menhir-20181113/src/parser.ml"
               : 'production_groups))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'productions) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Syntax.raw_action) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'optional_precedence) in
    Obj.repr(
# 323 "menhir-20181113/src/parser.mly"
    (
      let productions, action, oprec2 = _1, _2, _3 in
      (* If multiple productions share a single semantic action, check
         that all of them bind the same names. *)
      ParserAux.check_production_group productions;
      (* Then, *)
      List.map (fun (producers, oprec1, level, pos) ->
        (* Replace [$i] with [_i]. *)
        let pr_producers = ParserAux.normalize_producers producers in
        (* Distribute the semantic action. Also, check that every [$i]
           is within bounds. *)
        let names = ParserAux.producer_names producers in
        let pr_action = action Settings.dollars names in
        {
          pr_producers;
          pr_action;
          pr_branch_prec_annotation   = ParserAux.override pos oprec1 oprec2;
          pr_branch_production_level  = level;
          pr_branch_position          = pos
        })
      productions
    )
# 910 "menhir-20181113/src/parser.ml"
               : 'production_group))
; (fun __caml_parser_env ->
    Obj.repr(
# 348 "menhir-20181113/src/parser.mly"
    ( None )
# 916 "menhir-20181113/src/parser.ml"
               : 'optional_precedence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 350 "menhir-20181113/src/parser.mly"
    ( Some _2 )
# 923 "menhir-20181113/src/parser.ml"
               : 'optional_precedence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : ParserAux.early_production) in
    Obj.repr(
# 359 "menhir-20181113/src/parser.mly"
    ( [ _1 ] )
# 930 "menhir-20181113/src/parser.ml"
               : 'productions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : ParserAux.early_production) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bar_productions) in
    Obj.repr(
# 361 "menhir-20181113/src/parser.mly"
    ( _1 :: _2 )
# 938 "menhir-20181113/src/parser.ml"
               : 'productions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : ParserAux.early_production) in
    Obj.repr(
# 365 "menhir-20181113/src/parser.mly"
    ( [ _2 ] )
# 945 "menhir-20181113/src/parser.ml"
               : 'bar_productions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : ParserAux.early_production) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bar_productions) in
    Obj.repr(
# 367 "menhir-20181113/src/parser.mly"
    ( _2 :: _3 )
# 953 "menhir-20181113/src/parser.ml"
               : 'bar_productions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'producers) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_precedence) in
    Obj.repr(
# 371 "menhir-20181113/src/parser.mly"
    ( List.rev _1,
      _2,
      ParserAux.new_production_level(),
      Positions.import (symbol_start_pos(), symbol_end_pos())
    )
# 965 "menhir-20181113/src/parser.ml"
               : ParserAux.early_production))
; (fun __caml_parser_env ->
    Obj.repr(
# 379 "menhir-20181113/src/parser.mly"
    ( [] )
# 971 "menhir-20181113/src/parser.ml"
               : 'producers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'producers) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : ParserAux.early_producer) in
    Obj.repr(
# 381 "menhir-20181113/src/parser.mly"
    ( _2 :: _1 )
# 979 "menhir-20181113/src/parser.ml"
               : 'producers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actual) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'attributes) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'optional_semis) in
    Obj.repr(
# 389 "menhir-20181113/src/parser.mly"
    ( Positions.import (symbol_start_pos(), symbol_end_pos()),    None, _1, _2 )
# 988 "menhir-20181113/src/parser.ml"
               : ParserAux.early_producer))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string Positions.located) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'actual) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'attributes) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'optional_semis) in
    Obj.repr(
# 391 "menhir-20181113/src/parser.mly"
    ( Positions.import (symbol_start_pos(), symbol_end_pos()), Some _1, _3, _4 )
# 998 "menhir-20181113/src/parser.ml"
               : ParserAux.early_producer))
; (fun __caml_parser_env ->
    Obj.repr(
# 398 "menhir-20181113/src/parser.mly"
                      ( () )
# 1004 "menhir-20181113/src/parser.ml"
               : 'optional_semis))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'optional_semis) in
    Obj.repr(
# 399 "menhir-20181113/src/parser.mly"
                      ( () )
# 1011 "menhir-20181113/src/parser.ml"
               : 'optional_semis))
(* Entry grammar *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let grammar (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.partial_grammar)
;;
