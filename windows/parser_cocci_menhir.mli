exception Error

type token = 
  | Tvolatile of (Data.clt)
  | Tvoid of (Data.clt)
  | Tunsigned of (Data.clt)
  | Tunion of (Data.clt)
  | Ttypedef of (Data.clt)
  | Tstruct of (Data.clt)
  | Tstatic of (Data.clt)
  | Tsigned of (Data.clt)
  | Tshort of (Data.clt)
  | Tregister of (Data.clt)
  | Tlong of (Data.clt)
  | Tlist
  | Tint of (Data.clt)
  | Tinline of (Data.clt)
  | Tfloat of (Data.clt)
  | Textern of (Data.clt)
  | Tdouble of (Data.clt)
  | Tconst of (Data.clt)
  | Tchar of (Data.clt)
  | Tauto of (Data.clt)
  | Tattr of (string * Data.clt)
  | TXor of (Data.clt)
  | TWords
  | TWhy0
  | TWhy of (Data.clt)
  | TWhile of (Data.clt)
  | TWhen of (Data.clt)
  | TUsing
  | TTypedef
  | TTypeId of (string * Data.clt)
  | TType
  | TTilde of (Data.clt)
  | TSwitch of (Data.clt)
  | TSupEq of (Data.clt)
  | TSup of (Data.clt)
  | TString of (string * Data.clt)
  | TStatement
  | TSizeof of (Data.clt)
  | TShr of (Data.clt)
  | TShl of (Data.clt)
  | TRuleName of (string)
  | TRightIso
  | TReturn of (Data.clt)
  | TPure
  | TPtrOp of (Data.clt)
  | TPtVirg of (Data.clt)
  | TPragma of (string)
  | TPlusFile of (string * Data.clt)
  | TPlus0
  | TPlus of (Data.clt)
  | TPathIsoFile of (string)
  | TParameter
  | TPOEllipsis of (Data.clt)
  | TPCEllipsis of (Data.clt)
  | TOrLog of (Data.clt)
  | TOr of (Data.clt)
  | TOn
  | TOPar0 of (Data.clt)
  | TOPar of (Data.clt)
  | TOEllipsis of (Data.clt)
  | TOCro of (Data.clt)
  | TOBrace of (Data.clt)
  | TNothing
  | TNotEq of (Data.clt)
  | TNever
  | TMul of (Data.clt)
  | TMod of (Data.clt)
  | TMinusFile of (string * Data.clt)
  | TMinus of (Data.clt)
  | TMid0 of (Data.clt)
  | TMetaType of (Ast_cocci.meta_name * Ast0_cocci.pure * Data.clt)
  | TMetaStmList of (Ast_cocci.meta_name * Ast0_cocci.pure * Data.clt)
  | TMetaStm of (Ast_cocci.meta_name * Ast0_cocci.pure * Data.clt)
  | TMetaParamList of (Ast_cocci.meta_name * Ast_cocci.meta_name option * Ast0_cocci.pure * Data.clt)
  | TMetaParam of (Ast_cocci.meta_name * Ast0_cocci.pure * Data.clt)
  | TMetaLocalFunc of (Ast_cocci.meta_name * Ast0_cocci.pure * Data.clt)
  | TMetaIdExp of (Ast_cocci.meta_name * Ast0_cocci.pure * Type_cocci.typeC list option *
          Data.clt)
  | TMetaId of (Ast_cocci.meta_name * Ast0_cocci.pure * Data.clt)
  | TMetaFunc of (Ast_cocci.meta_name * Ast0_cocci.pure * Data.clt)
  | TMetaExpList of (Ast_cocci.meta_name * Ast_cocci.meta_name option * Ast0_cocci.pure * Data.clt)
  | TMetaExp of (Ast_cocci.meta_name * Ast0_cocci.pure * Type_cocci.typeC list option *
          Data.clt)
  | TMetaErr of (Ast_cocci.meta_name * Ast0_cocci.pure * Data.clt)
  | TMetaConst of (Ast_cocci.meta_name * Ast0_cocci.pure * Type_cocci.typeC list option *
          Data.clt)
  | TMPtVirg
  | TLocal
  | TLineEnd of (Data.clt)
  | TIteratorId of (string * Data.clt)
  | TIterator
  | TIsoType
  | TIsoTopLevel
  | TIsoTestExpression
  | TIsoStatement
  | TIsoExpression
  | TIsoDeclaration
  | TIsoArgExpression
  | TIso
  | TInvalid
  | TInt of (string * Data.clt)
  | TInfEq of (Data.clt)
  | TInf of (Data.clt)
  | TIncludeNL of (string * Data.clt)
  | TIncludeL of (string * Data.clt)
  | TInc of (Data.clt)
  | TIf of (Data.clt)
  | TIdentifier
  | TIdent of (string * Data.clt)
  | TIdExpression
  | TFunction
  | TFunDecl of (Data.clt)
  | TFresh
  | TFor of (Data.clt)
  | TFloat of (string * Data.clt)
  | TExtends
  | TExpression
  | TExists
  | TEver
  | TError
  | TEqEq of (Data.clt)
  | TEq of (Data.clt)
  | TElse of (Data.clt)
  | TEllipsis of (Data.clt)
  | TDotDot of (Data.clt)
  | TDot of (Data.clt)
  | TDo of (Data.clt)
  | TDiv of (Data.clt)
  | TDisable
  | TDepends
  | TDefineParam of (Data.clt * token * int)
  | TDefine of (Data.clt * token)
  | TDefault of (Data.clt)
  | TDeclarerId of (string * Data.clt)
  | TDeclarer
  | TDec of (Data.clt)
  | TContinue of (Data.clt)
  | TContext
  | TConstant
  | TComma of (Data.clt)
  | TChar of (string * Data.clt)
  | TCase of (Data.clt)
  | TCPar0 of (Data.clt)
  | TCPar of (Data.clt)
  | TCEllipsis of (Data.clt)
  | TCCro of (Data.clt)
  | TCBrace of (Data.clt)
  | TBreak of (Data.clt)
  | TBang0
  | TBang of (Data.clt)
  | TAssign of (Ast_cocci.assignOp * Data.clt)
  | TArobArob
  | TArob
  | TAny of (Data.clt)
  | TAndLog of (Data.clt)
  | TAnd of (Data.clt)
  | EOF


val rule_name: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string option * Ast_cocci.dependency * string list * string list *
  Ast_cocci.exists)
val reinit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
val plus_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val never_used: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
val minus_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val meta_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Ast_cocci.metavar,Ast_cocci.metavar) Common.either list)
val iso_rule_name: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string option * Ast_cocci.dependency * string list * string list *
  Ast_cocci.exists)
val iso_meta_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Ast_cocci.metavar,Ast_cocci.metavar) Common.either list)
val iso_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.anything list list)
val include_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((string,string) Common.either list)