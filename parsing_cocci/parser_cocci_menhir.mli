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
  | TWhenTrue of (Data.clt)
  | TWhenFalse of (Data.clt)
  | TWhen of (Data.clt)
  | TUsing
  | TTypedef
  | TTypeId of (string * Data.clt)
  | TType
  | TTilde of (Data.clt)
  | TSwitch of (Data.clt)
  | TString of (string * Data.clt)
  | TStrict of (Data.clt)
  | TStatement
  | TSizeof of (Data.clt)
  | TShOp of (Ast_cocci.arithOp * Data.clt)
  | TScriptData of (string)
  | TScript
  | TRuleName of (string)
  | TRightIso
  | TReverse
  | TReturn of (Data.clt)
  | TPure
  | TPtrOp of (Data.clt)
  | TPtVirg of (Data.clt)
  | TPragma of (string)
  | TPosition
  | TPosAny
  | TPlusFile of (string * Data.clt)
  | TPlus0
  | TPlus of (Data.clt)
  | TPathIsoFile of (string)
  | TParameter
  | TPOEllipsis of (Data.clt)
  | TPCEllipsis of (Data.clt)
  | TPArob
  | TOrLog of (Data.clt)
  | TOr of (Data.clt)
  | TOn
  | TOPar0 of (Data.clt)
  | TOPar of (Data.clt)
  | TOInit of (Data.clt)
  | TOEllipsis of (Data.clt)
  | TOCro of (Data.clt)
  | TOBrace of (Data.clt)
  | TNothing
  | TNotEq of (Data.clt)
  | TNever
  | TName
  | TMul of (Data.clt)
  | TMinusFile of (string * Data.clt)
  | TMinus of (Data.clt)
  | TMid0 of (Data.clt)
  | TMetaType of (Parse_aux.info)
  | TMetaStmList of (Parse_aux.info)
  | TMetaStm of (Parse_aux.info)
  | TMetaPos of (Parse_aux.pos_info)
  | TMetaParamList of (Parse_aux.list_info)
  | TMetaParam of (Parse_aux.info)
  | TMetaLocalIdExp of (Parse_aux.typed_info)
  | TMetaLocalFunc of (Parse_aux.idinfo)
  | TMetaIterator of (Parse_aux.idinfo)
  | TMetaIdExp of (Parse_aux.typed_info)
  | TMetaId of (Parse_aux.idinfo)
  | TMetaFunc of (Parse_aux.idinfo)
  | TMetaExpList of (Parse_aux.list_info)
  | TMetaExp of (Parse_aux.typed_info)
  | TMetaErr of (Parse_aux.expinfo)
  | TMetaDeclarer of (Parse_aux.idinfo)
  | TMetaConst of (Parse_aux.typed_info)
  | TMPtVirg
  | TLogOp of (Ast_cocci.logicalOp * Data.clt)
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
  | TIncludeNL of (string * Data.clt)
  | TIncludeL of (string * Data.clt)
  | TInc of (Data.clt)
  | TIf of (Data.clt)
  | TIdentifier
  | TIdent of (string * Data.clt)
  | TIdExpression
  | TGoto of (Data.clt)
  | TFunction
  | TFunDecl of (Data.clt)
  | TFresh
  | TForall
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
  | TDmOp of (Ast_cocci.arithOp * Data.clt)
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


val script_meta_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string * (string * string))
val rule_name: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast_cocci.rulename)
val reinit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
val plus_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val plus_exp_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val never_used: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
val minus_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val minus_exp_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val meta_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Ast_cocci.metavar,Ast_cocci.metavar) Common.either list)
val iso_rule_name: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast_cocci.rulename)
val iso_meta_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Ast_cocci.metavar,Ast_cocci.metavar) Common.either list)
val iso_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.anything list list)
val include_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((string,string) Common.either list)