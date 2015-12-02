exception Error

type token = 
  | Tvolatile of (Data.clt)
  | Tvoid of (Data.clt)
  | Tunsigned of (Data.clt)
  | Tunion of (Data.clt)
  | Ttypedef of (Data.clt)
  | Tstruct of (Data.clt)
  | Tstatic of (Data.clt)
  | Tssize_t of (Data.clt)
  | Tsize_t of (Data.clt)
  | Tsigned of (Data.clt)
  | Tshort of (Data.clt)
  | Tregister of (Data.clt)
  | Tptrdiff_t of (Data.clt)
  | Tlong of (Data.clt)
  | Tlist
  | Tint of (Data.clt)
  | Tinline of (Data.clt)
  | Tfloat of (Data.clt)
  | Textern of (Data.clt)
  | Texec of (Data.clt)
  | Tenum of (Data.clt)
  | Tdouble of (Data.clt)
  | Tdecimal of (Data.clt)
  | Tconst of (Data.clt)
  | Tchar of (Data.clt)
  | Tauto of (Data.clt)
  | Tattr of (string * Data.clt)
  | TXor of (Data.clt)
  | TWords
  | TWhy0
  | TWhy of (Data.clt)
  | TWhitespace of (string)
  | TWhile of (Data.clt)
  | TWhenTrue of (Data.clt)
  | TWhenFalse of (Data.clt)
  | TWhen of (Data.clt)
  | TVirtual
  | TVAEllipsis of (Data.clt)
  | TUsing
  | TUnderscore
  | TUndef of (Data.clt * token)
  | TTypedef
  | TTypeId of (string * Data.clt)
  | TType
  | TTildeExclEq of (Data.clt)
  | TTildeEq of (Data.clt)
  | TTilde of (Data.clt)
  | TSymbol
  | TSymId of (string * Data.clt)
  | TSwitch of (Data.clt)
  | TSub of (Data.clt)
  | TString of (string * Data.clt)
  | TStrict of (Data.clt)
  | TStatement
  | TSizeof of (Data.clt)
  | TShROp of (Ast_cocci.arithOp * Data.clt)
  | TShLOp of (Ast_cocci.arithOp * Data.clt)
  | TScriptData of (string)
  | TScript
  | TRuleName of (string)
  | TRightIso
  | TReturn of (Data.clt)
  | TPure
  | TPtrOp of (Data.clt)
  | TPtVirg of (Data.clt)
  | TPragma of (Data.clt)
  | TPosition
  | TPosAny
  | TPlusFile of (string * Data.clt)
  | TPlus0
  | TPlus of (Data.clt)
  | TPathIsoFile of (string)
  | TParameter
  | TPOEllipsis of (Data.clt)
  | TPCEllipsis of (Data.clt)
  | TPArob of (Data.clt)
  | TOrLog of (Data.clt)
  | TOr of (Data.clt)
  | TOperator
  | TOpAssign of (Ast_cocci.arithOp * Data.clt)
  | TOn
  | TOPar0 of (string * Data.clt)
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
  | TMid0 of (string * Data.clt)
  | TMetavariable
  | TMetaType of (Parse_aux.info)
  | TMetaStmList of (Parse_aux.info)
  | TMetaStm of (Parse_aux.info)
  | TMetaPos of (Parse_aux.pos_info)
  | TMetaParamList of (Parse_aux.list_info)
  | TMetaParam of (Parse_aux.info)
  | TMetaLocalIdExp of (Parse_aux.typed_expinfo)
  | TMetaLocalFunc of (Parse_aux.idinfo)
  | TMetaIterator of (Parse_aux.idinfo)
  | TMetaInitList of (Parse_aux.list_info)
  | TMetaInit of (Parse_aux.info)
  | TMetaIdExp of (Parse_aux.typed_expinfo)
  | TMetaId of (Parse_aux.midinfo)
  | TMetaGlobalIdExp of (Parse_aux.typed_expinfo)
  | TMetaFunc of (Parse_aux.idinfo)
  | TMetaFieldList of (Parse_aux.list_info)
  | TMetaField of (Parse_aux.info)
  | TMetaExpList of (Parse_aux.list_info)
  | TMetaExp of (Parse_aux.typed_expinfo)
  | TMetaErr of (Parse_aux.expinfo)
  | TMetaDeclarer of (Parse_aux.idinfo)
  | TMetaDecl of (Parse_aux.info)
  | TMetaConst of (Parse_aux.typed_expinfo)
  | TMetaBinaryOp of (Parse_aux.binaryOpinfo)
  | TMetaAssignOp of (Parse_aux.assignOpinfo)
  | TMeta of (Parse_aux.info)
  | TMPtVirg
  | TLogOp of (Ast_cocci.logicalOp * Data.clt)
  | TLocal
  | TLineEnd of (Data.clt)
  | TIteratorId of (string * Data.clt)
  | TIterator
  | TIsoType
  | TIsoTopLevel
  | TIsoToTestExpression
  | TIsoTestExpression
  | TIsoStatement
  | TIsoExpression
  | TIsoDeclaration
  | TIsoArgExpression
  | TIso
  | TInvalid
  | TInt of (string * Data.clt)
  | TInitialize
  | TInitialiser
  | TIncludeNL of (string * Data.clt)
  | TIncludeL of (string * Data.clt)
  | TInc of (Data.clt)
  | TIf of (Data.clt)
  | TIdentifier
  | TIdent of (string * Data.clt)
  | TIdExpression
  | TGoto of (Data.clt)
  | TGlobal
  | TGenerated
  | TFunction
  | TFunDecl of (Data.clt)
  | TFresh
  | TFormat
  | TForall
  | TFor of (Data.clt)
  | TFloat of (string * Data.clt)
  | TFinalize
  | TField
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
  | TDirective of (Ast_cocci.added_string * Data.clt)
  | TDepends
  | TDefineParam of (Data.clt * token * int * int)
  | TDefine of (Data.clt * token)
  | TDefault of (Data.clt)
  | TDeclarerId of (string * Data.clt)
  | TDeclarer
  | TDeclaration
  | TDecimalCst of (string * string (*n*) * string (*p*) * Data.clt)
  | TDec of (Data.clt)
  | TCppConcatOp
  | TContinue of (Data.clt)
  | TContext
  | TConstant
  | TComma of (Data.clt)
  | TChar of (string * Data.clt)
  | TCase of (Data.clt)
  | TCPar0 of (string * Data.clt)
  | TCPar of (Data.clt)
  | TCEllipsis of (Data.clt)
  | TCCro of (Data.clt)
  | TCBrace of (Data.clt)
  | TBreak of (Data.clt)
  | TBinary
  | TBang of (Data.clt)
  | TAttribute
  | TAssignment
  | TArobArob
  | TArob
  | TAny of (Data.clt)
  | TAndLog of (Data.clt)
  | TAnd0 of (string * Data.clt)
  | TAnd of (Data.clt)
  | TAnalysis
  | EOF


val script_meta_virt_nofresh_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((string option (*string*) * string option (*ast*)) * (Ast_cocci.meta_name * Ast_cocci.metavar) option * Ast_cocci.mvinit)
val script_meta_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((string option (*string*) * string option (*ast*)) * (Ast_cocci.meta_name * Ast_cocci.metavar) option * Ast_cocci.mvinit)
val rule_name: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast_cocci.rulename)
val plus_ty_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val plus_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val plus_id_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val plus_exp_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val never_used: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
val minus_ty_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val minus_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val minus_id_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val minus_exp_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.rule)
val meta_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Ast_cocci.metavar,Ast_cocci.metavar) Common.either list)
val iso_rule_name: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast_cocci.rulename)
val iso_meta_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Ast_cocci.metavar,Ast_cocci.metavar) Common.either list)
val iso_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast0_cocci.anything list list)
val include_main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Data.incl_iso list)