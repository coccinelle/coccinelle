(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* splits the entire file into minus and plus fragments, and parses each
separately (thus duplicating work for the parsing of the context elements) *)

exception SMPLParseError of string

let smplparseerror s = raise (SMPLParseError s)

module D = Data
module PC = Parser_cocci_menhir
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module Ast = Ast_cocci
module Ast0 = Ast0_cocci

exception Bad_virt of string

let pr = Printf.sprintf
(*let pr2 s = prerr_string s; prerr_string "\n"; flush stderr*)
let pr2 s = Printf.printf "%s\n" s

(* for isomorphisms.  all should be at the front!!! *)
let reserved_names =
  ["all";"optional_storage";"optional_qualifier";"value_format";"comm_assoc";
    "optional_attributes";"prototypes"]

(* ----------------------------------------------------------------------- *)
(* Debugging... *)

let line_type (d,_,_,_,_,_,_,_,_,_) = d

let line_type2c tok =
  match line_type tok with
    D.MINUS | D.OPTMINUS -> ":-"
  | D.PLUS -> ":+"
  | D.PLUSPLUS -> ":++"
  | D.CONTEXT | D.OPT -> ""

let real_line (_,d,_,_,_,_,_,_,_,_) = d
let log_line  (_,_,d,_,_,_,_,_,_,_) = d

let token2c (tok,_) add_clt =
  let prkey s1 s2 = if add_clt then s1^s2 else s2 in
  let add_clt str clt =
    if add_clt
    then
      Printf.sprintf "%s:%s:%d:%d" str (line_type2c clt)
	(real_line clt) (log_line clt)
    else str in
 match tok with
    PC.TMetavariable -> "metavariable"
  | PC.TIdentifier -> "identifier"
  | PC.TType -> "type"
  | PC.TParameter -> "parameter"
  | PC.TConstant -> "constant"
  | PC.TExpression -> "expression"
  | PC.TIdExpression -> "idexpression"
  | PC.TOperator -> "operator"
  | PC.TBinary -> "binary"
  | PC.TAssignment -> "assignment"
  | PC.TInitialiser -> "initialiser"
  | PC.TSymbol -> "symbol"
  | PC.TDeclaration -> "declaration"
  | PC.TField -> "field"
  | PC.TStatement -> "statement"
  | PC.TPosition -> "position"
  | PC.TFormat -> "format"
  | PC.TAnalysis -> "analysis"
  | PC.TPosAny -> "any"
  | PC.TFunction -> "function"
  | PC.TLocal -> "local"
  | PC.TGlobal -> "global"
  | PC.Tlist -> "list"
  | PC.TFresh -> "fresh"
  | PC.TCppConcatOp -> "##"
  | PC.TPure -> "pure"
  | PC.TContext -> "context"
  | PC.TTypedef -> "typedef"
  | PC.TAttribute -> "attribute"
  | PC.TDeclarer -> "declarer"
  | PC.TIterator -> "iterator"
  | PC.TName -> "name"
  | PC.TRuleName str -> "rule_name-"^str
  | PC.TUsing -> "using"
  | PC.TVirtual -> "virtual"
  | PC.TMerge -> "merge"
  | PC.TPathIsoFile str -> "path_iso_file-"^str
  | PC.TDisable -> "disable"
  | PC.TExtends -> "extends"
  | PC.TDepends -> "depends"
  | PC.TOn -> "on"
  | PC.TEver -> "ever"
  | PC.TNever -> "never"
  | PC.TExists -> "exists"
  | PC.TFile -> "file"
  | PC.TIn -> "in"
  | PC.TForall -> "forall"
  | PC.TError -> "error"
  | PC.TWords -> "words"
  | PC.TGenerated -> "generated"

  | PC.TNothing -> "nothing"

  | PC.Tchar(clt) -> add_clt "char"  clt
  | PC.Tshort(clt) -> add_clt "short" clt
  | PC.Tint(clt) -> add_clt "int" clt
  | PC.Tdouble(clt) -> add_clt "double" clt
  | PC.Tfloat(clt) -> add_clt "float" clt
  | PC.Tlong(clt) -> add_clt "long" clt
  | PC.Tvoid(clt) -> add_clt "void" clt
  | PC.Tsize_t(clt) -> add_clt "size_t" clt
  | PC.Tssize_t(clt) -> add_clt "ssize_t" clt
  | PC.Tptrdiff_t(clt) -> add_clt "ptrdiff_t" clt
  | PC.Tstruct(clt) -> add_clt "struct" clt
  | PC.Tunion(clt) -> add_clt "union" clt
  | PC.Tenum(clt) -> add_clt "enum" clt
  | PC.Tunsigned(clt) -> add_clt "unsigned" clt
  | PC.Tsigned(clt) -> add_clt "signed" clt
  | PC.Tstatic(clt) -> add_clt "static" clt
  | PC.Tinline(clt) -> add_clt "inline" clt
  | PC.Ttypedef(clt) -> add_clt "typedef" clt
  | PC.Tattr(s,clt) -> add_clt s clt
  | PC.Tauto(clt) -> add_clt "auto" clt
  | PC.Tregister(clt) -> add_clt "register" clt
  | PC.Textern(clt) -> add_clt "extern" clt
  | PC.Tconst(clt) -> add_clt "const" clt
  | PC.Tvolatile(clt) -> add_clt "volatile" clt
  | PC.Tdecimal(clt) -> add_clt "decimal" clt
  | PC.Texec(clt) -> add_clt "exec" clt

  | PC.TDirective(Ast.Noindent s,_) -> s
  | PC.TDirective(Ast.Indent s,_)   -> s
  | PC.TDirective(Ast.Space s,_)   -> s
  | PC.TAttr_(clt) -> add_clt "__attribute__" clt
  | PC.TIncludeL(s,clt) -> add_clt (pr "#include \"%s\"" s) clt
  | PC.TIncludeNL(s,clt) -> add_clt (pr "#include <%s>" s) clt
  | PC.TIncludeAny(s,clt) -> add_clt (pr "#include %s" s) clt
  | PC.TInclude(clt) -> add_clt "#include" clt
  | PC.TUndef(clt,_) -> add_clt "#undef" clt
  | PC.TDefine(clt,_) -> add_clt "#define" clt
  | PC.TDefineParam(clt,_,_,_) -> add_clt "#define_param" clt
  | PC.TPragma(clt) -> add_clt "#pragma" clt
  | PC.TCppEscapedNewline(clt) -> add_clt "\\" clt
  | PC.TMinusFile(s,clt) -> add_clt (pr "--- %s" s) clt
  | PC.TPlusFile(s,clt) -> add_clt (pr "+++ %s" s) clt

  | PC.TInc(clt) -> add_clt "++" clt
  | PC.TDec(clt) -> add_clt "--" clt

  | PC.TIf(clt) -> add_clt "if" clt
  | PC.TElse(clt) -> add_clt "else" clt
  | PC.TWhile(clt) -> add_clt "while" clt
  | PC.TFor(clt) -> add_clt "for" clt
  | PC.TDo(clt) -> add_clt "do" clt
  | PC.TSwitch(clt) -> add_clt "switch" clt
  | PC.TCase(clt) -> add_clt "case" clt
  | PC.TDefault(clt) -> add_clt "default" clt
  | PC.TReturn(clt) -> add_clt "return" clt
  | PC.TBreak(clt) -> add_clt "break" clt
  | PC.TContinue(clt) -> add_clt "continue" clt
  | PC.TGoto(clt) -> add_clt "goto" clt
  | PC.TIdent(s,clt) -> add_clt (prkey "ident-" s) clt
  | PC.TTypeId(s,clt) -> add_clt (prkey "typename-" s) clt
  | PC.TDeclarerId(s,clt) -> add_clt (prkey "declarername-" s) clt
  | PC.TIteratorId(s,clt) -> add_clt (prkey "iteratorname-" s) clt
  | PC.TSymId(s,clt)      -> add_clt (prkey "symbol-" s) clt
  | PC.TMetaDeclarer(_,_,_,clt) -> add_clt "declmeta" clt
  | PC.TMetaIterator(_,_,_,clt) -> add_clt "itermeta" clt

  | PC.TSizeof(clt) -> add_clt "sizeof" clt
  | PC.TTypeof(clt) -> add_clt "typeof" clt

  | PC.TString(x,clt) -> add_clt (Printf.sprintf "\"%s\"" x) clt
  | PC.TChar(x,clt) -> add_clt x clt
  | PC.TFloat(x,clt) -> add_clt x clt
  | PC.TInt(x,clt) -> add_clt x clt
  | PC.TDecimalCst(x,len,prc,clt) -> add_clt x clt

  | PC.TOrLog(clt) -> add_clt "||" clt
  | PC.TAndLog(clt) -> add_clt "&&" clt
  | PC.TOr(clt) -> add_clt "|" clt
  | PC.TXor(clt) -> add_clt "^" clt
  | PC.TAnd (clt) -> add_clt "&" clt
  | PC.TEqEq(clt) -> add_clt "==" clt
  | PC.TNotEq(clt) -> add_clt "!=" clt
  | PC.TSub(clt) -> add_clt "<=" clt
  | PC.TTildeEq(clt) -> add_clt "~=" clt
  | PC.TTildeExclEq(clt) -> add_clt "~!=" clt
  | PC.TLogOp(op,clt) ->
      add_clt
	(match op with
	  Ast.Inf -> "<"
	| Ast.InfEq -> "<="
	| Ast.Sup -> ">"
	| Ast.SupEq -> ">="
	| _ -> failwith "not possible")
	clt
  | PC.TShLOp(op,clt) -> add_clt "<<" clt
  | PC.TShROp(op,clt) -> add_clt ">>" clt
  | PC.TPlus(clt) -> add_clt "+" clt
  | PC.TMinus(clt) -> add_clt "-" clt
  | PC.TMul(clt) -> add_clt "*" clt
  | PC.TDmOp(op,clt) ->
      add_clt
	(match op with
	  Ast.Div -> "/"
	| Ast.Min -> "<?"
	| Ast.Max -> ">?"
	| Ast.Mod -> "%"
	|_ -> failwith "not possible")
	clt
  | PC.TTilde (clt) -> add_clt "~" clt

  | PC.TMeta(_,_,_,clt) -> add_clt "meta" clt
  | PC.TMetaAssignOp(_,_,_,clt) -> add_clt "metaassignop" clt
  | PC.TMetaBinaryOp(_,_,_,clt) -> add_clt "metabinaryop" clt
  | PC.TMetaParam(_,_,_,clt) -> add_clt "parammeta" clt
  | PC.TMetaParamList(_,_,_,_,clt) -> add_clt "paramlistmeta" clt
  | PC.TMetaConst(_,_,_,_,clt) -> add_clt "constmeta" clt
  | PC.TMetaErr(_,_,_,clt) -> add_clt "errmeta" clt
  | PC.TMetaExp(_,_,_,_,clt,_) -> add_clt "expmeta" clt
  | PC.TMetaIdExp(_,_,_,_,clt) -> add_clt "idexpmeta" clt
  | PC.TMetaLocalIdExp(_,_,_,_,clt) -> add_clt "localidexpmeta" clt
  | PC.TMetaGlobalIdExp(_,_,_,_,clt) -> add_clt "globalidexpmeta" clt
  | PC.TMetaExpList(_,_,_,_,clt) -> add_clt "explistmeta" clt
  | PC.TMetaId(nm,_,_,_,clt)    -> "idmeta-"^add_clt (Dumper.dump nm) clt
  | PC.TMetaType(_,_,_,clt)    -> add_clt "typemeta" clt
  | PC.TMetaInit(_,_,_,clt)    -> add_clt "initmeta" clt
  | PC.TMetaInitList(_,_,_,_,clt)    -> add_clt "initlistmeta" clt
  | PC.TMetaDecl(_,_,_,clt)    -> add_clt "declmeta" clt
  | PC.TMetaField(_,_,_,clt)   -> add_clt "fieldmeta" clt
  | PC.TMetaFieldList(_,_,_,_,clt)   -> add_clt "fieldlistmeta" clt
  | PC.TMetaStm(_,_,_,clt)     -> add_clt "stmmeta" clt
  | PC.TMetaStmList(_,_,_,_,clt) -> add_clt "stmlistmeta" clt
  | PC.TMetaDParamList(_,_,_,_,clt) -> add_clt "dparamlistmeta" clt
  | PC.TMetaFunc(_,_,_,clt)  -> add_clt "funcmeta" clt
  | PC.TMetaLocalFunc(_,_,_,clt) -> add_clt "funcmeta" clt
  | PC.TMetaPos(_,_,_,clt)   -> "posmeta"
  | PC.TMPtVirg -> ";"
  | PC.TArobArob -> "@@"
  | PC.TArob -> "@"
  | PC.TPArob clt -> "P@"
  | PC.TScript _ -> "script"
  | PC.TInitialize -> "initialize"
  | PC.TFinalize -> "finalize"

  | PC.TWhen(clt) -> add_clt "WHEN" clt
  | PC.TWhenTrue(clt) -> add_clt "WHEN TRUE" clt
  | PC.TWhenFalse(clt) -> add_clt "WHEN FALSE" clt
  | PC.TAny(clt) -> add_clt "ANY" clt
  | PC.TStrict(clt) -> add_clt "STRICT" clt
  | PC.TEllipsis(clt) -> add_clt "..." clt
  | PC.TVAEllipsis(clt) -> add_clt "......" clt

  | PC.TOEllipsis(clt) -> add_clt "<..." clt
  | PC.TCEllipsis(clt) -> add_clt "...>" clt
  | PC.TPOEllipsis(clt) -> add_clt "<+..." clt
  | PC.TPCEllipsis(clt) -> add_clt "...+>" clt
  | PC.TPlus0 -> "+"
  | PC.TWhy0  -> "?"

  | PC.TWhy(clt)   -> add_clt "?" clt
  | PC.TDotDot(clt)-> add_clt ":" clt
  | PC.TBang(clt)  -> add_clt "!" clt
  | PC.TOPar(clt)  -> add_clt "paren (" clt
  | PC.TOPar0(s,clt) -> add_clt "paren0 (" clt
  | PC.TMid0(s,clt)  -> add_clt s clt
  | PC.TAnd0(s,clt)  -> add_clt s clt
  | PC.TCPar(clt)  -> add_clt "paren )" clt
  | PC.TCPar0(s,clt) -> add_clt "paren0 )" clt

  | PC.TOBrace(clt) -> add_clt "{" clt
  | PC.TCBrace(clt) -> add_clt "}" clt
  | PC.TOCro(clt) -> add_clt "[" clt
  | PC.TCCro(clt) -> add_clt "]" clt
  | PC.TOInit(clt) -> add_clt "{" clt

  | PC.TPtrOp(clt) -> add_clt "->" clt

  | PC.TEq(clt) -> add_clt "=" clt
  | PC.TOpAssign(_,clt) -> add_clt "=op" clt
  | PC.TDot(clt) -> add_clt "." clt
  | PC.TComma(clt) -> add_clt "," clt
  | PC.TPtVirg(clt) -> add_clt ";" clt

  | PC.EOF -> "eof"
  | PC.TLineEnd(clt) -> "line end"
  | PC.TInvalid -> "invalid"
  | PC.TFunDecl(clt) -> "fundecl"

  | PC.TIso -> "<=>"
  | PC.TRightIso -> "=>"
  | PC.TIsoTopLevel -> "TopLevel"
  | PC.TIsoExpression -> "Expression"
  | PC.TIsoArgExpression -> "ArgExpression"
  | PC.TIsoTestExpression -> "TestExpression"
  | PC.TIsoToTestExpression -> "ToTestExpression"
  | PC.TIsoStatement -> "Statement"
  | PC.TIsoDeclaration -> "Declaration"
  | PC.TIsoType -> "Type"
  | PC.TUnderscore -> "_"
  | PC.TScriptData s -> s
  | PC.TWhitespace s -> "Whitespace(" ^ s ^ ")"

let print_tokens s tokens =
  Printf.printf "%s\n" s;
  List.iter (function x -> Printf.printf "|%s| " (token2c x true)) tokens;
  Printf.printf "\n\n";
  flush stdout

type plus = PLUS | NOTPLUS | SKIP

(* skip means ignore completely, notplus means keep in the recursion, but don't
attach to it, plus means that it is possible to attach to the token *)

let plus_attachable only_plus (tok,_) =
  match tok with
    PC.Tchar(clt) | PC.Tshort(clt) | PC.Tint(clt) | PC.Tdouble(clt)
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt)
  | PC.Tsize_t(clt) | PC.Tssize_t(clt) | PC.Tptrdiff_t(clt)
  | PC.Tstruct(clt)
  | PC.Tunion(clt) | PC.Tenum(clt) | PC.Tunsigned(clt) | PC.Tsigned(clt)
  | PC.Tdecimal(clt) | PC.Texec(clt) | PC.Tstatic(clt)
  | PC.Tinline(clt) | PC.Ttypedef(clt) | PC.Tattr(_,clt)
  | PC.Tauto(clt) | PC.Tregister(clt)
  | PC.Textern(clt) | PC.Tconst(clt) | PC.Tvolatile(clt)

  | PC.TIncludeL(_,clt) | PC.TIncludeNL(_,clt) | PC.TIncludeAny(_,clt)
  | PC.TInclude(clt)
  | PC.TUndef(clt,_) | PC.TDefine(clt,_) | PC.TPragma(clt)
  | PC.TDefineParam(clt,_,_,_) | PC.TCppEscapedNewline(clt)
  | PC.TMinusFile(_,clt) | PC.TPlusFile(_,clt)

  | PC.TInc(clt) | PC.TDec(clt)

  | PC.TIf(clt) | PC.TElse(clt) | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt)
  | PC.TSwitch(clt) | PC.TCase(clt) | PC.TDefault(clt) | PC.TReturn(clt)
  | PC.TBreak(clt) | PC.TContinue(clt) | PC.TGoto(clt) | PC.TIdent(_,clt)
  | PC.TSymId(_,clt)
  | PC.TTypeId(_,clt) | PC.TDeclarerId(_,clt) | PC.TIteratorId(_,clt)

  | PC.TSizeof(clt) | PC.TTypeof(clt)

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt)
  | PC.TDecimalCst(_,_,_,clt)

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TTildeEq(clt)
  | PC.TLogOp(_,clt)
  | PC.TShLOp(_,clt) | PC.TShROp(_,clt)
  | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt)
  | PC.TDmOp(_,clt) | PC.TTilde (clt)

  | PC.TMeta(_,_,_,clt) | PC.TMetaParam(_,_,_,clt)
  | PC.TMetaParamList(_,_,_,_,clt)
  | PC.TMetaConst(_,_,_,_,clt) | PC.TMetaErr(_,_,_,clt)
  | PC.TMetaExp(_,_,_,_,clt,_) | PC.TMetaIdExp(_,_,_,_,clt)
  | PC.TMetaLocalIdExp(_,_,_,_,clt) | PC.TMetaGlobalIdExp(_,_,_,_,clt)
  | PC.TMetaAssignOp(_,_,_,clt) | PC.TMetaBinaryOp(_,_,_,clt)
  | PC.TMetaExpList(_,_,_,_,clt)
  | PC.TMetaId(_,_,_,_,clt)
  | PC.TMetaType(_,_,_,clt) | PC.TMetaInit(_,_,_,clt)
  | PC.TMetaInitList(_,_,_,_,clt)
  | PC.TMetaStm(_,_,_,clt) | PC.TMetaStmList(_,_,_,_,clt)
  | PC.TMetaDParamList(_,_,_,_,clt)
  | PC.TMetaDecl(_,_,_,clt) | PC.TMetaField(_,_,_,clt)
  | PC.TMetaFieldList(_,_,_,_,clt)
  | PC.TMetaFunc(_,_,_,clt) | PC.TMetaLocalFunc(_,_,_,clt)

(* it would seem that this should all be skips
  | PC.TWhen(clt) |  PC.TWhenTrue(clt) |  PC.TWhenFalse(clt)
  | PC.TAny(clt) | PC.TStrict(clt) | PC.TEllipsis(clt)
  | PC.TOEllipsis(clt) | PC.TCEllipsis(clt)
  | PC.TPOEllipsis(clt) | PC.TPCEllipsis(clt)
*)

  | PC.TWhy(clt) | PC.TDotDot(clt) | PC.TBang(clt) | PC.TOPar(clt)
  | PC.TCPar(clt)

  | PC.TOBrace(clt) | PC.TCBrace(clt) | PC.TOCro(clt) | PC.TCCro(clt)
  | PC.TOInit(clt)

  | PC.TPtrOp(clt)

  | PC.TEq(clt) | PC.TOpAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt)
  | PC.TPtVirg(clt) ->
      if List.mem (line_type clt) [D.PLUS;D.PLUSPLUS]
      then PLUS
      else if only_plus then NOTPLUS
      else if line_type clt = D.CONTEXT then PLUS else NOTPLUS

  | PC.TOPar0(s,clt) | PC.TMid0(s,clt) | PC.TAnd0(s,clt)
  | PC.TCPar0(s,clt) -> NOTPLUS
  | PC.TMetaPos(nm,_,_,_) -> NOTPLUS
  | PC.TSub(clt) -> NOTPLUS
  | PC.TDirective(_,clt) -> NOTPLUS
  | PC.TAttr_(clt) -> NOTPLUS

  | _ -> SKIP

exception NoClt of string

let get_clt (tok,_) =
  match tok with
    PC.Tchar(clt) | PC.Tshort(clt) | PC.Tint(clt) | PC.Tdouble(clt)
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt)
  | PC.Tsize_t(clt) | PC.Tssize_t(clt) | PC.Tptrdiff_t(clt)
  | PC.Tstruct(clt)
  | PC.Tunion(clt) | PC.Tenum(clt) | PC.Tunsigned(clt) | PC.Tsigned(clt)
  | PC.Tdecimal(clt) | PC.Texec(clt) | PC.Tstatic(clt) | PC.Ttypedef(clt)
  | PC.Tinline(clt) | PC.Tattr(_,clt) | PC.Tauto(clt) | PC.Tregister(clt)
  | PC.Textern(clt) | PC.Tconst(clt) | PC.Tvolatile(clt)

  | PC.TIncludeL(_,clt) | PC.TIncludeNL(_,clt) | PC.TIncludeAny(_,clt)
  | PC.TInclude(clt)
  | PC.TUndef(clt,_) | PC.TDefine(clt,_) | PC.TPragma(clt)
  | PC.TDefineParam(clt,_,_,_) | PC.TCppEscapedNewline(clt)
  | PC.TMinusFile(_,clt) | PC.TPlusFile(_,clt)

  | PC.TInc(clt) | PC.TDec(clt)

  | PC.TIf(clt) | PC.TElse(clt) | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt)
  | PC.TSwitch(clt) | PC.TCase(clt) | PC.TDefault(clt) | PC.TReturn(clt)
  | PC.TBreak(clt) | PC.TContinue(clt) | PC.TGoto(clt) | PC.TIdent(_,clt)
  | PC.TTypeId(_,clt) | PC.TSymId(_,clt)
  | PC.TDeclarerId(_,clt) | PC.TIteratorId(_,clt)

  | PC.TSizeof(clt) | PC.TTypeof(clt)

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt)
  | PC.TDecimalCst(_,_,_,clt)

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TTildeEq(clt)
  | PC.TSub(clt) | PC.TLogOp(_,clt)
  | PC.TShLOp(_,clt) | PC.TShROp(_,clt)
  | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt)
  | PC.TDmOp(_,clt) | PC.TTilde (clt) | PC.TTildeExclEq(clt)

  | PC.TMeta(_,_,_,clt) | PC.TMetaParam(_,_,_,clt)
  | PC.TMetaParamList(_,_,_,_,clt)
  | PC.TMetaConst(_,_,_,_,clt) | PC.TMetaErr(_,_,_,clt)
  | PC.TMetaExp(_,_,_,_,clt,_) | PC.TMetaIdExp(_,_,_,_,clt)
  | PC.TMetaLocalIdExp(_,_,_,_,clt) | PC.TMetaGlobalIdExp(_,_,_,_,clt)
  | PC.TMetaAssignOp(_,_,_,clt) | PC.TMetaBinaryOp(_,_,_,clt)
  | PC.TMetaExpList(_,_,_,_,clt)
  | PC.TMetaId(_,_,_,_,clt)
  | PC.TMetaType(_,_,_,clt) | PC.TMetaInit(_,_,_,clt)
  | PC.TMetaInitList(_,_,_,_,clt)
  | PC.TMetaStm(_,_,_,clt) | PC.TMetaStmList(_,_,_,_,clt)
  | PC.TMetaDParamList(_,_,_,_,clt)
  | PC.TMetaDecl(_,_,_,clt) | PC.TMetaField(_,_,_,clt)
  | PC.TMetaFieldList(_,_,_,_,clt)
  | PC.TMetaFunc(_,_,_,clt) | PC.TMetaLocalFunc(_,_,_,clt)
  | PC.TMetaPos(_,_,_,clt)
  | PC.TMetaDeclarer(_,_,_,clt) | PC.TMetaIterator(_,_,_,clt)

  | PC.TWhen(clt) | PC.TWhenTrue(clt) | PC.TWhenFalse(clt)
  | PC.TAny(clt) | PC.TStrict(clt) | PC.TEllipsis(clt)

  | PC.TWhy(clt) | PC.TDotDot(clt) | PC.TBang(clt) | PC.TOPar(clt)
  | PC.TCPar(clt)

  | PC.TOBrace(clt) | PC.TCBrace(clt) | PC.TOCro(clt) | PC.TCCro(clt)
  | PC.TOInit(clt)

  | PC.TPtrOp(clt)

  | PC.TEq(clt) | PC.TOpAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt)
  | PC.TPArob(clt) | PC.TPtVirg(clt)

  | PC.TOPar0(_,clt) | PC.TMid0(_,clt) | PC.TAnd0(_,clt) | PC.TCPar0(_,clt)
  | PC.TOEllipsis(clt) | PC.TCEllipsis(clt)
  | PC.TPOEllipsis(clt) | PC.TPCEllipsis(clt)
  | PC.TFunDecl(clt) | PC.TDirective(_,clt) | PC.TAttr_(clt)
  | PC.TLineEnd(clt) -> clt
  | PC.TVAEllipsis(clt) -> clt

  | PC.Tlist -> failwith "No clt attached to token Tlist"
  | PC.TWords -> failwith "No clt attached to token TWords"
  | PC.TWhy0 -> failwith "No clt attached to token TWhy0"
  | PC.TWhitespace _ -> failwith "No clt attached to token TWhitespace"
  | PC.TVirtual -> failwith "No clt attached to token TVirtual"
  | PC.TMerge -> failwith "No clt attached to token TMerge"
  | PC.TUsing -> failwith "No clt attached to token TUsing"
  | PC.TUnderscore -> failwith "No clt attached to token TUnderscore"
  | PC.TTypedef -> failwith "No clt attached to token TTypedef"
  | PC.TType -> failwith "No clt attached to token TType"
  | PC.TSymbol -> failwith "No clt attached to token TSymbol"
  | PC.TStatement -> failwith "No clt attached to token TStatement"
  | PC.TScriptData _ -> failwith "No clt attached to token TScriptData"
  | PC.TScript _ -> failwith "No clt attached to token TScript"
  | PC.TRuleName _ -> failwith "No clt attached to token TRuleName"
  | PC.TRightIso -> failwith "No clt attached to token TRightIso"
  | PC.TPure -> failwith "No clt attached to token TPure"
  | PC.TPosition -> failwith "No clt attached to token TPosition"
  | PC.TPosAny -> failwith "No clt attached to token TPosAny"
  | PC.TPlus0 -> failwith "No clt attached to token TPlus0"
  | PC.TPathIsoFile _ -> failwith "No clt attached to token TPathIsoFile"
  | PC.TParameter -> failwith "No clt attached to token TParameter"
  | PC.TOperator -> failwith "No clt attached to token TOperator"
  | PC.TOn -> failwith "No clt attached to token TOn"
  | PC.TNothing -> failwith "No clt attached to token TNothing"
  | PC.TNever -> failwith "No clt attached to token TNever"
  | PC.TName -> failwith "No clt attached to token TName"
  | PC.TMetavariable -> failwith "No clt attached to token TMetavariable"
  | PC.TMPtVirg -> failwith "No clt attached to token TMPtVirg"
  | PC.TLocal -> failwith "No clt attached to token TLocal"
  | PC.TIterator -> failwith "No clt attached to token TIterator"
  | PC.TIsoType -> failwith "No clt attached to token TIsoType"
  | PC.TIsoTopLevel -> failwith "No clt attached to token TIsoTopLevel"
  | PC.TIsoToTestExpression -> failwith "No clt attached to token TIsoToTestExpression"
  | PC.TIsoTestExpression -> failwith "No clt attached to token TIsoTestExpression"
  | PC.TIsoStatement -> failwith "No clt attached to token TIsoStatement"
  | PC.TIsoExpression -> failwith "No clt attached to token TIsoExpression"
  | PC.TIsoDeclaration -> failwith "No clt attached to token TIsoDeclaration"
  | PC.TIsoArgExpression -> failwith "No clt attached to token TIsoArgExpression"
  | PC.TIso -> failwith "No clt attached to token TIso"
  | PC.TInvalid -> failwith "No clt attached to token TInvalid"
  | PC.TInitialize -> failwith "No clt attached to token TInitialize"
  | PC.TInitialiser -> failwith "No clt attached to token TInitialiser"
  | PC.TIn -> failwith "No clt attached to token TIn"
  | PC.TIdentifier -> failwith "No clt attached to token TIdentifier"
  | PC.TIdExpression -> failwith "No clt attached to token TIdExpression"
  | PC.TGlobal -> failwith "No clt attached to token TGlobal"
  | PC.TGenerated -> failwith "No clt attached to token TGenerated"
  | PC.TFunction -> failwith "No clt attached to token TFunction"
  | PC.TFresh -> failwith "No clt attached to token TFresh"
  | PC.TFormat -> failwith "No clt attached to token TFormat"
  | PC.TForall -> failwith "No clt attached to token TForall"
  | PC.TFinalize -> failwith "No clt attached to token TFinalize"
  | PC.TFile -> failwith "No clt attached to token TFile"
  | PC.TField -> failwith "No clt attached to token TField"
  | PC.TExtends -> failwith "No clt attached to token TExtends"
  | PC.TExpression -> failwith "No clt attached to token TExpression"
  | PC.TExists -> failwith "No clt attached to token TExists"
  | PC.TEver -> failwith "No clt attached to token TEver"
  | PC.TError -> failwith "No clt attached to token TError"
  | PC.TDisable -> failwith "No clt attached to token TDisable"
  | PC.TDepends -> failwith "No clt attached to token TDepends"
  | PC.TDeclarer -> failwith "No clt attached to token TDeclarer"
  | PC.TDeclaration -> failwith "No clt attached to token TDeclaration"
  | PC.TCppConcatOp -> failwith "No clt attached to token TCppConcatOp"
  | PC.TContext -> failwith "No clt attached to token TContext"
  | PC.TConstant -> failwith "No clt attached to token TConstant"
  | PC.TBinary -> failwith "No clt attached to token TBinary"
  | PC.TAttribute -> failwith "No clt attached to token TAttribute"
  | PC.TAssignment -> failwith "No clt attached to token TAssignment"
  | PC.TArobArob -> failwith "No clt attached to token TArobArob"
  | PC.TArob -> failwith "No clt attached to token TArob"
  | PC.TAnalysis -> failwith "No clt attached to token TAnalysis"
  | PC.EOF -> failwith "No clt attached to token EOF"

let update_clt (tok,x) clt =
  match tok with
    PC.Tchar(_) -> (PC.Tchar(clt),x)
  | PC.Tshort(_) -> (PC.Tshort(clt),x)
  | PC.Tint(_) -> (PC.Tint(clt),x)
  | PC.Tdouble(_) -> (PC.Tdouble(clt),x)
  | PC.Tfloat(_) -> (PC.Tfloat(clt),x)
  | PC.Tlong(_) -> (PC.Tlong(clt),x)
  | PC.Tvoid(_) -> (PC.Tvoid(clt),x)
  | PC.Tsize_t(_) -> (PC.Tsize_t(clt),x)
  | PC.Tssize_t(_) -> (PC.Tssize_t(clt),x)
  | PC.Tptrdiff_t(_) -> (PC.Tptrdiff_t(clt),x)
  | PC.Tstruct(_) -> (PC.Tstruct(clt),x)
  | PC.Tunion(_) -> (PC.Tunion(clt),x)
  | PC.Tenum(_) -> (PC.Tenum(clt),x)
  | PC.Tdecimal(_) -> (PC.Tdecimal(clt),x)
  | PC.Texec(_) -> (PC.Texec(clt),x)
  | PC.Tunsigned(_) -> (PC.Tunsigned(clt),x)
  | PC.Tsigned(_) -> (PC.Tsigned(clt),x)
  | PC.Tstatic(_) -> (PC.Tstatic(clt),x)
  | PC.Tinline(_) -> (PC.Tinline(clt),x)
  | PC.Ttypedef(_) -> (PC.Ttypedef(clt),x)
  | PC.Tattr(s,_) -> (PC.Tattr(s,clt),x)
  | PC.Tauto(_) -> (PC.Tauto(clt),x)
  | PC.Tregister(_) -> (PC.Tregister(clt),x)
  | PC.Textern(_) -> (PC.Textern(clt),x)
  | PC.Tconst(_) -> (PC.Tconst(clt),x)
  | PC.Tvolatile(_) -> (PC.Tvolatile(clt),x)

  | PC.TIncludeL(s,_) -> (PC.TIncludeL(s,clt),x)
  | PC.TIncludeNL(s,_) -> (PC.TIncludeNL(s,clt),x)
  | PC.TIncludeAny(s,_) -> (PC.TIncludeAny(s,clt),x)
  | PC.TInclude(_) -> (PC.TInclude(clt),x)
  | PC.TUndef(_,a) -> (PC.TUndef(clt,a),x)
  | PC.TDefine(_,a) -> (PC.TDefine(clt,a),x)
  | PC.TDefineParam(_,a,b,c) -> (PC.TDefineParam(clt,a,b,c),x)
  | PC.TPragma(_) -> (PC.TPragma(clt),x)
  | PC.TCppEscapedNewline(_) -> (PC.TCppEscapedNewline(clt),x)
  | PC.TMinusFile(s,_) -> (PC.TMinusFile(s,clt),x)
  | PC.TPlusFile(s,_) -> (PC.TPlusFile(s,clt),x)

  | PC.TInc(_) -> (PC.TInc(clt),x)
  | PC.TDec(_) -> (PC.TDec(clt),x)

  | PC.TIf(_) -> (PC.TIf(clt),x)
  | PC.TElse(_) -> (PC.TElse(clt),x)
  | PC.TWhile(_) -> (PC.TWhile(clt),x)
  | PC.TFor(_) -> (PC.TFor(clt),x)
  | PC.TDo(_) -> (PC.TDo(clt),x)
  | PC.TSwitch(_) -> (PC.TSwitch(clt),x)
  | PC.TCase(_) -> (PC.TCase(clt),x)
  | PC.TDefault(_) -> (PC.TDefault(clt),x)
  | PC.TReturn(_) -> (PC.TReturn(clt),x)
  | PC.TBreak(_) -> (PC.TBreak(clt),x)
  | PC.TContinue(_) -> (PC.TContinue(clt),x)
  | PC.TGoto(_) -> (PC.TGoto(clt),x)
  | PC.TIdent(s,_) -> (PC.TIdent(s,clt),x)
  | PC.TTypeId(s,_) -> (PC.TTypeId(s,clt),x)
  | PC.TDeclarerId(s,_) -> (PC.TDeclarerId(s,clt),x)
  | PC.TIteratorId(s,_) -> (PC.TIteratorId(s,clt),x)
  | PC.TSymId(a,_) -> (PC.TSymId(a,clt),x)

  | PC.TSizeof(_) -> (PC.TSizeof(clt),x)
  | PC.TTypeof(_) -> (PC.TTypeof(clt),x)

  | PC.TString(s,_) -> (PC.TString(s,clt),x)
  | PC.TChar(s,_) -> (PC.TChar(s,clt),x)
  | PC.TFloat(s,_) -> (PC.TFloat(s,clt),x)
  | PC.TInt(s,_) -> (PC.TInt(s,clt),x)
  | PC.TDecimalCst(s,l,p,_) -> (PC.TDecimalCst(s,l,p,clt),x)

  | PC.TOrLog(_) -> (PC.TOrLog(clt),x)
  | PC.TAndLog(_) -> (PC.TAndLog(clt),x)
  | PC.TOr(_) -> (PC.TOr(clt),x)
  | PC.TXor(_) -> (PC.TXor(clt),x)
  | PC.TAnd (_) -> (PC.TAnd (clt),x)
  | PC.TEqEq(_) -> (PC.TEqEq(clt),x)
  | PC.TNotEq(_) -> (PC.TNotEq(clt),x)
  | PC.TTildeEq(_) -> (PC.TTildeEq(clt),x)
  | PC.TSub(_) -> (PC.TSub(clt),x)
  | PC.TLogOp(op,_) -> (PC.TLogOp(op,clt),x)
  | PC.TShLOp(op,_) -> (PC.TShLOp(op,clt),x)
  | PC.TShROp(op,_) -> (PC.TShROp(op,clt),x)
  | PC.TPlus(_) -> (PC.TPlus(clt),x)
  | PC.TMinus(_) -> (PC.TMinus(clt),x)
  | PC.TMul(_) -> (PC.TMul(clt),x)
  | PC.TDmOp(op,_) -> (PC.TDmOp(op,clt),x)
  | PC.TTilde (_) -> (PC.TTilde (clt),x)

  | PC.TMeta(a,b,c,_)      -> (PC.TMeta(a,b,c,clt),x)
  | PC.TMetaParam(a,b,c,_) -> (PC.TMetaParam(a,b,c,clt),x)
  | PC.TMetaParamList(a,b,c,d,_) -> (PC.TMetaParamList(a,b,c,d,clt),x)
  | PC.TMetaConst(a,b,c,d,_) -> (PC.TMetaConst(a,b,c,d,clt),x)
  | PC.TMetaErr(a,b,c,_) -> (PC.TMetaErr(a,b,c,clt),x)
  | PC.TMetaExp(a,b,c,d,_,e) -> (PC.TMetaExp(a,b,c,d,clt,e),x)
  | PC.TMetaIdExp(a,b,c,d,_) -> (PC.TMetaIdExp(a,b,c,d,clt),x)
  | PC.TMetaLocalIdExp(a,b,c,d,_) -> (PC.TMetaLocalIdExp(a,b,c,d,clt),x)
  | PC.TMetaGlobalIdExp(a,b,c,d,_) -> (PC.TMetaGlobalIdExp(a,b,c,d,clt),x)
  | PC.TMetaExpList(a,b,c,d,_) -> (PC.TMetaExpList(a,b,c,d,clt),x)
  | PC.TMetaId(a,b,c,d,_)    -> (PC.TMetaId(a,b,c,d,clt),x)
  | PC.TMetaAssignOp(a,b,c,_)    -> (PC.TMetaAssignOp(a,b,c,clt),x)
  | PC.TMetaBinaryOp(a,b,c,_)    -> (PC.TMetaBinaryOp(a,b,c,clt),x)
  | PC.TMetaType(a,b,c,_)    -> (PC.TMetaType(a,b,c,clt),x)
  | PC.TMetaInit(a,b,c,_)    -> (PC.TMetaInit(a,b,c,clt),x)
  | PC.TMetaInitList(a,b,c,d,_) -> (PC.TMetaInitList(a,b,c,d,clt),x)
  | PC.TMetaDecl(a,b,c,_)    -> (PC.TMetaDecl(a,b,c,clt),x)
  | PC.TMetaField(a,b,c,_)   -> (PC.TMetaField(a,b,c,clt),x)
  | PC.TMetaFieldList(a,b,c,d,_)   -> (PC.TMetaFieldList(a,b,c,d,clt),x)
  | PC.TMetaStm(a,b,c,_)     -> (PC.TMetaStm(a,b,c,clt),x)
  | PC.TMetaStmList(a,b,c,d,_) -> (PC.TMetaStmList(a,b,c,d,clt),x)
  | PC.TMetaDParamList(a,b,c,d,_) -> (PC.TMetaDParamList(a,b,c,d,clt),x)
  | PC.TMetaFunc(a,b,c,_)  -> (PC.TMetaFunc(a,b,c,clt),x)
  | PC.TMetaLocalFunc(a,b,c,_) -> (PC.TMetaLocalFunc(a,b,c,clt),x)

  | PC.TMetaDeclarer(a,b,c,_) -> (PC.TMetaDeclarer(a,b,c,clt),x)
  | PC.TMetaIterator(a,b,c,_) -> (PC.TMetaIterator(a,b,c,clt),x)

  | PC.TWhen(_) -> (PC.TWhen(clt),x)
  | PC.TWhenTrue(_) -> (PC.TWhenTrue(clt),x)
  | PC.TWhenFalse(_) -> (PC.TWhenFalse(clt),x)
  | PC.TAny(_) -> (PC.TAny(clt),x)
  | PC.TStrict(_) -> (PC.TStrict(clt),x)
  | PC.TEllipsis(_) -> (PC.TEllipsis(clt),x)

  | PC.TOEllipsis(_) -> (PC.TOEllipsis(clt),x)
  | PC.TCEllipsis(_) -> (PC.TCEllipsis(clt),x)
  | PC.TPOEllipsis(_) -> (PC.TPOEllipsis(clt),x)
  | PC.TPCEllipsis(_) -> (PC.TPCEllipsis(clt),x)

  | PC.TWhy(_)   -> (PC.TWhy(clt),x)
  | PC.TDotDot(_)   -> (PC.TDotDot(clt),x)
  | PC.TBang(_)  -> (PC.TBang(clt),x)
  | PC.TOPar(_)  -> (PC.TOPar(clt),x)
  | PC.TOPar0(s,_) -> (PC.TOPar0(s,clt),x)
  | PC.TMid0(s,_)  -> (PC.TMid0(s,clt),x)
  | PC.TAnd0(s,_)  -> (PC.TAnd0(s,clt),x)
  | PC.TCPar(_)  -> (PC.TCPar(clt),x)
  | PC.TCPar0(s,_) -> (PC.TCPar0(s,clt),x)

  | PC.TOBrace(_) -> (PC.TOBrace(clt),x)
  | PC.TCBrace(_) -> (PC.TCBrace(clt),x)
  | PC.TOCro(_) -> (PC.TOCro(clt),x)
  | PC.TCCro(_) -> (PC.TCCro(clt),x)
  | PC.TOInit(_) -> (PC.TOInit(clt),x)

  | PC.TPtrOp(_) -> (PC.TPtrOp(clt),x)

  | PC.TEq(_) -> (PC.TEq(clt),x)
  | PC.TOpAssign(s,_) -> (PC.TOpAssign(s,clt),x)
  | PC.TDot(_) -> (PC.TDot(clt),x)
  | PC.TComma(_) -> (PC.TComma(clt),x)
  | PC.TPArob(_) -> (PC.TPArob(clt),x)
  | PC.TPtVirg(_) -> (PC.TPtVirg(clt),x)

  | PC.TLineEnd(_) -> (PC.TLineEnd(clt),x)
  | PC.TFunDecl(_) -> (PC.TFunDecl(clt),x)
  | PC.TTildeExclEq(_) -> (PC.TTildeExclEq(clt),x)
  | PC.TDirective(a,_) -> (PC.TDirective(a,clt),x)
  | PC.TAttr_(_) -> (PC.TAttr_(clt),x)
  | PC.TVAEllipsis(_) -> (PC.TVAEllipsis(clt),x)

  | PC.Tlist -> assert false
  | PC.TWords -> assert false
  | PC.TWhy0 -> assert false
  | PC.TWhitespace _ -> assert false
  | PC.TVirtual -> assert false
  | PC.TMerge -> assert false
  | PC.TUsing -> assert false
  | PC.TUnderscore -> assert false
  | PC.TTypedef -> assert false
  | PC.TType -> assert false
  | PC.TSymbol -> assert false
  | PC.TStatement -> assert false
  | PC.TScriptData _ -> assert false
  | PC.TScript _ -> assert false
  | PC.TRuleName _ -> assert false
  | PC.TRightIso -> assert false
  | PC.TPure -> assert false
  | PC.TPosition -> assert false
  | PC.TPosAny -> assert false
  | PC.TPlus0 -> assert false
  | PC.TPathIsoFile _ -> assert false
  | PC.TParameter -> assert false
  | PC.TOperator -> assert false
  | PC.TOn -> assert false
  | PC.TNothing -> assert false
  | PC.TNever -> assert false
  | PC.TName -> assert false
  | PC.TMetavariable -> assert false
  | PC.TMetaPos _ -> assert false
  | PC.TMPtVirg -> assert false
  | PC.TLocal -> assert false
  | PC.TIterator -> assert false
  | PC.TIsoType -> assert false
  | PC.TIsoTopLevel -> assert false
  | PC.TIsoToTestExpression -> assert false
  | PC.TIsoTestExpression -> assert false
  | PC.TIsoStatement -> assert false
  | PC.TIsoExpression -> assert false
  | PC.TIsoDeclaration -> assert false
  | PC.TIsoArgExpression -> assert false
  | PC.TIso -> assert false
  | PC.TInvalid -> assert false
  | PC.TInitialize -> assert false
  | PC.TInitialiser -> assert false
  | PC.TIn -> assert false
  | PC.TIdentifier -> assert false
  | PC.TIdExpression -> assert false
  | PC.TGlobal -> assert false
  | PC.TGenerated -> assert false
  | PC.TFunction -> assert false
  | PC.TFresh -> assert false
  | PC.TFormat -> assert false
  | PC.TForall -> assert false
  | PC.TFinalize -> assert false
  | PC.TFile -> assert false
  | PC.TField -> assert false
  | PC.TExtends -> assert false
  | PC.TExpression -> assert false
  | PC.TExists -> assert false
  | PC.TEver -> assert false
  | PC.TError -> assert false
  | PC.TDisable -> assert false
  | PC.TDepends -> assert false
  | PC.TDeclarer -> assert false
  | PC.TDeclaration -> assert false
  | PC.TCppConcatOp -> assert false
  | PC.TContext -> assert false
  | PC.TConstant -> assert false
  | PC.TBinary -> assert false
  | PC.TAttribute -> assert false
  | PC.TAssignment -> assert false
  | PC.TArobArob -> assert false
  | PC.TArob -> assert false
  | PC.TAnalysis -> assert false
  | PC.EOF -> assert false

(* ----------------------------------------------------------------------- *)

let make_name prefix ln = Printf.sprintf "%s starting on line %d" prefix ln

(* ----------------------------------------------------------------------- *)
(* Read tokens *)

let wrap_lexbuf_info lexbuf =
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)

let tokens_all_full token table file get_ats lexbuf end_predicate :
    (bool * ((PC.token * (string * (int * int) * (int * int))) list)) =
  try
    let rec aux () =
      let result = token lexbuf in
      let info = (Lexing.lexeme lexbuf,
                  (table.(Lexing.lexeme_start lexbuf)),
                  (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) in
      if result = PC.EOF
      then
	if get_ats
	then
	  smplparseerror
	    "unexpected end of file in a metavariable declaration"
	else (false,[(result,info)])
      else if end_predicate result
      then (true,[(result,info)])
      else
	let (more,rest) = aux() in
	(more,(result, info)::rest)
    in aux ()
  with
    e -> smplparseerror (Common.error_message file (wrap_lexbuf_info lexbuf) )

let in_list list tok =
  List.mem tok list

let tokens_all table file get_ats lexbuf end_markers :
    (bool * ((PC.token * (string * (int * int) * (int * int))) list)) =
  tokens_all_full Lexer_cocci.token table file get_ats lexbuf end_markers

let metavariable_decl_tokens_all table file get_ats lexbuf end_markers :
    (bool * ((PC.token * (string * (int * int) * (int * int))) list)) =
  tokens_all_full Lexer_cocci.metavariable_decl_token
    table file get_ats lexbuf end_markers

let tokens_script_all table file get_ats lexbuf end_markers :
    (bool * ((PC.token * (string * (int * int) * (int * int))) list)) =
  tokens_all_full Lexer_script.token table file get_ats lexbuf end_markers

(* ----------------------------------------------------------------------- *)
(* Split tokens into minus and plus fragments *)

let split t clt =
  let (d,_,_,_,_,_,_,_,_,_) = clt in
  match d with
    D.MINUS | D.OPTMINUS -> ([t],[])
  | D.PLUS | D.PLUSPLUS -> ([],[t])
  | D.CONTEXT | D.OPT -> ([t],[t])

let split_token ((tok,_) as t) =
  match tok with
    PC.TMetavariable | PC.TIdentifier | PC.TOperator
  | PC.TBinary | PC.TAssignment
  | PC.TConstant | PC.TExpression | PC.TIdExpression
  | PC.TDeclaration | PC.TField
  | PC.TStatement | PC.TPosition | PC.TFormat | PC.TAnalysis | PC.TPosAny
  | PC.TInitialiser | PC.TSymbol
  | PC.TFunction | PC.TTypedef | PC.TDeclarer | PC.TIterator | PC.TName
  | PC.TAttribute
  | PC.TType | PC.TParameter | PC.TLocal | PC.TGlobal | PC.Tlist | PC.TFresh
  | PC.TCppConcatOp | PC.TPure
  | PC.TContext | PC.TRuleName(_) | PC.TUsing | PC.TVirtual | PC.TMerge
  | PC.TDisable
  | PC.TExtends | PC.TPathIsoFile(_)
  | PC.TDepends | PC.TOn | PC.TFile | PC.TIn
  | PC.TEver | PC.TNever | PC.TExists | PC.TForall
  | PC.TError | PC.TWords | PC.TGenerated | PC.TNothing -> ([t],[t])

  | PC.Tchar(clt) | PC.Tshort(clt) | PC.Tint(clt) | PC.Tdouble(clt)
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt)
  | PC.Tsize_t(clt) | PC.Tssize_t(clt) | PC.Tptrdiff_t(clt)
  | PC.Tstruct(clt)
  | PC.Tunion(clt) | PC.Tenum(clt) | PC.Tdecimal(clt) | PC.Texec(clt)
  | PC.Tunsigned(clt) | PC.Tsigned(clt)
  | PC.Tstatic(clt) | PC.Tauto(clt) | PC.Tregister(clt) | PC.Textern(clt)
  | PC.Tinline(clt) | PC.Ttypedef(clt) | PC.Tattr(_,clt)
  | PC.TVAEllipsis(clt) | PC.Tconst(clt) | PC.Tvolatile(clt) -> split t clt

  | PC.TDirective(_,_) | PC.TAttr_(_) -> ([],[t]) (* only allowed in + *)
  | PC.TPlusFile(s,clt) | PC.TMinusFile(s,clt)
  | PC.TIncludeL(s,clt) | PC.TIncludeNL(s,clt) | PC.TIncludeAny(s,clt) ->
      split t clt
  | PC.TInclude(clt) -> split t clt
  | PC.TUndef(clt,_) | PC.TDefine(clt,_) | PC.TDefineParam(clt,_,_,_)
  | PC.TCppEscapedNewline(clt) | PC.TPragma(clt) ->
      split t clt

  | PC.TIf(clt) | PC.TElse(clt)  | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt)
  | PC.TSwitch(clt) | PC.TCase(clt) | PC.TDefault(clt)
  | PC.TSizeof(clt) | PC.TTypeof(clt)
  | PC.TReturn(clt) | PC.TBreak(clt) | PC.TContinue(clt) | PC.TGoto(clt)
  | PC.TIdent(_,clt)
  | PC.TTypeId(_,clt) | PC.TDeclarerId(_,clt) | PC.TIteratorId(_,clt)
  | PC.TSymId(_,clt)
  | PC.TMeta(_,_,_,clt) | PC.TMetaConst(_,_,_,_,clt)
  | PC.TMetaExp(_,_,_,_,clt,_)
  | PC.TMetaIdExp(_,_,_,_,clt)
  | PC.TMetaLocalIdExp(_,_,_,_,clt) | PC.TMetaGlobalIdExp(_,_,_,_,clt)
  | PC.TMetaAssignOp(_,_,_,clt) | PC.TMetaBinaryOp(_,_,_,clt)
  | PC.TMetaExpList(_,_,_,_,clt)
  | PC.TMetaParam(_,_,_,clt) | PC.TMetaParamList(_,_,_,_,clt)
  | PC.TMetaId(_,_,_,_,clt) | PC.TMetaType(_,_,_,clt)
  | PC.TMetaInit(_,_,_,clt) | PC.TMetaInitList(_,_,_,_,clt)
  | PC.TMetaDecl(_,_,_,clt) | PC.TMetaField(_,_,_,clt)
  | PC.TMetaFieldList(_,_,_,_,clt)
  | PC.TMetaStm(_,_,_,clt) | PC.TMetaStmList(_,_,_,_,clt)
  | PC.TMetaDParamList(_,_,_,_,clt) | PC.TMetaErr(_,_,_,clt)
  | PC.TMetaFunc(_,_,_,clt) | PC.TMetaLocalFunc(_,_,_,clt)
  | PC.TMetaDeclarer(_,_,_,clt) | PC.TMetaIterator(_,_,_,clt) -> split t clt
  | PC.TMPtVirg | PC.TArob | PC.TArobArob | PC.TScript _
  | PC.TInitialize | PC.TFinalize -> ([t],[t])
  | PC.TPArob clt | PC.TMetaPos(_,_,_,clt) -> split t clt

  | PC.TFunDecl(clt)
  | PC.TWhen(clt) | PC.TWhenTrue(clt) | PC.TWhenFalse(clt)
  | PC.TAny(clt) | PC.TStrict(clt) | PC.TLineEnd(clt)
  | PC.TEllipsis(clt)
  | PC.TOEllipsis(clt) | PC.TCEllipsis(clt)
  | PC.TPOEllipsis(clt) | PC.TPCEllipsis(clt) -> split t clt

  | PC.TPlus0 | PC.TWhy0 ->
      ([t],[t])

  | PC.TWhy(clt)  | PC.TDotDot(clt)
  | PC.TBang(clt) | PC.TOPar(clt) | PC.TOPar0(_,clt)
  | PC.TMid0(_,clt) | PC.TAnd0(_,clt) | PC.TCPar(clt) | PC.TCPar0(_,clt) ->
      split t clt

  | PC.TInc(clt) | PC.TDec(clt) -> split t clt

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt)
  | PC.TDecimalCst(_,_,_,clt) ->
      split t clt

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TTildeEq(clt)
  | PC.TTildeExclEq(clt) | PC.TSub(clt) | PC.TLogOp(_,clt)
  | PC.TShLOp(_,clt) | PC.TShROp(_,clt)
  | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt)
  | PC.TDmOp(_,clt) | PC.TTilde (clt) -> split t clt

  | PC.TOBrace(clt) | PC.TCBrace(clt) | PC.TOInit(clt) -> split t clt
  | PC.TOCro(clt) | PC.TCCro(clt) -> split t clt

  | PC.TPtrOp(clt) -> split t clt

  | PC.TEq(clt) | PC.TOpAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt)
  | PC.TPtVirg(clt) -> split t clt

  | PC.EOF | PC.TInvalid | PC.TUnderscore -> ([t],[t])

  | PC.TIso | PC.TRightIso
  | PC.TIsoExpression | PC.TIsoStatement | PC.TIsoDeclaration | PC.TIsoType
  | PC.TIsoTopLevel | PC.TIsoArgExpression | PC.TIsoTestExpression
  | PC.TIsoToTestExpression ->
      failwith "unexpected tokens"
  | PC.TScriptData s -> ([t],[t])
  | PC.TWhitespace _ -> ([t],[t])


let split_token_stream tokens =
  let rec loop = function
      [] -> ([],[])
    | token::tokens ->
	let (minus,plus) = split_token token in
	let (minus_stream,plus_stream) = loop tokens in
	(minus@minus_stream,plus@plus_stream) in
  loop tokens

(* ----------------------------------------------------------------------- *)
(* Find function names *)
(* This addresses a shift-reduce problem in the parser, allowing us to
distinguish a function declaration from a function call even if the latter
has no return type.  Undoubtedly, this is not very nice, but it doesn't
seem very convenient to refactor the grammar to get around the problem. *)

exception Irrelevant

let find_function_names l =
  let is_ident = function
      (PC.TIdent(_,clt),info)
    | (PC.TMeta(_,_,_,clt),info)
    | (PC.TMetaId(_,_,_,_,clt),info)
    | (PC.TMetaFunc(_,_,_,clt),info)
    | (PC.TMetaLocalFunc(_,_,_,clt),info) -> true
    | _ -> false in
  let is_mid = function
      (PC.TMid0(_),info) | (PC.TAnd0(_),info) -> true
    | _ -> false in
  let is_par = function
      (PC.TOPar0(_),info) -> true
    | _ -> false in
  let rec split acc = function
      [] | [_] -> raise Irrelevant
    | ((PC.TCPar(_),_) as t1) :: ((PC.TOBrace(_),_) as t2) :: rest ->
	(List.rev (t1::acc),(t2::rest))
    | x::xs -> split (x::acc) xs in
  let rec balanced_name level = function
      [] -> raise Irrelevant
    | (PC.TCPar0(_),_)::rest ->
	let level = level - 1 in
	if level = 0
	then rest
	else balanced_name level rest
    | (PC.TOPar0(_),_)::rest ->
	let level = level + 1 in
	balanced_name level rest
    | (PC.TArobArob,_)::_ | (PC.TArob,_)::_ | (PC.EOF,_)::_ ->
	raise Irrelevant
    | t::rest when is_ident t && level = 0 -> rest
    | t::rest when is_ident t || is_mid t -> balanced_name level rest
    | _ -> raise Irrelevant in
  let rec balanced_args level = function
      [] -> raise Irrelevant
    | (PC.TCPar(_),_)::rest ->
	let level = level - 1 in
	if level = 0
	then rest
	else balanced_args level rest
    | (PC.TOPar(_),_)::rest ->
	let level = level + 1 in
	balanced_args level rest
    | (PC.TArobArob,_)::_ | (PC.TArob,_)::_ | (PC.EOF,_)::_ ->
	raise Irrelevant
    | t::rest -> balanced_args level rest in
  let rec loop = function
      [] -> []
    | t :: rest ->
	if is_par t || is_mid t || is_ident t
	then
	  let (t,rest) =
	    try
	      let (bef,aft) = split [] (t::rest) in
	      let rest = balanced_name 0 bef in
	      (match rest with
		(PC.TOPar(_),_)::_ ->
		  (match balanced_args 0 rest with
		    [] ->
		      let (_,info) as h = List.hd bef in
		      let clt = get_clt h in
		      (((PC.TFunDecl(clt),info) :: bef), aft)
		  | (PC.TAttr_(_),_)::rest ->
		      (match balanced_args 0 rest with
			[] ->
			  let (_,info) as h = List.hd bef in
			  let clt = get_clt h in
			  (((PC.TFunDecl(clt),info) :: bef), aft)
		      | _ -> raise Irrelevant)
		  | _ -> raise Irrelevant)
	      | _ -> raise Irrelevant)
	    with Irrelevant -> ([t],rest) in
	  t @ (loop rest)
	else t :: (loop rest) in
  loop l

(* ----------------------------------------------------------------------- *)
(* an attribute is an identifier that precedes another identifier and
   begins with __ *)

let detect_attr l =
  let is_id = function
      (PC.TIdent(_,_),_) | (PC.TMetaId(_,_,_,_,_),_) | (PC.TMetaFunc(_,_,_,_),_)
    | (PC.TMetaLocalFunc(_,_,_,_),_) -> true
    | _ -> false in
  let rec loop = function
      [] -> []
    | [x] -> [x]
    | ((PC.Tstruct _,_) as t1)::x::rest ->
	t1::x::loop rest
    | ((PC.Tunion _,_) as t1)::x::rest ->
	t1::x::loop rest
    | ((PC.TIdent(nm,clt),info) as t1)::id::rest when is_id id ->
	if String.length nm > 2 && String.sub nm 0 2 = "__"
	then
	  begin
	    Flag.add_cocci_attribute_names nm;
	    (if not (Hashtbl.mem Lexer_cocci.attr_names nm)
	    then !D.add_attribute nm);
	    (PC.Tattr(nm,clt),info)::(loop (id::rest))
	  end
	else t1::(loop (id::rest))
    | x::xs -> x::(loop xs) in
  loop l

(* ----------------------------------------------------------------------- *)
(* Look for variable declarations where the name is a typedef name.
We assume that C code does not contain a multiplication as a top-level
statement. *)

(* bug: once a type, always a type, even if the same name is later intended
   to be used as a real identifier *)
let detect_types in_meta_decls l =
  let is_delim infn = function
      (PC.TOEllipsis(_),_)
    | (PC.TPOEllipsis(_),_)
    | (PC.TEllipsis(_),_)
    | (PC.TPtVirg(_),_) | (PC.TOBrace(_),_) | (PC.TOInit(_),_)
    | (PC.TCBrace(_),_)
    | (PC.TPure,_) | (PC.TContext,_)
    | (PC.Tstatic(_),_) | (PC.Textern(_),_)
    | (PC.Tinline(_),_) | (PC.Ttypedef(_),_) | (PC.Tattr(_),_) -> true
    | (PC.TComma(_),_) when infn > 0 || in_meta_decls -> true
    | (PC.TDotDot(_),_) when in_meta_decls -> true
    | _ -> false in
  let is_choices_delim = function
      (PC.TOBrace(_),_) | (PC.TComma(_),_) -> true | _ -> false in
  let is_id = function
      (PC.TIdent(_,_),_) | (PC.TMetaId(_,_,_,_,_),_)
    | (PC.TMetaFunc(_,_,_,_),_)
    | (PC.TMetaLocalFunc(_,_,_,_),_) -> true
    | (PC.TMetaParam(_,_,_,_),_)
    | (PC.TMetaParamList(_,_,_,_,_),_)
    | (PC.TMetaConst(_,_,_,_,_),_)
    | (PC.TMetaErr(_,_,_,_),_)
    | (PC.TMetaExp(_,_,_,_,_,_),_)
    | (PC.TMetaIdExp(_,_,_,_,_),_)
    | (PC.TMetaLocalIdExp(_,_,_,_,_),_)
    | (PC.TMetaAssignOp(_,_,_,_),_)
    | (PC.TMetaBinaryOp(_,_,_,_),_)
    | (PC.TMetaGlobalIdExp(_,_,_,_,_),_)
    | (PC.TMetaExpList(_,_,_,_,_),_)
    | (PC.TMetaType(_,_,_,_),_)
    | (PC.TMetaInit(_,_,_,_),_)
    | (PC.TMetaInitList(_,_,_,_,_),_)
    | (PC.TMetaDecl(_,_,_,_),_)
    | (PC.TMetaField(_,_,_,_),_)
    | (PC.TMetaFieldList(_,_,_,_,_),_)
    | (PC.TMetaStm(_,_,_,_),_)
    | (PC.TMetaStmList(_,_,_,_,_),_)
    | (PC.TMetaDParamList(_,_,_,_,_),_)
    | (PC.TMetaPos(_,_,_,_),_) -> in_meta_decls
    | _ -> false in
  let is_tyleft = function (* things that can start a var decl *)
      (PC.TMul(_),_)
    | (PC.TOPar(_),_) -> true
    | _ -> false in
  let redo_id ident clt v =
    !D.add_type_name ident;
    (PC.TTypeId(ident,clt),v) in
  let rec loop start infn type_names = function
      (* infn: 0 means not in a function header
	 > 0 means in a function header, after infn - 1 unmatched open parens*)
      [] -> []
    | ((PC.TOBrace(clt),v)::_) as all when in_meta_decls ->
	collect_choices type_names all (* never a function header *)
    | delim::(PC.TIdent(ident,clt),v)::((PC.TMul(_),_) as x)::((id::_) as rest)
      when is_delim infn delim && (is_id id || is_tyleft id) ->
	let newid = redo_id ident clt v in
	delim::newid::x::(loop false infn (ident::type_names) rest)
    | delim::(PC.TIdent(ident,clt),v)::id::rest
      when is_delim infn delim && is_id id ->
	let newid = redo_id ident clt v in
	delim::newid::id::(loop false infn (ident::type_names) rest)
    | ((PC.TFunDecl(_),_) as fn)::rest ->
	fn::(loop false 1 type_names rest)
    | ((PC.TOPar(_),_) as lp)::rest when infn > 0 ->
	lp::(loop false (infn + 1) type_names rest)
    | ((PC.TCPar(_),_) as rp)::rest when infn > 0 ->
	if infn - 1 = 1
	then rp::(loop false 0 type_names rest) (* 0 means not in fn header *)
	else rp::(loop false (infn - 1) type_names rest)
    | (PC.TIdent(ident,clt),v)::((PC.TMul(_),_) as x)::((id::_) as rest)
      when start && (is_id id || is_tyleft id) ->
	let newid = redo_id ident clt v in
	newid::x::(loop false infn (ident::type_names) rest)
    | (PC.TIdent(ident,clt),v)::id::rest when start && is_id id ->
	let newid = redo_id ident clt v in
	newid::id::(loop false infn (ident::type_names) rest)
    | (PC.TIdent(ident,clt),v)::rest when List.mem ident type_names ->
	(PC.TTypeId(ident,clt),v)::(loop false infn type_names rest)
    | ((PC.TIdent(ident,clt),v) as x)::rest ->
	x::(loop false infn type_names rest)
    | x::rest -> x::(loop false infn type_names rest)
  and collect_choices type_names = function
      [] -> [] (* should happen, but let the parser detect that *)
    | (PC.TCBrace(clt),v)::rest ->
	(PC.TCBrace(clt),v)::(loop false 0 type_names rest)
    | delim::(PC.TIdent(ident,clt),v)::rest
      when is_choices_delim delim ->
	let newid = redo_id ident clt v in
	delim::newid::(collect_choices (ident::type_names) rest)
    | x::rest -> x::(collect_choices type_names rest) in
  loop true 0 [] l


(* ----------------------------------------------------------------------- *)
(* Insert TLineEnd tokens at the end of a line that contains a WHEN.
   WHEN is restricted to a single line, to avoid ambiguity in eg:
   ... WHEN != x
   +3 *)

let token2line (tok,_) =
  match tok with
    PC.Tchar(clt) | PC.Tshort(clt) | PC.Tint(clt) | PC.Tdouble(clt)
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt)
  | PC.Tsize_t(clt) | PC.Tssize_t(clt) | PC.Tptrdiff_t(clt)
  | PC.Tstruct(clt)
  | PC.Tunion(clt) | PC.Tenum(clt) | PC.Tdecimal(clt) | PC.Texec(clt)
  | PC.Tunsigned(clt) | PC.Tsigned(clt)
  | PC.Tstatic(clt) | PC.Tauto(clt) | PC.Tregister(clt) | PC.Textern(clt)
  | PC.Tinline(clt) | PC.Ttypedef(clt) | PC.Tattr(_,clt) | PC.Tconst(clt)
  | PC.Tvolatile(clt)

  | PC.TInc(clt) | PC.TDec(clt)

  | PC.TIf(clt) | PC.TElse(clt) | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt)
  | PC.TSwitch (clt) | PC.TCase (clt) | PC.TDefault (clt)
  | PC.TSizeof (clt) | PC.TTypeof (clt)
  | PC.TReturn(clt) | PC.TBreak(clt) | PC.TContinue(clt) | PC.TGoto(clt)
  | PC.TIdent(_,clt)
  | PC.TTypeId(_,clt) | PC.TDeclarerId(_,clt) | PC.TIteratorId(_,clt)
  | PC.TMetaDeclarer(_,_,_,clt) | PC.TMetaIterator(_,_,_,clt)

  | PC.TSymId(_,clt)

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt)
  | PC.TDecimalCst(_,_,_,clt)

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TLogOp(_,clt)
  | PC.TShLOp(_,clt) | PC.TShROp(_,clt)
  | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt)
  | PC.TDmOp(_,clt) | PC.TTilde (clt)

  | PC.TMeta(_,_,_,clt) | PC.TMetaParam(_,_,_,clt)
  | PC.TMetaParamList(_,_,_,_,clt)
  | PC.TMetaConst(_,_,_,_,clt) | PC.TMetaExp(_,_,_,_,clt,_)
  | PC.TMetaIdExp(_,_,_,_,clt)
  | PC.TMetaLocalIdExp(_,_,_,_,clt) | PC.TMetaGlobalIdExp(_,_,_,_,clt)
  | PC.TMetaAssignOp(_,_,_,clt) | PC.TMetaBinaryOp(_,_,_,clt)
  | PC.TMetaExpList(_,_,_,_,clt)
  | PC.TMetaId(_,_,_,_,clt) | PC.TMetaType(_,_,_,clt)
  | PC.TMetaInit(_,_,_,clt) | PC.TMetaInitList(_,_,_,_,clt)
  | PC.TMetaDecl(_,_,_,clt) | PC.TMetaField(_,_,_,clt)
  | PC.TMetaFieldList(_,_,_,_,clt)
  | PC.TMetaStm(_,_,_,clt) | PC.TMetaStmList(_,_,_,_,clt)
  | PC.TMetaDParamList(_,_,_,_,clt) | PC.TMetaFunc(_,_,_,clt)
  | PC.TMetaLocalFunc(_,_,_,clt) | PC.TMetaPos(_,_,_,clt)

  | PC.TFunDecl(clt)
  | PC.TWhen(clt) | PC.TWhenTrue(clt) | PC.TWhenFalse(clt)
  | PC.TAny(clt) | PC.TStrict(clt) | PC.TEllipsis(clt)

  | PC.TOEllipsis(clt) | PC.TCEllipsis(clt)
  | PC.TPOEllipsis(clt) | PC.TPCEllipsis(clt)

  | PC.TWhy(clt) | PC.TDotDot(clt) | PC.TBang(clt) | PC.TOPar(clt)
  | PC.TOPar0(_,clt) | PC.TMid0(_,clt) | PC.TCPar(clt)
  | PC.TCPar0(_,clt)

  | PC.TOBrace(clt) | PC.TCBrace(clt) | PC.TOCro(clt) | PC.TCCro(clt)
  | PC.TOInit(clt)

  | PC.TPtrOp(clt)

  | PC.TUndef(clt,_) | PC.TDefine(clt,_) | PC.TDefineParam(clt,_,_,_)
  | PC.TPragma(clt) | PC.TCppEscapedNewline(clt)
  | PC.TIncludeL(_,clt) | PC.TIncludeNL(_,clt) | PC.TIncludeAny(_,clt)
  | PC.TInclude(clt)

  | PC.TEq(clt) | PC.TOpAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt)
  | PC.TPArob(clt) | PC.TPtVirg(clt) ->
      let (_,line,_,_,_,_,_,_,_,_) = clt in Some line

  | _ -> None

let rec insert_line_end = function
    [] -> []
  | (((PC.TWhen(clt),q) as x)::xs) ->
      x::(find_line_end true (token2line x) clt q xs)
  | (((PC.TUndef(clt,_),q) as x)::xs)
  | (((PC.TDefine(clt,_),q) as x)::xs)
  | (((PC.TDefineParam(clt,_,_,_),q) as x)::xs)
  | (((PC.TPragma(clt),q) as x)::xs) ->
      x::(find_line_end false (token2line x) clt q xs)
  | x::xs -> x::(insert_line_end xs)

and find_line_end inwhen line clt q = function
    (* don't know what 2nd component should be so just use the info of
       the When.  Also inherit - of when, if any *)
    [] -> [(PC.TLineEnd(clt),q)]
  | ((PC.TIdent("strict",clt),a) as x)::xs when token2line x = line ->
      (PC.TStrict(clt),a) :: (find_line_end inwhen line clt q xs)
  | ((PC.TIdent("STRICT",clt),a) as x)::xs when token2line x = line ->
      (PC.TStrict(clt),a) :: (find_line_end inwhen line clt q xs)
  | ((PC.TIdent("any",clt),a) as x)::xs when token2line x = line ->
      (PC.TAny(clt),a) :: (find_line_end inwhen line clt q xs)
  | ((PC.TIdent("ANY",clt),a) as x)::xs when token2line x = line ->
      (PC.TAny(clt),a) :: (find_line_end inwhen line clt q xs)
  | ((PC.TIdent("forall",clt),a) as x)::xs when token2line x = line ->
      (PC.TForall,a) :: (find_line_end inwhen line clt q xs)
  | ((PC.TIdent("exists",clt),a) as x)::xs when token2line x = line ->
      (PC.TExists,a) :: (find_line_end inwhen line clt q xs)
  | ((PC.TComma(clt),a) as x)::xs when token2line x = line ->
      (PC.TComma(clt),a) :: (find_line_end inwhen line clt q xs)
  | ((PC.TPArob(clt),a) as x)::xs when token2line x = line ->
      (PC.TPArob(clt),a) :: (find_line_end inwhen line clt q xs)
  | ((PC.TCppEscapedNewline(clt),a) as x)::xs when token2line x = line ->
      (match xs with
	x::_ -> find_line_end inwhen (token2line x) clt q xs
      | [] -> find_line_end inwhen line clt q xs (* line doesn't matter *))
  | x::xs when token2line x = line -> x :: (find_line_end inwhen line clt q xs)
  | xs -> (PC.TLineEnd(clt),q)::(insert_line_end xs)

let rec translate_when_true_false = function
    [] -> []
  | (PC.TWhen(clt),q)::((PC.TNotEq(_),_) as x)::(PC.TIdent("true",_),_)::xs ->
      (PC.TWhenTrue(clt),q)::x::(translate_when_true_false xs)
  | (PC.TWhen(clt),q)::((PC.TNotEq(_),_) as x)::(PC.TIdent("false",_),_)::xs ->
      (PC.TWhenFalse(clt),q)::x::(translate_when_true_false xs)
  | x::xs -> x :: (translate_when_true_false xs)

(* ----------------------------------------------------------------------- *)

(* In a nest, if the nest is -, all of the nested code must also be -. *)
let check_nests tokens =
  let is_minus t =
    let (line_type,a,b,c,d,e,f,g,h,i) = get_clt t in
    List.mem line_type [D.MINUS;D.OPTMINUS] in
  let check_minus t =
    match fst t with
      PC.TOPar0(_,clt) | PC.TMid0(_,clt) | PC.TCPar0(_,clt) -> t
    | _ ->
	let clt = try Some(get_clt t) with Failure _ -> None in
	match clt with
	  Some (line_type,l,ll,c,d,e,f,g,h,i) ->
	    (match line_type with
	      D.MINUS | D.OPTMINUS -> t
	    | _ ->
		failwith
		  (Printf.sprintf "minus expected, on %s, line %d"
		     (token2c t true) l))
	| None -> t in
  let rec outside = function
      [] -> []
    | ((PC.TPOEllipsis(clt),q) as t)::r when is_minus t -> t :: inside 0 r
    | t::r -> t :: outside r
  and inside stack = function
      [] -> failwith "missing nest end"
    | ((PC.TPCEllipsis(clt),q) as t)::r ->
	(check_minus t)
	:: (if stack = 0 then outside r else inside (stack - 1) r)
    | ((PC.TPOEllipsis(clt),q) as t)::r ->
	(check_minus t) :: (inside (stack + 1) r)
    | t :: r -> (check_minus t) :: (inside stack r) in
  outside tokens

(* This doesn't need to be done on plus code.  If it fails, it will already
fail on the minus code.  It causes a problem when * is used and the * marks
on the parentheses are unbalanced *)
let check_parentheses plus tokens =
  if plus
  then tokens
  else
    begin
      let clt2line (_,line,_,_,_,_,_,_,_,_) = line in
      let rec loop seen_open = function
	  [] -> tokens
	| (PC.TOPar(clt),q) :: rest
	| (PC.TDefineParam(clt,_,_,_),q) :: rest ->
	    loop (Common.Left (clt2line clt) :: seen_open) rest
	| (PC.TOPar0(_,clt),q) :: rest ->
	    loop (Common.Right (clt2line clt) :: seen_open) rest
	| (PC.TCPar(clt),q) :: rest ->
	    (match seen_open with
	      [] ->
		failwith
		  (Printf.sprintf
		     "unexpected close parenthesis in line %d\n"
		     (clt2line clt))
	    | Common.Left _ :: seen_open -> loop seen_open rest
	    | Common.Right open_line :: _ ->
		failwith
		  (Printf.sprintf
		     "disjunction parenthesis in line %d column 0 matched to normal parenthesis on line %d\n" open_line (clt2line clt)))
	| (PC.TCPar0(_,clt),q) :: rest ->
	    (match seen_open with
	      [] ->
		failwith
		  (Printf.sprintf
		     "unexpected close parenthesis in line %d\n" (clt2line clt))
	    | Common.Right _ :: seen_open -> loop seen_open rest
	    | Common.Left open_line :: _ ->
		failwith
		  (Printf.sprintf
		     "normal parenthesis in line %d matched to disjunction parenthesis on line %d column 0\n" open_line (clt2line clt)))
	| x::rest -> loop seen_open rest in
      loop [] tokens
    end

(* ----------------------------------------------------------------------- *)
(* top level initializers: a sequence of braces followed by a dot *)

let find_top_init tokens =
  match tokens with
    (PC.TOBrace(clt),q) :: rest ->
      let rec dot_start acc = function
	  ((PC.TOBrace(_),_) as x) :: rest ->
	    dot_start (x::acc) rest
	| ((PC.TDot(_),_) :: rest) as x ->
	    Some ((PC.TOInit(clt),q) :: (List.rev acc) @ x)
	| l -> None in
      let rec comma_end acc = function
	  ((PC.TCBrace(_),_) as x) :: rest ->
	    comma_end (x::acc) rest
	| ((PC.TComma(_),_) :: rest) as x ->
	    Some ((PC.TOInit(clt),q) :: (List.rev x) @ acc)
	| l -> None in
      (match dot_start [] rest with
	Some x -> x
      |	None ->
	  (match List.rev rest with
	    (* not super sure what this does, but EOF, @, and @@ should be
	       the same, markind the end of a rule *)
	    ((PC.EOF,_) as x)::rest | ((PC.TArob,_) as x)::rest
	  | ((PC.TArobArob,_) as x)::rest ->
	      (match comma_end [x] rest with
		Some x -> x
	      | None -> tokens)
	  | _ ->
	      failwith "unexpected empty token list"))
  | _ -> tokens

(* ----------------------------------------------------------------------- *)
(* Integrate pragmas into some adjacent token.  + tokens are preferred.  Dots
are not allowed. *)

let rec collect_all_pragmas collected = function
    (PC.TDirective(s,(_,line,logical_line,logical_line_end,
                      offset,col,_,_,pos,_)),_)::rest ->
      let i =
	{ Ast0.line_start = line; Ast0.line_end = line;
	  Ast0.logical_start = logical_line;
	  Ast0.logical_end = logical_line_end;
	  Ast0.column = col; Ast0.offset = offset; } in
      collect_all_pragmas ((s,i)::collected) rest
  | l -> (List.rev collected,l)

let rec collect_pass = function
    [] -> ([],[])
  | x::xs ->
      match plus_attachable false x with
	SKIP ->
	  let (pass,rest) = collect_pass xs in
	  (x::pass,rest)
      |	_ -> ([],x::xs)

let collect_attr toks =
  let rec loop n ok = function
      [] ->
	if n > 0 || not ok
	then failwith "missing )) on __attribute__"
	else ("",[])
    | (PC.TOPar(clt),_)::xs ->
	let ok = if n+1 = 2 then true else ok in
	let (attr,rest) = loop (n+1) ok xs in
	("("^attr,rest)
    | (PC.TCPar(clt),_)::xs when n > 1 ->
	let (attr,rest) = loop (n-1) ok xs in
	(")"^attr,rest)
    | (PC.TCPar(clt),_)::xs when n = 1 -> (")",xs)
    | x::xs ->
	if n >=2
	then
	  let (attr,rest) = loop n ok xs in
	  ((token2c x false)^attr,rest)
	else failwith "attribute code must be in double parens" in
  loop 0 false toks

let plus_attach strict = function
    None -> NOTPLUS
  | Some x -> plus_attachable strict x

let add_bef = function Some x -> [x] | None -> []

(*skips should be things like line end
skips is things before pragmas that can't be attached to, pass is things
after.  pass is used immediately.  skips accumulates.
When stuff is added before some + code, the logical line of the + code
becomes that of the pragma.  context_neg relies on things that are adjacent
having sequential logical lines.  Not sure that this is good enough,
as it might result in later gaps in the logical lines... *)
let rec process_pragmas (bef : 'a option) (skips : 'a list) = function
    [] -> add_bef bef @ List.rev skips
  | ((PC.TEllipsis(_),_) as a)::((PC.TComma(_),_) as b)::xs ->
      (* This is a ..., in an argument list, field initializer list etc,
	 which might go away, so nothing should be attached to the , *)
      process_pragmas bef (b::a::skips) xs
  | (PC.TAttr_(i),x)::xs ->
      let (attr,rest) = collect_attr xs in
      process_pragmas bef skips
	((PC.TDirective(Ast.Space("__attribute__"^attr),i),x)::rest)
  | ((PC.TDirective(s,i),_)::_) as l ->
      let (pragmas,rest) = collect_all_pragmas [] l in
      let (pass,rest0) = collect_pass rest in
      let (_,_,prag_lline,_,_,_,_,_,_,_) = i in
      let (next,rest) =
	match rest0 with [] -> (None,[]) | next::rest -> (Some next,rest) in
      (match (bef,plus_attach true bef,next,plus_attach true next) with
	(Some bef,PLUS,_,_) ->
	  let (a,b,c,d,e,f,strbef,straft,pos,ws) = get_clt bef in
	  (update_clt bef (a,b,c,d,e,f,strbef,pragmas,pos,ws))::List.rev skips@
	  pass@process_pragmas None [] rest0
      |	(_,_,Some next,PLUS) ->
	  let (a,b,lline,llineend,d,e,strbef,straft,pos,ws) = get_clt next in
	  (add_bef bef) @ List.rev skips @ pass @
	  (process_pragmas
	     (Some (update_clt next
               (a,b,prag_lline,llineend,d,e,pragmas,straft,pos,ws)))
	     [] rest)
      |	_ ->
	  (match (bef,plus_attach false bef,next,plus_attach false next) with
	    (Some bef,PLUS,_,_) ->
	      let (a,b,c,d,e,f,strbef,straft,pos,ws) = get_clt bef in
	      (update_clt bef (a,b,c,d,e,f,strbef,pragmas,pos,ws))::
              List.rev skips@
	      pass@process_pragmas None [] rest0
	  | (_,_,Some next,PLUS) ->
	      let (a,b,lline,llineend,d,e,strbef,straft,pos,ws) =
                get_clt next in
	      (add_bef bef) @ List.rev skips @ pass @
	      (process_pragmas
		 (Some
		  (update_clt next
                    (a,b,prag_lline,llineend,d,e,pragmas,straft,pos,ws)))
		 [] rest)
	  | _ -> failwith "nothing to attach pragma to"))
  | x::xs ->
      (match plus_attachable false x with
	SKIP -> process_pragmas bef (x::skips) xs
      |	_ -> (add_bef bef) @ List.rev skips @ (process_pragmas (Some x) [] xs))

(* Appends whitespace tokens to the nearest following token (assumes that
 * any such token contains a clt as defined in parser_cocci_menhir.mly,
 * otherwise get_clt and update_clt will fail).
 * The third case handles double whitespaces which occur around script comments
 * We only want to keep both whitespaces if the whitespaces are on the same
 * line.

 * Returns token list with whitespace tokens removed.
 *)

let process_whitespaces tok =
  let rec pw fn = function
    | [] -> fn []
    | (PC.TWhitespace(_),_)::((endtok,_) as e)::(_)
        when endtok = PC.EOF || endtok = PC.TArob || endtok = PC.TArobArob ->
        fn [e]
    | (PC.TWhitespace(a),((_,(l1,_),_) as b))::
      (PC.TWhitespace(c),(_,(l2,_),_))::xs ->
        let s = if (l1 <> l2) then c else a ^ c in
        pw fn ((PC.TWhitespace(s),b) :: xs)
    | (PC.TWhitespace(s),_)::((tok,_) as aft)::xs ->
       (try
          let (a, b, c, d, e, f, g, h, i, _) = get_clt aft in
          let aft = update_clt aft (a, b, c, d, e, f, g, h, i, s) in
          pw (fun lst -> fn (aft :: lst)) xs
        with NoClt(a) -> failwith ("process_whitespaces: "^a))
    | x::xs -> pw (fun lst -> fn (x :: lst)) xs in
  pw (fun l -> l) tok

(* ----------------------------------------------------------------------- *)
(* Drop ... ... .  This is only allowed in + code, and arises when there is
some - code between the ... *)
(* drop whens as well - they serve no purpose in + code and they cause
problems for drop_double_dots *)

let rec drop_when = function
    [] -> []
  | (PC.TWhen(clt),info)::xs ->
      let rec loop = function
	  [] -> []
	| (PC.TLineEnd(_),info)::xs -> drop_when xs
	| x::xs -> loop xs in
      loop xs
  | x::xs -> x::drop_when xs

(* instead of dropping the double dots, we put TNothing in between them.
these vanish after the parser, but keeping all the ...s in the + code makes
it easier to align the + and - code in context_neg and in preparation for the
isomorphisms.  This shouldn't matter because the context code of the +
slice is mostly ignored anyway *)
let minus_to_nothing l =
  (* for cases like | <..., which may or may not arise from removing minus
     code, depending on whether <... is a statement or expression *)
  let is_minus tok =
    try
      let (d,_,_,_,_,_,_,_,_,_) = get_clt tok in
      (match d with
	D.MINUS | D.OPTMINUS -> true
      | D.PLUS | D.PLUSPLUS -> false
      | D.CONTEXT | D.OPT -> false)
    with _ -> false in
  let rec minus_loop = function
      [] -> []
    | (d::ds) as l -> if is_minus d then minus_loop ds else l in
  let rec loop = function
      [] -> []
    | ((PC.TMid0(clt),i) as x)::t1::ts when is_minus t1 ->
	(match minus_loop ts with
	  ((PC.TOEllipsis(_),_)::_) | ((PC.TPOEllipsis(_),_)::_)
	| ((PC.TEllipsis(_),_)::_) as l -> x::(PC.TNothing,i)::(loop l)
	| l -> x::(loop l))
    | t::ts -> t::(loop ts) in
  loop l

let drop_double_dots l =
  let start = function
      (PC.TOEllipsis(_),_) | (PC.TPOEllipsis(_),_) -> true
    | _ -> false in
  let middle = function
      (PC.TEllipsis(_),_) -> true
    | _ -> false in
  let whenline = function
      (PC.TLineEnd(_),_) -> true
    (*| (PC.TMid0(_),_) -> true*)
    | _ -> false in
  let final = function
      (PC.TCEllipsis(_),_) | (PC.TPCEllipsis(_),_) -> true
    | _ -> false in
  let any_before x = start x || middle x || final x || whenline x in
  let any_after x = start x || middle x || final x in
  let rec loop ((_,i) as prev) = function
      [] -> []
    | x::rest when any_before prev && any_after x ->
	(PC.TNothing,i)::x::(loop x rest)
    | ((PC.TComma(_),_) as c)::x::rest when any_before prev && any_after x ->
	c::(PC.TNothing,i)::x::(loop x rest)
    | x::rest -> x :: (loop x rest) in
  match l with
    [] -> []
  | (x::xs) -> x :: loop x xs

(* ignore uncomparable pcre regular expressions *)
let strip_for_fix l =
  List.map
    (function
	(PC.TMetaId(nm,_,seed,pure,clt),info) ->
	  (PC.TMetaId(nm,Ast.CstrTrue,seed,pure,clt),info)
      |	(PC.TMetaFunc(nm,_,pure,clt),info) ->
	  (PC.TMetaFunc(nm,Ast.CstrTrue,pure,clt),info)
      |	(PC.TMetaLocalFunc(nm,_,pure,clt),info) ->
	  (PC.TMetaLocalFunc(nm,Ast.CstrTrue,pure,clt),info)
      |	(PC.TMetaErr(nm,_,pure,clt),info) ->
	  (PC.TMetaErr(nm,Ast.CstrTrue,pure,clt),info)
      |	(PC.TMetaExp(nm,_,pure,ty,clt,bitfield),info) ->
	  (PC.TMetaExp(nm,Ast.CstrTrue,pure,ty,clt,bitfield),info)
      |	(PC.TMetaIdExp(nm,_,pure,ty,clt),info) ->
	  (PC.TMetaIdExp(nm,Ast.CstrTrue,pure,ty,clt),info)
      |	(PC.TMetaLocalIdExp(nm,_,pure,ty,clt),info) ->
	  (PC.TMetaLocalIdExp(nm,Ast.CstrTrue,pure,ty,clt),info)
      |	(PC.TMetaGlobalIdExp(nm,_,pure,ty,clt),info) ->
	  (PC.TMetaGlobalIdExp(nm,Ast.CstrTrue,pure,ty,clt),info)
      |	(PC.TMetaConst(nm,_,pure,ty,clt),info) ->
	  (PC.TMetaConst(nm,Ast.CstrTrue,pure,ty,clt),info)
      |	t -> t)
    l

let fix f l =
  let rec loop f l stripped_l =
    let cur = f l in
    let stripped_cur = strip_for_fix cur in
    if stripped_l = stripped_cur then l else loop f cur stripped_cur in
  loop f l (strip_for_fix l)

(* ( | ... | ) also causes parsing problems *)

exception Not_empty

let rec drop_empty_thing starter middle ender = function
    [] -> []
  | hd::rest when starter hd ->
      let rec loop = function
	  x::rest when middle x -> loop rest
	| x::rest when ender x -> rest
	| _ -> raise Not_empty in
      (match try Some(loop rest) with Not_empty -> None with
	Some x -> drop_empty_thing starter middle ender x
      |	None -> hd :: drop_empty_thing starter middle ender rest)
  | x::rest -> x :: drop_empty_thing starter middle ender rest

let drop_empty_or =
  drop_empty_thing
    (function (PC.TOPar0(_),_) -> true | _ -> false)
    (function (PC.TMid0(_),_) -> true | _ -> false)
    (function (PC.TCPar0(_),_) -> true | _ -> false)

let drop_empty_nest = drop_empty_thing

(* ----------------------------------------------------------------------- *)
(* Read tokens *)

let get_s_starts (_, (s,_,(starts, ends))) = (s, starts)

let pop2 l =
  let v = List.hd !l in
  l := List.tl !l;
  v

(*
let reinit _ =
  PC.reinit (function _ -> PC.TArobArob (* a handy token *))
    (Lexing.from_function
       (function buf -> function n -> raise (Common.Impossible 157)))
*)

let parse_one str parsefn file toks =
  let all_tokens = ref toks in
  let cur_tok    = ref (List.hd !all_tokens) in

  let lexer_function _ =
      let (v, info) = pop2 all_tokens in
      cur_tok := (v, info);
      v in

  let lexbuf_fake =
    Lexing.from_function
      (function buf -> function n -> raise (Common.Impossible 158))
  in
  (* reinit(); *)

  try parsefn lexer_function lexbuf_fake
  with
    Lexer_cocci.Lexical s ->
      smplparseerror
	(Printf.sprintf "%s: lexical error: %s\n  %s\n" str s
	   (Common.error_message file (get_s_starts !cur_tok) ))
  | Parser_cocci_menhir.Error ->
      smplparseerror
	(Printf.sprintf "%s: parse error: \n  %s\n" str
	   (Common.error_message file (get_s_starts !cur_tok) ))
  | Semantic_cocci.Semantic s ->
      smplparseerror
	(Printf.sprintf "%s: semantic error: %s\n  %s\n" str s
	   (Common.error_message file (get_s_starts !cur_tok) ))

  | e -> raise e

let prepare_tokens plus tokens =
  find_top_init
    (translate_when_true_false (* after insert_line_end *)
       (insert_line_end
	  (detect_types false
	     (find_function_names
		(detect_attr
		   (check_nests
		      (check_parentheses plus tokens)))))))

let prepare_mv_tokens tokens =
  detect_types false (detect_attr tokens)

let unminus (d,x1,x2,x3,x4,x5,x6,x7,x8,x9) = (* for hidden variables *)
  match d with
    D.MINUS | D.OPTMINUS ->
      (D.CONTEXT,x1,x2,x3,x4,x5,x6,x7,x8,x9)
  | D.PLUS -> failwith "unexpected plus code"
  | D.PLUSPLUS -> failwith "unexpected plus code"
  | D.CONTEXT | D.OPT -> (D.CONTEXT,x1,x2,x3,x4,x5,x6,x7,x8,x9)

let process_minus_positions x name clt meta =
  let (arity,ln,lln,llne,offset,col,strbef,straft,pos,ws) = get_clt x in
  let name = Parse_aux.clt2mcode name (unminus clt) in
  update_clt x (arity,ln,lln,llne,offset,col,strbef,straft,meta name::pos,ws)

(* first attach positions, then the others, so that positions can refer to
the larger term represented by the preceding metavariable *)
let consume_minus_positions toks =
  let rec loop_pos = function
      [] -> []
    | ((PC.TOPar0(_),_) as x)::xs | ((PC.TCPar0(_),_) as x)::xs
    | ((PC.TMid0(_),_) as x)::xs -> x::loop_pos xs
    | x::(PC.TPArob _,_)::(PC.TMetaPos(name,constraints,per,clt),_)::xs ->
	let x =
	  process_minus_positions x name clt
	    (function name ->
	      Ast0.MetaPosTag(Ast0.MetaPos(name,constraints,per))) in
	(loop_pos (x::xs))
    | x::xs -> x::loop_pos xs in
  let rec loop_other = function
      [] -> []
    | ((PC.TOPar0(_),_) as x)::xs | ((PC.TCPar0(_),_) as x)::xs
    | ((PC.TMid0(_),_) as x)::xs -> x::loop_other xs
    | x::(PC.TPArob _,_)::(PC.TMetaId(name,constraints,seed,pure,clt),_)::xs ->
	let x =
	  process_minus_positions x name clt
	    (function name ->
	      Ast0.IdentTag
		(Ast0.wrap
		   (Ast0.MetaId(name,constraints,seed,pure)))) in
	(loop_other (x::xs))
    | x::(PC.TPArob _,_)
      ::(PC.TMetaExp(name,constraints,pure,ty,clt,bitfield),_)::xs ->
	let x =
	  process_minus_positions x name clt
	    (function name ->
	      Ast0.ExprTag
		(Ast0.wrap
		   (Ast0.MetaExpr
		      (name,constraints,ty,Ast.ANY,pure,
		       Common.map_option (Parse_aux.dolen clt) bitfield)))) in
	(loop_other (x::xs))
    | x::(PC.TPArob _,_)::(PC.TMetaExpList(name,len,cstr,pure,clt),_)::xs ->
	let x =
	  process_minus_positions x name clt
	    (function name ->
	      let len = Parse_aux.dolen clt len in
	      Ast0.ExprTag
		(Ast0.wrap
		   (Ast0.MetaExprList(name,len,cstr,pure)))) in
	(loop_other (x::xs))
    | x::(PC.TPArob _,_)::(PC.TMetaInit(name,cstr,pure,clt),_)::xs ->
	let x =
	  process_minus_positions x name clt
	    (function name ->
	      Ast0.InitTag(Ast0.wrap(Ast0.MetaInit(name,cstr,pure)))) in
	(loop_other (x::xs))
    | x::(PC.TPArob _,_)::(PC.TMetaType(name,cstr,pure,clt),_)::xs ->
	let x =
	  process_minus_positions x name clt
	    (function name ->
	      Ast0.TypeCTag(Ast0.wrap(Ast0.MetaType(name,cstr,pure)))) in
	(loop_other (x::xs))
    | x::(PC.TPArob _,_)::(PC.TMetaDecl(name,cstr,pure,clt),_)::xs ->
	let x =
	  process_minus_positions x name clt
	    (function name ->
	      Ast0.DeclTag(Ast0.wrap(Ast0.MetaDecl(name,cstr,pure)))) in
	(loop_other (x::xs))
    | x::(PC.TPArob _,_)::(PC.TMetaStm(name,cstr,pure,clt),_)::xs ->
	let x =
	  process_minus_positions x name clt
	    (function name ->
	      Ast0.StmtTag(Ast0.wrap(Ast0.MetaStmt(name,cstr,pure)))) in
	(loop_other (x::xs))
    | x::(PC.TPArob _,_)::(PC.TMetaIdExp(name,constraints,pure,ty,clt),_)::xs ->
	let x =
	  process_minus_positions x name clt
	    (function name ->
	      Ast0.ExprTag
		(Ast0.wrap
		   (Ast0.MetaExpr(name,constraints,ty,Ast.ANY,pure,None)))) in
	(loop_other (x::xs))

    | x::((PC.TPArob _,_) as x')::x''::xs ->
	x::loop_other (x'::x''::xs)

    | x::xs -> x::loop_other xs in
  loop_other(loop_pos toks)

let rec consume_plus_positions = function
    [] -> []
  | (PC.TPArob _,_)::x::xs -> consume_plus_positions xs
  | x::xs -> x::consume_plus_positions xs

let any_modif rule =
  let mcode x =
    match Ast0.get_mcode_mcodekind x with
      Ast0.MINUS _ | Ast0.PLUS _ -> true
    | _ -> false in
  let donothing r k e = k e in
  let bind x y = x || y in
  let option_default = false in
  let fn =
    V0.flat_combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing in
  List.exists fn.VT0.combiner_rec_top_level rule

let eval_virt virt =
  List.iter
    (function x ->
      if not (List.mem x virt)
      then raise (Bad_virt x))
    !Flag.defined_virtual_rules

let drop_last extra l = List.rev(extra@(List.tl(List.rev l)))

let partition_either l =
  let rec part_either left right = function
  | [] -> (List.rev left, List.rev right)
  | x :: l ->
      (match x with
      | Common.Left  e -> part_either (e :: left) right l
      | Common.Right e -> part_either left (e :: right) l) in
  part_either [] [] l

let rec collect_script_tokens = function
    [(PC.EOF,_)] | [(PC.TArobArob,_)] | [(PC.TArob,_)] -> ""
  | (PC.TScriptData(s),_)::[] ->
      s
  | (PC.TScriptData(s),_)::xs ->
      s^(collect_script_tokens xs)
  | toks ->
      List.iter
	(function x ->
	  Printf.printf "%s\n" (token2c x true))
	toks;
      failwith "Malformed script rule"

let get_metavars parse_fn table file lexbuf rule_name add_to_alldecls =
  Lexer_cocci.reinit(); (* string metavariable initializations *)
  let rec meta_loop acc (* read one decl at a time *) =
    let (_,tokens) =
      D.call_in_meta
	(function _ ->
	  metavariable_decl_tokens_all table file true lexbuf
	    (in_list [PC.TArobArob;PC.TMPtVirg;PC.TAnalysis])) in
    let tokens = prepare_mv_tokens tokens in
    match tokens with
      [(PC.TArobArob,_)] -> List.rev acc
    | (PC.TAnalysis, _) :: tl ->
	Lexer_script.language := "ocaml";
        let get_tokens = tokens_script_all table file false lexbuf in
	let rec loop n toks =
	  let (more, newtoks) = get_tokens (in_list [PC.TScriptData ")"]) in
	  (* we stop at the first close paren*)
	  let n = n - 1 in
	  (* count open parens *)
	  let count str toks =
	    List.fold_left (fun n (t, _) ->
	      if t = PC.TScriptData str
	      then n + 1
	      else n) 0 toks in
	  let n = n + count "(" newtoks in
	  (* continue parsing *)
	  if n = 0
	  then toks @ newtoks
	  else loop n (toks @ newtoks) in
	begin
	  match get_tokens (in_list [PC.TScriptData "("]) with
	  | (_, ([(s, _)] as toks)) ->
	      let data = collect_script_tokens (loop 1 toks) in
	      let (_,tokens) =
		D.call_in_meta
		  (function _ ->
		    metavariable_decl_tokens_all table file true lexbuf
		      (in_list [PC.TArobArob;PC.TMPtVirg])) in
	      begin
		match tokens with
		| [(PC.TIdent (id, _), _); (PC.TMPtVirg, _)] ->
		    let mv =
		      Ast.MetaAnalysisDecl (data, (!Ast0.rule_name, id)) in
		    let metavar = Common.Left mv in
		    (if add_to_alldecls
		    then Common.hashadd D.all_metadecls rule_name mv);
		    meta_loop (metavar :: acc)
		| _ -> failwith "'analysis' can only have one variable"
	      end
	  | (_, toks) ->
	      failwith
		("'analysis' should be followed by '(', but was followed by:\n"
		 ^ (collect_script_tokens toks))
	end

    | _ ->
	let metavars = parse_one "meta" parse_fn file tokens in
	(if add_to_alldecls
	then
	  List.iter
	    (function
		Common.Left mv -> Common.hashadd D.all_metadecls rule_name mv
	      | Common.Right _ -> ())
	    metavars);
	meta_loop (metavars@acc) in
  partition_either (meta_loop [])

let get_script_metavars parse_fn table file lexbuf =
  let rec meta_loop acc =
    let (_, tokens) =
      metavariable_decl_tokens_all table file true lexbuf
	(in_list [PC.TArobArob; PC.TMPtVirg]) in
    let tokens = prepare_tokens false tokens in
    match tokens with
      [(PC.TArobArob, _)] -> List.rev acc
    | _ ->
      let metavar = parse_one "scriptmeta" parse_fn file tokens in
      meta_loop (metavar :: acc)
  in
  meta_loop []

let get_rule_name parse_fn starts_with_name get_tokens file prefix =
  D.in_rule_name := true;
  let mknm _ = make_name prefix (!Lexer_cocci.line) in
  let name_res =
    if starts_with_name
    then
      let (_,tokens) = get_tokens (in_list [PC.TArob]) in
      let check_name = function
	  None -> Some (mknm())
	| Some nm ->
	    (if List.mem nm reserved_names
	    then failwith (Printf.sprintf "invalid name %s\n" nm));
	    Some nm in
      match parse_one "rule name" parse_fn file tokens with
	Ast.CocciRulename (nm,a,b,c,d,e) ->
          Ast.CocciRulename (check_name nm,a,b,c,d,e)
      | Ast.GeneratedRulename (nm,a,b,c,d,e) ->
          Ast.GeneratedRulename (check_name nm,a,b,c,d,e)
      | Ast.ScriptRulename(nm,s,deps) ->
	  Ast.ScriptRulename(check_name nm,s,deps)
      | Ast.InitialScriptRulename(_,s,deps) ->
	  Ast.InitialScriptRulename(check_name None,s,deps)
      | Ast.FinalScriptRulename(_,s,deps) ->
	  Ast.FinalScriptRulename(check_name None,s,deps)
    else
      Ast.CocciRulename(Some(mknm()),Ast.NoDep,[],[],Ast.Undetermined,
			Ast.AnyP) in
  D.in_rule_name := false;
  name_res

let parse_iso file =
  let table = Common.full_charpos_to_pos file in
  Common.with_open_infile file (fun channel ->
    Lexer_cocci.file := file;
    let lexbuf = Lexing.from_channel channel in
    let get_tokens = tokens_all table file false lexbuf in
    let res =
      match get_tokens (in_list [PC.TArobArob;PC.TArob]) with
	(true,start) ->
	  let parse_start start =
	    let rev = List.rev start in
	    let (arob,_) = List.hd rev in
	    (arob = PC.TArob,List.rev(List.tl rev)) in
	  let (starts_with_name,start) = parse_start start in
	  let rec loop starts_with_name start =
	    (!D.init_rule)();
	    (* get metavariable declarations - have to be read before the
	       rest *)
	    let (rule_name,_,_,_,_,_) =
              match get_rule_name PC.iso_rule_name starts_with_name get_tokens
		file ("iso file "^file) with
                Ast.CocciRulename (Some n,a,b,c,d,e) -> (n,a,b,c,d,e)
              | _ -> failwith "Script rules cannot appear in isomorphism rules"
              in
	    Ast0.rule_name := rule_name;
	    let iso_metavars =
	      let res =
		get_metavars PC.iso_meta_main table file lexbuf rule_name
		  false in
	      match res with
		(iso_metavars,[]) -> iso_metavars
	      | _ -> failwith "unexpected inheritance in iso" in
	    (* get the rule *)
	    let (more,tokens) =
	      get_tokens
		(in_list
		   [PC.TIsoStatement;PC.TIsoExpression;PC.TIsoArgExpression;
		     PC.TIsoTestExpression; PC.TIsoToTestExpression;
		     PC.TIsoDeclaration;PC.TIsoType;PC.TIsoTopLevel]) in
	    let next_start = List.hd(List.rev tokens) in
	    let dummy_info = ("",(-1,-1),(-1,-1)) in
	    let tokens = drop_last [(PC.EOF,dummy_info)] tokens in
	    let tokens = consume_minus_positions tokens in
	    let tokens = prepare_tokens false (start@tokens) in
            (*
	       print_tokens "iso tokens" tokens;
	    *)
	    let entry = parse_one "iso main" PC.iso_main file tokens in
	    let entry = List.map (List.map Test_exps.process_anything) entry in
	    let entry = List.map (List.map Get_metas.process_anything) entry in
	    if more
	    then (* The code below allows a header like Statement list,
		    which is more than one word.  We don't have that any more,
		    but the code is left here in case it is put back. *)
	      match get_tokens (in_list [PC.TArobArob;PC.TArob]) with
		(true,start) ->
		  let (starts_with_name,start) = parse_start start in
		  (iso_metavars,entry,rule_name) ::
		  (loop starts_with_name (next_start::start))
	      |	_ -> failwith "isomorphism ends early"
	    else [(iso_metavars,entry,rule_name)] in
	  loop starts_with_name start
      | (false,_) -> [] in
    List.iter Iso_compile.process res;
    res)

let parse_iso_files existing_isos iso_files extra_path =
  let get_names = List.map (function (_,_,nm) -> nm) in
  let old_names = get_names existing_isos in
  D.in_iso := true;
  let (res,_) =
    List.fold_left
      (function (prev,names) ->
	function file ->
	  let file =
	    match file with
	      Common.Left(fl)  -> Filename.concat extra_path fl
	    | Common.Right(fl) -> Filename.concat Config.path fl in
	  Lexer_cocci.init ();
	  let current = parse_iso file in
	  let new_names = get_names current in
	  if List.exists (function x -> List.mem x names) new_names
	  then failwith (Printf.sprintf "repeated iso name found in %s" file);
	  (current::prev,new_names @ names))
      ([],old_names) iso_files in
  D.in_iso := false;
  existing_isos@(List.concat (List.rev res))

(* None = dependency not satisfied
   Some dep = dependency satisfied or unknown and dep has virts optimized
   away *)
let eval_depend nofiles dep virt =
  let rec loop dep =
    match dep with
      Ast.Dep req | Ast.EverDep req ->
	if List.mem req virt
	then
	  if List.mem req !Flag.defined_virtual_rules
	  then Common.Left (Ast.NoDep)
	  else Common.Left (Ast.FailDep)
	else Common.Right dep
    | Ast.AntiDep antireq | Ast.NeverDep antireq ->
	if List.mem antireq virt
	then
	  if not(List.mem antireq !Flag.defined_virtual_rules)
	  then Common.Left (Ast.NoDep)
	  else Common.Left (Ast.FailDep)
	else Common.Right dep
    | Ast.AndDep(d1,d2) ->
	(match (loop d1, loop d2) with
	  (Common.Left Ast.NoDep,x) | (x,Common.Left Ast.NoDep) -> x
	| (Common.Left Ast.FailDep,x) | (x,Common.Left Ast.FailDep) ->
	    Common.Left Ast.FailDep
	| (Common.Right x,Common.Right y) -> Common.Right (Ast.AndDep(x,y))
	| _ -> failwith "not possible")
    | Ast.OrDep(d1,d2) ->
	(match (loop d1, loop d2) with
	  (Common.Left Ast.NoDep,x) | (x,Common.Left Ast.NoDep) ->
	    Common.Left Ast.NoDep
	| (Common.Left Ast.FailDep,x) | (x,Common.Left Ast.FailDep) -> x
	| (Common.Right x,Common.Right y) -> Common.Right (Ast.OrDep(x,y))
	| _ -> failwith "not possible")
    | Ast.FileIn s | Ast.NotFileIn s ->
	if nofiles
	then failwith "file dependencies not allowed in script rules"
	else Common.Right dep in
  match dep with
    Ast.NoDep | Ast.FailDep -> dep
  | Ast.ExistsDep d ->
      (match loop d with
	Common.Left d -> d
      | Common.Right d -> Ast.ExistsDep d)
  | Ast.ForallDep d ->
      (match loop d with
	Common.Left d -> d
      | Common.Right d -> Ast.ForallDep d)

let print_dep_image name deps virt depimage =
  Printf.fprintf stderr "Rule: %s\n" name;
  Printf.fprintf stderr "Dependencies: %s\n"
    (Common.format_to_string
       (function _ -> Pretty_print_cocci.dependency deps));
  Format.print_newline();
  Printf.fprintf stderr "Virtual rules: %s\n" (String.concat " " virt);
  Printf.fprintf stderr "Res: %s\n\n"
    (Common.format_to_string
       (function _ -> Pretty_print_cocci.dependency depimage))

let parse file =
  D.constraint_scripts := [];
  Lexer_cocci.init ();
  let rec parse_loop file =
  Lexer_cocci.include_init ();
  let table = Common.full_charpos_to_pos file in
  Common.with_open_infile file (fun channel ->
  Lexer_cocci.file := file;
  let lexbuf = Lexing.from_channel channel in
  let get_tokens = tokens_all table file false lexbuf in
  D.in_prolog := true;
  let initial_tokens = get_tokens (in_list [PC.TArobArob;PC.TArob]) in
  D.in_prolog := false;
  let res =
    match initial_tokens with
    (true,data) ->
      (match List.rev data with
	((PC.TArobArob as x),_)::_ | ((PC.TArob as x),_)::_ ->
	  let include_and_iso_files =
	    parse_one "include and iso file names" PC.include_main file data in

	  let (include_files,iso_files,virt) =
	    List.fold_left
	      (function (include_files,iso_files,virt) ->
		function
		    D.Include s -> (s::include_files,iso_files,virt)
		  | D.Iso s -> (include_files,s::iso_files,virt)
		  | D.Virt l -> (include_files,iso_files,l@virt))
	      ([],[],[]) include_and_iso_files in

	  List.iter (function x -> Hashtbl.add Lexer_cocci.rule_names x ())
	    virt;

	  let (extra_iso_files, extra_rules, extra_virt, extra_metas) =
	    let rec loop = function
		[] -> ([],[],[],[])
	      |	(a,b,c,d)::rest ->
		  let (x,y,z,zz) = loop rest in
		  (a::x,b::y,c::z,d@zz) in
	    loop (List.map parse_loop include_files) in

          let parse_cocci_rule ruletype old_metas
	      (rule_name, dependencies, iso, dropiso, exists, is_expression) =
	    let dropiso = !Flag_parsing_cocci.disabled_isos @ dropiso in
            Ast0.rule_name := rule_name;
            D.inheritable_positions :=
		rule_name :: !D.inheritable_positions;

            (* get metavariable declarations *)
            let (metavars, inherited_metavars) =
	      get_metavars PC.meta_main table file lexbuf rule_name true in
	    Hashtbl.add Lexer_cocci.rule_names rule_name ();
	    Hashtbl.add Lexer_cocci.all_metavariables rule_name
	      (Hashtbl.fold
		 (fun key v rest -> (key,v)::rest)
		 Lexer_cocci.metavariables []);

            (* get transformation rules *)
            let (more, tokens) =
	      get_tokens (in_list [PC.TArobArob; PC.TArob]) in

            (* remove whitespaces and glue to immediately following tokens *)
            let tokens = process_whitespaces tokens in

            let (minus_tokens, _) = split_token_stream tokens in
            let (_, plus_tokens) =
	      split_token_stream (minus_to_nothing tokens) in
            (*
	       print_tokens "minus tokens" minus_tokens;
	       print_tokens "plus tokens" plus_tokens;
            *)
	    let minus_tokens = consume_minus_positions minus_tokens in
	    let plus_tokens = consume_plus_positions plus_tokens in
	    let minus_tokens = prepare_tokens false minus_tokens in
	    let plus_tokens = prepare_tokens true plus_tokens in

	    (*
	       print_tokens "minus tokens" minus_tokens;
	       print_tokens "plus tokens" plus_tokens;
	    *)

            let plus_tokens =
	      process_pragmas None []
		(fix (function x -> drop_double_dots (drop_empty_or x))
		   (drop_when plus_tokens)) in
	    (*
               print_tokens "plus tokens" plus_tokens;
	       Printf.printf "before minus parse\n";
	    *)
	    Flag_parsing_cocci.in_minus := true;
	    let minus_res =
	      let minus_parser =
		match is_expression with
		  Ast.AnyP -> PC.minus_main
		| Ast.TyP -> PC.minus_ty_main
		| Ast.IdP -> PC.minus_id_main
		| Ast.ExpP -> PC.minus_exp_main in
	      parse_one "minus" minus_parser file minus_tokens in
	    (*
	       Unparse_ast0.unparse minus_res;
	       Printf.printf "before plus parse\n";
	    *)
	    let plus_res =
	      (* put ignore_patch_or_match with * case, which is less
		 constraining *)
	      if !Flag.sgrep_mode2 || !D.ignore_patch_or_match
	      then (* not actually used for anything, except context_neg *)
		List.map
		  (Iso_pattern.rebuild_mcode None).VT0.rebuilder_rec_top_level
		  (Top_level.top_level false minus_res)
	      else
		begin
		  Flag_parsing_cocci.in_minus := false;
		  let plus_parser =
		    match is_expression with
		      Ast.AnyP -> PC.plus_main
		    | Ast.TyP -> PC.plus_ty_main
		    | Ast.IdP -> PC.plus_id_main
		    | Ast.ExpP -> PC.plus_exp_main in
		  parse_one "plus" plus_parser file plus_tokens
		end in
	    let plus_res = Top_level.top_level false plus_res in
	    (* minus code has to be CODE if the + code is CODE, otherwise
	       doesn't matter if + code is CODE or DECL or TOPCODE *)
	    let minus_res =
	      let any_code =
		List.exists
		  (function x ->
		    match Ast0.unwrap x with Ast0.CODE _ -> true | _ -> false)
		  plus_res in
	      if any_code
	      then Top_level.top_level true minus_res
	      else Top_level.top_level false minus_res in
	    let minus_res = Top_level.clean minus_res in
	    let plus_res = Top_level.clean plus_res in
	    (*
	       Unparse_ast0.unparse plus_res;
	       Printf.printf "after plus parse\n";
	    *)

	    (if not !Flag.sgrep_mode2 &&
	      (any_modif minus_res || any_modif plus_res) &&
	      not(dependencies = Ast.FailDep)
	    then D.inheritable_positions := []);

	    if not(dependencies = Ast.FailDep)
	    then
	      Check_meta.check_meta rule_name old_metas inherited_metavars
		metavars minus_res plus_res;

            (more,
	     Ast0.CocciRule((minus_res, metavars,
			     (iso, dropiso, dependencies, rule_name, exists)),
			    (plus_res, metavars), inherited_metavars, ruletype),
	     metavars, tokens) in

          let parse_any_script_rule meta_parser builder
	      name language old_metas deps =
	    Lexer_script.language := language;
	    let pos = (!Lexer_cocci.file, !Lexer_cocci.line) in
            let get_tokens = tokens_script_all table file false lexbuf in

              (* meta-variables *)
            let metavars =
	      D.call_in_meta
		(function _ ->
		  get_script_metavars meta_parser table file lexbuf) in
	    let (metavars,script_metavars) =
	      List.fold_left
		(function (metavars,script_metavars) ->
		  function
		      (script_var,Some(parent,var),initval) ->
			((script_var,parent,var,initval) :: metavars,
			 script_metavars)
		    | ((Some script_var,None),None,_initval) ->
			(metavars, (name,script_var) :: script_metavars)
		    | _ -> failwith "not possible")
		([],[]) metavars in
	    let metavars = List.rev metavars in
	    let script_metavars = List.rev script_metavars in
	    (* No idea whether any vars are position vars, but if there are
	       any, they can be inherited. Probably provides a way of
	       laundering positions over changes. *)
            D.inheritable_positions :=
		name :: !D.inheritable_positions;

	    Hashtbl.add D.all_metadecls name
	      (ref
		 (List.map (function x -> Ast.MetaScriptDecl(ref None,x))
		    script_metavars));
	    Hashtbl.add Lexer_cocci.rule_names name ();
	    (*TODOHashtbl.add Lexer_cocci.all_metavariables name script_metavars;*)

(*
            let exists_in old_metas (py,(r,m)) =
	      r = "virtual" or
              let test (rr,mr) x =
                let (ro,vo) = Ast.get_meta_name x in
                ro = rr && vo = mr in
              List.exists (test (r,m)) old_metas in

	    List.iter
	      (function x ->
		let meta2c (r,n) = Printf.sprintf "%s.%s" r n in
		if not (exists_in old_metas x) then
		  failwith
		    (Printf.sprintf
		       "Script references unknown meta-variable: %s"
		       (meta2c(snd x))))
	      metavars;
*)
              (* script code *)
            let (more, tokens) =
	      get_tokens (in_list [PC.TArobArob; PC.TArob]) in
            let data = collect_script_tokens tokens in
            (more,
	     builder
	       (name, language, deps, metavars, script_metavars, pos, data),
	     [],tokens) in

	  let parse_script_rule =
	    parse_any_script_rule PC.script_meta_main
	      (function (name, language, deps, mvs, script_mvs, pos, data) ->
		Ast0.ScriptRule(name,language,deps,mvs,script_mvs,pos,data)) in

	  let parse_iscript_rule =
	    parse_any_script_rule PC.script_meta_virt_nofresh_main
	      (function (name, language, deps, mvs, script_mvs, pos, data) ->
		match script_mvs with
		  [] ->
		    Ast0.InitialScriptRule(name,language,deps,mvs,pos,data)
		| _ ->
		    failwith "new metavariables not allowed in initalize") in

	  let parse_fscript_rule =
	    parse_any_script_rule PC.script_meta_virt_nofresh_main
	      (function (name, language, deps, mvs, script_mvs, pos, data) ->
		match script_mvs with
		  [] ->
		    Ast0.FinalScriptRule(name,language,deps,mvs,pos,data)
		| _ -> failwith "new metavariables not allowed in finalize") in

	  let do_parse_script_rule fn name l old_metas deps =
            (* in generating mode, we want to keep all the dependencies *)
	    let depimage =
	      if !Flag_parsing_cocci.generating_mode
	      then deps
              else eval_depend true deps virt in
	    (if !Flag_parsing_cocci.debug_parse_cocci
	    then print_dep_image name deps virt depimage);
	    fn name l old_metas depimage in

          let parse_rule old_metas starts_with_name =
            let rulename =
	      get_rule_name PC.rule_name starts_with_name get_tokens file
		"rule" in
            match rulename with
              Ast.CocciRulename (Some s, dep, b, c, d, e) ->
                (* in generating mode, keep all dependencies *)
		let depimage =
		  if !Flag_parsing_cocci.generating_mode
		  then dep
		  else eval_depend false dep virt in
		(if !Flag_parsing_cocci.debug_parse_cocci
		then print_dep_image s dep virt depimage);
		(match depimage with
		  Ast.FailDep ->
		    (*parsing faildep code allows getting some warnings on it*)
		    D.ignore_patch_or_match := true;
                    let res =
		      parse_cocci_rule Ast.Normal old_metas
			(s, Ast.FailDep, b, c, d, e) in
		    D.ignore_patch_or_match := false;
		    res
		| dep -> parse_cocci_rule Ast.Normal old_metas (s,dep,b,c,d,e))
            | Ast.GeneratedRulename (Some s, dep, b, c, d, e) ->
		(match eval_depend true dep virt with
		  Ast.FailDep ->
		    D.ignore_patch_or_match := true;
		    D.in_generating := true;
                    let res =
		      parse_cocci_rule Ast.Generated old_metas
			(s, Ast.FailDep, b, c, d, e) in
		    D.ignore_patch_or_match := false;
		    D.in_generating := false;
		    res
		| dep ->
		    D.in_generating := true;
		    let res =
		      parse_cocci_rule Ast.Generated old_metas
			(s,dep,b,c,d,e) in
		    D.in_generating := false;
		    res)
            | Ast.ScriptRulename(Some s,l,deps) ->
		do_parse_script_rule parse_script_rule s l old_metas deps
            | Ast.InitialScriptRulename(Some s,l,deps) ->
		do_parse_script_rule parse_iscript_rule s l old_metas deps
            | Ast.FinalScriptRulename(Some s,l,deps)   ->
		do_parse_script_rule parse_fscript_rule s l old_metas deps
            | _ -> failwith "Malformed rule name" in

	  let rec loop old_metas starts_with_name =
	    (!D.init_rule)();

            let gen_starts_with_name more tokens =
              more &&
              (match List.hd (List.rev tokens) with
                    (PC.TArobArob,_) -> false
                  | (PC.TArob,_) -> true
                  | _ -> failwith "unexpected token")
            in

            let (more, rule, metavars, tokens) =
              parse_rule old_metas starts_with_name in
	    let all_metas = metavars @ old_metas in

            if more then
	      let (all_rules,all_metas) =
		loop all_metas (gen_starts_with_name more tokens) in
	      (rule::all_rules,all_metas)
            else ([rule],all_metas) in

	  let (all_rules,all_metas) =
	    loop extra_metas (x = PC.TArob) in

	  (List.fold_left
	     (function prev -> function cur -> Common.union_set cur prev)
	     iso_files extra_iso_files,
	   (* included rules first *)
	   List.fold_left (function prev -> function cur -> cur@prev)
	     all_rules (List.rev extra_rules),
	   List.fold_left (@) virt extra_virt (*no dups allowed*),
	   (all_metas : 'a list))
      |	_ -> failwith "unexpected code before the first rule\n")
  | (false,[(PC.TArobArob,_)]) | (false,[(PC.TArob,_)]) ->
      ([],([] : Ast0.parsed_rule list),[] (*virtual rules*), [] (*all metas*))
  | _ ->
      failwith
	(Printf.sprintf "unexpected code before the first rule: %s\n"
	   (Dumper.dump initial_tokens)) in
  res) in
  let res = parse_loop file in
  Lexer_cocci.post_init ();
  res

let contains_modifs ast =
  let donothing r k e = k e in
  let bind x y = x || y in
  let option_default = false in
  let mcode r mc =
    match Ast.get_mcodekind mc with
      Ast.CONTEXT _ -> false
    | Ast.MINUS _ | Ast.PLUS _ -> true in
  let recursor = Visitor_ast.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing
      donothing donothing in
  List.exists
    (function
	Ast.CocciRule(nm,infos,ast,_,_) ->
	  List.exists recursor.Visitor_ast.combiner_top_level ast
      | _ -> false)
    ast

(* parse to ast0 and then convert to ast *)
let process file isofile verbose =
  Parse_aux.contains_string_constant := false;
  let extra_path = Filename.dirname file in
  Lexer_cocci.spinit();
  let (iso_files, rules, virt, _metas) = parse file in
  eval_virt virt;
  let std_isos =
    match isofile with
      None -> []
    | Some iso_file -> parse_iso_files [] [Common.Left iso_file] "" in
  let global_isos = parse_iso_files std_isos iso_files extra_path in
  let rules = Unitary_ast0.do_unitary rules in
  let (dropped,parsed,scripts) =
    List.fold_left
      (function (dropped,prev,scripts) ->
	function
            Ast0.ScriptRule (a,b,dep,params,fv,e,f) ->
	      let rule = Ast.ScriptRule (a,b,dep,params,fv,e,f) in
	      if dep = Ast.FailDep
	      then (a::dropped,prev,rule::scripts)
	      else
		let undefined =
		  List.exists
		    (function
			(_,(nm,_),_,Ast.NoMVInit) -> List.mem nm dropped
		      | _ -> false)
		    params in
		if undefined
		then (a::dropped,prev,rule::scripts)
		else (dropped,[([],rule)]::prev,rule::scripts)
	  | Ast0.InitialScriptRule(a,b,c,d,e,f) ->
	      let rule = Ast.InitialScriptRule (a,b,c,d,e,f) in
	      (dropped,[([],rule)]::prev,rule::scripts)
	  | Ast0.FinalScriptRule (a,b,c,d,e,f) ->
	      let rule = Ast.FinalScriptRule (a,b,c,d,e,f) in
	      (dropped,[([],rule)]::prev,rule::scripts)
	  | Ast0.CocciRule
	      ((minus, metavarsm,
		(iso, dropiso, dependencies, rule_name, exists)),
	       (plus, metavars),_inh,ruletype) ->
		 let chosen_isos =
		   parse_iso_files global_isos
		     (List.map (function x -> Common.Left x) iso)
		     extra_path in
		 let chosen_isos =
            (* check that dropped isos are actually available *)
		   (try
		     let iso_names =
		       List.map (function (_,_,nm) -> nm) chosen_isos in
		     let local_iso_names = reserved_names @ iso_names in
		     let bad_dropped =
		       List.find
			 (function dropped ->
			   not (List.mem dropped local_iso_names))
			 dropiso in
		     failwith
		       ("invalid iso name " ^ bad_dropped ^ " in " ^ rule_name)
		   with Not_found -> ());
		   if List.mem "all" dropiso
		   then
		     if List.length dropiso = 1
		     then []
		     else failwith "disable all should only be by itself"
		   else (* drop those isos *)
		     List.filter
		       (function (_,_,nm) -> not (List.mem nm dropiso))
		       chosen_isos in
		 let dropped_isos =
		   match reserved_names with
		     "all"::others ->
		       (match dropiso with
			 ["all"] -> others
		       | _ ->
			   List.filter (function x -> List.mem x dropiso)
			     others)
		   | _ ->
		       failwith
			 "bad list of reserved names - all must be at start" in
		 let minus = Test_exps.process minus in
		 let minus = Compute_lines.compute_lines false minus in
		 let plus = Compute_lines.compute_lines false plus in
		 let is_exp =
		 (* only relevant to Flag.make_hrule *)
		 (* doesn't handle multiple minirules properly, but since
		    we don't really handle them in lots of other ways, it
		    doesn't seem very important *)
		   match plus with
		     [] -> [false]
		   | p::_ ->
		       [match Ast0.unwrap p with
			 Ast0.CODE c ->
			   (match List.map Ast0.unwrap (Ast0.unwrap c) with
			     [Ast0.Exp e] -> true | _ -> false)
		       | _ -> false] in
		 let minus = Arity.minus_arity minus in
		 let plus = Adjust_pragmas.process plus in
		 let ((metavars,minus),function_prototypes) =
		   Function_prototypes.process
		     rule_name metavars dropped_isos minus plus ruletype in
          (* warning! context_neg side-effects its arguments *)
		 let (m,p) = List.split (Context_neg.context_neg minus plus) in
		 Type_infer.type_infer p;
		 (if not (!Flag.sgrep_mode2 || dependencies = Ast.FailDep)
		 then Insert_plus.insert_plus m p (chosen_isos = []));
		 Type_infer.type_infer minus;
		 let (extra_meta, minus) =
		   match (chosen_isos,ruletype) with
		   (* separate case for [] because applying isos puts
		      some restrictions on the -+ code *)
		     ([],_) | (_,Ast.Generated) -> ([],minus)
		   | _ -> Iso_pattern.apply_isos chosen_isos minus rule_name in
	       (* must be before adj *)
		 let minus = Commas_on_lists.process minus in
	       (* after iso, because iso can intro ... *)
		 let minus = Adjacency.compute_adjacency minus in
		 let minus = Comm_assoc.comm_assoc minus rule_name dropiso in
		 let minus =
		   if !Flag.sgrep_mode2 then minus
		   else Single_statement.single_statement minus in
		 let minus = Simple_assignments.simple_assignments minus in
	       (* has to be last, introduced AsExpr, etc *)
		 let minus = Get_metas.process minus in
		 let minus_ast =
		   Ast0toast.ast0toast rule_name dependencies dropped_isos
		     exists minus is_exp ruletype in
		 let minus_ast = Stmtlist.stmtlist minus_ast in

		 if dependencies = Ast.FailDep
		 then (rule_name::dropped,prev,scripts)
		 else
		   match function_prototypes with
		     None ->
		       (dropped,[(extra_meta @ metavars, minus_ast)]::prev,
			scripts)
		   | Some mv_fp ->
		       (dropped,
			[(extra_meta @ metavars, minus_ast); mv_fp]::prev,
			scripts))
(*          Ast0.CocciRule ((minus, metavarsm, (iso, dropiso, dependencies, rule_name, exists)), (plus, metavars))*)
      ([],[],[]) rules in

  let parsed = List.concat (List.rev parsed) in
  let scripts = List.rev scripts in
  let (parsed,dropped) = Cleanup_rules.cleanup_rules parsed dropped in
  let parsed = Safe_for_multi_decls.safe_for_multi_decls parsed in
  let disjd = Disjdistr.disj parsed in

  let (metavars,code,fvs,neg_pos,ua,pos) = Free_vars.free_vars disjd dropped in
  let code = Re_constraints.re_constraints code in
  if !Flag_parsing_cocci.show_SP
  then List.iter2 Pretty_print_cocci.unparse metavars code;

  let search_tokens = Get_constants2.get_constants code neg_pos virt in

  (metavars,code,scripts,fvs,neg_pos,ua,pos,search_tokens,
   !Parse_aux.contains_string_constant,contains_modifs code)
