(* splits the entire file into minus and plus fragments, and parses each
separately (thus duplicating work for the parsing of the context elements) *)

module D = Data
module PC = Parser_cocci_menhir
module V0 = Visitor_ast0
let pr = Printf.sprintf
(*let pr2 s = prerr_string s; prerr_string "\n"; flush stderr*)
let pr2 s = Printf.printf "%s\n" s

(* for isomorphisms.  all should be at the front!!! *)
let reserved_names = ["all";"optional_storage";"optional_qualifier"]

(* ----------------------------------------------------------------------- *)
(* Debugging... *)

let line_type (d,_,_,_,_,_,_) = d

let line_type2c tok =
  match line_type tok with
    D.MINUS | D.OPTMINUS | D.UNIQUEMINUS | D.MULTIMINUS -> ":-"
  | D.PLUS -> ":+"
  | D.CONTEXT | D.UNIQUE | D.OPT | D.MULTI -> ""

let token2c (tok,_) =
 match tok with
    PC.TIdentifier -> "identifier"
  | PC.TType -> "type"
  | PC.TParameter -> "parameter"
  | PC.TConstant -> "constant"
  | PC.TExpression -> "expression"
  | PC.TStatement -> "statement"
  | PC.TFunction -> "function"
  | PC.TLocal -> "local"
  | PC.Tlist -> "list"
  | PC.TFresh -> "fresh"
  | PC.TPure -> "pure"
  | PC.TContext -> "context"
  | PC.TTypedef -> "typedef"
  | PC.TDeclarer -> "declarer"
  | PC.TIterator -> "iterator"
  | PC.TRuleName str -> "rule_name-"^str
  | PC.TUsing -> "using"
  | PC.TPathIsoFile str -> "path_iso_file-"^str
  | PC.TDisable -> "disable"
  | PC.TExtends -> "extends"
  | PC.TDepends -> "depends"
  | PC.TOn -> "on"
  | PC.TEver -> "ever"
  | PC.TNever -> "never"
  | PC.TError -> "error"
  | PC.TWords -> "words"

  | PC.TNothing -> "nothing"

  | PC.Tchar(clt) -> "char"^(line_type2c  clt)
  | PC.Tshort(clt) -> "short"^(line_type2c clt)
  | PC.Tint(clt) -> "int"^(line_type2c clt)
  | PC.Tdouble(clt) -> "double"^(line_type2c clt)
  | PC.Tfloat(clt) -> "float"^(line_type2c clt)
  | PC.Tlong(clt) -> "long"^(line_type2c clt)
  | PC.Tvoid(clt) -> "void"^(line_type2c clt)
  | PC.Tstruct(clt) -> "struct"^(line_type2c clt)
  | PC.Tunion(clt) -> "union"^(line_type2c clt)
  | PC.Tunsigned(clt) -> "unsigned"^(line_type2c clt)
  | PC.Tsigned(clt) -> "signed"^(line_type2c clt)
  | PC.Tstatic(clt) -> "static"^(line_type2c clt)
  | PC.Tinline(clt) -> "inline"^(line_type2c clt)
  | PC.Tattr(s,clt) -> s^(line_type2c clt)
  | PC.Tauto(clt) -> "auto"^(line_type2c clt)
  | PC.Tregister(clt) -> "register"^(line_type2c clt)
  | PC.Textern(clt) -> "extern"^(line_type2c clt)
  | PC.Tconst(clt) -> "const"^(line_type2c clt)
  | PC.Tvolatile(clt) -> "volatile"^(line_type2c clt)

  | PC.TPragma(s) -> s
  | PC.TIncludeL(s,clt) -> (pr "#include \"%s\"" s)^(line_type2c clt)
  | PC.TIncludeNL(s,clt) -> (pr "#include <%s>" s)^(line_type2c clt)
  | PC.TDefine(clt,_) -> "#define"^(line_type2c clt)
  | PC.TDefineParam(clt,_,_) -> "#define_param"^(line_type2c clt)
  | PC.TMinusFile(s,clt) -> (pr "--- %s" s)^(line_type2c clt)
  | PC.TPlusFile(s,clt) -> (pr "+++ %s" s)^(line_type2c clt)

  | PC.TInc(clt) -> "++"^(line_type2c clt)
  | PC.TDec(clt) -> "--"^(line_type2c clt)
	
  | PC.TIf(clt) -> "if"^(line_type2c clt)
  | PC.TElse(clt) -> "else"^(line_type2c clt)
  | PC.TWhile(clt) -> "while"^(line_type2c clt)
  | PC.TFor(clt) -> "for"^(line_type2c clt)
  | PC.TDo(clt) -> "do"^(line_type2c clt)
  | PC.TSwitch(clt) -> "switch"^(line_type2c clt)
  | PC.TCase(clt) -> "case"^(line_type2c clt)
  | PC.TDefault(clt) -> "default"^(line_type2c clt)
  | PC.TReturn(clt) -> "return"^(line_type2c clt)
  | PC.TBreak(clt) -> "break"^(line_type2c clt)
  | PC.TContinue(clt) -> "continue"^(line_type2c clt)
  | PC.TIdent(s,clt) -> (pr "ident-%s" s)^(line_type2c clt)
  | PC.TTypeId(s,clt) -> (pr "typename-%s" s)^(line_type2c clt)
  | PC.TDeclarerId(s,clt) -> (pr "declarername-%s" s)^(line_type2c clt)
  | PC.TIteratorId(s,clt) -> (pr "iteratorname-%s" s)^(line_type2c clt)

  | PC.TSizeof(clt) -> "sizeof"^(line_type2c clt)

  | PC.TString(x,clt) -> x^(line_type2c clt)
  | PC.TChar(x,clt) -> x^(line_type2c clt)
  | PC.TFloat(x,clt) -> x^(line_type2c clt)
  | PC.TInt(x,clt) -> x^(line_type2c clt)

  | PC.TOrLog(clt) -> "||"^(line_type2c clt)
  | PC.TAndLog(clt) -> "&&"^(line_type2c clt)
  | PC.TOr(clt) -> "|"^(line_type2c clt)
  | PC.TXor(clt) -> "^"^(line_type2c clt)
  | PC.TAnd (clt) -> "&"^(line_type2c clt)
  | PC.TEqEq(clt) -> "=="^(line_type2c clt)
  | PC.TNotEq(clt) -> "!="^(line_type2c clt)
  | PC.TInf(clt) -> "<"^(line_type2c clt)
  | PC.TSup(clt) -> ">"^(line_type2c clt)
  | PC.TInfEq(clt) -> "<="^(line_type2c clt)
  | PC.TSupEq (clt) -> ">="^(line_type2c clt)
  | PC.TShl(clt) -> "<<"^(line_type2c clt)
  | PC.TShr(clt) -> ">>"^(line_type2c clt)
  | PC.TPlus(clt) -> "+"^(line_type2c clt)
  | PC.TMinus(clt) -> "-"^(line_type2c clt)
  | PC.TMul(clt) -> "*"^(line_type2c clt)
  | PC.TDiv(clt) -> "/"^(line_type2c clt)
  | PC.TMod (clt) -> "%"^(line_type2c clt)
  | PC.TTilde (clt) -> "~"^(line_type2c clt)

  | PC.TMetaParam(_,_,clt) -> "parammeta"^(line_type2c clt)
  | PC.TMetaParamList(_,_,clt) -> "paramlistmeta"^(line_type2c clt)
  | PC.TMetaConst(_,_,_,clt) -> "constmeta"^(line_type2c clt)
  | PC.TMetaErr(_,_,clt) -> "errmeta"^(line_type2c clt)
  | PC.TMetaExp(_,_,_,clt) -> "expmeta"^(line_type2c clt)
  | PC.TMetaExpList(_,_,clt) -> "explistmeta"^(line_type2c clt)
  | PC.TMetaId(_,_,clt)    -> "idmeta"^(line_type2c clt)
  | PC.TMetaType(_,_,clt)    -> "typemeta"^(line_type2c clt)
  | PC.TMetaStm(_,_,clt)   -> "stmmeta"^(line_type2c clt)
  | PC.TMetaStmList(_,_,clt)   -> "stmlistmeta"^(line_type2c clt)
  | PC.TMetaFunc(_,_,clt)  -> "funcmeta"^(line_type2c clt)
  | PC.TMetaLocalFunc(_,_,clt) -> "funcmeta"^(line_type2c clt)
  | PC.TMPtVirg -> ";"
  | PC.TArobArob -> "@@"
  | PC.TArob -> "@"

  | PC.TWhen(clt) -> "WHEN"^(line_type2c clt)
  | PC.TEllipsis(clt) -> "..."^(line_type2c clt)
(*
  | PC.TCircles(clt)  -> "ooo"^(line_type2c clt)
  | PC.TStars(clt)    -> "***"^(line_type2c clt)
*)

  | PC.TOEllipsis(clt) -> "<..."^(line_type2c clt)
  | PC.TCEllipsis(clt) -> "...>"^(line_type2c clt)
(*
  | PC.TOCircles(clt)  -> "<ooo"^(line_type2c clt)
  | PC.TCCircles(clt)  -> "ooo>"^(line_type2c clt)
  | PC.TOStars(clt)    -> "<***"^(line_type2c clt)
  | PC.TCStars(clt)    -> "***>"^(line_type2c clt)
*)
  | PC.TBang0 -> "!"
  | PC.TPlus0 -> "+"
  | PC.TWhy0  -> "?"

  | PC.TWhy(clt)   -> "?"^(line_type2c clt)
  | PC.TDotDot(clt)   -> ":"^(line_type2c clt)
  | PC.TBang(clt)  -> "!"^(line_type2c clt)
  | PC.TOPar(clt)  -> "("^(line_type2c clt)
  | PC.TOPar0(clt) -> "("^(line_type2c clt)
  | PC.TMid0(clt)  -> "|"^(line_type2c clt)
  | PC.TCPar(clt)  -> ")"^(line_type2c clt)
  | PC.TCPar0(clt) -> ")"^(line_type2c clt)

  | PC.TOBrace(clt) -> "{"^(line_type2c clt)
  | PC.TCBrace(clt) -> "}"^(line_type2c clt)
  | PC.TOCro(clt) -> "["^(line_type2c clt)
  | PC.TCCro(clt) -> "]"^(line_type2c clt)

  | PC.TPtrOp(clt) -> "->"^(line_type2c clt)

  | PC.TEq(clt) -> "="^(line_type2c clt)
  | PC.TAssign(_,clt) -> "=op"^(line_type2c clt)
  | PC.TDot(clt) -> "."^(line_type2c clt)
  | PC.TComma(clt) -> ","^(line_type2c clt)
  | PC.TPtVirg(clt) -> ";"^(line_type2c clt)

  | PC.EOF -> "eof"
  | PC.TLineEnd(clt) -> "line end"
  | PC.TInvalid -> "invalid"
  | PC.TFunDecl(clt) -> "fundecl"

  | PC.TIso -> "<=>"
  | PC.TRightIso -> "=>"
  | PC.TIsoTopLevel -> "TopLevel"
  | PC.TIsoExpression -> "Expression"
  | PC.TIsoStatement -> "Statement"
  | PC.TIsoDeclaration -> "Declaration"
  | PC.TIsoType -> "Type"

type plus = PLUS | NOTPLUS | SKIP

let plus_attachable (tok,_) =
  match tok with
    PC.Tchar(clt) | PC.Tshort(clt) | PC.Tint(clt) | PC.Tdouble(clt)
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt) | PC.Tstruct(clt)
  | PC.Tunion(clt) | PC.Tunsigned(clt) | PC.Tsigned(clt) | PC.Tstatic(clt)
  | PC.Tinline(clt) | PC.Tattr(_,clt) | PC.Tauto(clt) | PC.Tregister(clt)
  | PC.Textern(clt) | PC.Tconst(clt) | PC.Tvolatile(clt)

  | PC.TIncludeL(_,clt) | PC.TIncludeNL(_,clt) | PC.TDefine(clt,_)
  | PC.TDefineParam(clt,_,_) | PC.TMinusFile(_,clt) | PC.TPlusFile(_,clt)

  | PC.TInc(clt) | PC.TDec(clt)
	
  | PC.TIf(clt) | PC.TElse(clt) | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt)
  | PC.TSwitch(clt) | PC.TCase(clt) | PC.TDefault(clt) | PC.TReturn(clt)
  | PC.TBreak(clt) | PC.TContinue(clt) | PC.TIdent(_,clt)
  | PC.TTypeId(_,clt) | PC.TDeclarerId(_,clt) | PC.TIteratorId(_,clt)

  | PC.TSizeof(clt)

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt)

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TInf(clt)
  | PC.TSup(clt) | PC.TInfEq(clt) | PC.TSupEq (clt) | PC.TShl(clt)
  | PC.TShr(clt) | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt)
  | PC.TDiv(clt) | PC.TMod (clt) | PC.TTilde (clt)

  | PC.TMetaParam(_,_,clt) | PC.TMetaParamList(_,_,clt)
  | PC.TMetaConst(_,_,_,clt) | PC.TMetaErr(_,_,clt)
  | PC.TMetaExp(_,_,_,clt) | PC.TMetaExpList(_,_,clt)
  | PC.TMetaId(_,_,clt)
  | PC.TMetaType(_,_,clt) | PC.TMetaStm(_,_,clt)  
  | PC.TMetaStmList(_,_,clt)  | PC.TMetaFunc(_,_,clt) 
  | PC.TMetaLocalFunc(_,_,clt)

  | PC.TWhen(clt) | PC.TEllipsis(clt) (* | PC.TCircles(clt) | PC.TStars(clt) *)

  | PC.TWhy(clt) | PC.TDotDot(clt) | PC.TBang(clt) | PC.TOPar(clt) 
  | PC.TCPar(clt)

  | PC.TOBrace(clt) | PC.TCBrace(clt) | PC.TOCro(clt) | PC.TCCro(clt)

  | PC.TPtrOp(clt)

  | PC.TEq(clt) | PC.TAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt)
  | PC.TPtVirg(clt) ->
      if line_type clt = D.PLUS then PLUS else NOTPLUS

  | PC.TOPar0(clt) | PC.TMid0(clt) | PC.TCPar0(clt)
  | PC.TOEllipsis(clt) | PC.TCEllipsis(clt) (* | PC.TOCircles(clt)
  | PC.TCCircles(clt) | PC.TOStars(clt) | PC.TCStars(clt) *) -> NOTPLUS

  | _ -> SKIP

let get_clt (tok,_) =
  match tok with
    PC.Tchar(clt) | PC.Tshort(clt) | PC.Tint(clt) | PC.Tdouble(clt)
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt) | PC.Tstruct(clt)
  | PC.Tunion(clt) | PC.Tunsigned(clt) | PC.Tsigned(clt) | PC.Tstatic(clt)
  | PC.Tinline(clt) | PC.Tattr(_,clt) | PC.Tauto(clt) | PC.Tregister(clt)
  | PC.Textern(clt) | PC.Tconst(clt) | PC.Tvolatile(clt)

  | PC.TIncludeL(_,clt) | PC.TIncludeNL(_,clt) | PC.TDefine(clt,_)
  | PC.TDefineParam(clt,_,_) | PC.TMinusFile(_,clt) | PC.TPlusFile(_,clt)

  | PC.TInc(clt) | PC.TDec(clt)
	
  | PC.TIf(clt) | PC.TElse(clt) | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt)
  | PC.TSwitch(clt) | PC.TCase(clt) | PC.TDefault(clt) | PC.TReturn(clt)
  | PC.TBreak(clt) | PC.TContinue(clt) | PC.TIdent(_,clt)
  | PC.TTypeId(_,clt) | PC.TDeclarerId(_,clt) | PC.TIteratorId(_,clt)

  | PC.TSizeof(clt)

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt)

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TInf(clt)
  | PC.TSup(clt) | PC.TInfEq(clt) | PC.TSupEq (clt) | PC.TShl(clt)
  | PC.TShr(clt) | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt)
  | PC.TDiv(clt) | PC.TMod (clt) | PC.TTilde (clt)

  | PC.TMetaParam(_,_,clt) | PC.TMetaParamList(_,_,clt)
  | PC.TMetaConst(_,_,_,clt) | PC.TMetaErr(_,_,clt)
  | PC.TMetaExp(_,_,_,clt) | PC.TMetaExpList(_,_,clt)
  | PC.TMetaId(_,_,clt)
  | PC.TMetaType(_,_,clt) | PC.TMetaStm(_,_,clt)  
  | PC.TMetaStmList(_,_,clt)  | PC.TMetaFunc(_,_,clt) 
  | PC.TMetaLocalFunc(_,_,clt)

  | PC.TWhen(clt) | PC.TEllipsis(clt) (* | PC.TCircles(clt) | PC.TStars(clt) *)

  | PC.TWhy(clt) | PC.TDotDot(clt) | PC.TBang(clt) | PC.TOPar(clt) 
  | PC.TCPar(clt)

  | PC.TOBrace(clt) | PC.TCBrace(clt) | PC.TOCro(clt) | PC.TCCro(clt)

  | PC.TPtrOp(clt)

  | PC.TEq(clt) | PC.TAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt)
  | PC.TPtVirg(clt)

  | PC.TOPar0(clt) | PC.TMid0(clt) | PC.TCPar0(clt)
  | PC.TOEllipsis(clt) | PC.TCEllipsis(clt) (* | PC.TOCircles(clt)
  | PC.TCCircles(clt) | PC.TOStars(clt) | PC.TCStars(clt) *) -> clt

  | _ -> failwith "no clt"

let update_clt (tok,x) clt =
  match tok with
    PC.Tchar(_) -> (PC.Tchar(clt),x)
  | PC.Tshort(_) -> (PC.Tshort(clt),x)
  | PC.Tint(_) -> (PC.Tint(clt),x)
  | PC.Tdouble(_) -> (PC.Tdouble(clt),x)
  | PC.Tfloat(_) -> (PC.Tfloat(clt),x)
  | PC.Tlong(_) -> (PC.Tlong(clt),x)
  | PC.Tvoid(_) -> (PC.Tvoid(clt),x)
  | PC.Tstruct(_) -> (PC.Tstruct(clt),x)
  | PC.Tunion(_) -> (PC.Tunion(clt),x)
  | PC.Tunsigned(_) -> (PC.Tunsigned(clt),x)
  | PC.Tsigned(_) -> (PC.Tsigned(clt),x)
  | PC.Tstatic(_) -> (PC.Tstatic(clt),x)
  | PC.Tinline(_) -> (PC.Tinline(clt),x)
  | PC.Tattr(s,_) -> (PC.Tattr(s,clt),x)
  | PC.Tauto(_) -> (PC.Tauto(clt),x)
  | PC.Tregister(_) -> (PC.Tregister(clt),x)
  | PC.Textern(_) -> (PC.Textern(clt),x)
  | PC.Tconst(_) -> (PC.Tconst(clt),x)
  | PC.Tvolatile(_) -> (PC.Tvolatile(clt),x)

  | PC.TIncludeL(s,_) -> (PC.TIncludeL(s,clt),x)
  | PC.TIncludeNL(s,_) -> (PC.TIncludeNL(s,clt),x)
  | PC.TDefine(_,a) -> (PC.TDefine(clt,a),x)
  | PC.TDefineParam(_,a,b) -> (PC.TDefineParam(clt,a,b),x)
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
  | PC.TIdent(s,_) -> (PC.TIdent(s,clt),x)
  | PC.TTypeId(s,_) -> (PC.TTypeId(s,clt),x)
  | PC.TDeclarerId(s,_) -> (PC.TDeclarerId(s,clt),x)
  | PC.TIteratorId(s,_) -> (PC.TIteratorId(s,clt),x)

  | PC.TSizeof(_) -> (PC.TSizeof(clt),x)

  | PC.TString(s,_) -> (PC.TString(s,clt),x)
  | PC.TChar(s,_) -> (PC.TChar(s,clt),x)
  | PC.TFloat(s,_) -> (PC.TFloat(s,clt),x)
  | PC.TInt(s,_) -> (PC.TInt(s,clt),x)

  | PC.TOrLog(_) -> (PC.TOrLog(clt),x)
  | PC.TAndLog(_) -> (PC.TAndLog(clt),x)
  | PC.TOr(_) -> (PC.TOr(clt),x)
  | PC.TXor(_) -> (PC.TXor(clt),x)
  | PC.TAnd (_) -> (PC.TAnd (clt),x)
  | PC.TEqEq(_) -> (PC.TEqEq(clt),x)
  | PC.TNotEq(_) -> (PC.TNotEq(clt),x)
  | PC.TInf(_) -> (PC.TInf(clt),x)
  | PC.TSup(_) -> (PC.TSup(clt),x)
  | PC.TInfEq(_) -> (PC.TInfEq(clt),x)
  | PC.TSupEq (_) -> (PC.TSupEq (clt),x)
  | PC.TShl(_) -> (PC.TShl(clt),x)
  | PC.TShr(_) -> (PC.TShr(clt),x)
  | PC.TPlus(_) -> (PC.TPlus(clt),x)
  | PC.TMinus(_) -> (PC.TMinus(clt),x)
  | PC.TMul(_) -> (PC.TMul(clt),x)
  | PC.TDiv(_) -> (PC.TDiv(clt),x)
  | PC.TMod (_) -> (PC.TMod (clt),x)
  | PC.TTilde (_) -> (PC.TTilde (clt),x)

  | PC.TMetaParam(a,b,_) -> (PC.TMetaParam(a,b,clt),x)
  | PC.TMetaParamList(a,b,_) -> (PC.TMetaParamList(a,b,clt),x)
  | PC.TMetaConst(a,b,c,_) -> (PC.TMetaConst(a,b,c,clt),x)
  | PC.TMetaErr(a,b,_) -> (PC.TMetaErr(a,b,clt),x)
  | PC.TMetaExp(a,b,c,_) -> (PC.TMetaExp(a,b,c,clt),x)
  | PC.TMetaExpList(a,b,_) -> (PC.TMetaExpList(a,b,clt),x)
  | PC.TMetaId(a,b,_)    -> (PC.TMetaId(a,b,clt),x)
  | PC.TMetaType(a,b,_)    -> (PC.TMetaType(a,b,clt),x)
  | PC.TMetaStm(a,b,_)   -> (PC.TMetaStm(a,b,clt),x)
  | PC.TMetaStmList(a,b,_)   -> (PC.TMetaStmList(a,b,clt),x)
  | PC.TMetaFunc(a,b,_)  -> (PC.TMetaFunc(a,b,clt),x)
  | PC.TMetaLocalFunc(a,b,_) -> (PC.TMetaLocalFunc(a,b,clt),x)

  | PC.TWhen(_) -> (PC.TWhen(clt),x)
  | PC.TEllipsis(_) -> (PC.TEllipsis(clt),x)
(*
  | PC.TCircles(_)  -> (PC.TCircles(clt),x)
  | PC.TStars(_)    -> (PC.TStars(clt),x)
*)

  | PC.TOEllipsis(_) -> (PC.TOEllipsis(clt),x)
  | PC.TCEllipsis(_) -> (PC.TCEllipsis(clt),x)
(*
  | PC.TOCircles(_)  -> (PC.TOCircles(clt),x)
  | PC.TCCircles(_)  -> (PC.TCCircles(clt),x)
  | PC.TOStars(_)    -> (PC.TOStars(clt),x)
  | PC.TCStars(_)    -> (PC.TCStars(clt),x)
*)

  | PC.TWhy(_)   -> (PC.TWhy(clt),x)
  | PC.TDotDot(_)   -> (PC.TDotDot(clt),x)
  | PC.TBang(_)  -> (PC.TBang(clt),x)
  | PC.TOPar(_)  -> (PC.TOPar(clt),x)
  | PC.TOPar0(_) -> (PC.TOPar0(clt),x)
  | PC.TMid0(_)  -> (PC.TMid0(clt),x)
  | PC.TCPar(_)  -> (PC.TCPar(clt),x)
  | PC.TCPar0(_) -> (PC.TCPar0(clt),x)

  | PC.TOBrace(_) -> (PC.TOBrace(clt),x)
  | PC.TCBrace(_) -> (PC.TCBrace(clt),x)
  | PC.TOCro(_) -> (PC.TOCro(clt),x)
  | PC.TCCro(_) -> (PC.TCCro(clt),x)

  | PC.TPtrOp(_) -> (PC.TPtrOp(clt),x)

  | PC.TEq(_) -> (PC.TEq(clt),x)
  | PC.TAssign(s,_) -> (PC.TAssign(s,clt),x)
  | PC.TDot(_) -> (PC.TDot(clt),x)
  | PC.TComma(_) -> (PC.TComma(clt),x)
  | PC.TPtVirg(_) -> (PC.TPtVirg(clt),x)

  | PC.TLineEnd(_) -> (PC.TLineEnd(clt),x)
  | PC.TFunDecl(_) -> (PC.TFunDecl(clt),x)

  | _ -> failwith "no clt"


(* ----------------------------------------------------------------------- *)

let make_name prefix ln = Printf.sprintf "%s starting on line %d" prefix ln

(* ----------------------------------------------------------------------- *)
(* Read tokens *)

let wrap_lexbuf_info lexbuf =
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)    

let tokens_all table file get_ats lexbuf end_markers :
    (bool * ((PC.token * (string * (int * int) * (int * int))) list)) =
  try 
    let rec aux () = 
      let result = Lexer_cocci.token lexbuf in
      let info = (Lexing.lexeme lexbuf, 
                  (table.(Lexing.lexeme_start lexbuf)),
                  (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) in
      if result = PC.EOF 
      then
	if get_ats
	then failwith "unexpected end of file in a metavariable declaration"
	else (false,[(result,info)])
      else if List.mem result end_markers
      then (true,[(result,info)])
      else
	let (more,rest) = aux() in
	(more,(result, info)::rest)
    in aux () 
  with
    e -> pr2 (Common.error_message file (wrap_lexbuf_info lexbuf) ); raise e

(* ----------------------------------------------------------------------- *)
(* Split tokens into minus and plus fragments *)

let split t = function
    (D.MINUS,_,_,_,_,_,_) | (D.OPTMINUS,_,_,_,_,_,_)
  | (D.UNIQUEMINUS,_,_,_,_,_,_) | (D.MULTIMINUS,_,_,_,_,_,_) -> ([t],[])
  | (D.PLUS,_,_,_,_,_,_) -> ([],[t])
  | (D.CONTEXT,_,_,_,_,_,_) | (D.UNIQUE,_,_,_,_,_,_)
  | (D.OPT,_,_,_,_,_,_) | (D.MULTI,_,_,_,_,_,_) -> ([t],[t])

let split_token ((tok,_) as t) =
  match tok with
    PC.TIdentifier | PC.TConstant | PC.TExpression | PC.TStatement
  | PC.TFunction | PC.TTypedef | PC.TDeclarer | PC.TIterator
  | PC.TType | PC.TParameter | PC.TLocal | PC.Tlist | PC.TFresh | PC.TPure
  | PC.TContext | PC.TRuleName(_) | PC.TUsing | PC.TDisable | PC.TExtends
  | PC.TPathIsoFile(_)
  | PC.TDepends | PC.TOn | PC.TEver | PC.TNever
  | PC.TError | PC.TWords | PC.TNothing -> ([t],[t])

  | PC.Tchar(clt) | PC.Tshort(clt) | PC.Tint(clt) | PC.Tdouble(clt)
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt) | PC.Tstruct(clt)
  | PC.Tunion(clt) | PC.Tunsigned(clt) | PC.Tsigned(clt)
  | PC.Tstatic(clt) | PC.Tauto(clt) | PC.Tregister(clt) | PC.Textern(clt)
  | PC.Tinline(clt) | PC.Tattr(_,clt)
  | PC.Tconst(clt) | PC.Tvolatile(clt) -> split t clt

  | PC.TPragma(s) -> ([],[t]) (* only allowed in + *)
  | PC.TPlusFile(s,clt) | PC.TMinusFile(s,clt)
  | PC.TIncludeL(s,clt) | PC.TIncludeNL(s,clt) ->
      split t clt
  | PC.TDefine(clt,_) | PC.TDefineParam(clt,_,_) -> split t clt

  | PC.TIf(clt) | PC.TElse(clt)  | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt)
  | PC.TSwitch(clt) | PC.TCase(clt) | PC.TDefault(clt)
  | PC.TSizeof(clt)
  | PC.TReturn(clt) | PC.TBreak(clt) | PC.TContinue(clt) | PC.TIdent(_,clt)
  | PC.TTypeId(_,clt) | PC.TDeclarerId(_,clt) | PC.TIteratorId(_,clt)
  | PC.TMetaConst(_,_,_,clt) | PC.TMetaExp(_,_,_,clt)
  | PC.TMetaExpList(_,_,clt)
  | PC.TMetaParam(_,_,clt) | PC.TMetaParamList(_,_,clt)
  | PC.TMetaId(_,_,clt) | PC.TMetaType(_,_,clt)
  | PC.TMetaStm(_,_,clt) | PC.TMetaStmList(_,_,clt) | PC.TMetaErr(_,_,clt)
  | PC.TMetaFunc(_,_,clt) | PC.TMetaLocalFunc(_,_,clt) -> split t clt
  | PC.TMPtVirg | PC.TArob | PC.TArobArob -> ([t],[t])

  | PC.TFunDecl(clt)
  | PC.TWhen(clt) | PC.TLineEnd(clt)
  | PC.TEllipsis(clt) (* | PC.TCircles(clt) | PC.TStars(clt) *) -> split t clt

  | PC.TOEllipsis(_) | PC.TCEllipsis(_) (* clt must be context *)
(*
  | PC.TOCircles(_) | PC.TCCircles(_)   (* clt must be context *)
  | PC.TOStars(_) | PC.TCStars(_)       (* clt must be context *)
*)
  | PC.TBang0 | PC.TPlus0 | PC.TWhy0 ->
      ([t],[t])

  | PC.TWhy(clt)  | PC.TDotDot(clt)
  | PC.TBang(clt) | PC.TOPar(clt) | PC.TOPar0(clt)
  | PC.TMid0(clt) | PC.TCPar(clt) | PC.TCPar0(clt) -> split t clt

  | PC.TInc(clt) | PC.TDec(clt) -> split t clt

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt) ->
      split t clt

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TInf(clt)
  | PC.TSup(clt) | PC.TInfEq(clt) | PC.TSupEq (clt) | PC.TShl(clt)
  | PC.TShr(clt) | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt)
  | PC.TDiv(clt) | PC.TMod (clt) | PC.TTilde (clt) -> split t clt

  | PC.TOBrace(clt) | PC.TCBrace(clt) -> split t clt
  | PC.TOCro(clt) | PC.TCCro(clt) -> split t clt

  | PC.TPtrOp(clt) -> split t clt

  | PC.TEq(clt) | PC.TAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt)
  | PC.TPtVirg(clt) -> split t clt

  | PC.EOF | PC.TInvalid -> ([t],[t])

  | PC.TIso | PC.TRightIso
  | PC.TIsoExpression | PC.TIsoStatement | PC.TIsoDeclaration | PC.TIsoType
  | PC.TIsoTopLevel ->
      failwith "unexpected tokens"

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

let rec find_function_names = function
    [] -> []
  | ((PC.TIdent(_,clt),info) as t1) :: ((PC.TOPar(_),_) as t2) :: rest
  | ((PC.TMetaId(_,_,clt),info) as t1) :: ((PC.TOPar(_),_) as t2) :: rest
  | ((PC.TMetaFunc(_,_,clt),info) as t1) :: ((PC.TOPar(_),_) as t2) :: rest
  | ((PC.TMetaLocalFunc(_,_,clt),info) as t1) :: ((PC.TOPar(_),_) as t2)::rest
    ->
      let rec skip level = function
	  [] -> ([],false,[])
	| ((PC.TCPar(_),_) as t)::rest ->
	    let level = level - 1 in
	    if level = 0
	    then ([t],true,rest)
	    else let (pre,found,post) = skip level rest in (t::pre,found,post)
	| ((PC.TOPar(_),_) as t)::rest ->
	    let level = level + 1 in
	    let (pre,found,post) = skip level rest in (t::pre,found,post)
	| ((PC.TArobArob,_) as t)::rest
	| ((PC.TArob,_) as t)::rest
	| ((PC.EOF,_) as t)::rest -> ([t],false,rest)
	| t::rest ->
      	    let (pre,found,post) = skip level rest in (t::pre,found,post) in
      let (pre,found,post) = skip 1 rest in
      (match (found,post) with
	(true,((PC.TOBrace(_),_) as t3)::rest) ->
	  (PC.TFunDecl(clt),info) :: t1 :: t2 :: pre @
	  t3 :: (find_function_names rest)
      |	_ -> t1 :: t2 :: pre @ find_function_names post)
  | t :: rest -> t :: find_function_names rest

(* ----------------------------------------------------------------------- *)
(* an attribute is an identifier that preceeds another identifier and
   begins with __ *)

let rec detect_attr l =
  let is_id = function
      (PC.TIdent(_,_),_) | (PC.TMetaId(_,_,_),_) | (PC.TMetaFunc(_,_,_),_)
    | (PC.TMetaLocalFunc(_,_,_),_) -> true
    | _ -> false in    
  let rec loop = function
      [] -> []
    | [x] -> [x]
    | ((PC.TIdent(nm,clt),info) as t1)::id::rest when is_id id ->
	if String.length nm > 2 && String.sub nm 0 2 = "__"
	then (PC.Tattr(nm,clt),info)::(loop (id::rest))
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
      (PC.TOEllipsis(_),_) (* | (PC.TOCircles(_),_) | (PC.TOStars(_),_) *)
    | (PC.TEllipsis(_),_) (* | (PC.TCircles(_),_) | (PC.TStars(_),_) *)
    | (PC.TPtVirg(_),_) | (PC.TOBrace(_),_) | (PC.TCBrace(_),_)
    | (PC.TPure,_) | (PC.TContext,_)
    | (PC.Tstatic(_),_) | (PC.Textern(_),_)
    | (PC.Tinline(_),_) | (PC.Tattr(_),_) -> true
    | (PC.TComma(_),_) when infn > 0 or in_meta_decls -> true
    | (PC.TDotDot(_),_) when in_meta_decls -> true
    | _ -> false in
  let is_choices_delim = function
      (PC.TOBrace(_),_) | (PC.TComma(_),_) -> true | _ -> false in
  let is_id = function
      (PC.TIdent(_,_),_) | (PC.TMetaId(_,_,_),_) | (PC.TMetaFunc(_,_,_),_)
    | (PC.TMetaLocalFunc(_,_,_),_) -> true
    | (PC.TMetaParam(_,_,clt),_)
    | (PC.TMetaParamList(_,_,clt),_)
    | (PC.TMetaConst(_,_,_,clt),_)
    | (PC.TMetaErr(_,_,clt),_)
    | (PC.TMetaExp(_,_,_,clt),_)
    | (PC.TMetaExpList(_,_,clt),_)
    | (PC.TMetaType(_,_,clt),_)
    | (PC.TMetaStm(_,_,clt),_)
    | (PC.TMetaStmList(_,_,clt),_) -> in_meta_decls 
    | _ -> false in
  let redo_id ident clt v =
    !Data.add_type_name ident;
    (PC.TTypeId(ident,clt),v) in
  let rec loop start infn type_names = function
      (* infn: 0 means not in a function header
	 > 0 means in a function header, after infn - 1 unmatched open parens*)
      [] -> []
    | ((PC.TOBrace(clt),v)::_) as all when in_meta_decls ->
	collect_choices type_names all (* never a function header *)
    | delim::(PC.TIdent(ident,clt),v)::((PC.TMul(_),_) as x)::rest
      when is_delim infn delim ->
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
    | (PC.TIdent(ident,clt),v)::((PC.TMul(_),_) as x)::rest when start ->
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
  | PC.Tfloat(clt) | PC.Tlong(clt) | PC.Tvoid(clt) | PC.Tstruct(clt) 
  | PC.Tunion(clt) | PC.Tunsigned(clt) | PC.Tsigned(clt)
  | PC.Tstatic(clt) | PC.Tauto(clt) | PC.Tregister(clt) | PC.Textern(clt) 
  | PC.Tinline(clt) | PC.Tattr(_,clt) | PC.Tconst(clt) | PC.Tvolatile(clt) 

  | PC.TInc(clt) | PC.TDec(clt) 
	
  | PC.TIf(clt) | PC.TElse(clt) | PC.TWhile(clt) | PC.TFor(clt) | PC.TDo(clt) 
  | PC.TSwitch (clt) | PC.TCase (clt) | PC.TDefault (clt) | PC.TSizeof (clt)
  | PC.TReturn(clt) | PC.TBreak(clt) | PC.TContinue(clt) | PC.TIdent(_,clt)
  | PC.TTypeId(_,clt) | PC.TDeclarerId(_,clt) | PC.TIteratorId(_,clt)

  | PC.TString(_,clt) | PC.TChar(_,clt) | PC.TFloat(_,clt) | PC.TInt(_,clt) 

  | PC.TOrLog(clt) | PC.TAndLog(clt) | PC.TOr(clt) | PC.TXor(clt)
  | PC.TAnd (clt) | PC.TEqEq(clt) | PC.TNotEq(clt) | PC.TInf(clt) 
  | PC.TSup(clt) | PC.TInfEq(clt) | PC.TSupEq (clt) | PC.TShl(clt) 
  | PC.TShr(clt) | PC.TPlus(clt) | PC.TMinus(clt) | PC.TMul(clt) 
  | PC.TDiv(clt) | PC.TMod (clt) | PC.TTilde (clt) 

  | PC.TMetaParam(_,_,clt) | PC.TMetaParamList(_,_,clt) 
  | PC.TMetaConst(_,_,_,clt) | PC.TMetaExp(_,_,_,clt)
  | PC.TMetaExpList(_,_,clt) 
  | PC.TMetaId(_,_,clt) | PC.TMetaType(_,_,clt)
  | PC.TMetaStm(_,_,clt)   
  | PC.TMetaStmList(_,_,clt) | PC.TMetaFunc(_,_,clt)
  | PC.TMetaLocalFunc(_,_,clt) 

  | PC.TFunDecl(clt)
  | PC.TWhen(clt) | PC.TEllipsis(clt) (* | PC.TCircles(clt) | PC.TStars(clt) *)

  | PC.TOEllipsis(clt) (* | PC.TCEllipsis(clt) | PC.TOCircles(clt)
  | PC.TCCircles(clt) | PC.TOStars(clt) | PC.TCStars(clt) *)

  | PC.TWhy(clt) | PC.TDotDot(clt) | PC.TBang(clt) | PC.TOPar(clt)
  | PC.TOPar0(clt) | PC.TMid0(clt) | PC.TCPar(clt)  
  | PC.TCPar0(clt) 

  | PC.TOBrace(clt) | PC.TCBrace(clt) | PC.TOCro(clt) | PC.TCCro(clt) 

  | PC.TPtrOp(clt) 

  | PC.TDefine(clt,_) | PC.TDefineParam(clt,_,_)
  | PC.TIncludeL(_,clt) | PC.TIncludeNL(_,clt)

  | PC.TEq(clt) | PC.TAssign(_,clt) | PC.TDot(clt) | PC.TComma(clt) 
  | PC.TPtVirg(clt) ->
      let (_,line,_,_,_,_,_) = clt in Some line

  | _ -> None

let rec insert_line_end = function
    [] -> []
  | (((PC.TWhen(clt),q) as x)::xs) ->
      x::(find_line_end (token2line x) clt q xs)
  | (((PC.TDefine(clt,_),q) as x)::xs)
  | (((PC.TDefineParam(clt,_,_),q) as x)::xs) ->
      x::(find_line_end (token2line x) clt q xs)
  | x::xs -> x::(insert_line_end xs)

and find_line_end line clt q = function
    (* don't know what 2nd component should be so just use the info of
       the When.  Also inherit - of when, if any *)
    [] -> [(PC.TLineEnd(clt),q)]
  | x::xs when token2line x = line -> x :: (find_line_end line clt q xs)
  | xs -> (PC.TLineEnd(clt),q)::(insert_line_end xs)

(* ----------------------------------------------------------------------- *)
(* process pragmas: they can only be used in + code, and adjacent to
another + token.  They are concatenated to the string representation of
that other token. *)

let rec collect_all_pragmas collected = function
    (PC.TPragma(s),_)::rest -> collect_all_pragmas (s::collected) rest
  | l -> (List.rev collected,l)

let rec collect_up_to_pragmas skipped = function
    [] -> None (* didn't reach a pragma, so nothing to do *)
  | ((PC.TPragma(s),_) as t)::rest ->
      let (pragmas,rest) = collect_all_pragmas [] (t::rest) in
      Some (List.rev skipped,pragmas,rest)
  | x::xs ->
      match plus_attachable x with
	PLUS -> None
      |	NOTPLUS -> None
      |	SKIP -> collect_up_to_pragmas (x::skipped) xs

let rec collect_up_to_plus skipped = function
    [] -> failwith "nothing to attach a pragma to"
  | x::xs ->
      match plus_attachable x with
	PLUS -> (List.rev skipped,x,xs)
      |	NOTPLUS -> failwith "nothing to attach a pragma to"
      |	SKIP -> collect_up_to_plus (x::skipped) xs

let rec process_pragmas = function
    [] -> []
  | ((PC.TPragma(s),_)::_) as l ->
      let (pragmas,rest) = collect_all_pragmas [] l in
      let (skipped,aft,rest) = collect_up_to_plus [] rest in
      let (a,b,c,d,e,strbef,straft) = get_clt aft in
      skipped@
      (process_pragmas ((update_clt aft (a,b,c,d,e,pragmas,straft))::rest))
  | bef::xs ->
      (match plus_attachable bef with
	PLUS ->
	  (match collect_up_to_pragmas [] xs with
	    Some(skipped,pragmas,rest) ->
	      let (a,b,c,d,e,strbef,straft) = get_clt bef in
	      (update_clt bef (a,b,c,d,e,strbef,pragmas))::
	      skipped@(process_pragmas rest)
	  | None -> bef::(process_pragmas xs))
      |	_ -> bef::(process_pragmas xs))

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
let rec drop_double_dots l =
  let start = function
      (PC.TOEllipsis(_),_) (* | (PC.TOCircles(_),_) | (PC.TOStars(_),_) *) ->
	true
    | _ -> false in
  let middle = function
      (PC.TEllipsis(_),_) (* | (PC.TCircles(_),_) | (PC.TStars(_),_) *) -> true
    | _ -> false in
  let final = function
      (PC.TCEllipsis(_),_) (* | (PC.TCCircles(_),_) | (PC.TCStars(_),_) *) ->
	true
    | _ -> false in
  let rec loop ((_,i) as prev) = function
      [] -> []
    | x::rest when middle prev && middle x -> (PC.TNothing,i)::x::(loop x rest)
    | x::rest when start prev && middle x ->  (PC.TNothing,i)::x::(loop x rest)
    | x::rest when start prev && final x ->   (PC.TNothing,i)::x::(loop x rest)
    | x::rest when middle prev && final x ->  (PC.TNothing,i)::x::(loop x rest)
    | x::rest -> x :: (loop x rest) in
  match l with
    [] -> []
  | (x::xs) -> x :: loop x xs

let rec fix f l =
  let cur = f l in
  if l = cur then l else fix f cur

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

let drop_empty_nest =
  drop_empty_thing

(* ----------------------------------------------------------------------- *)
(* Read tokens *)

let get_s_starts (_, (s,_,(starts, ends))) =
  Printf.printf "%d %d\n" starts ends; (s, starts)

let pop2 l = 
  let v = List.hd !l in
  l := List.tl !l;
  v

let reinit _ =
  PC.reinit (function _ -> PC.TArobArob (* a handy token *))
    (Lexing.from_function
       (function buf -> function n -> raise Common.Impossible))

let parse_one str parsefn file toks =
  let all_tokens = ref toks in
  let cur_tok    = ref (List.hd !all_tokens) in

  let lexer_function _ =
      let (v, info) = pop2 all_tokens in
      cur_tok := (v, info);
      v in

  let lexbuf_fake =
    Lexing.from_function
      (function buf -> function n -> raise Common.Impossible)
  in

  reinit();

  try parsefn lexer_function lexbuf_fake 
  with 
    Lexer_cocci.Lexical s ->
      failwith
	(Printf.sprintf "%s: lexical error %s\n =%s\n" str s
	   (Common.error_message file (get_s_starts !cur_tok) ))
  | Parser_cocci_menhir.Error ->
      failwith
	(Printf.sprintf "%s: parse error \n = %s\n" str
	   (Common.error_message file (get_s_starts !cur_tok) ))
  | Semantic_cocci.Semantic s ->
      failwith
	(Printf.sprintf "%s: semantic error %s\n =%s\n" str s
	   (Common.error_message file (get_s_starts !cur_tok) ))

  | e -> raise e

let prepare_tokens tokens =
  insert_line_end
    (detect_types false (find_function_names (detect_attr tokens)))

let drop_last extra l = List.rev(extra@(List.tl(List.rev l)))

let partition_either l =
  let rec part_either left right = function
  | [] -> (List.rev left, List.rev right)
  | x :: l -> 
      (match x with
      | Common.Left  e -> part_either (e :: left) right l
      | Common.Right e -> part_either left (e :: right) l) in
  part_either [] [] l

let get_metavars parse_fn table file lexbuf =
  let rec meta_loop acc (* read one decl at a time *) =
    let (_,tokens) =
      tokens_all table file true lexbuf [PC.TArobArob;PC.TMPtVirg] in
    let tokens = prepare_tokens tokens in
    match tokens with
      [(PC.TArobArob,_)] -> List.rev acc
    | _ ->
	(*
	Printf.printf "meta tokens\n";
	List.iter (function x -> Printf.printf "%s " (token2c x)) tokens;
	Printf.printf "\n\n";
        flush stdout;
	*)
	let metavars = parse_one "meta" parse_fn file tokens in
	meta_loop (metavars@acc) in
  partition_either (meta_loop [])

let get_rule_name parse_fn starts_with_name get_tokens file prefix =
  Data.in_rule_name := true;
  let mknm _ = make_name prefix (!Lexer_cocci.line) in
  let name_res =
    if starts_with_name
    then
      let (_,tokens) = get_tokens [PC.TArob] in
      match parse_one "rule name" parse_fn file tokens with
	(None,a,b,c) -> (mknm(),a,b,c)
      |	(Some nm,a,b,c) ->
	  (if List.mem nm reserved_names
	  then failwith (Printf.sprintf "invalid name %s\n" nm));
	  (nm,a,b,c)
    else (mknm(),Ast_cocci.NoDep,[],[]) in
  Data.in_rule_name := false;
  name_res

let parse_iso file =
  let table = Common.full_charpos_to_pos file in
  Common.with_open_infile file (fun channel ->
    let lexbuf = Lexing.from_channel channel in
    let get_tokens = tokens_all table file false lexbuf in
    let res =
      match get_tokens [PC.TArobArob;PC.TArob] with
	(true,start) ->
	  let parse_start start =
	    let rev = List.rev start in
	    let (arob,_) = List.hd rev in
	    (arob = PC.TArob,List.rev(List.tl rev)) in
	  let (starts_with_name,start) = parse_start start in
	  let rec loop starts_with_name start =
	    (!Data.init_rule)();
	    (* get metavariable declarations - have to be read before the
	       rest *)
	    let (rule_name,_,_,_) =
	      get_rule_name PC.iso_rule_name starts_with_name get_tokens
		file ("iso file "^file) in
	    Ast0_cocci.rule_name := rule_name;
	    Data.in_meta := true;
	    let iso_metavars =
	      match get_metavars PC.iso_meta_main table file lexbuf with
		(iso_metavars,[]) -> iso_metavars
	      |	_ -> failwith "unexpected inheritance in iso" in
	    Data.in_meta := false;
	    (* get the rule *)
	    let (more,tokens) =
	      get_tokens
		[PC.TIsoStatement;PC.TIsoExpression;PC.TIsoDeclaration;
		  PC.TIsoType;PC.TIsoTopLevel] in
	    let next_start = List.hd(List.rev tokens) in
	    let dummy_info = ("",(-1,-1),(-1,-1)) in
	    let tokens = drop_last [(PC.EOF,dummy_info)] tokens in
	    let tokens = prepare_tokens (start@tokens) in
            (*
	       Printf.printf "iso tokens\n";
	       List.iter
	       (function x -> Printf.printf "%s " (token2c x)) tokens;
	       Printf.printf "\n\n";
	    *)
	    let entry = parse_one "iso main" PC.iso_main file tokens in
	    if more
	    then (* The code below allows a header like Statement list,
		    which is more than one word.  We don't have that any more,
		    but the code is left here in case it is put back. *)
	      match get_tokens [PC.TArobArob;PC.TArob] with
		(true,start) ->
		  let (starts_with_name,start) = parse_start start in
		  (iso_metavars,entry,rule_name) ::
		  (loop starts_with_name (next_start::start))
	      |	_ -> failwith "isomorphism ends early"
	    else [(iso_metavars,entry,rule_name)] in
	  loop starts_with_name start
      | (false,_) -> [] in
    res)

let parse_iso_files existing_isos iso_files extra_path =
  let get_names = List.map (function (_,_,nm) -> nm) in
  let old_names = get_names existing_isos in
  Data.in_iso := true;
  let (res,_) =
    List.fold_left
      (function (prev,names) ->
	function file ->
	  Lexer_cocci.init ();
	  let file =
	    match file with
	      Common.Left(fl)  -> Filename.concat extra_path fl
	    | Common.Right(fl) -> Filename.concat Config.path fl in
	  let current = parse_iso file in
	  let new_names = get_names current in
	  if List.exists (function x -> List.mem x names) new_names
	  then failwith (Printf.sprintf "repeated iso name found in %s" file);
	  (current::prev,new_names @ names))
      ([],old_names) iso_files in
  Data.in_iso := false;
  (List.concat (List.rev res))@existing_isos

let parse file =
  let table = Common.full_charpos_to_pos file in
  Common.with_open_infile file (fun channel ->
  let lexbuf = Lexing.from_channel channel in
  let get_tokens = tokens_all table file false lexbuf in
  Data.in_prolog := true;
  let initial_tokens = get_tokens [PC.TArobArob;PC.TArob] in
  Data.in_prolog := false;
  let res =
    match initial_tokens with
    (true,data) ->
      (match List.rev data with
	((PC.TArobArob as x),_)::_ | ((PC.TArob as x),_)::_ ->
	  let iso_files =
	    parse_one "iso file names" PC.include_main file data in
	  let rec loop old_metas starts_with_name =
	    (!Data.init_rule)();
	    let (rule_name,dependencies,iso,dropiso) =
	      get_rule_name PC.rule_name starts_with_name get_tokens file
		"rule" in
	    Ast0_cocci.rule_name := rule_name;
	    (* get metavariable declarations *)
	    Data.in_meta := true;
	    let (metavars,inherited_metavars) =
	      get_metavars PC.meta_main table file lexbuf in
	    Data.in_meta := false;
	    Hashtbl.add Data.all_metadecls rule_name metavars;
	    Hashtbl.add Lexer_cocci.rule_names rule_name ();
	    Hashtbl.add Lexer_cocci.all_metavariables rule_name
	      (Hashtbl.fold (fun key v rest -> (key,v)::rest)
		 Lexer_cocci.metavariables []);
	    (* get transformation rules *)
	    let (more,tokens) = get_tokens [PC.TArobArob;PC.TArob] in
	    let starts_with_name =
	      more &&
	      (match List.hd (List.rev tokens) with
		(PC.TArobArob,_) -> false
	      | (PC.TArob,_) -> true
	      | _ -> failwith "unexpected token") in
	    let (minus_tokens,plus_tokens) = split_token_stream tokens in 
	    let minus_tokens = prepare_tokens minus_tokens in
	    let plus_tokens = prepare_tokens plus_tokens in
	    (*
	       Printf.printf "minus tokens\n";
	       List.iter
	       (function x -> Printf.printf "%s " (token2c x)) minus_tokens;
	       Printf.printf "\n\n";
	       Printf.printf "plus tokens\n";
	       List.iter
	       (function x -> Printf.printf "%s " (token2c x)) plus_tokens;
	       Printf.printf "\n\n";
            *)
	    let plus_tokens =
	      process_pragmas
		(fix (function x -> drop_double_dots (drop_empty_or x))
		   (drop_when plus_tokens)) in
	    (*
	       Printf.printf "plus tokens\n";
	       List.iter
	       (function x -> Printf.printf "%s " (token2c x)) plus_tokens;
	       Printf.printf "\n\n";
	       Printf.printf "before minus parse\n";
	    *)
	    let minus_res =
	      parse_one "minus" PC.minus_main file minus_tokens in
	    (*
	       Unparse_ast0.unparse minus_res;
	       Printf.printf "before plus parse\n";
	    *)
	    let plus_res =
	      if !Flag.sgrep_mode2
	      then (* not actually used for anything, except context_neg *)
		List.map
		  (Iso_pattern.rebuild_mcode None).V0.rebuilder_top_level
		  minus_res
	      else parse_one "plus" PC.plus_main file plus_tokens in
	    (*
	       Printf.printf "after plus parse\n";
	    *)
	    Check_meta.check_meta rule_name old_metas inherited_metavars
	      metavars minus_res plus_res;
	    if more
	    then
	      let (minus_ress,plus_ress) =
		loop (metavars@old_metas) starts_with_name in
	      ((minus_res,metavars,(iso,dropiso,dependencies,rule_name))::
	       minus_ress,
	       (plus_res, metavars)::plus_ress)
	    else ([(minus_res,metavars,(iso,dropiso,dependencies,rule_name))],
		  [(plus_res, metavars)]) in
	  (iso_files, loop [] (x = PC.TArob))
      |	_ -> failwith "unexpected code before the first rule\n")
  | (false,[(PC.TArobArob,_)]) | (false,[(PC.TArob,_)]) -> ([],([],[]))
  | _ -> failwith "unexpected code before the first rule\n" in
  res)

(* parse to ast0 and then convert to ast *)
let process file isofile verbose =
  let extra_path = Filename.dirname file in
  Lexer_cocci.init ();
  let (iso_files,(minus,plus)) = parse file in
  let std_isos = parse_iso_files [] [Common.Left (!Config.std_iso)] "" in
  let global_isos = parse_iso_files std_isos iso_files extra_path in
  let minus = Unitary_ast0.do_unitary minus plus in
  let parsed =
    List.concat
      (List.map2
	 (function (minus, metavars, (iso,dropiso,dependencies,rule_name)) ->
	   function (plus, metavars) ->
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
		 failwith ("invalid iso name "^bad_dropped^" in "^rule_name)
	       with Not_found -> ());
	       if List.mem "all" dropiso
	       then
		 if List.length dropiso = 1
		 then []
		 else failwith "disable all should only be by itself"
	       else
		   (* drop those isos *)
		 List.filter (function (_,_,nm) -> not (List.mem nm dropiso))
		   chosen_isos in
	     let dropped_isos =
	       match reserved_names with
		 "all"::others ->
		   (match dropiso with
		     ["all"] -> others
		   | _ ->
		       List.filter (function x -> List.mem x dropiso) others)
	       | _ ->
		   failwith
		     "bad list of reserved names - all must be at start" in
	     let minus = Compute_lines.compute_lines minus in
	     let plus = Compute_lines.compute_lines plus in
	     let minus = Arity.minus_arity minus in
	     let function_prototypes =
	       Function_prototypes.process rule_name dropped_isos minus plus in
	     (* warning! context_neg side-effects its arguments! *)
	     let (m,p) = List.split(Context_neg.context_neg minus plus) in
	     (if not !Flag.sgrep_mode2
	     then Insert_plus.insert_plus m p);
	     Type_infer.type_infer minus;
	     let (extra_meta,minus) =
	       Iso_pattern.apply_isos chosen_isos minus rule_name in
	     let minus =
	       if !Flag.sgrep_mode2
	       then minus
	       else Single_statement.single_statement minus in
	     let minus_ast =
	       Ast0toast.ast0toast rule_name dependencies dropped_isos minus in
	     match function_prototypes with
	       None -> [(extra_meta@metavars, minus_ast)]
	     | Some mv_fp ->
		 [(extra_meta@metavars, minus_ast);mv_fp])
	 minus plus) in
  let disjd = Disjdistr.disj parsed in
  let (code,fvs,ua) = Free_vars.free_vars disjd in
  if !Flag_parsing_cocci.show_SP 
  then List.iter Pretty_print_cocci.unparse code;
  let tokens = Get_constants.get_constants code in
  (code,fvs,ua,tokens)
