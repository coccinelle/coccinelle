/*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 */

%{

(* Not clear how to allow function declarations to specify a return type
and how to allow both to be specified as static, because they are in
different rules.  The rules seem to have to be combined, which would allow
functions to be declared as local variables *)

(* Not clear how to let a function have a parameter of type void.  At the
moment, void is allowed to be the type of a variable, which is wrong, and a
parameter needs both a type and an identifier *)
module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module P = Parse_aux
module U = Unparse_ast0

(* ---------------------------------------------------------------------- *)
(* support for argument lists *)

type 'a argorellipsis =
  | Nothing
  | Arg of 'a
  | Ellipsis of Data.clt
  | VAEllipsis of Data.clt
  | Separator of Data.clt

let string_of_arg = function
  | Nothing -> "Nothing"
  | Arg _ -> "Arg"
  | Ellipsis _ -> "Ellipsis"
  | VAEllipsis _ -> "VAEllipsis"
  | Separator _ -> "Comma"

let is_nothing = function
  | Nothing -> true
  | _ -> false

let is_separator = function
  | Separator _ -> true
  | _ -> false

let is_vaellipsis = function
  | VAEllipsis _ -> true
  | _ -> false

let rec adjacent_ellipsis = function
  | [] -> false
  | [_] -> false
  | (Ellipsis _) :: (Ellipsis _) :: _ -> true
  | x::xs -> adjacent_ellipsis xs

let build_arg = function
  | Arg arg -> arg
  | Ellipsis e -> Ast0.wrap (Ast0.Pdots(P.clt2mcode "..." e))
  | Separator comma -> Ast0.wrap (Ast0.PComma (P.clt2mcode "," comma))
  | VAEllipsis _ -> assert false
  | Nothing -> assert false

let string_of_arglist l =
  "[" ^ (String.concat ";" (List.map string_of_arg l)) ^ "]"

let cleanup_arglist l =
  if l=[] then ([], None)
  else begin
    let (args, vararg) = match l with
      | (VAEllipsis vaellipsis)::(Separator comma)::rem ->
        let c = P.clt2mcode "," comma in
        let e = P.clt2mcode "......" vaellipsis in
        (rem, Some (c, e))
      | _ -> (l, None) in
    let just_args = List.filter (fun x -> not (is_separator x)) args in
    if List.exists is_vaellipsis just_args then failwith "...... can occur only as last argument"
    else if adjacent_ellipsis just_args then failwith "Argument list contains adjacent ellipsis"
    else
      let pure_args = List.filter (fun x -> not (is_nothing x)) args in
      (List.map build_arg (List.rev pure_args), vararg)
  end

(* ---------------------------------------------------------------------- *)
(* support for TMeta *)

let print_meta (r,n) = r^"."^n

let meta_metatable = Hashtbl.create(51)

let coerce_tmeta newty name builder matcher =
  try
    let x = Hashtbl.find meta_metatable name in
    if not (matcher x)
    then
      failwith
	(Printf.sprintf "Metavariable %s is used as %s"
	   (print_meta name) newty)
  with Not_found ->
    (if !Flag_parsing_cocci.show_SP
    then
      Common.pr2
	(Printf.sprintf
	   "Metavariable %s is assumed to be %s metavariable"
	   (print_meta name) newty));
    Hashtbl.add meta_metatable name builder

let tmeta_to_type (name,cstr,pure,clt) =
  (coerce_tmeta "a type" name (TMetaType(name,cstr,pure,clt))
     (function TMetaType(_,_,_,_) -> true | _ -> false));
  Ast0.wrap(Ast0.MetaType(P.clt2mcode name clt,cstr,pure))

let tmeta_to_field (name,cstr,pure,clt) =
  (coerce_tmeta "a field" name (TMetaField(name,cstr,pure,clt))
     (function TMetaField(_,_,_,_) -> true | _ -> false));
  P.meta_field (name,cstr,pure,clt)

let tmeta_to_exp (name,cstr,pure,clt) =
  (coerce_tmeta "an expression" name
     (TMetaExp(name,cstr,pure,None,clt,None))
     (function TMetaExp(_,_,_,_,_,_) -> true | _ -> false));
  Ast0.wrap
    (Ast0.MetaExpr(P.clt2mcode name clt,cstr,None,Ast.ANY,pure,None))

let tmeta_to_param (name,cstr,pure,clt) =
  (coerce_tmeta "a parameter" name (TMetaParam(name,cstr,pure,clt))
     (function TMetaParam(_,_,_,_) -> true | _ -> false));
  Ast0.wrap(Ast0.MetaParam(P.clt2mcode name clt,cstr,pure))

let tmeta_to_assignOp (name,cstr,pure,clt) =
  (coerce_tmeta "an assignment operator" name
     (TMetaAssignOp(name,cstr,pure,clt))
     (function TMetaAssignOp(_,_,_,_) -> true | _ -> false));
  Ast0.wrap
    (Ast0.MetaAssign(P.clt2mcode name clt,cstr, pure))

let tmeta_to_binaryOp (name,cstr,pure,clt) =
  (coerce_tmeta "a binary operator" name
     (TMetaBinaryOp(name,cstr,pure,clt))
     (function TMetaBinaryOp(_,_,_,_) -> true | _ -> false));
  Ast0.wrap
    (Ast0.MetaBinary(P.clt2mcode name clt,cstr, pure),clt)

let tmeta_to_statement (name,cstr,pure,clt) =
  (coerce_tmeta "a statement" name (TMetaType(name,cstr,pure,clt))
     (function TMetaType(_,_,_,_) -> true | _ -> false));
  P.meta_stm (name,cstr,pure,clt)

let tmeta_to_seed_id (name,pure,clt) =
  (coerce_tmeta "an identifier" name
     (TMetaId(name,Ast.CstrTrue,Ast.NoVal,pure,clt))
     (function TMetaId(_,_,_,_,_) -> true | _ -> false));
  Ast.SeedId name

let tmeta_to_ident (name,cstr,pure,clt) =
  (coerce_tmeta "an identifier" name
     (TMetaId(name,cstr,Ast.NoVal,pure,clt))
     (function TMetaId(_,_,_,_,_) -> true | _ -> false));
  Ast0.wrap(Ast0.MetaId(P.clt2mcode name clt,cstr,Ast.NoVal,pure))

and  arithOp = function
    Ast.Plus -> "+"
  | Ast.Minus -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Min -> "<?"
  | Ast.Max -> ">?"
  | Ast.Mod -> "%"
  | Ast.DecLeft -> "<<"
  | Ast.DecRight -> ">>"
  | Ast.And -> "&"
  | Ast.Or -> "|"
  | Ast.Xor -> "^"

and  logicalOp = function
    Ast.Inf -> "<"
  | Ast.Sup -> ">"
  | Ast.InfEq -> "<="
  | Ast.SupEq -> ">="
  | Ast.Eq -> "=="
  | Ast.NotEq -> "!="
  | Ast.AndLog -> "&&"
  | Ast.OrLog -> "||"

let mkarithop (op, clt) =
  let op' = P.clt2mcode op clt in
  Ast0.wrap (Ast0.Arith op')

let mklogop (op,clt) =
  let op' = P.clt2mcode op clt in
  Ast0.wrap (Ast0.Logical op')

let unknown_type = Ast0.wrap (Ast0.BaseType (Ast.Unknown, []))

let check_constraint_allowed () =
  if !Data.in_iso then
    failwith "constraints not allowed in iso file";
  if !Data.in_generating then
    failwith "constraints not allowed in a generated rule file"
%}

%token EOF

%token TIdentifier TExpression TStatement TFunction TType TParameter
%token TIdExpression TInitialiser TDeclaration TField TMetavariable TSymbol
%token TOperator TBinary TAssignment
%token Tlist TFresh TConstant TError TWords TWhy0 TPlus0
%token TPure TContext TGenerated TFormat TLocal TGlobal
%token TTypedef TAttribute TDeclarer TIterator TName TPosition TAnalysis
%token TPosAny
%token TUsing TDisable TExtends TDepends TOn TEver TNever TExists TForall
%token TFile TIn
%token TInitialize TFinalize TNothing TVirtual TMerge
%token<string> TRuleName
%token<string * int> TScript

%token<Data.clt> Tchar Tshort Tint Tdouble Tfloat Tlong
%token<Data.clt> Tsize_t Tssize_t Tptrdiff_t
%token<Data.clt> Tvoid Tstruct Tunion Tenum
%token<Data.clt> Tunsigned Tsigned

%token<Data.clt> Tstatic Tauto Tregister Textern Tinline Ttypedef
%token<Data.clt> Tconst Tvolatile
%token<string * Data.clt> Tattr

%token <Data.clt> TVAEllipsis
%token <Data.clt> TIf TElse TWhile TFor TDo TSwitch TCase TDefault TReturn
%token <Data.clt> TBreak TContinue TGoto TSizeof TTypeof TFunDecl Tdecimal Texec
%token <string * Data.clt> TIdent TTypeId TDeclarerId TIteratorId TSymId
%token <Ast_cocci.added_string * Data.clt> TDirective
%token <Data.clt> TAttr_

%token <Parse_aux.midinfo>       TMetaId
%token <Parse_aux.cstrinfo>        TMetaFunc TMetaLocalFunc
%token <Parse_aux.cstrinfo>        TMetaIterator TMetaDeclarer
%token <Parse_aux.assignOpinfo>  TMetaAssignOp TMetaType
%token <Parse_aux.binaryOpinfo>  TMetaBinaryOp
%token <Parse_aux.expinfo>       TMetaErr
%token <Parse_aux.cstrinfo>          TMetaParam TMetaStm
%token <Parse_aux.cstrinfo>          TMetaInit TMetaDecl TMetaField TMeta
%token <Parse_aux.list_info>     TMetaParamList TMetaExpList TMetaInitList
%token <Parse_aux.list_info>     TMetaFieldList TMetaStmList TMetaDParamList
%token <Parse_aux.typed_expinfo_bitfield> TMetaExp
%token <Parse_aux.typed_expinfo> TMetaIdExp TMetaLocalIdExp
%token <Parse_aux.typed_expinfo> TMetaGlobalIdExp TMetaConst
%token <Parse_aux.pos_info>      TMetaPos

%token TArob TArobArob
%token <Data.clt> TPArob
%token <string> TScriptData TWhitespace

%token <Data.clt> TEllipsis TOEllipsis TCEllipsis TPOEllipsis TPCEllipsis
%token <Data.clt> TWhen TWhenTrue TWhenFalse TAny TStrict TLineEnd

%token <Data.clt> TWhy TDotDot TBang TOPar TCPar
%token <string * Data.clt> TOPar0 TMid0 TAnd0 TCPar0

%token <string>  TPathIsoFile
%token <string * Data.clt> TIncludeL TIncludeNL TIncludeAny
%token <Data.clt * token> TDefine TUndef
%token <Data.clt> TPragma TCppEscapedNewline TInclude
%token <Data.clt * token * int * int> TDefineParam
%token <string * Data.clt> TMinusFile TPlusFile

%token <Data.clt> TInc TDec

%token <string * Data.clt> TString TChar TFloat TInt
%token <string * string (*n*) * string (*p*) * Data.clt> TDecimalCst

%token <Data.clt> TOrLog
%token <Data.clt> TAndLog
%token <Data.clt> TOr
%token <Data.clt> TXor
%token <Data.clt> TAnd
%token <Data.clt> TEqEq TNotEq TTildeEq TTildeExclEq TSub
%token <Ast_cocci.logicalOp * Data.clt> TLogOp /* TInf TSup TInfEq TSupEq */
%token <Ast_cocci.arithOp * Data.clt>   TShLOp TShROp  /* TShl TShr */
%token <Ast_cocci.arithOp * Data.clt>   TDmOp  /* TDiv TMod TMin TMax */
%token <Data.clt> TPlus TMinus
%token <Data.clt> TMul TTilde

%token <Data.clt> TOBrace TCBrace TOInit
%token <Data.clt> TOCro TCCro

%token <Data.clt> TPtrOp

%token TMPtVirg TCppConcatOp
%token <Data.clt> TEq TDot TComma TPtVirg
%token <Ast_cocci.arithOp * Data.clt> TOpAssign

%token TIso TRightIso TIsoExpression TIsoStatement TIsoDeclaration TIsoType
%token TIsoTopLevel TIsoArgExpression TIsoTestExpression TIsoToTestExpression

%token TUnderscore

%token TInvalid

/* operator precedence */
%nonassoc TIf
%nonassoc TElse

%left TOrLog
%left TAndLog
%left TOr
%left TXor
%left TAnd
%left TEqEq TNotEq
%left TLogOp /* TInf TSup TInfEq TSupEq */
%left TShLOp TShROp /* TShl TShr */
%left TPlus TMinus TMetaBinaryOp
%left TMul TDmOp /* TDiv TMod TMin TMax */
%nonassoc TBang

/*
%start reinit
%type <unit> reinit
*/

%start minus_main
%type <Ast0_cocci.rule> minus_main

%start minus_exp_main
%type <Ast0_cocci.rule> minus_exp_main

%start minus_ty_main
%type <Ast0_cocci.rule> minus_ty_main

%start minus_id_main
%type <Ast0_cocci.rule> minus_id_main

%start plus_main
%type <Ast0_cocci.rule> plus_main

%start plus_exp_main
%type <Ast0_cocci.rule> plus_exp_main

%start plus_ty_main
%type <Ast0_cocci.rule> plus_ty_main

%start plus_id_main
%type <Ast0_cocci.rule> plus_id_main

%start include_main
%type <Data.incl_iso list> include_main

%start iso_rule_name
%type <Ast_cocci.rulename>
iso_rule_name

%start rule_name
%type <Ast_cocci.rulename>
rule_name

%start meta_main
%type <(Ast_cocci.metavar,Ast_cocci.metavar) Common.either list> meta_main

%start <(string option (*string*) * string option (*ast*)) * (Ast_cocci.meta_name * Ast_cocci.metavar) option * Ast_cocci.mvinit> script_meta_main
%start <(string option (*string*) * string option (*ast*)) * (Ast_cocci.meta_name * Ast_cocci.metavar) option * Ast_cocci.mvinit> script_meta_virt_nofresh_main

%start iso_main
%type <Ast0_cocci.anything list list> iso_main

%start iso_meta_main
%type <(Ast_cocci.metavar,Ast_cocci.metavar) Common.either list> iso_meta_main

%start never_used
%type <unit> never_used

%type <'a * Ast0.constraints> pure_ident_or_meta_ident_with_constraints

%%

minus_main: minus_body EOF { $1 } | m=minus_body TArobArob { m }
| m=minus_body TArob { m }
plus_main: plus_body EOF { $1 } | p=plus_body TArobArob { p }
| p=plus_body TArob { p }
minus_exp_main: minus_exp_body EOF { $1 } | m=minus_exp_body TArobArob { m }
| m=minus_exp_body TArob { m }
minus_ty_main: minus_ty_body EOF { $1 } | m=minus_ty_body TArobArob { m }
| m=minus_ty_body TArob { m }
minus_id_main: minus_id_body EOF { $1 } | m=minus_id_body TArobArob { m }
| m=minus_id_body TArob { m }
plus_exp_main: plus_exp_body EOF { $1 } | p=plus_exp_body TArobArob { p }
| p=plus_exp_body TArob { p }
plus_ty_main: plus_ty_body EOF { $1 } | p=plus_ty_body TArobArob { p }
| p=plus_ty_body TArob { p }
plus_id_main: plus_id_body EOF { $1 } | p=plus_id_body TArobArob { p }
| p=plus_id_body TArob { p }
meta_main: m=metadec   { m (!Ast0.rule_name) }
iso_meta_main: m=metadec { m "" }

/*****************************************************************************
*
*
*****************************************************************************/

pure:
  TPure          { Ast0.Pure }
| TContext       { Ast0.Context }
| TPure TContext { Ast0.PureContext }
| TContext TPure { Ast0.PureContext }
| /* empty */    { Ast0.Impure }

iso_rule_name:
  nm=pure_ident TArob { P.make_iso_rule_name_result (P.id2name nm) }

rule_name:
  nm=ioption(pure_ident) extends d=depends i=loption(choose_iso)
    a=loption(disable) e=exists ee=is_expression TArob
      { P.make_cocci_rule_name_result nm d i a e ee }
  | TGenerated extends d=depends i=loption(choose_iso)
    a=loption(disable) e=exists ee=is_expression TArob
      /* these rules have no name as a cheap way to ensure that no normal
      rule inherits their metavariables or depends on them */
      { P.make_generated_rule_name_result None d i a e ee }
  | TScript TDotDot lang=pure_ident nm=ioption(pure_ident) d=depends TArob
      { P.make_script_rule_name_result lang nm d }
  | TInitialize TDotDot lang=pure_ident d=depends TArob
      { P.make_initial_script_rule_name_result lang d }
  | TFinalize TDotDot lang=pure_ident d=depends TArob
      { P.make_final_script_rule_name_result lang d }

extends:
  /* empty */                                     { () }
| TExtends parent=TRuleName
    { !Data.install_bindings (parent) }

depends:
  /* empty */              { Ast0.NoDep }
| TDepends TOn parents=dep { Ast0.ExistsDep parents }
| TDepends TOn TExists parents=dep { Ast0.ExistsDep parents }
| TDepends TOn TForall parents=dep { Ast0.ForallDep parents }

dep:
  TRuleName        { Ast0.Dep $1 }
| TBang TRuleName  { Ast0.AntiDep (Ast0.Dep $2) }
| TBang TOPar dep TCPar
                   { Ast0.AntiDep $3 }
| TEver TRuleName  { Ast0.EverDep $2 }
| TNever TRuleName { Ast0.NeverDep $2 }
| dep TAndLog dep  { Ast0.AndDep($1, $3) }
| dep TOrLog  dep  { Ast0.OrDep ($1, $3) }
| TOPar dep TCPar  { $2 }
| TFile TIn TString
    { if !Flag.dir = ""
      then Ast0.FileIn(fst $3)
      else Ast0.FileIn(Filename.concat !Flag.dir (fst $3)) }

choose_iso:
  TUsing separated_nonempty_list(TComma,TString) { List.map P.id2name $2 }

disable:
  TDisable separated_nonempty_list(TComma,pure_ident) { List.map P.id2name $2 }

exists:
  TExists { Ast.Exists }
| TForall { Ast.Forall }
|         { Ast.Undetermined }

is_expression: // for more flexible parsing of top level expressions
              { Ast.AnyP }
| TExpression { Ast.ExpP }
| TIdentifier { Ast.IdP }
| TType       { Ast.TyP }

include_main:
  list(incl) TArob     { $1 }
| list(incl) TArobArob { $1 }

incl:
  TIncludeL           { let (x,_) = $1 in Data.Include(x) }
| TUsing TString      { Data.Iso(Common.Left(P.id2name $2)) }
| TUsing TPathIsoFile { Data.Iso(Common.Right $2) }
| TVirtual ids=comma_list(pure_ident)
    { let names = List.map P.id2name ids in
      Iteration.parsed_virtual_rules :=
	Common.union_set names !Iteration.parsed_virtual_rules;
      (* ensure that the names of virtual and real rules don't overlap *)
      List.iter
      (function name -> Hashtbl.add Data.all_metadecls name (ref []))
      names;
      Data.Virt(names) }

metadec:
  ar=arity ispure=pure kindfn=metakind
  ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_metadec_with_constraints ar ispure kindfn ids }
| ar=arity ispure=pure kindfn=metakind_bitfield bf=ioption(bitfield)
  ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { match bf with
      None ->
	P.create_metadec_with_constraints ar ispure (kindfn None) ids
    | Some bf' ->
    P.create_len_metadec ar ispure (fun lenname -> kindfn (Some lenname))
    bf' ids }
| ar=arity ispure=pure
  kind_ids=metakindnosym TMPtVirg
    { let (ids,kindfn) = kind_ids in P.create_metadec ar ispure kindfn ids }
| kindfn=metakind_fresh ids=comma_list(pure_ident_or_meta_ident_with_seed)
    TMPtVirg
    { P.create_fresh_metadec kindfn ids }
| ar=arity ispure=pure
  kindfn=metakind_atomic_maybe_virt
  ids=
  comma_list(pure_ident_or_meta_ident_with_constraints_virt)
    TMPtVirg
    { let (normal,virt) = Common.partition_either (fun x -> x) ids in
    let (idfn,virtfn) = kindfn in
    function cr ->
      (P.create_metadec_with_constraints ar ispure idfn normal cr) @
      (P.create_metadec_virt ar ispure virtfn virt cr) }
| ar=arity TPosition a=option(TPosAny)
    ids=
    comma_list
    (pure_ident_or_meta_ident_with_constraints_pos)
    TMPtVirg
    (* pb: position variables can't be inherited from normal rules, and then
       there is no way to inherit from a generated rule, so there is no point
       to have a position variable *)
    { (if !Data.in_generating
      then failwith "position variables not allowed in a generated rule file");
      let kindfn arity name pure check_meta constraints =
	let tok = check_meta(Ast.MetaPosDecl(arity,name)) in
	let any = match a with None -> Ast.PER | Some _ -> Ast.ALL in
	!Data.add_pos_meta name constraints any; tok in
    P.create_metadec_with_constraints ar false kindfn ids }
| ar=arity ispure=pure
    TParameter Tlist TOCro len=list_len TCCro
    ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_len_metadec ar ispure
	(fun lenname arity name pure check_meta cstr ->
	  let tok = check_meta(Ast.MetaParamListDecl(arity,name,lenname)) in
	  !Data.add_paramlist_meta name lenname cstr pure; tok)
	len ids }
| ar=arity ispure=pure
    TExpression Tlist TOCro len=list_len TCCro
    ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_len_metadec ar ispure
	(fun lenname arity name pure check_meta cstr ->
	  let tok = check_meta(Ast.MetaExpListDecl(arity,name,lenname)) in
	  !Data.add_explist_meta name lenname cstr pure; tok)
	len ids }
| ar=arity ispure=pure
    TField Tlist TOCro len=list_len TCCro
    ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_len_metadec ar ispure
	(fun lenname arity name pure check_meta cstr ->
	  let tok = check_meta(Ast.MetaFieldListDecl(arity,name,lenname)) in
	  !Data.add_field_list_meta name lenname cstr pure; tok)
	len ids }
| ar=arity ispure=pure
    TInitialiser Tlist TOCro len=list_len TCCro
    ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_len_metadec ar ispure
	(fun lenname arity name pure check_meta cstr ->
	  let tok = check_meta(Ast.MetaInitListDecl(arity,name,lenname)) in
	  !Data.add_initlist_meta name lenname cstr pure; tok)
	len ids }
| ar=arity ispure=pure
    TIdentifier Tlist TOCro len=list_len TCCro
    ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_len_metadec ar ispure
	(fun lenname arity name pure check_meta cstr ->
	  let tok = check_meta(Ast.MetaDParamListDecl(arity,name,lenname)) in
	  !Data.add_dparamlist_meta name lenname cstr pure; tok)
	len ids }
| ar=arity ispure=pure TStatement Tlist TOCro len=list_len TCCro
    ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_len_metadec ar ispure
	(fun lenname arity name pure check_meta cstr ->
	  let tok = check_meta(Ast.MetaStmListDecl(arity,name,lenname)) in
	  !Data.add_stmlist_meta name lenname cstr pure; tok)
	len ids }
| TSymbol ids=comma_list(pure_ident_or_symbol) TMPtVirg
    { (fun _ ->
        let add_sym = fun (nm,_) -> !Data.add_symbol_meta nm in
          List.iter add_sym ids; [])
    }
| ar=arity TFormat
    ids=comma_list(pure_ident_or_meta_ident_with_constraints)
    TMPtVirg
    { P.create_metadec_with_constraints ar Ast0.Impure
	(fun arity name pure check_meta constraints ->
	  let tok = check_meta(Ast.MetaFmtDecl(arity,name)) in
	  !Data.add_fmt_meta name constraints; tok)
    ids }
| ar=arity TFormat Tlist
    ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_metadec_with_constraints ar Ast0.Impure
	(fun arity name pure check_meta cstr ->
	  let len = Ast.AnyLen in
	  let tok = check_meta(Ast.MetaFragListDecl(arity,name,len)) in
	  !Data.add_fmtlist_meta name cstr len; tok)
	ids }
| ar=arity
    TFormat Tlist TOCro len=list_len TCCro
    ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_len_metadec ar Ast0.Impure
	(fun lenname arity name pure check_meta cstr ->
	  let tok = check_meta(Ast.MetaFragListDecl(arity,name,lenname)) in
	  !Data.add_fmtlist_meta name cstr lenname; tok)
	len ids }
| ar=arity TBinary TOperator
    ids=comma_list(pure_ident_or_meta_ident_with_constraints) TMPtVirg
    { P.create_metadec_with_constraints ar Ast0.Impure
	(fun arity name pure check_meta constraints ->
	  let tok = check_meta(Ast.MetaBinaryOperatorDecl(arity,name)) in
	  !Data.add_binaryOp_meta name constraints pure; tok)
        ids }
| ar=arity TAssignment TOperator
    ids=comma_list(pure_ident_or_meta_ident_with_constraints)
    TMPtVirg
    { P.create_metadec_with_constraints ar Ast0.Impure
	(fun arity name pure check_meta constraints ->
	  let tok = check_meta(Ast.MetaAssignmentOperatorDecl(arity,name)) in
	  !Data.add_assignOp_meta name constraints pure; tok)
        ids }

%inline bitfield:
  TDotDot l=delimited_list_len { l }

list_len:
  pure_ident_or_meta_ident_with_constraints
    { let (id,cstr) = $1 in Common.Left(id,cstr) }
| l=list_len_pure { l }

list_len_pure:
  TInt { let (x,clt) = $1 in Common.Right (int_of_string x) }
| TVirtual TDot pure_ident
    { let nm = ("virtual",P.id2name $3) in
    Iteration.parsed_virtual_identifiers :=
      Common.union_set [snd nm]
        !Iteration.parsed_virtual_identifiers;
    try
    Common.Right (int_of_string
		    (List.assoc (snd nm) !Flag.defined_virtual_env))
    with Not_found | Failure _ ->
      begin
	Common.Left ((Some "virtual",P.id2name $3),Ast.CstrTrue)
      end
    }

delimited_list_len:
  id=pure_ident_or_meta_ident { Common.Left (id, Ast.CstrTrue) }
| TOPar idcstr=pure_ident_or_meta_ident_with_constraints TCPar
    { Common.Left idcstr }
| l=list_len_pure { l }

%inline metakind_fresh:
  TFresh TIdentifier
    { (fun name check_meta seed ->
      let tok = check_meta(Ast.MetaFreshIdDecl(name,seed)) in
      !Data.add_fresh_id_meta name seed; tok) }

/* metavariable kinds with no constraints, etc */
%inline metakind:
  TMetavariable
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaMetaDecl(arity,name)) in
      !Data.add_meta_meta name constraints pure; tok) }
| TParameter
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaParamDecl(arity,name)) in
      !Data.add_param_meta name constraints pure; tok) }
| TParameter Tlist
    { (fun arity name pure check_meta constraints ->
      let len = Ast.AnyLen in
      let tok = check_meta(Ast.MetaParamListDecl(arity,name,len)) in
      !Data.add_paramlist_meta name len constraints pure; tok) }
| TExpression Tlist
    { (fun arity name pure check_meta constraints ->
      let len = Ast.AnyLen in
      let tok = check_meta(Ast.MetaExpListDecl(arity,name,len)) in
      !Data.add_explist_meta name len constraints pure; tok) }
/* | TType
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaTypeDecl(arity,name)) in
      !Data.add_type_meta name constraints pure; tok) } */
| TInitialiser
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaInitDecl(arity,name)) in
      !Data.add_init_meta name constraints pure; tok) }
| TInitialiser Tlist
    { (fun arity name pure check_meta constraints ->
      let len = Ast.AnyLen in
      let tok = check_meta(Ast.MetaInitListDecl(arity,name,len)) in
      !Data.add_initlist_meta name len constraints pure; tok) }
| TStatement
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaStmDecl(arity,name)) in
      !Data.add_stm_meta name constraints pure; tok) }
| TDeclaration
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaDeclDecl(arity,name)) in
      !Data.add_decl_meta name constraints pure; tok) }
| TField
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaFieldDecl(arity,name)) in
      !Data.add_field_meta name constraints pure; tok) }
| TField Tlist
    { (fun arity name pure check_meta constraints ->
      let len = Ast.AnyLen in
      let tok = check_meta(Ast.MetaFieldListDecl(arity,name,len)) in
      !Data.add_field_list_meta name len constraints pure; tok) }
| TStatement Tlist
    { (fun arity name pure check_meta constraints ->
      let len = Ast.AnyLen in
      let tok = check_meta(Ast.MetaStmListDecl(arity,name,len)) in
      !Data.add_stmlist_meta name len constraints pure; tok) }
| TIdentifier Tlist
    { (fun arity name pure check_meta constraints ->
      let len = Ast.AnyLen in
      let tok = check_meta(Ast.MetaDParamListDecl(arity,name,len)) in
      !Data.add_dparamlist_meta name len constraints pure; tok) }
| TFunction
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaFuncDecl(arity,name)) in
      !Data.add_func_meta name constraints pure; tok) }
| TLocal TFunction
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaLocalFuncDecl(arity,name)) in
      !Data.add_local_func_meta name constraints pure;
      tok) }
| TDeclarer
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaDeclarerDecl(arity,name)) in
      !Data.add_declarer_meta name constraints pure; tok) }
| TIterator
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaIteratorDecl(arity,name)) in
      !Data.add_iterator_meta name constraints pure; tok) }
| TType
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaTypeDecl(arity,name)) in
      !Data.add_type_meta name constraints pure; tok) }
| TError
    { (fun arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaErrDecl(arity,name)) in
      !Data.add_err_meta name constraints pure; tok) }
| l=option(TLocal) TIdExpression ty=ioption(meta_exp_type)
    { (fun arity name pure check_meta constraints ->
      match l with
        None ->
          !Data.add_idexp_meta ty name constraints pure;
          let ty' = Common.map_option (List.map (Ast0toast.typeC false)) ty in
          check_meta(Ast.MetaIdExpDecl(arity,name,ty'))
      | Some _ ->
          !Data.add_local_idexp_meta ty name constraints pure;
          let ty' = Common.map_option (List.map (Ast0toast.typeC false)) ty in
          check_meta(Ast.MetaLocalIdExpDecl(arity,name,ty'))) }
| l=option(TLocal) TIdExpression m=nonempty_list(TMul)
    { (fun arity name pure check_meta constraints ->
      let ty = Some [P.ty_pointerify unknown_type m] in
      match l with
        None ->
          !Data.add_idexp_meta ty name constraints pure;
          let ty' = Common.map_option (List.map (Ast0toast.typeC false)) ty in
          check_meta(Ast.MetaIdExpDecl(arity,name,ty'))
      | Some _ ->
          !Data.add_local_idexp_meta ty name constraints pure;
          let ty' = Common.map_option (List.map (Ast0toast.typeC false)) ty in
          check_meta(Ast.MetaLocalIdExpDecl(arity,name,ty'))) }
| TGlobal TIdExpression ty=ioption(meta_exp_type)
    { (fun arity name pure check_meta constraints ->
      !Data.add_global_idexp_meta ty name constraints pure;
      let ty' = Common.map_option (List.map (Ast0toast.typeC false)) ty in
      check_meta(Ast.MetaGlobalIdExpDecl(arity,name,ty'))) }
| TGlobal TIdExpression m=nonempty_list(TMul)
    { (fun arity name pure check_meta constraints ->
      let ty = P.ty_pointerify unknown_type m in
      !Data.add_global_idexp_meta (Some [ty]) name constraints pure;
      let ty' = Some [Ast0toast.typeC false ty] in
      check_meta(Ast.MetaGlobalIdExpDecl(arity,name,ty'))) }
| TConstant ty=ioption(meta_exp_type)
    { (fun arity name pure check_meta constraints ->
      !Data.add_const_meta ty name constraints pure;
      let ty' = Common.map_option (List.map (Ast0toast.typeC false)) ty in
      check_meta (Ast.MetaConstDecl(arity,name,ty'))) }

%inline metakind_bitfield:
| TExpression ty=expression_type
    { (fun lenname arity name pure check_meta constraints ->
      !Data.add_exp_meta (Some [ty]) name constraints pure lenname;
      let ty' = Some [Ast0toast.typeC false ty] in
      check_meta (Ast.MetaExpDecl (arity, name, ty', lenname))) }
| TExpression
    { (fun lenname arity name pure check_meta constraints ->
      let tok = check_meta(Ast.MetaExpDecl(arity,name,None,lenname)) in
      !Data.add_exp_meta None name constraints pure lenname; tok) }
| vl=meta_exp_type // no error if use $1 but doesn't type check
    { (fun lenname arity name pure check_meta constraints ->
      let ty = Some vl in
      let cstr_expr = Some begin function c ->
	match Ast0.unwrap c with
	  Ast0.Constant(_) ->
	    if not
		(List.exists
		   (fun ty ->
		     match Ast0.unwrap ty with
		       Ast0.BaseType (Ast.IntType, _) -> true
		     | Ast0.BaseType (Ast.ShortType, _) -> true
		     | Ast0.BaseType (Ast.LongType, _) -> true
			   | _ -> false)
			 vl)
		  then
		    failwith "metavariable with int constraint must be an int"
	      | _ -> ()
	end in
      Ast.cstr_iter { Ast.empty_cstr_transformer with Ast.cstr_expr }
	constraints;
      !Data.add_exp_meta ty name constraints pure lenname;
      let ty' = Some (List.map (Ast0toast.typeC false) vl) in
      let tok = check_meta (Ast.MetaExpDecl (arity,name,ty',lenname)) in
      tok)
    }


%inline metakindnosym:
  TTypedef ids=comma_list(pure_ident_or_meta_ident_nosym2(TTypeId))
    { (ids,fun arity (_,name) pure check_meta ->
      if arity = Ast.NONE && pure = Ast0.Impure
      then (!Data.add_type_name name; [])
      else raise (Semantic_cocci.Semantic "bad typedef")) }
| TAttribute TName ids=comma_list(pure_ident_or_meta_ident_nosym2(Tattr))
    { (ids,fun arity (_,name) pure check_meta ->
      if arity = Ast.NONE && pure = Ast0.Impure
      then
	(!Data.add_attribute name;
	 Flag.add_cocci_attribute_names name;
	 [])
      else raise (Semantic_cocci.Semantic "bad attribute")) }
| TDeclarer TName ids=comma_list(pure_ident_or_meta_ident_nosym2(TDeclarerId))
    { (ids,fun arity (_,name) pure check_meta ->
      if arity = Ast.NONE && pure = Ast0.Impure
      then (!Data.add_declarer_name name; [])
      else raise (Semantic_cocci.Semantic "bad declarer")) }
| TIterator TName ids=comma_list(pure_ident_or_meta_ident_nosym2(TIteratorId))
    { (ids,fun arity (_,name) pure check_meta ->
      if arity = Ast.NONE && pure = Ast0.Impure
      then (!Data.add_iterator_name name; [])
      else raise (Semantic_cocci.Semantic "bad iterator")) }

%inline metakind_atomic_maybe_virt:
  TIdentifier
    {
     let idfn arity name pure check_meta constraints =
       let tok = check_meta(Ast.MetaIdDecl(arity,name)) in
       !Data.add_id_meta name constraints pure; tok in
     let virtfn arity name pure check_meta virtual_env =
       try
	 let vl = List.assoc name virtual_env in
	 !Data.add_virt_id_meta_found name vl; []
       with Not_found ->
	 Iteration.parsed_virtual_identifiers :=
	   Common.union_set [name]
	     !Iteration.parsed_virtual_identifiers;
	 let name = ("virtual",name) in
	 let tok = check_meta(Ast.MetaIdDecl(arity,name)) in
	 !Data.add_virt_id_meta_not_found name pure; tok in
     (idfn,virtfn) }

expression_type:
  m=nonempty_list(TMul) { P.ty_pointerify unknown_type m }
| Tenum m=list(TMul)
    { P.ty_pointerify (Ast0.wrap (Ast0.EnumName (Ast0.make_mcode "", None))) m }
| Tstruct m=list(TMul)
    { P.ty_pointerify
        (Ast0.wrap
           (Ast0.StructUnionName (Ast0.make_mcode Ast.Struct, None))) m }
| Tunion m=list(TMul)
    { P.ty_pointerify
        (Ast0.wrap
           (Ast0.StructUnionName (Ast0.make_mcode Ast.Union, None))) m }

meta_exp_type:
  t=typedef_ctype
    { [t] }
| t=typedef_ctype TOCro TCCro
    { [Ast0.wrap
         (Ast0.Array
            (t,
             Ast0.make_mcode "", None, Ast0.make_mcode ""))] }
| TOBrace t=comma_list(ctype) TCBrace m=list(TMul)
    { List.map (fun x -> P.ty_pointerify x m) t }

arity: TWhy0  { Ast.OPT }
     | TPlus0 { Ast.MULTI }
     | /* empty */ { Ast.NONE }

/* ---------------------------------------------------------------------- */

signable_types_no_ident:
  ty=Tchar
    { Ast0.wrap(Ast0.BaseType(Ast.CharType,[P.clt2mcode "char" ty])) }
| ty=Tshort
    { Ast0.wrap(Ast0.BaseType(Ast.ShortType,[P.clt2mcode "short" ty])) }
| ty1=Tshort ty2=Tint
    { Ast0.wrap
	(Ast0.BaseType
	   (Ast.ShortIntType,[P.clt2mcode "short" ty1;P.clt2mcode "int" ty2])) }
| ty=Tint
    { Ast0.wrap(Ast0.BaseType(Ast.IntType,[P.clt2mcode "int" ty])) }
| p=TMetaType
    { let (nm,cstr,pure,clt) = p in
      Ast0.wrap(Ast0.MetaType(P.clt2mcode nm clt,cstr,pure)) }
| ty1=Tlong
    { Ast0.wrap(Ast0.BaseType(Ast.LongType,[P.clt2mcode "long" ty1])) }
| ty1=Tlong ty2=Tint
    { Ast0.wrap
	(Ast0.BaseType
	   (Ast.LongIntType,[P.clt2mcode "long" ty1;P.clt2mcode "int" ty2])) }
| ty1=Tlong ty2=Tlong
    { Ast0.wrap
	(Ast0.BaseType
	   (Ast.LongLongType,
	    [P.clt2mcode "long" ty1;P.clt2mcode "long" ty2])) }
| ty1=Tlong ty2=Tlong ty3=Tint
    { Ast0.wrap
	(Ast0.BaseType
	   (Ast.LongLongIntType,
	    [P.clt2mcode "long" ty1;P.clt2mcode "long" ty2;
	      P.clt2mcode "int" ty3])) }

signable_types:
  ty=signable_types_no_ident { ty }
| r=TRuleName TDot p=TIdent
    { let nm = (r,P.id2name p) in
    (* this is only possible when we are in a metavar decl.  Otherwise,
       it will be represented already as a MetaType *)
    let _ = P.check_meta(Ast.MetaTypeDecl(Ast.NONE,nm)) in
    Ast0.wrap(Ast0.MetaType(P.clt2mcode nm (P.id2clt p),Ast.CstrTrue,
			    Ast0.Impure (*will be ignored*))) }

non_signable_types_no_ident:
  ty=Tvoid
    { Ast0.wrap(Ast0.BaseType(Ast.VoidType,[P.clt2mcode "void" ty])) }
| ty1=Tlong ty2=Tdouble
    { Ast0.wrap
	(Ast0.BaseType
	   (Ast.LongDoubleType,
	    [P.clt2mcode "long" ty1;P.clt2mcode "double" ty2])) }
| ty=Tdouble
    { Ast0.wrap(Ast0.BaseType(Ast.DoubleType,[P.clt2mcode "double" ty])) }
| ty=Tfloat
    { Ast0.wrap(Ast0.BaseType(Ast.FloatType,[P.clt2mcode "float" ty])) }
| ty=Tsize_t
    { Ast0.wrap(Ast0.BaseType(Ast.SizeType,[P.clt2mcode "size_t" ty])) }
| ty=Tssize_t
    { Ast0.wrap(Ast0.BaseType(Ast.SSizeType,[P.clt2mcode "ssize_t" ty])) }
| ty=Tptrdiff_t
    { Ast0.wrap(Ast0.BaseType(Ast.PtrDiffType,[P.clt2mcode "ptrdiff_t" ty])) }
| s=Tenum i=ident
    { Ast0.wrap(Ast0.EnumName(P.clt2mcode "enum" s, Some i)) }
| s=Tenum i=ioption(ident) l=TOBrace ids=enum_decl_list r=TCBrace
    { (if i = None && !Data.in_iso
    then failwith "enums must be named in the iso file");
      Ast0.wrap(Ast0.EnumDef(Ast0.wrap(Ast0.EnumName(P.clt2mcode "enum" s, i)),
			     P.clt2mcode "{" l, ids, P.clt2mcode "}" r)) }
| s=struct_or_union i=type_ident // allow typedef name
    { Ast0.wrap(Ast0.StructUnionName(s, Some i)) }
| s=struct_or_union i=ioption(type_ident)
    l=TOBrace d=struct_decl_list r=TCBrace
    { (if i = None && !Data.in_iso
    then failwith "structures must be named in the iso file");
      Ast0.wrap(Ast0.StructUnionDef(Ast0.wrap(Ast0.StructUnionName(s, i)),
				    P.clt2mcode "{" l,
				    d, P.clt2mcode "}" r)) }
| s=TMetaType l=TOBrace d=struct_decl_list r=TCBrace
    { let (nm,cstr,pure,clt) = s in
    let ty = Ast0.wrap(Ast0.MetaType(P.clt2mcode nm clt,cstr,pure)) in
    Ast0.wrap(Ast0.StructUnionDef(ty,P.clt2mcode "{" l,d,P.clt2mcode "}" r)) }
| Tdecimal TOPar enum_val TComma enum_val TCPar
    { Ast0.wrap(Ast0.Decimal(P.clt2mcode "decimal" $1,
			     P.clt2mcode "(" $2,$3,
			     Some (P.clt2mcode "," $4), Some $5,
			     P.clt2mcode ")" $6)) }
| Tdecimal TOPar enum_val TCPar
    { Ast0.wrap(Ast0.Decimal(P.clt2mcode "decimal" $1,
			     P.clt2mcode "(" $2,$3,None,None,
			     P.clt2mcode ")" $4)) }
| TTypeof TOPar eexpr TCPar
    { Ast0.wrap(Ast0.TypeOfExpr(P.clt2mcode "typeof" $1,
                                   P.clt2mcode "(" $2,$3,
                                   P.clt2mcode ")" $4)) }
| TTypeof TOPar ctype TCPar
    { Ast0.wrap(Ast0.TypeOfType(P.clt2mcode "typeof" $1,
                                   P.clt2mcode "(" $2,$3,
                                   P.clt2mcode ")" $4)) }

non_signable_types:
  ty=non_signable_types_no_ident { ty }
| p=TTypeId
    { Ast0.wrap(Ast0.TypeName(P.id2mcode p)) }

signed_basic_types:
  r=Tsigned ty=signable_types
    { Ast0.wrap(Ast0.Signed(P.clt2mcode Ast.Signed r,Some ty)) }
| r=Tunsigned ty=signable_types
    { Ast0.wrap(Ast0.Signed(P.clt2mcode Ast.Unsigned r,Some ty)) }

all_basic_types:
  ty=signed_basic_types { ty }
| ty=signable_types { ty }
| ty=non_signable_types { ty }

all_basic_types_no_ident:
  ty=signed_basic_types { ty }
| ty=signable_types_no_ident { ty }
| ty=non_signable_types { ty }

signed_or_unsigned:
| r=Tsigned { Ast0.wrap(Ast0.Signed(P.clt2mcode Ast.Signed r,None)) }
| r=Tunsigned { Ast0.wrap(Ast0.Signed(P.clt2mcode Ast.Unsigned r,None)) }

top_ctype:
  ctype { Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Ty($1)))) }

ctype:
  cv=ioption(const_vol) ty=all_basic_types m=list(mul)
    { List.fold_left
	(function prev ->
	  function (star,cv) ->
	    P.make_cv cv (P.pointerify prev [star]))
	(P.make_cv cv ty) m }
| ty=signed_or_unsigned { ty }
| lp=TOPar0 t=midzero_list(ctype,ctype) rp=TCPar0
    { let (mids,code) = t in
      Ast0.wrap
	(Ast0.DisjType(P.id2mcode lp,code,mids, P.id2mcode rp)) }
| lp=TOPar0 t=andzero_list(ctype,ctype) rp=TCPar0
    { let (mids,code) = t in
      Ast0.wrap
	(Ast0.ConjType(P.id2mcode lp,code,mids, P.id2mcode rp)) }

mul: a=TMul b=ioption(const_vol) { (a,b) }

mctype:
| TMeta { tmeta_to_type $1 }
| ctype {$1}

/* signed, unsigned alone not allowed */
typedef_ctype:
  cv=ioption(const_vol) ty=all_basic_types m=list(TMul)
    { P.pointerify (P.make_cv cv ty) m }
| lp=TOPar0 t=midzero_list(mctype,mctype) rp=TCPar0
    { let (mids,code) = t in
      Ast0.wrap
	(Ast0.DisjType(P.id2mcode lp,code,mids, P.id2mcode rp)) }
| TMeta { tmeta_to_type $1 }

/* ---------------------------------------------------------------------- */

struct_or_union:
       s=Tstruct { P.clt2mcode Ast.Struct s }
     | u=Tunion  { P.clt2mcode Ast.Union u }

struct_decl:
      TNothing        { [] }
    | struct_decl_one { [$1] }

struct_decl_one:
    | TMetaField { P.meta_field $1 }
    | TMetaFieldList { P.meta_field_list $1 }
    | TMeta { tmeta_to_field $1 }
    | lp=TOPar0 t=midzero_list(struct_decl_one,struct_decl_one) rp=TCPar0
	{ let (mids,code) = t in
	Ast0.wrap
	  (Ast0.DisjField(P.id2mcode lp,code,mids, P.id2mcode rp)) }
    | lp=TOPar0 t=andzero_list(struct_decl_one,struct_decl_one) rp=TCPar0
	{ let (mids,code) = t in
	Ast0.wrap
	  (Ast0.ConjField(P.id2mcode lp,code,mids, P.id2mcode rp)) }
    | t=ctype d=d_ident_option bf=struct_bitfield? pv=TPtVirg
	 { let (id,fn) = d in
	 Ast0.wrap(Ast0.Field(fn t,id,bf,P.clt2mcode ";" pv)) }
    | t=ctype lp1=TOPar st=TMul d=d_ident_option rp1=TCPar
	lp2=TOPar p=decl_list(name_opt_decl) rp2=TCPar
	bf=struct_bitfield? pv=TPtVirg
        { let (id,fn) = d in
        let t =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" st,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,p,P.clt2mcode ")" rp2)) in
        Ast0.wrap(Ast0.Field(fn t,id,bf,P.clt2mcode ";" pv)) }
     | cv=ioption(const_vol) i=pure_ident_or_symbol d=d_ident_option
	 bf=struct_bitfield?
	 pv=TPtVirg
	 { let (id,fn) = d in
	 let idtype = P.make_cv cv (Ast0.wrap (Ast0.TypeName(P.id2mcode i))) in
	 Ast0.wrap(Ast0.Field(fn idtype,id,bf,P.clt2mcode ";" pv)) }

d_ident_option:
	 { None, (fun x -> x) }
     | d=d_ident {
       let (id, fn) = d in
       (Some id, fn)
    }

struct_bitfield:
   c=TDotDot e=expr { (P.clt2mcode ":" c, e) }

struct_decl_list:
   struct_decl_list_start { Ast0.wrap $1 }

struct_decl_list_start:
  /* empty */                        { [] }
| struct_decl struct_decl_list_start { $1@$2 }
| d=edots_when(TEllipsis,struct_decl_one)
    { [(P.mkfdots_one "..." d)] }
| d=edots_when(TEllipsis,struct_decl_one) s=struct_decl r=struct_decl_list_start
    { (P.mkfdots_one "..." d)::s@r }

/* ---------------------------------------------------------------------- */
/* very restricted what kinds of expressions can appear in an enum decl */

enum_decl_one:
    | disj_ident    { Ast0.wrap(Ast0.Ident($1)) }
    | disj_ident TEq enum_val
	{ let id = Ast0.wrap(Ast0.Ident($1)) in
        let (op,clt) = ("=",$2) in
        let op' = P.clt2mcode op clt in
        let op'' = Ast0.wrap (Ast0.SimpleAssign op') in
	Ast0.wrap
	  (Ast0.Assignment
	     (id, op'', Ast0.set_arg_exp $3, false)) }

enum_val:
   ident    { Ast0.wrap(Ast0.Ident($1)) }
 | TInt
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (P.clt2mcode (Ast.Int x) clt)) }
 | TMeta { tmeta_to_exp $1 }
 | TMetaConst
     { let (nm,constraints,pure,ty,clt) = $1 in
     Ast0.wrap
       (Ast0.MetaExpr(P.clt2mcode nm clt,constraints,ty,Ast.CONST,pure,None)) }
 | TMetaExp
     { let (nm,constraints,pure,ty,clt,bitfield) = $1 in
     let bitfield' = Common.map_option (P.dolen clt) bitfield in
     Ast0.wrap
       (Ast0.MetaExpr
	  (P.clt2mcode nm clt,constraints,ty,Ast.ANY,pure,bitfield')) }
 | TMetaIdExp
     { let (nm,constraints,pure,ty,clt) = $1 in
     Ast0.wrap
       (Ast0.MetaExpr(P.clt2mcode nm clt,constraints,ty,Ast.ID,pure,None)) }

enum_decl_list:
   nonempty_list_start(enum_decl_one,edots_when(TEllipsis,enum_decl_one))
     { Ast0.wrap($1 P.mkedots (fun c -> Ast0.EComma c)) }

/*****************************************************************************/

/* have to inline everything to avoid conflicts? switch to proper
declarations, statements, and expressions for the subterms */

minus_body:
    f=loption(filespec)
    b=loption(minus_start)
    /*ew=loption(error_words)*/
    { match f@b(*@ew*) with
      [] -> raise (Semantic_cocci.Semantic "minus slice can't be empty")
    | code -> code }

plus_body:
    f=loption(filespec)
    b=loption(plus_start)
    /*ew=loption(error_words)*/
    { f@b(*@ew*) }

minus_exp_body:
    f=loption(filespec)
    b=top_eexpr
    /*ew=loption(error_words)*/
    { match f@[b](*@ew*) with
      [] -> raise (Semantic_cocci.Semantic "minus slice can't be empty")
    | code -> code }

plus_exp_body:
    f=loption(filespec)
    b=top_eexpr
    /*ew=loption(error_words)*/
    { f@[b](*@ew*) }

minus_ty_body:
    f=loption(filespec)
    b=top_ctype
    /*ew=loption(error_words)*/
    { match f@[b](*@ew*) with
      [] -> raise (Semantic_cocci.Semantic "minus slice can't be empty")
    | code -> code }

plus_ty_body:
    f=loption(filespec)
    b=top_ctype
    /*ew=loption(error_words)*/
    { f@[b](*@ew*) }

minus_id_body:
    f=loption(filespec)
    b=top_ident
    /*ew=loption(error_words)*/
    { match f@[b](*@ew*) with
      [] -> raise (Semantic_cocci.Semantic "minus slice can't be empty")
    | code -> code }

plus_id_body:
    f=loption(filespec)
    b=top_ident
    /*ew=loption(error_words)*/
    { f@[b](*@ew*) }

filespec:
  TMinusFile TPlusFile
    { [Ast0.wrap
	  (Ast0.FILEINFO(P.id2mcode $1,
			 P.id2mcode $2))] }

includes:
  TIncludeL
    { Ast0.wrap
	(Ast0.Include(P.clt2mcode "#include"
			(P.drop_pos (P.drop_aft (P.id2clt $1))),
		      let (arity,ln,lln,llne,offset,col,strbef,straft,pos,_) =
			P.id2clt $1 in
		      let clt = (* default to one space whitespace *)
			(arity,ln,lln,llne,offset,0,strbef,straft,pos," ") in
		      P.clt2mcode
			(Ast.Local (Parse_aux.str2inc (P.id2name $1)))
			(P.drop_bef clt))) }
| TIncludeNL
    { Ast0.wrap
	(Ast0.Include(P.clt2mcode "#include"
			(P.drop_pos (P.drop_aft (P.id2clt $1))),
		      let (arity,ln,lln,llne,offset,col,strbef,straft,pos,_) =
			P.id2clt $1 in
		      let clt = (* default to one space whitespace *)
			(arity,ln,lln,llne,offset,0,strbef,straft,pos," ") in
		      P.clt2mcode
			(Ast.NonLocal (Parse_aux.str2inc (P.id2name $1)))
			(P.drop_bef clt))) }
| TIncludeAny
    { Ast0.wrap
	(Ast0.Include(P.clt2mcode "#include"
			(P.drop_pos (P.drop_aft (P.id2clt $1))),
		      let (arity,ln,lln,llne,offset,col,strbef,straft,pos,_) =
			P.id2clt $1 in
		      let clt = (* default to one space whitespace *)
			(arity,ln,lln,llne,offset,0,strbef,straft,pos," ") in
		      P.clt2mcode Ast.AnyInc (P.drop_bef clt))) }
| TInclude TMetaExp
     { Ast0.wrap
	 (Ast0.MetaInclude
	    (P.clt2mcode "#include" $1,
	     match $2 with
	       (nm,constraints,pure,ty,clt,None) ->
		 Ast0.wrap
		   (Ast0.MetaExpr(P.clt2mcode nm clt,constraints,ty,Ast.CONST,
				  pure,None))
	     | _ -> failwith "length not allowed for include arugment")) }

| TUndef TLineEnd
    { let (clt,ident) = $1 in
      let aft = P.get_aft clt in (* move stuff after the define to the ident *)
      Ast0.wrap
      (Ast0.Undef
	 (P.clt2mcode "#undef" (P.drop_aft clt),
	  (match ident with
	    TMetaId((nm,constraints,seed,pure,clt)) ->
	      let clt = P.set_aft aft clt in
	      Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,constraints,seed,pure))
	  | TIdent((nm,clt)) ->
	      let clt = P.set_aft aft clt in
	      Ast0.wrap(Ast0.Id(P.clt2mcode nm clt))
	  | TSymId(nm,clt) ->
	      let clt = P.set_aft aft clt in
	      Ast0.wrap(Ast0.Id(P.clt2mcode nm clt))
	  | _ ->
	      raise
		(Semantic_cocci.Semantic
		   "unexpected name for a #define")))) }
| d=defineop TLineEnd
    { d (Ast0.wrap []) }
| d=defineop t=ctype TLineEnd
    { let ty = Ast0.wrap(Ast0.TopExp(Ast0.wrap(Ast0.TypeExp(t)))) in
      d (Ast0.wrap [ty]) }
| defineop b=toplevel_seq_start(toplevel_after_dots) TLineEnd
    { let body =
	match b with
	  [e] ->
	    (match Ast0.unwrap e with
	      Ast0.Exp(e1) ->
		[Ast0.rewrap e (Ast0.TopExp(Ast0.set_arg_exp (e1)))]
	    | _ -> b)
	| _ -> b in
      $1 (Ast0.wrap body) }
| TPragma ident_or_kwd pragmabody TLineEnd
    { Ast0.wrap(Ast0.Pragma(P.clt2mcode "#pragma" $1, $2, $3)) }

pragmabody:
    TOPar eexpr_list_option TCPar
    { Ast0.wrap(Ast0.PragmaTuple(P.clt2mcode "(" $1,$2,P.clt2mcode ")" $3)) }
| l=nonempty_list(ident)
    { Ast0.wrap(Ast0.PragmaIdList(Ast0.wrap l)) }
| TEllipsis { Ast0.wrap(Ast0.PragmaDots(P.clt2mcode "..." $1)) }

defineop:
  TDefine
    { let (clt,ident) = $1 in
      let aft = P.get_aft clt in (* move stuff after the define to the ident *)
      function body ->
	Ast0.wrap
	  (Ast0.Define
	     (P.clt2mcode "#define" (P.drop_aft clt),
	      (match ident with
		TMetaId((nm,constraints,seed,pure,clt)) ->
		  let clt = P.set_aft aft clt in
		  Ast0.wrap
		    (Ast0.MetaId(P.clt2mcode nm clt,constraints,seed,pure))
	      | TIdent((nm,clt)) ->
		  let clt = P.set_aft aft clt in
		  Ast0.wrap(Ast0.Id(P.clt2mcode nm clt))
	      | TSymId(nm,clt) ->
		  let clt = P.set_aft aft clt in
		  Ast0.wrap(Ast0.Id(P.clt2mcode nm clt))
	      | _ ->
		  raise
		    (Semantic_cocci.Semantic
		       "unexpected name for a #define")),
	      Ast0.wrap Ast0.NoParams,
	      body)) }
| TDefineParam define_param_list_option TCPar
    { let (clt,ident,parenoff,parencol) = $1 in
      let aft = P.get_aft clt in (* move stuff after the define to the ( *)
      (* clt is the start of the #define itself *)
      let (arity,line,lline,llineend,offset,col,strbef,straft,pos,ws) = clt in
      let lp =
	P.clt2mcode "("
	  (arity,line,lline,llineend,parenoff,parencol,[],[],[],"") in
      function body ->
	Ast0.wrap
	  (Ast0.Define
	     (P.clt2mcode "#define" (P.drop_aft clt),
	      (match ident with
		TMetaId((nm,constraints,seed,pure,clt)) ->
		  Ast0.wrap
		    (Ast0.MetaId(P.clt2mcode nm clt,constraints,seed,pure))
	      | TIdent((nm,clt)) ->
		  Ast0.wrap(Ast0.Id(P.clt2mcode nm clt))
	      | TSymId(nm,clt) ->
		  Ast0.wrap(Ast0.Id(P.clt2mcode nm clt))
	      | _ ->
		  raise
		    (Semantic_cocci.Semantic
		       "unexpected name for a #define")),
	      (let clt = P.set_aft aft $3 in
	      Ast0.wrap (Ast0.DParams (lp,$2,P.clt2mcode ")" clt))),body)) }

/* ---------------------------------------------------------------------- */

dparam: mident { Ast0.wrap(Ast0.DParam $1) }
| TMetaDParamList { P.meta_dparam_list $1 }

define_param_list_option:
    empty_list_start(dparam,TEllipsis)
      { Ast0.wrap
	  ($1
	     (fun _ d -> Ast0.wrap(Ast0.DPdots(P.clt2mcode "..." d)))
	     (fun c -> Ast0.DPComma c)) }

/*****************************************************************************/

/* Lists of arguments in function declarations */

arg_list(arg):
  arglist=separated_llist(TComma, argorellipsis(one_arg(arg)))
     { let (args,vararg) = cleanup_arglist arglist in
       ((Ast0.wrap args), vararg) }

argorellipsis(arg):
  arg=arg { Arg arg }
| x=TVAEllipsis { VAEllipsis (x) }
| y=TEllipsis { Ellipsis (y) }

one_arg(arg):
  arg=arg  { arg }
| metaparamlist=TMetaParamList
    { let (nm,lenname,cstr,pure,clt) = metaparamlist in
      let nm = P.clt2mcode nm clt in
      let lenname = P.dolen clt lenname in
      Ast0.wrap(Ast0.MetaParamList(nm,lenname,cstr,pure)) }

%inline separated_llist(separator, X):
  xs = reverse_separated_llist(separator, X)
    { xs }

%inline reverse_separated_llist(separator, X):
    { [] }
| xs = reverse_separated_nonempty_llist(separator, X)
    { xs }

reverse_separated_nonempty_llist(separator, X):
  x = X
    { [ x ] }
| xs = reverse_separated_nonempty_llist(separator, X); s=separator; x = X
    { x :: (Separator s) :: xs }
| xs = reverse_separated_nonempty_llist(separator, X); s=separator; TNothing; x = X
    { x :: Nothing :: (Separator s) :: xs }

funproto:
  s=ioption(storage) i=ioption(Tinline) t=ctype
  id=fn_ident lp=TOPar arglist=arg_list(name_opt_decl) rp=TCPar pt=TPtVirg
      { let s = match s with None -> [] | Some s -> [Ast0.FStorage s] in
        let i =
	  match i with
	    None -> []
	  | Some i -> [Ast0.FInline (P.clt2mcode "inline" i)] in
	let t = [Ast0.FType t] in
        let (args,vararg) = arglist in
	Ast0.wrap
	  (Ast0.FunProto
	     (s @ i @ t, id,
	      P.clt2mcode "(" lp, args, vararg, P.clt2mcode ")" rp,
	      P.clt2mcode ";" pt)) }
| i=Tinline s=storage t=ctype
  id=fn_ident lp=TOPar arglist=arg_list(name_opt_decl) rp=TCPar pt=TPtVirg
      { let s = [Ast0.FStorage s] in
        let i = [Ast0.FInline (P.clt2mcode "inline" i)] in
	let t = [Ast0.FType t] in
        let (args,vararg) = arglist in
	Ast0.wrap
	  (Ast0.FunProto
	     (s @ i @ t, id,
	      P.clt2mcode "(" lp, args, vararg, P.clt2mcode ")" rp,
	      P.clt2mcode ";" pt)) }

fundecl:
  f = single_fundecl { f }
| s = close_boolean_statement(fundecl) { s }

single_fundecl:
  f=fninfo
  TFunDecl i=fn_ident lp=TOPar arglist=arg_list(decl) rp=TCPar
  lb=TOBrace b=fun_start rb=TCBrace
      { let (args,vararg) = arglist in
        Ast0.wrap(Ast0.FunDecl((Ast0.default_info(),Ast0.context_befaft()),
			       f, i,
			       P.clt2mcode "(" (lp), args, vararg,
			       P.clt2mcode ")" rp,
			       P.clt2mcode "{" lb, b,
			       P.clt2mcode "}" rb,
			       (Ast0.default_info(),Ast0.context_befaft()))) }

fninfo:
    /* empty */ { [] }
  | storage  fninfo
      { try
	let _ =
	  List.find (function Ast0.FStorage(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate storage")
      with Not_found -> (Ast0.FStorage($1))::$2 }
  | t=ctype r=fninfo_nt { (Ast0.FType(t))::r }
  | Tinline  fninfo
      { try
	let _ = List.find (function Ast0.FInline(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate inline")
      with Not_found -> (Ast0.FInline(P.clt2mcode "inline" $1))::$2 }
  | Tattr    fninfo
      { try
	let _ = List.find (function Ast0.FAttr(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "multiple attributes")
      with Not_found -> (Ast0.FAttr(P.id2mcode $1))::$2 }

fninfo_nt:
    /* empty */ { [] }
  | storage  fninfo_nt
      { try
	let _ =
	  List.find (function Ast0.FStorage(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate storage")
      with Not_found -> (Ast0.FStorage($1))::$2 }
  | Tinline  fninfo_nt
      { try
	let _ = List.find (function Ast0.FInline(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate inline")
      with Not_found -> (Ast0.FInline(P.clt2mcode "inline" $1))::$2 }
  | Tattr    fninfo_nt
      { try
	let _ = List.find (function Ast0.FAttr(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate init")
      with Not_found -> (Ast0.FAttr(P.id2mcode $1))::$2 }

storage:
         s=Tstatic      { P.clt2mcode Ast.Static s }
       | s=Tauto        { P.clt2mcode Ast.Auto s }
       | s=Tregister    { P.clt2mcode Ast.Register s }
       | s=Textern      { P.clt2mcode Ast.Extern s }

decl: t=ctype i=disj_ident a=list(array_dec)
	{ let t = P.arrayify t a in Ast0.wrap(Ast0.Param(t, Some i)) }
    | t=ctype { (*verify in FunDecl*) Ast0.wrap(Ast0.Param(t, None)) }
    | t=ctype lp=TOPar s=TMul i=disj_ident rp=TCPar
	lp1=TOPar d=decl_list(name_opt_decl) rp1=TCPar
        { let fnptr =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp,P.clt2mcode "*" s,P.clt2mcode ")" rp,
		P.clt2mcode "(" lp1,d,P.clt2mcode ")" rp1)) in
	Ast0.wrap(Ast0.Param(fnptr, Some i)) }
    | TMetaParam
	{ let (nm,cstr,pure,clt) = $1 in
	Ast0.wrap(Ast0.MetaParam(P.clt2mcode nm clt,cstr,pure)) }
    | TMeta { tmeta_to_param $1 }

name_opt_decl:
      decl  { $1 }
    | t=ctype lp=TOPar s=TMul rp=TCPar
	lp1=TOPar d=decl_list(name_opt_decl) rp1=TCPar
        { let fnptr =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp,P.clt2mcode "*" s,P.clt2mcode ")" rp,
		P.clt2mcode "(" lp1,d,P.clt2mcode ")" rp1)) in
	Ast0.wrap(Ast0.Param(fnptr, None)) }

const_vol:
      Tconst       { P.clt2mcode Ast.Const $1 }
    | Tvolatile    { P.clt2mcode Ast.Volatile $1 }

/*****************************************************************************/

statement:
  includes { $1 } /* shouldn't be allowed to be a single_statement... */
| TMeta { tmeta_to_statement $1}
| TMetaStm
    { P.meta_stm $1 }
| option(expr) TPtVirg
    { P.exp_stm $1 $2 }
| TIf TOPar eexpr TCPar single_statement %prec TIf
    { P.ifthen $1 $2 $3 $4 $5 }
| TIf TOPar eexpr TCPar single_statement TElse single_statement
    { P.ifthenelse $1 $2 $3 $4 $5 $6 $7 }
| TFor TOPar option(eexpr) TPtVirg option(eexpr) TPtVirg
    option(eexpr) TCPar single_statement
    { P.forloop $1 $2 $3 $4 $5 $6 $7 $8 $9 }
| TFor TOPar one_decl_var option(eexpr) TPtVirg
    option(eexpr) TCPar single_statement
    { P.forloop2 $1 $2 $3 $4 $5 $6 $7 $8 }
| TWhile TOPar eexpr TCPar single_statement
    { P.whileloop $1 $2 $3 $4 $5 }
| TDo single_statement TWhile TOPar eexpr TCPar TPtVirg
    { P.doloop $1 $2 $3 $4 $5 $6 $7 }
| iter_ident TOPar eexpr_list_option TCPar single_statement
    { P.iterator $1 $2 $3 $4 $5 }
| TSwitch TOPar eexpr TCPar TOBrace list(decl_var) list(case_line) TCBrace
    { P.switch $1 $2 $3 $4 $5 (List.concat $6) $7 $8 }
| TReturn eexpr TPtVirg { P.ret_exp $1 $2 $3 }
| TReturn TPtVirg { P.ret $1 $2 }
| TBreak TPtVirg { P.break $1 $2 }
| TContinue TPtVirg { P.cont $1 $2 }
| mident TDotDot { P.label $1 $2 }
| TGoto disj_ident TPtVirg { P.goto $1 $2 $3 }
| TOBrace fun_start TCBrace
    { P.seq $1 $2 $3 }
| Texec TIdent exec_list TPtVirg
    { Ast0.wrap(
      Ast0.Exec(P.clt2mcode "EXEC" $1,P.clt2mcode (fst $2) (snd $2),
		Ast0.wrap $3,P.clt2mcode ";" $4)) }

stm_dots:
  TEllipsis w=list(whenppdecs)
    { Ast0.wrap(Ast0.Dots(P.clt2mcode "..." $1, List.concat w)) }
| TOEllipsis w=list(whenppdecs) b=nest_start c=TCEllipsis
    { Ast0.wrap(Ast0.Nest(P.clt2mcode "<..." $1, b,
			  P.clt2mcode "...>" c, List.concat w, false)) }
| TPOEllipsis w=list(whenppdecs) b=nest_start c=TPCEllipsis
    { Ast0.wrap(Ast0.Nest(P.clt2mcode "<+..." $1, b,
			  P.clt2mcode "...+>" c, List.concat w, true)) }

%inline stm_dots_ell:
  a=TEllipsis w=list(whenppdecs)
    { Ast0.wrap(Ast0.Dots(P.clt2mcode "..." a, List.concat w)) }

%inline stm_dots_nest:
  a=TOEllipsis w=list(whenppdecs) b=nest_start c=TCEllipsis
    { Ast0.wrap(Ast0.Nest(P.clt2mcode "<..." a, b,
			  P.clt2mcode "...>" c, List.concat w, false)) }
| a=TPOEllipsis w=list(whenppdecs) b=nest_start c=TPCEllipsis
    { Ast0.wrap(Ast0.Nest(P.clt2mcode "<+..." a, b,
			  P.clt2mcode "...+>" c, List.concat w, true)) }

whenppdecs: w=whens(when_start,rule_elem_statement,any_strict)
    { w }

/* a statement that fits into a single rule_elem.  should nests be included?
what about statement metavariables? */
rule_elem_statement:
  one_decl_var
    { Ast0.wrap(Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),$1)) }
| option(expr) TPtVirg { P.exp_stm $1 $2 }
| TReturn eexpr TPtVirg { P.ret_exp $1 $2 $3 }
| TReturn TPtVirg { P.ret $1 $2 }
| TBreak TPtVirg { P.break $1 $2 }
| TContinue TPtVirg { P.cont $1 $2 }
| s = close_boolean_statement(rule_elem_statement) { s }

close_boolean_statement(item):
  lp = TOPar0 t = midzero_list(item,item) rp = TCPar0
    { let (mids,code) = t in
    Ast0.wrap
      (Ast0.Disj(P.id2mcode lp,
		 List.map (function x -> Ast0.wrap [x]) code,
		 mids, P.id2mcode rp)) }
| lp = TOPar0 t = andzero_list(item,item) rp = TCPar0
    { let (mids,code) = t in
    Ast0.wrap
      (Ast0.Conj(P.id2mcode lp,
		 List.map (function x -> Ast0.wrap [x]) code,
		 mids, P.id2mcode rp)) }

/* a statement on its own */
single_statement:
  statement                         { $1 }
| s = close_boolean_statement(statement) { s }

iso_statement: /* statement or declaration used in statement context */
    statement                         { $1 }
  | decl_var
      { match $1 with
	[decl] ->
	  Ast0.wrap
	    (Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),decl))
      |	_ -> failwith "exactly one decl allowed in statement iso" }

case_line:
    TDefault TDotDot fun_start
      { Ast0.wrap
	  (Ast0.Default(P.clt2mcode "default" $1,P.clt2mcode ":" $2,$3)) }
  | TCase eexpr TDotDot fun_start
      { Ast0.wrap(Ast0.Case(P.clt2mcode "case" $1,$2,P.clt2mcode ":" $3,$4)) }
/*  | lp=TOPar0 t=midzero_list(case_line,case_line) rp=TCPar0
    { let (mids,code) = ([],[t]) in
      Ast0.wrap
	(Ast0.DisjCase(P.id2mcode lp,code,mids, P.id2mcode rp)) } */

/* In the following, an identifier as a type is not fully supported.  Indeed,
the language is ambiguous: what is foo * bar; */
/* The AST DisjDecl cannot be generated because it would be ambiguous with
a disjunction on a statement with a declaration in each branch */
decl_var:
    t=ctype pv=TPtVirg
      { [Ast0.wrap(Ast0.TyDecl(t,P.clt2mcode ";" pv))] }
  | TMetaDecl { [P.meta_decl $1] }
  | s=ioption(storage) t=ctype d=comma_list(d_ident) pv=TPtVirg
      { List.map
	  (function (id,fn) ->
	    Ast0.wrap(Ast0.UnInit(s,fn t,id,[],P.clt2mcode ";" pv)))
	  d }
  | f=funproto { [f] }
  | s=ioption(storage) t=ctype d=d_ident a=attr_list q=TEq e=initialize
      pv=TPtVirg
      {let (id,fn) = d in
      [Ast0.wrap
	  (Ast0.Init(s,fn t,id,a,P.clt2mcode "=" q,e,P.clt2mcode ";" pv))]}
  /* type is a typedef name */
  | s=ioption(storage) cv=ioption(const_vol) i=pure_ident_or_symbol
      d=comma_list(d_ident) pv=TPtVirg
      { List.map
	  (function (id,fn) ->
	    let idtype =
	      P.make_cv cv (Ast0.wrap (Ast0.TypeName(P.id2mcode i))) in
	    Ast0.wrap(Ast0.UnInit(s,fn idtype,id,[],P.clt2mcode ";" pv)))
	  d }
  | s=ioption(storage) cv=ioption(const_vol) i=pure_ident_or_symbol
      d=d_ident a=attr_list q=TEq e=initialize pv=TPtVirg
      { let (id,fn) = d in
      !Data.add_type_name (P.id2name i);
      let idtype = P.make_cv cv (Ast0.wrap (Ast0.TypeName(P.id2mcode i))) in
      [Ast0.wrap(Ast0.Init(s,fn idtype,id,a,P.clt2mcode "=" q,e,
			   P.clt2mcode ";" pv))] }
  /* function pointer type */
  | s=ioption(storage)
    t=ctype lp1=TOPar st=TMul d=d_ident rp1=TCPar
    lp2=TOPar p=decl_list(name_opt_decl) rp2=TCPar
    pv=TPtVirg
      { let (id,fn) = d in
        let t =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" st,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,p,P.clt2mcode ")" rp2)) in
        [Ast0.wrap(Ast0.UnInit(s,fn t,id,[],P.clt2mcode ";" pv))] }
  | s=ioption(storage) d=decl_ident o=TOPar e=eexpr_list_option c=TCPar
      p=TPtVirg
      { [Ast0.wrap(Ast0.MacroDecl(s,d,P.clt2mcode "(" o,e,
				  P.clt2mcode ")" c,P.clt2mcode ";" p))] }
  | s=ioption(storage)
      d=decl_ident o=TOPar e=eexpr_list_option c=TCPar q=TEq i=initialize
      p=TPtVirg
      { [Ast0.wrap
	    (Ast0.MacroDeclInit
	       (s,d,P.clt2mcode "(" o,e,
		P.clt2mcode ")" c,P.clt2mcode "=" q,i,
		P.clt2mcode ";" p))] }
  | s=ioption(storage)
    t=ctype lp1=TOPar st=TMul d=d_ident rp1=TCPar
    lp2=TOPar p=decl_list(name_opt_decl) rp2=TCPar
    q=TEq e=initialize pv=TPtVirg
      { let (id,fn) = d in
        let t =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" st,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,p,P.clt2mcode ")" rp2)) in
      [Ast0.wrap
	  (Ast0.Init(s,fn t,id,[],P.clt2mcode "=" q,e,P.clt2mcode ";" pv))]}
  | s=Ttypedef t=typedef_ctype id=comma_list(typedef_ident) pv=TPtVirg
      { let s = P.clt2mcode "typedef" s in
        List.map
	  (function id ->
	    Ast0.wrap(Ast0.Typedef(s,t,id,P.clt2mcode ";" pv)))
	  id }
  | s=Ttypedef
    t=typedef_ctype lp1=TOPar st=TMul id=typedef_ident rp1=TCPar
    lp2=TOPar p=decl_list(name_opt_decl) rp2=TCPar pv=TPtVirg
      { let s = P.clt2mcode "typedef" s in
        let t =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" st,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,p,P.clt2mcode ")" rp2)) in
	[Ast0.wrap(Ast0.Typedef(s,t,id,P.clt2mcode ";" pv))]}

one_decl_var:
    t=ctype pv=TPtVirg
      { Ast0.wrap(Ast0.TyDecl(t,P.clt2mcode ";" pv)) }
  | TMetaDecl { P.meta_decl $1 }
  | s=ioption(storage) t=ctype d=d_ident a=attr_list pv=TPtVirg
      { let (id,fn) = d in
        Ast0.wrap(Ast0.UnInit(s,fn t,id,a,P.clt2mcode ";" pv)) }
  | f=funproto { f }
  | s=ioption(storage) t=ctype d=d_ident a=attr_list q=TEq e=initialize
      pv=TPtVirg
      { let (id,fn) = d in
      Ast0.wrap
	(Ast0.Init(s,fn t,id,a,P.clt2mcode "=" q,e,P.clt2mcode ";" pv)) }
  /* type is a typedef name */
  | s=ioption(storage) cv=ioption(const_vol) i=pure_ident_or_symbol
      d=d_ident a=attr_list pv=TPtVirg
      { let (id,fn) = d in
        let idtype = P.make_cv cv (Ast0.wrap (Ast0.TypeName(P.id2mcode i))) in
	Ast0.wrap(Ast0.UnInit(s,fn idtype,id,a,P.clt2mcode ";" pv)) }
  | s=ioption(storage) cv=ioption(const_vol) i=pure_ident_or_symbol
      d=d_ident a=attr_list q=TEq e=initialize pv=TPtVirg
      { let (id,fn) = d in
      !Data.add_type_name (P.id2name i);
      let idtype = P.make_cv cv (Ast0.wrap (Ast0.TypeName(P.id2mcode i))) in
      Ast0.wrap(Ast0.Init(s,fn idtype,id,a,P.clt2mcode "=" q,e,
			   P.clt2mcode ";" pv)) }
  /* function pointer type */
  | s=ioption(storage)
    t=ctype lp1=TOPar st=TMul d=d_ident rp1=TCPar
    lp2=TOPar p=decl_list(name_opt_decl) rp2=TCPar
    pv=TPtVirg
      { let (id,fn) = d in
        let t =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" st,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,p,P.clt2mcode ")" rp2)) in
        Ast0.wrap(Ast0.UnInit(s,fn t,id,[],P.clt2mcode ";" pv)) }
  | s=ioption(storage) d=decl_ident o=TOPar e=eexpr_list_option c=TCPar
      p=TPtVirg
      { Ast0.wrap(Ast0.MacroDecl(s,d,P.clt2mcode "(" o,e,
				  P.clt2mcode ")" c,P.clt2mcode ";" p)) }
  | s=ioption(storage)
      d=decl_ident o=TOPar e=eexpr_list_option c=TCPar q=TEq i=initialize
      p=TPtVirg
      { Ast0.wrap
            (Ast0.MacroDeclInit
               (s,d,P.clt2mcode "(" o,e,
                P.clt2mcode ")" c,P.clt2mcode "=" q,i,
                P.clt2mcode ";" p)) }
  | s=ioption(storage)
    t=ctype lp1=TOPar st=TMul d=d_ident rp1=TCPar
    lp2=TOPar p=decl_list(name_opt_decl) rp2=TCPar a=attr_list
    q=TEq e=initialize pv=TPtVirg
      { let (id,fn) = d in
        let t =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" st,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,p,P.clt2mcode ")" rp2)) in
      Ast0.wrap(Ast0.Init(s,fn t,id,a,P.clt2mcode "=" q,e,P.clt2mcode ";" pv))}


d_ident:
    disj_ident list(array_dec)
      { ($1, function t -> P.arrayify t $2) }

array_dec: l=TOCro i=option(eexpr) r=TCCro { (l,i,r) }

initialize:
    eexpr
      { Ast0.wrap(Ast0.InitExpr($1)) }
  | TOBrace initialize_list TCBrace
    { if P.struct_initializer $2
    then
      let il = P.drop_dot_commas $2 in
      Ast0.wrap(Ast0.InitList(P.clt2mcode "{" $1,il,P.clt2mcode "}" $3,false))
    else
      Ast0.wrap(Ast0.InitList(P.clt2mcode "{" $1,$2,P.clt2mcode "}" $3,true)) }
  | TMetaInit
      {let (nm,cstr,pure,clt) = $1 in
      Ast0.wrap(Ast0.MetaInit(P.clt2mcode nm clt,cstr,pure)) }

initialize2:
  /*arithexpr and not eexpr because can have ambiguity with comma*/
  /*dots and nests probably not allowed at top level, haven't looked into why*/
  arith_expr(eexpr,invalid) { Ast0.wrap(Ast0.InitExpr($1)) }
| nest_expressions_only     { Ast0.wrap(Ast0.InitExpr($1)) }
| TOBrace initialize_list TCBrace
    { if P.struct_initializer $2
    then
      let il = P.drop_dot_commas $2 in
      Ast0.wrap(Ast0.InitList(P.clt2mcode "{" $1,il,P.clt2mcode "}" $3,false))
    else
      Ast0.wrap(Ast0.InitList(P.clt2mcode "{" $1,$2,P.clt2mcode "}" $3,true)) }
           /* gccext:, labeled elements */
| nonempty_list(designator) TEq initialize2
    /*can we have another of these on the rhs?*/
    { Ast0.wrap(Ast0.InitGccExt($1,P.clt2mcode "=" $2,$3)) }
| mident TDotDot initialize2
    { Ast0.wrap(Ast0.InitGccName($1,P.clt2mcode ":" $2,$3)) } /*in old kernel*/
| TMetaInit
    {let (nm,cstr,pure,clt) = $1 in
    Ast0.wrap(Ast0.MetaInit(P.clt2mcode nm clt,cstr,pure)) }
| TMetaInitList
    {let (nm,lenname,cstr,pure,clt) = $1 in
    let nm = P.clt2mcode nm clt in
    let lenname = P.dolen clt lenname in
    Ast0.wrap(Ast0.MetaInitList(nm,lenname,cstr,pure)) }

designator:
 | TDot disj_ident
     { Ast0.DesignatorField (P.clt2mcode "." $1,$2) }
 | TOCro eexpr TCCro
     { Ast0.DesignatorIndex (P.clt2mcode "[" $1,$2,P.clt2mcode "]" $3) }
 | TOCro eexpr TEllipsis eexpr TCCro
     { Ast0.DesignatorRange (P.clt2mcode "[" $1,$2,P.clt2mcode "..." $3,
			     $4,P.clt2mcode "]" $5) }

initialize_list:
   empty_list_start(initialize2,edots_when(TEllipsis,initialize))
     { Ast0.wrap($1 P.mkidots (fun c -> Ast0.IComma c)) }

/* a statement that is part of a list */
decl_statement:
    TMetaStmList { [P.meta_stm_list $1] }
  | decl_var
      { List.map
	  (function x ->
	    Ast0.wrap
	      (Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),x)))
	  $1 }
  | statement { [$1] }
  /* this doesn't allow expressions at top level, because the parser doesn't
	know whether there is one.  If there is one, this is not sequencible.
	If there is not one, then it is.  It seems complicated to get around
    this at the parser level.  We would have to have a check afterwards to
    allow this.  One case where this would be useful is for a when.  Now
	we allow a sequence of whens, so one can be on only statements and
    one can be on only expressions. */
  | TOPar0 t=midzero_list(fun_start,fun_start) TCPar0
      { let (mids,code) = t in
	if List.for_all
	    (function x ->
	      match Ast0.unwrap x with [] -> true | _ -> false)
	    code
      then []
      else
	  [Ast0.wrap(Ast0.Disj(P.id2mcode $1, code, mids,
			       P.id2mcode $3))] }
  | TOPar0 t=andzero_list(fun_start,fun_start) TCPar0
      { let (mids,code) = t in
	if List.for_all
	    (function x ->
	      match Ast0.unwrap x with [] -> true | _ -> false)
	    code
      then []
      else
	  [Ast0.wrap(Ast0.Conj(P.id2mcode $1, code, mids,
			       P.id2mcode $3))] }

/* a statement that is part of a list */
decl_statement_expr:
    TMetaStmList { [P.meta_stm_list $1] }
  | decl_var
      { List.map
	  (function x ->
	    Ast0.wrap
	      (Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),x)))
	  $1 }
  | statement { [$1] }
  /* this doesn't allow expressions at top level, because the parser doesn't
	know whether there is one.  If there is one, this is not sequencible.
	If there is not one, then it is.  It seems complicated to get around
    this at the parser level.  We would have to have a check afterwards to
    allow this.  One case where this would be useful is for a when.  Now
	we allow a sequence of whens, so one can be on only statements and
    one can be on only expressions. */
  | TOPar0 t=midzero_list(fun_after_stm,fun_after_dots_or) TCPar0
      { let (mids,code) = t in
	if List.for_all (function [] -> true | _ -> false) code
      then []
      else
	  let dot_code = List.map Ast0.wrap code in
	  [Ast0.wrap(Ast0.Disj(P.id2mcode $1, dot_code, mids,
			       P.id2mcode $3))] }
  | TOPar0 t=andzero_list(fun_after_stm,fun_after_dots_or) TCPar0
      { let (mids,code) = t in
	if List.for_all (function [] -> true | _ -> false) code
      then []
      else
	  let dot_code = List.map Ast0.wrap code in
	  [Ast0.wrap(Ast0.Conj(P.id2mcode $1, dot_code, mids,
			       P.id2mcode $3))] }

/*****************************************************************************/

/* expr cannot contain <... ...> at the top level.  This can only
be allowed as an expression when the expression is delimited on the left
by an expression-specific marker.  In that case, the rule eexpr is used, which
allows <... ...> anywhere.  Hopefully, this will not be too much of a problem
in practice.
dot_expressions is the most permissive.  all three kinds of expressions use
this once an expression_specific token has been seen
The arg versions don't allow sequences, to avoid conflicting with commas in
argument lists.
 */
expr:  pre_basic_expr(expr,invalid) { $1 }
/* allows ... and nests */
eexpr: pre_basic_expr(eexpr,dot_expressions) { $1 }
eargexpr: basic_expr(eexpr,dot_expressions) { $1 } /* no sequences */
/* allows nests but not .... */
dexpr: pre_basic_expr(eexpr,nest_expressions) { $1 }
dargexpr: basic_expr(eexpr,nest_expressions) { $1 } /* no sequences */

top_eexpr:
  eexpr { Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1)))) }

invalid:
  TInvalid { raise (Semantic_cocci.Semantic "not matchable") }

dot_expressions:
  TEllipsis { Ast0.wrap(Ast0.Edots(P.clt2mcode "..." $1,None)) }
| nest_expressions { $1 }

/* not clear what whencode would mean, so just drop it */
nest_expressions:
  TOEllipsis e=expr_dots(TEllipsis) c=TCEllipsis
    { Ast0.wrap(Ast0.NestExpr(P.clt2mcode "<..." $1,
			      Ast0.wrap(e (P.mkedots "...")),
			      P.clt2mcode "...>" c, None, false)) }
| TPOEllipsis e=expr_dots(TEllipsis) c=TPCEllipsis
    { Ast0.wrap(Ast0.NestExpr(P.clt2mcode "<+..." $1,
			      Ast0.wrap(e (P.mkedots "...")),
			      P.clt2mcode "...+>" c, None, true)) }
| TMeta { tmeta_to_exp $1 }

nest_expressions_only:
  TOEllipsis e=expr_dots(TEllipsis) c=TCEllipsis
    { Ast0.wrap(Ast0.NestExpr(P.clt2mcode "<..." $1,
			      Ast0.wrap(e (P.mkedots "...")),
			      P.clt2mcode "...>" c, None, false)) }
| TPOEllipsis e=expr_dots(TEllipsis) c=TPCEllipsis
    { Ast0.wrap(Ast0.NestExpr(P.clt2mcode "<+..." $1,
			      Ast0.wrap(e (P.mkedots "...")),
			      P.clt2mcode "...+>" c, None, true)) }

//whenexp: TWhen TNotEq w=eexpr TLineEnd { w }

pre_basic_expr(recurser,primary_extra):
   basic_expr(recurser,primary_extra)                     { $1 }
 | pre_basic_expr(recurser,primary_extra) TComma
     basic_expr(recurser,primary_extra)
     { Ast0.wrap(Ast0.Sequence($1,P.clt2mcode "," $2,$3)) }

basic_expr(recurser,primary_extra):
   assign_expr(recurser,primary_extra)                     { $1 }

assign_expr(r,pe):
    cond_expr(r,pe)                        { $1 }
  | unary_expr(r,pe) TOpAssign assign_expr_bis
      { let (op,clt) = $2 in
      let op' = P.clt2mcode op clt in
      let op'' = Ast0.wrap (Ast0.OpAssign op') in
      Ast0.wrap(Ast0.Assignment($1, op'', Ast0.set_arg_exp $3,false)) }
  | unary_expr(r,pe) TEq assign_expr_bis
      { let (op,clt) = ("=",$2) in
      let op' = P.clt2mcode op clt in
      let op'' = Ast0.wrap (Ast0.SimpleAssign op') in
      Ast0.wrap
	  (Ast0.Assignment
	     ($1, op'', Ast0.set_arg_exp $3,false)) }
  | unary_expr(r,pe) TMetaAssignOp assign_expr_bis
      { let (mv, cstrt, pure, clt) = $2 in
      let op' = P.clt2mcode mv clt in
      let op'' = Ast0.wrap (Ast0.MetaAssign (op', cstrt, pure)) in
      Ast0.wrap
	  (Ast0.Assignment
	     ($1, op'', Ast0.set_arg_exp $3,false)) }

assign_expr_bis:
    cond_expr(eexpr,dot_expressions)                        { $1 }
  | unary_expr(eexpr,dot_expressions) TOpAssign assign_expr_bis
      { let (op,clt) = $2 in
      let op' = P.clt2mcode op clt in
      let op'' = Ast0.wrap (Ast0.OpAssign op') in
      Ast0.wrap(Ast0.Assignment($1, op'', Ast0.set_arg_exp $3,false)) }
  | unary_expr(eexpr,dot_expressions) TEq assign_expr_bis
      { let (op,clt) = ("=",$2) in
      let op' = P.clt2mcode op clt in
      let op'' = Ast0.wrap (Ast0.SimpleAssign op') in
      Ast0.wrap
	  (Ast0.Assignment
	     ($1, op'', Ast0.set_arg_exp $3,false)) }

cond_expr(r,pe):
    arith_expr(r,pe)                         { $1 }
  | l=arith_expr(r,pe) w=TWhy t=option(eexpr)
      dd=TDotDot r=eargexpr/*see parser_c*/
      { Ast0.wrap(Ast0.CondExpr (l, P.clt2mcode "?" w, t,
				 P.clt2mcode ":" dd, r)) }

arith_expr(r,pe):
    cast_expr(r,pe)                         { $1 }
  | arith_expr(r,pe) TMul    arith_expr_bis
    { P.arith_op Ast.Mul $1 $2 $3 }
  | arith_expr(r,pe) TDmOp    arith_expr_bis
      { let (op,clt) = $2 in P.arith_op op $1 clt $3 }
  | arith_expr(r,pe) TPlus   arith_expr_bis
      { P.arith_op Ast.Plus $1 $2 $3 }
  | arith_expr(r,pe) TMinus  arith_expr_bis
      { P.arith_op Ast.Minus $1 $2 $3 }
  | arith_expr(r,pe) TShLOp    arith_expr_bis
      { let (op,clt) = $2 in P.arith_op op $1 clt $3 }
  | arith_expr(r,pe) TShROp    arith_expr_bis
      { let (op,clt) = $2 in P.arith_op op $1 clt $3 }
  | arith_expr(r,pe) TLogOp    arith_expr_bis
      { let (op,clt) = $2 in P.logic_op op $1 clt $3 }
  | arith_expr(r,pe) TEqEq   arith_expr_bis
      { P.logic_op Ast.Eq $1 $2 $3 }
  | arith_expr(r,pe) TNotEq  arith_expr_bis
      { P.logic_op Ast.NotEq $1 $2 $3 }
  | arith_expr(r,pe) TAnd    arith_expr_bis
      { P.arith_op Ast.And $1 $2 $3 }
  | arith_expr(r,pe) TOr     arith_expr_bis
      { P.arith_op Ast.Or $1 $2 $3 }
  | arith_expr(r,pe) TXor    arith_expr_bis
      { P.arith_op Ast.Xor $1 $2 $3 }
  | arith_expr(r,pe) TAndLog arith_expr_bis
      { P.logic_op Ast.AndLog $1 $2 $3 }
  | arith_expr(r,pe) TOrLog  arith_expr_bis
      { P.logic_op Ast.OrLog $1 $2 $3 }
  | arith_expr(r,pe) TMetaBinaryOp  arith_expr_bis
      { let (mv, cstrt, pure, clt) = $2 in
      let op' = P.clt2mcode mv clt in
      let op = Ast0.wrap (Ast0.MetaBinary (op', cstrt, pure)) in
      Ast0.wrap (Ast0.Binary($1, op, $3)) }

// allows dots now that an expression-specific token has been seen
// need an extra rule because of recursion restrictions
arith_expr_bis:
    cast_expr(eexpr,dot_expressions)                         { $1 }
  | arith_expr_bis TMul    arith_expr_bis
      { P.arith_op Ast.Mul $1 $2 $3 }
  | arith_expr_bis TDmOp    arith_expr_bis
      { let (op,clt) = $2 in P.arith_op op $1 clt $3 }
  | arith_expr_bis TPlus   arith_expr_bis
      { P.arith_op Ast.Plus $1 $2 $3 }
  | arith_expr_bis TMinus  arith_expr_bis
      { P.arith_op Ast.Minus $1 $2 $3 }
  | arith_expr_bis TShLOp    arith_expr_bis
      { let (op,clt) = $2 in P.arith_op op $1 clt $3 }
  | arith_expr_bis TShROp    arith_expr_bis
      { let (op,clt) = $2 in P.arith_op op $1 clt $3 }
  | arith_expr_bis TLogOp    arith_expr_bis
      { let (op,clt) = $2 in P.logic_op op $1 clt $3 }
  | arith_expr_bis TEqEq   arith_expr_bis
      { P.logic_op Ast.Eq $1 $2 $3 }
  | arith_expr_bis TNotEq  arith_expr_bis
      { P.logic_op Ast.NotEq $1 $2 $3 }
  | arith_expr_bis TAnd    arith_expr_bis
      { P.arith_op Ast.And $1 $2 $3 }
  | arith_expr_bis TOr     arith_expr_bis
      { P.arith_op Ast.Or $1 $2 $3 }
  | arith_expr_bis TXor    arith_expr_bis
      { P.arith_op Ast.Xor $1 $2 $3 }
  | arith_expr_bis TAndLog arith_expr_bis
      { P.logic_op Ast.AndLog $1 $2 $3 }
// no OrLog because it is left associative and this is for
// a right argument, not sure why not the same problem for AndLog

cast_expr(r,pe):
    unary_expr(r,pe)                      { $1 }
  | lp=TOPar t=ctype rp=TCPar e=cast_expr(r,pe)
      { Ast0.wrap(Ast0.Cast (P.clt2mcode "(" lp, t,
			     P.clt2mcode ")" rp, e)) }
  | lp=TOPar t=ctype lp1=TOPar s=TMul rp1=TCPar
      lp2=TOPar d=decl_list(name_opt_decl) rp2=TCPar rp=TCPar
      e=cast_expr(r,pe)
      { let fnptr =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" s,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,d,P.clt2mcode ")" rp2)) in
      Ast0.wrap(Ast0.Cast (P.clt2mcode "(" lp, fnptr, P.clt2mcode ")" rp, e)) }

unary_expr(r,pe):
    postfix_expr(r,pe)                   { $1 }
  | TInc unary_expr_bis
      { Ast0.wrap(Ast0.Infix ($2, P.clt2mcode Ast.Inc $1)) }
  | TDec unary_expr_bis
      { Ast0.wrap(Ast0.Infix ($2, P.clt2mcode Ast.Dec $1)) }
  | unary_op cast_expr(r,pe)
      { let mcode = $1 in Ast0.wrap(Ast0.Unary($2, mcode)) }
  | TSizeof unary_expr_bis
      { Ast0.wrap(Ast0.SizeOfExpr (P.clt2mcode "sizeof" $1, $2)) }
  | s=TSizeof lp=TOPar t=ctype rp=TCPar
      { Ast0.wrap(Ast0.SizeOfType (P.clt2mcode "sizeof" s,
                                   P.clt2mcode "(" lp,t,
                                   P.clt2mcode ")" rp)) }

// version that allows dots
unary_expr_bis:
    postfix_expr(eexpr,dot_expressions)                   { $1 }
  | TInc unary_expr_bis
      { Ast0.wrap(Ast0.Infix ($2, P.clt2mcode Ast.Inc $1)) }
  | TDec unary_expr_bis
      { Ast0.wrap(Ast0.Infix ($2, P.clt2mcode Ast.Dec $1)) }
  | unary_op cast_expr(eexpr,dot_expressions)
      { let mcode = $1 in Ast0.wrap(Ast0.Unary($2, mcode)) }
  | TSizeof unary_expr_bis
      { Ast0.wrap(Ast0.SizeOfExpr (P.clt2mcode "sizeof" $1, $2)) }
  | s=TSizeof lp=TOPar t=ctype rp=TCPar
      { Ast0.wrap(Ast0.SizeOfType (P.clt2mcode "sizeof" s,
                                   P.clt2mcode "(" lp,t,
                                   P.clt2mcode ")" rp)) }

unary_op: TAnd    { P.clt2mcode Ast.GetRef $1 }
	| TMul    { P.clt2mcode Ast.DeRef $1 }
	| TPlus   { P.clt2mcode Ast.UnPlus $1 }
	| TMinus  { P.clt2mcode Ast.UnMinus $1 }
	| TTilde  { P.clt2mcode Ast.Tilde $1 }
	| TBang   { P.clt2mcode Ast.Not $1 }

postfix_expr(r,pe):
   primary_expr(r,pe)                            { $1 }
 | postfix_expr(r,pe) TOCro eexpr TCCro
     { Ast0.wrap(Ast0.ArrayAccess ($1,P.clt2mcode "[" $2,$3,
				       P.clt2mcode "]" $4)) }
 | postfix_expr(r,pe) TDot   disj_ident
     { Ast0.wrap(Ast0.RecordAccess($1, P.clt2mcode "." $2, $3)) }
 | postfix_expr(r,pe) TPtrOp disj_ident
     { Ast0.wrap(Ast0.RecordPtAccess($1, P.clt2mcode "->" $2,
				     $3)) }
 | postfix_expr(r,pe) TInc
     { Ast0.wrap(Ast0.Postfix ($1, P.clt2mcode Ast.Inc $2)) }
 | postfix_expr(r,pe) TDec
     { Ast0.wrap(Ast0.Postfix ($1, P.clt2mcode Ast.Dec $2)) }
 | postfix_expr(r,pe) TOPar eexpr_list_option TCPar
     { Ast0.wrap(Ast0.FunCall($1,P.clt2mcode "(" $2,
			      $3,
			      P.clt2mcode ")" $4)) }
 /*(* gccext: also called compound literals *)
   empty case causes conflicts */
 | TOPar ctype TCPar TOBrace initialize_list TCBrace
     { let init =
       if P.struct_initializer $5
       then
	 let il = P.drop_dot_commas $5 in
	 Ast0.wrap
	   (Ast0.InitList(P.clt2mcode "{" $4,il,P.clt2mcode "}" $6,false))
       else
	 Ast0.wrap
	   (Ast0.InitList(P.clt2mcode "{" $4,$5,P.clt2mcode "}" $6,true)) in
     Ast0.wrap
       (Ast0.Constructor(P.clt2mcode "(" $1, $2, P.clt2mcode ")" $3, init)) }

primary_expr(recurser,primary_extra):
   func_ident   { Ast0.wrap(Ast0.Ident($1)) }
 | TAndLog ident
     { let op = P.clt2mcode Ast.GetRefLabel $1 in
     Ast0.wrap(Ast0.Unary(Ast0.wrap(Ast0.Ident($2)), op)) }
 | TInt
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (P.clt2mcode (Ast.Int x) clt)) }
 | TFloat
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (P.clt2mcode (Ast.Float x) clt)) }
 | TString
     { let (x,clt) = $1 in P.parse_string x clt }
 | TChar
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (P.clt2mcode (Ast.Char x) clt)) }
 | TDecimalCst
     { let (x,l,p,clt) = $1 in
     Ast0.wrap(Ast0.Constant (P.clt2mcode (Ast.DecimalConst(x,l,p)) clt)) }
 | TMetaConst
     { let (nm,constraints,pure,ty,clt) = $1 in
     Ast0.wrap
       (Ast0.MetaExpr(P.clt2mcode nm clt,constraints,ty,Ast.CONST,pure,None)) }
 | TMetaErr
     { let (nm,constraints,pure,clt) = $1 in
     Ast0.wrap(Ast0.MetaErr(P.clt2mcode nm clt,constraints,pure)) }
 | TMetaExp
     { let (nm,constraints,pure,ty,clt,bitfield) = $1 in
     let bitfield' = Common.map_option (P.dolen clt) bitfield in
     Ast0.wrap
       (Ast0.MetaExpr
	  (P.clt2mcode nm clt,constraints,ty,Ast.ANY,pure,bitfield')) }
 | TMetaIdExp
     { let (nm,constraints,pure,ty,clt) = $1 in
     Ast0.wrap
       (Ast0.MetaExpr(P.clt2mcode nm clt,constraints,ty,Ast.ID,pure,None)) }
 | TMetaLocalIdExp
     { let (nm,constraints,pure,ty,clt) = $1 in
     Ast0.wrap
       (Ast0.MetaExpr
	  (P.clt2mcode nm clt,constraints,ty,Ast.LocalID,pure,None)) }
 | TMetaGlobalIdExp
     { let (nm,constraints,pure,ty,clt) = $1 in
     Ast0.wrap
       (Ast0.MetaExpr
	  (P.clt2mcode nm clt,constraints,ty,Ast.GlobalID,pure,None)) }
 | TOPar eexpr TCPar
     { Ast0.wrap(Ast0.Paren(P.clt2mcode "(" $1,$2,
			    P.clt2mcode ")" $3)) }
 | TOPar0 midzero_list(recurser,eexpr) TCPar0
     { let (mids,code) = $2 in
       Ast0.wrap(Ast0.DisjExpr(P.id2mcode $1,
			       code, mids,
			       P.id2mcode $3)) }
 | TOPar0 andzero_list(recurser,eexpr) TCPar0
     { let (mids,code) = $2 in
       Ast0.wrap(Ast0.ConjExpr(P.id2mcode $1,
			       code, mids,
			       P.id2mcode $3)) }
 | primary_extra { $1 }

expr_dots(dotter):
    r=no_dot_start_end(dexpr,edots_when(dotter,eexpr)) { r }

// used in NEST
no_dot_start_end(grammar,dotter):
  g=grammar dg=list(pair(dotter,grammar))
  { function dot_builder ->
      g :: (List.concat(List.map (function (d,g) -> [dot_builder d;g]) dg)) }

/*****************************************************************************/

pure_ident:
    TIdent { $1 }

pure_ident_or_symbol:
    pure_ident { $1 }
  | TSymId { $1 }

pure_ident_kwd:
   | TIdentifier { "identifier" }
   | TExpression { "expression" }
   | TStatement { "statement" }
   | TFunction { "function" }
   | TLocal { "local" }
   | TType { "type" }
   | TParameter { "parameter" }
   | TIdExpression { "idexpression" }
   | TInitialiser { "initialiser" }
   | Tlist { "list" }
   | TFresh { "fresh" }
   | TConstant { "constant" }
   | TError { "error" }
   | TWords { "words" }
   | TPure { "pure" }
   | TContext { "context" }
   | TGenerated { "generated" }
   | TTypedef { "typedef" }
   | TDeclarer { "declarer" }
   | TIterator { "iterator" }
   | TName { "name" }
   | TPosition { "position" }
   | TSymbol { "symbol" }

meta_ident_sym: /* these can only be redefined as metavars, not other syms */
     TSymId { P.id2name $1 }
   | TTypeId { P.id2name $1 }
   | TIteratorId { P.id2name $1 }
   | TDeclarerId { P.id2name $1 }

meta_ident:
     TRuleName TDot pure_ident     { (Some $1,P.id2name $3) }
   | TRuleName TDot pure_ident_kwd { (Some $1,$3) }

pure_ident_or_meta_ident:
       pure_ident                { (None,P.id2name $1) }
     | pure_ident_kwd            { (None,$1) }
     | meta_ident                { $1 }
     | meta_ident_sym            { (None,$1) }

pure_ident_or_meta_ident_nosym:
       pure_ident                { (None,P.id2name $1) }
     | pure_ident_kwd            { (None,$1) }
     | meta_ident                { $1 }

pure_ident_or_meta_ident_nosym2(extra):
       pure_ident_or_meta_ident_nosym { $1 }
     | extra                          { (None,P.id2name $1) }

local_meta:
       TMetaId { let (nm,_,_,_,_) = $1 in nm }
     | TMetaFunc { let (nm,_,_,_) = $1 in nm }
     | TMetaLocalFunc { let (nm,_,_,_) = $1 in nm }
     | TMetaIterator { let (nm,_,_,_) = $1 in nm }
     | TMetaDeclarer { let (nm,_,_,_) = $1 in nm }
     | TMetaAssignOp { let (nm,_,_,_) = $1 in nm }
     | TMetaType { let (nm,_,_,_) = $1 in nm }
     | TMetaBinaryOp { let (nm,_,_,_) = $1 in nm }
     | TMetaErr { let (nm,_,_,_) = $1 in nm }
     | TMetaParam { let (nm,_,_,_) = $1 in nm }
     | TMetaStm { let (nm,_,_,_) = $1 in nm }
     | TMetaInit { let (nm,_,_,_) = $1 in nm }
     | TMetaDecl { let (nm,_,_,_) = $1 in nm }
     | TMetaField { let (nm,_,_,_) = $1 in nm }
     | TMeta { let (nm,_,_,_) = $1 in nm }
     | TMetaParamList { let (nm,_,_,_,_) = $1 in nm }
     | TMetaExpList { let (nm,_,_,_,_) = $1 in nm }
     | TMetaInitList { let (nm,_,_,_,_) = $1 in nm }
     | TMetaFieldList { let (nm,_,_,_,_) = $1 in nm }
     | TMetaStmList { let (nm,_,_,_,_) = $1 in nm }
     | TMetaDParamList { let (nm,_,_,_,_) = $1 in nm }
     | TMetaExp { let (nm,_,_,_,_,_) = $1 in nm }
     | TMetaIdExp { let (nm,_,_,_,_) = $1 in nm }
     | TMetaLocalIdExp { let (nm,_,_,_,_) = $1 in nm }
     | TMetaGlobalIdExp { let (nm,_,_,_,_) = $1 in nm }
     | TMetaConst { let (nm,_,_,_,_) = $1 in nm }
     | TMetaPos { let (nm,_,_,_) = $1 in nm }

inherited_or_local_meta:
       local_meta                    { $1 }
     | TRuleName TDot pure_ident     { ($1,P.id2name $3) }
     | TRuleName TDot pure_ident_kwd { ($1,$3) }

wrapped_sym_ident:
  TSymId { Ast0.wrap(Ast0.Id(P.sym2mcode $1)) }

pure_ident_or_meta_ident_with_seed:
       pure_ident_or_meta_ident { ($1,Ast.NoVal) }
     | pure_ident_or_meta_ident TEq
	 separated_nonempty_list(TCppConcatOp,seed_elem)
	 { match $3 with
	   [Ast.SeedString s] -> ($1,Ast.StringSeed s)
	 | _ -> ($1,Ast.ListSeed $3) }

seed_elem:
  TString { let (x,_) = $1 in Ast.SeedString x }
| TMetaId { let (x,_,_,_,_) = $1 in Ast.SeedId x }
| TMeta {failwith "tmeta"}
| TVirtual TDot pure_ident
    { let nm = ("virtual",P.id2name $3) in
     Iteration.parsed_virtual_identifiers :=
       Common.union_set [snd nm]
	 !Iteration.parsed_virtual_identifiers;
    try Ast.SeedString (List.assoc (snd nm) !Flag.defined_virtual_env)
    with Not_found -> Ast.SeedId nm }
| TRuleName TDot pure_ident
    { let nm = ($1,P.id2name $3) in
      P.check_meta(Ast.MetaIdDecl(Ast.NONE,nm));
      Ast.SeedId nm }

pure_ident_or_meta_ident_with_constraints:
  i=pure_ident_or_meta_ident c=constraints { (i,c false i) }

pure_ident_or_meta_ident_with_constraints_pos:
  i=pure_ident_or_meta_ident c=constraints { (i,c true i) }

pure_ident_or_meta_ident_with_constraints_virt:
  i=pure_ident_or_meta_ident_with_constraints {  Common.Left i }
| TVirtual TDot pure_ident
    { let nm = P.id2name $3 in
      Iteration.parsed_virtual_identifiers :=
	Common.union_set [nm]
	  !Iteration.parsed_virtual_identifiers;
      Common.Right nm }

constraints:
    { fun _ _ -> Ast.CstrTrue }
| c=nonempty_constraints
    { check_constraint_allowed ();
      c }

nonempty_constraints:
  TTildeEq re=TString
    { fun _ _ -> let (s,_) = re in Ast.CstrRegexp (s,Regexp.regexp s) }
| TTildeExclEq re=TString
    { fun _ _ ->
      let (s,_) = re in Ast.CstrNot (Ast.CstrRegexp (s,Regexp.regexp s)) }
| TEq l=item_or_brace_list(cstr_ident) { fun _ _ -> Ast.CstrOr l }
| TNotEq l=item_or_brace_list(cstr_ident)
    { fun _ _ -> Ast.CstrNot (Ast.CstrOr l) }
| TSub i=TInt { fun _ _ ->
  let i = int_of_string (fst i) in
  Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntLeq i)) }
| o=TLogOp i=TInt { fun _ _ ->
  let i = int_of_string (fst i) in
  match o with
    Ast.SupEq, _ -> Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntGeq i))
  | Ast.Inf, _ -> Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntLeq (pred i)))
  | Ast.Sup, _ -> Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntGeq (succ i)))
  | _ -> raise (Semantic_cocci.Semantic "unknown constraint operator") }
| TSub l=item_or_brace_list(sub_meta_ident) { fun _ _ -> Ast.CstrSub l }
| TDotDot pos=TScript TDotDot lang=pure_ident
    TOPar params=loption(comma_list(inherited_or_local_meta)) TCPar
    TOBrace c=list(anything) TCBrace
    { fun posvar nm ->
      let rule =
	String.concat "_" (Str.split (Str.regexp " ") !Ast0.rule_name) in
      let key = Printf.sprintf "constraint_code_%s_0_%s" rule (snd nm) in
      let code = String.concat " " c in
      let lang' = P.id2name lang in
      let code' =
	if lang' = "ocaml" then
	  let (file, line) = pos in
	  Printf.sprintf "\n# %d \"%s\"\n%s" line file code
	else code in
      let nm' =
	match nm with
	  None, n -> "", n
	| Some r, n -> r, n in
      let local =
	List.for_all (fun (rl,_) -> not (rl = !Ast0.rule_name)) params in
      let params =
	List.map
	  (fun (rl,nm) ->
            let mv = Parse_aux.lookup rl nm in
	    ((rl,nm),mv))
	  (List.rev params) in
      let script = (key, lang', params, pos, code') in
      Data.constraint_scripts :=
	(posvar, nm', script) :: !Data.constraint_scripts;
      (if not local
      then
	(let mv =
	  match nm with
	    (None,nm) -> (!Ast0.rule_name,nm)
	  | (Some rule,nm) -> (rule,nm) in
	Common.hashadd Data.non_local_script_constraints
	  (!Ast0.rule_name,mv) (mv,script)));
      Ast.CstrScript (local,script) }
| TBang c = nonempty_constraints { fun posvar nm -> Ast.CstrNot (c posvar nm) }
| l=nonempty_constraints TAndLog r=nonempty_constraints
    { fun posvar nm -> Ast.CstrAnd [l posvar nm; r posvar nm] }
| l=nonempty_constraints TOrLog r=nonempty_constraints
    { fun posvar nm -> Ast.CstrOr [l posvar nm; r posvar nm] }
| TOPar c=nonempty_constraints TCPar { c }

item_or_brace_list(item):
  i=item { [i] }
| TOBrace l=comma_list(item) TCBrace { l }

cstr_ident:
  c=ctype_or_ident
  { match c with
      Common.Left ty -> Ast.CstrType (Ast0toast.typeC false ty)
    | Common.Right i ->
	match i with
	  (Some rn,id) ->
	    let i = P.check_inherited_constraint_without_type i in
	    (Ast.CstrMeta_name i)
	| (None,s) -> Ast.CstrConstant (Ast.CstrString s) }
| i=TInt {
  let i = int_of_string (fst i) in
  Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntEq i)) }
| i=TInt TDot TDot TDot j=TInt {
  let i = int_of_string (fst i) and j = int_of_string (fst j) in
  Ast.CstrAnd [
  Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntGeq i));
  Ast.CstrConstant (Ast.CstrInt (Ast.CstrIntLeq j))] }
| op=operator_constraint { Ast.CstrOperator op }

ctype_or_ident:
| cv=ioption(const_vol) c=all_basic_types_or_ident m=list(mul)
    { match cv, c, m with
	None, Common.Right ident, [] -> Common.Right ident
      | _ ->
	  let ty =
	    match c with
	      Common.Left ty -> ty
	    | Common.Right (None, p) ->
		Ast0.wrap(Ast0.TypeName(Ast0.make_mcode p))
	    | Common.Right (Some r, p) ->
		let nm = (r, p) in
		let _ = P.check_meta(Ast.MetaTypeDecl(Ast.NONE,nm)) in
		Ast0.wrap(
		  Ast0.MetaType(Ast0.make_mcode nm,Ast.CstrTrue,
				Ast0.Impure (*will be ignored*))) in
	  let f prev (star,cv) = P.make_cv cv (P.pointerify prev [star]) in
	  Common.Left (List.fold_left f (P.make_cv cv ty) m) }
| ty=signed_or_unsigned { Common.Left ty }

all_basic_types_or_ident:
  ty=all_basic_types_no_ident { Common.Left ty }
| i=pure_ident_or_meta_ident_nosym { Common.Right i }

operator_constraint:
  op=binary_operator { Ast.CstrBinaryOp (Ast0toast.binaryOp op) }
| op=assignment_operator { Ast.CstrAssignOp (Ast0toast.assignOp op) }

binary_operator:
  TShLOp { mkarithop $1 } (* Ast.Arith Ast.DecLeft *)
| TMul { mkarithop (Ast.Mul,$1) }
| TEqEq { mklogop (Ast.Eq,$1) }
| TNotEq { mklogop (Ast.NotEq,$1) }
| TSub { mklogop (Ast.InfEq,$1) }
| TPlus { mkarithop (Ast.Plus,$1) }
| TMinus { mkarithop (Ast.Minus,$1) }
| TDmOp { mkarithop $1 }
| TShROp { mkarithop $1 }
| TAnd { mkarithop (Ast.And,$1) }
| TOr { mkarithop (Ast.Or,$1) }
| TXor { mkarithop (Ast.Xor,$1) }
| TLogOp { mklogop $1 }
| TAndLog { mklogop (Ast.AndLog,$1) }
| TOrLog { mklogop (Ast.OrLog,$1) }

assignment_operator:
  TEq
    { let clt = $1 in
      let op' = P.clt2mcode "=" clt in
      Ast0.wrap (Ast0.SimpleAssign op') }
| TOpAssign
    { let (op,clt) = $1 in
      let op' = P.clt2mcode op clt in
      Ast0.wrap (Ast0.OpAssign op') }

sub_meta_ident:
     (* has to be inherited because not clear how to check subterm constraints
	in the functorized CTL engine, so need the variable to be bound
	already when bind the subterm constrained metavariable *)
  i=meta_ident
    { P.check_inherited_constraint i
	(function mv -> Ast.MetaExpDecl(Ast.NONE,mv,None,None)) }

func_ident:
       ident { $1 }
     | TMetaFunc
         { let (nm,constraints,pure,clt) = $1 in
	 Ast0.wrap(Ast0.MetaFunc(P.clt2mcode nm clt,constraints,pure)) }
     | TMetaLocalFunc
	 { let (nm,constraints,pure,clt) = $1 in
	 Ast0.wrap
	   (Ast0.MetaLocalFunc(P.clt2mcode nm clt,constraints,pure)) }

fn_ident: disj_ident { $1 }
     | TMetaFunc
         { let (nm,constraints,pure,clt) = $1 in
	 Ast0.wrap(Ast0.MetaFunc(P.clt2mcode nm clt,constraints,pure)) }
     | TMetaLocalFunc
	 { let (nm,constraints,pure,clt) = $1 in
	 Ast0.wrap
	   (Ast0.MetaLocalFunc(P.clt2mcode nm clt,constraints,pure)) }

ident: pure_ident
         { Ast0.wrap(Ast0.Id(P.id2mcode $1)) }
     | wrapped_sym_ident { $1 }
     | TMetaId
         { let (nm,constraints,seed,pure,clt) = $1 in
         Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,constraints,seed,pure)) }

ident_or_kwd: pure_ident
         { Ast0.wrap(Ast0.Id(P.id2mcode $1)) }
     | wrapped_sym_ident { $1 }
     | TMeta { tmeta_to_ident $1 }
     | TMetaId
         { let (nm,constraints,seed,pure,clt) = $1 in
         Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,constraints,seed,pure)) }
     | Tinline { Ast0.wrap(Ast0.Id(P.clt2mcode "inline" $1)) }

mident: pure_ident
         { Ast0.wrap(Ast0.Id(P.id2mcode $1)) }
     | wrapped_sym_ident { $1 }
     | TMeta { tmeta_to_ident $1 }
     | TMetaId
         { let (nm,constraints,seed,pure,clt) = $1 in
         Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,constraints,seed,pure)) }

disj_ident:
       mident { $1 }
     | s = close_disj_ident(disj_ident) { s }

close_disj_ident(sub_ident):
     | lp=TOPar0 t=midzero_list(sub_ident,sub_ident) rp=TCPar0
	 { let (mids,code) = t in
	 Ast0.wrap
	   (Ast0.DisjId(P.id2mcode lp,code,mids, P.id2mcode rp)) }

top_ident:
  disj_ident { Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.TopId($1)))) }

type_ident: disj_ident { $1 }
     | TTypeId
         { Ast0.wrap(Ast0.Id(P.id2mcode $1)) }

decl_ident:
       TDeclarerId
         { Ast0.wrap(Ast0.Id(P.id2mcode $1)) }
     | TMetaDeclarer
         { let (nm,constraints,pure,clt) = $1 in
         Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,constraints,Ast.NoVal,pure)) }
     | s = close_disj_ident(decl_ident) { s }

iter_ident:
       TIteratorId
         { Ast0.wrap(Ast0.Id(P.id2mcode $1)) }
     | TMetaIterator
         { let (nm,constraints,pure,clt) = $1 in
         Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,constraints,Ast.NoVal,pure)) }

typedef_ident:
       pure_ident_or_symbol
         { Ast0.wrap(Ast0.TypeName(P.id2mcode $1)) }
     | TMeta { tmeta_to_type $1 }
     | TMetaType
         { let (nm,cstr,pure,clt) = $1 in
	 Ast0.wrap(Ast0.MetaType(P.clt2mcode nm clt,cstr,pure)) }

/*****************************************************************************/

decl_list(decl):
  empty_list_start(one_dec(decl),TEllipsis)
     { Ast0.wrap
	 ($1
	    (fun _ d -> Ast0.wrap(Ast0.Pdots(P.clt2mcode "..." d)))
	    (fun c -> Ast0.PComma c)) }

one_dec(decl):
  decl  { $1 }
| TMetaParamList
    { let (nm,lenname,cstr,pure,clt) = $1 in
    let nm = P.clt2mcode nm clt in
      let lenname = P.dolen clt lenname in
    Ast0.wrap(Ast0.MetaParamList(nm,lenname,cstr,pure)) }

/* ---------------------------------------------------------------------- */
/* comma list parser, used for fn params, fn args, enums, initlists,
   #define params */

/* enums: enum_decl, edots_when(TEllipsis,enum_decl_one)
fun s d -> P.mkedots "..." d
fun c -> Ast0.EComma c
 */

empty_list_start(elem,dotter):
  /* empty */ { fun build_dots build_comma -> [] }
| list=nonempty_list_start(elem,dotter) { list }

nonempty_list_start(elem,dotter): /* dots allowed */
  element=elem { fun build_dots build_comma -> [element] }
| element=elem comma=TComma
    { fun build_dots build_comma ->
      element::[Ast0.wrap(build_comma(P.clt2mcode "," comma))] }
| element=elem comma=TComma remainder=nonempty_list_start(elem,dotter)
    { fun build_dots build_comma ->
      element::(Ast0.wrap(build_comma(P.clt2mcode "," comma)))::
      (remainder build_dots build_comma) }
| TNothing list=nonempty_list_start(elem,dotter) { list }
| dotter=dotter { fun build_dots build_comma -> [(build_dots "..." dotter)] }
| dotter=dotter comma=TComma
      { fun build_dots build_comma ->
	[(build_dots "..." dotter);Ast0.wrap(build_comma(P.clt2mcode "," comma))] }
| dotter=dotter comma=TComma remainder=continue_list(elem,dotter)
    { fun build_dots build_comma ->
      (build_dots "..." dotter)::
      (Ast0.wrap(build_comma(P.clt2mcode "," comma)))::
      (remainder build_dots build_comma) }

continue_list(elem,dotter): /* dots not allowed */
  element=elem { fun build_dots build_comma -> [element] }
| element=elem comma=TComma
    { fun build_dots build_comma ->
      element::[Ast0.wrap(build_comma(P.clt2mcode "," comma))] }
| element=elem comma=TComma remainder=nonempty_list_start(elem,dotter)
    { fun build_dots build_comma ->
      element::(Ast0.wrap(build_comma(P.clt2mcode "," comma)))::
      (remainder build_dots build_comma) }
| TNothing list=nonempty_list_start(elem,dotter) { list }

/* ---------------------------------------------------------------------- */

/* error words make it complicated to be able to use error as a metavariable
name or a type in a metavariable list; for that we would like to allow TError
as an ident, but that makes conflicts with this rule.  To add back error words,
need to find some appropriate delimiter for it, but it has not been used much
so just drop it */
/*error_words:
    TError TWords TEq TOCro cl=comma_list(dexpr) TCCro
      { [Ast0.wrap(Ast0.ERRORWORDS(cl))] }
*/

/* ---------------------------------------------------------------------- */
/* sequences of statements and expressions */

/* There are number of cases that must be considered:

1. Top level:
   Dots and nests allowed at the beginning or end
   Expressions allowed at the beginning or end
   One function allowed, by itself
2. A function body:
   Dots and nests allowed at the beginning or end
   Expressions not allowed at the beginning or end
   Functions not allowed
3. The body of a nest:
   Dots and nests not allowed at the beginning or end
   Expressions allowed at the beginning or end
   Functions not allowed
4. Whencode:
   Dots and nests not allowed at the beginning but allowed at the end
   Expressions allowed at the beginning or end
   Functions not allowed

These are implemented by the rules minus_toplevel_sequence,
plus_toplevel_sequence, function_body_sequence, nest_body_sequence, and
when_body_sequence.
*/
/* ------------------------------------------------------------------------ */
/* Minus top level */

/* doesn't allow only ... */
minus_start:
  fundecl                { [Ast0.wrap(Ast0.OTHER($1))] }
| ctype                  { [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Ty($1))))] }
| top_init          { [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.TopInit($1))))] }
| toplevel_seq_startne(toplevel_after_dots_init)
    { List.map (function x -> Ast0.wrap(Ast0.OTHER(x))) $1 }

toplevel_seq_startne(after_dots_init):
  a=stm_dots_ell b=after_dots_init           { a::b }
| a=stm_dots_nest b=after_dots_init           { a::b }
| a=stm_dots_nest                      { [a] }
| expr toplevel_after_exp            { (Ast0.wrap(Ast0.Exp($1)))::$2 }
| decl_statement_expr toplevel_after_stm  { $1@$2 }

toplevel_seq_start(after_dots_init):
  stm_dots after_dots_init           { $1::$2 }
| expr toplevel_after_exp            { (Ast0.wrap(Ast0.Exp($1)))::$2 }
| decl_statement_expr toplevel_after_stm  { $1@$2 }

toplevel_after_dots_init:
  TNothing toplevel_after_exp        {$2}
| expr toplevel_after_exp            {(Ast0.wrap(Ast0.Exp($1)))::$2}
| decl_statement_expr toplevel_after_stm  {$1@$2}

toplevel_after_exp:
  /* empty */                        {[]}
| stm_dots toplevel_after_dots       {$1::$2}

toplevel_after_dots:
  /* empty */                        {[]}
| TNothing toplevel_after_exp        {$2}
| expr toplevel_after_exp            {(Ast0.wrap(Ast0.Exp($1)))::$2}
| decl_statement_expr toplevel_after_stm  {$1@$2}

toplevel_after_stm:
  /* empty */                        {[]}
| stm_dots toplevel_after_dots       {$1::$2}
| decl_statement toplevel_after_stm  {$1@$2}

top_init:
  TOInit initialize_list TCBrace
    { if P.struct_initializer $2
    then
      let il = P.drop_dot_commas $2 in
      Ast0.wrap(Ast0.InitList(P.clt2mcode "{" $1,il,P.clt2mcode "}" $3,false))
    else
      Ast0.wrap(Ast0.InitList(P.clt2mcode "{" $1,$2,P.clt2mcode "}" $3,true)) }

/* ------------------------------------------------------------------------ */
/* Plus top level */

/* does allow only ... also allows multiple top-level functions */
plus_start:
  ctype                   { [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Ty($1))))] }
| top_init           { [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.TopInit($1))))] }
| stm_dots plus_after_dots
                                          { (Ast0.wrap(Ast0.OTHER($1)))::$2 }
| expr plus_after_exp
                     { (Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1)))))::$2 }
| fundecl plus_after_stm                     { Ast0.wrap(Ast0.OTHER($1))::$2 }
| decl_statement_expr plus_after_stm
                { (List.map (function x -> Ast0.wrap(Ast0.OTHER(x))) $1)@$2 }

plus_after_exp:
  /* empty */                                                            {[]}
| stm_dots plus_after_dots                { (Ast0.wrap(Ast0.OTHER($1)))::$2 }

plus_after_dots:
  /* empty */                                                            {[]}
| TNothing plus_after_exp                                                {$2}
| expr plus_after_exp
                     { (Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1)))))::$2 }
| fundecl plus_after_stm                     { Ast0.wrap(Ast0.OTHER($1))::$2 }
| decl_statement_expr plus_after_stm
                { (List.map (function x -> Ast0.wrap(Ast0.OTHER(x))) $1)@$2 }

plus_after_stm:
  /* empty */                                                            {[]}
| stm_dots plus_after_dots                { (Ast0.wrap(Ast0.OTHER($1)))::$2 }
| fundecl plus_after_stm                     { Ast0.wrap(Ast0.OTHER($1))::$2 }
| decl_statement plus_after_stm
                { (List.map (function x -> Ast0.wrap(Ast0.OTHER(x))) $1)@$2 }

/* ------------------------------------------------------------------------ */
/* Function body */

fun_start:
  fun_after_stm  { Ast0.wrap $1 }

fun_after_stm:
  /* empty */                  {[]}
| stm_dots fun_after_dots      {$1::$2}
| decl_statement fun_after_stm {$1@$2}

fun_after_dots:
  /* empty */                  {[]}
| TNothing fun_after_exp       {$2}
| expr fun_after_exp           {Ast0.wrap(Ast0.Exp($1))::$2}
| decl_statement_expr fun_after_stm {$1@$2}

fun_after_exp:
  stm_dots fun_after_dots      {$1::$2}

/* hack to allow mixing statements and expressions in an or */
fun_after_dots_or:
  /* empty */                  {[]}
| TNothing fun_after_exp_or    {$2}
| expr fun_after_exp_or        {Ast0.wrap(Ast0.Exp($1))::$2}
| decl_statement_expr fun_after_stm {$1@$2}

fun_after_exp_or:
  /* empty */                  {[]}
| stm_dots fun_after_dots      {$1::$2}

/* ------------------------------------------------------------------------ */
/* Nest body */

nest_start:
  nest_after_dots  { Ast0.wrap $1 }

nest_after_dots:
  decl_statement_expr nest_after_stm {$1@$2}
| TNothing nest_after_exp       {$2}
| expr nest_after_exp           {(Ast0.wrap(Ast0.Exp($1)))::$2}

nest_after_stm:
  /* empty */                   {[]}
| stm_dots nest_after_dots      {$1::$2}
| decl_statement nest_after_stm {$1@$2}

nest_after_exp:
  /* empty */                   {[]}
| stm_dots nest_after_dots      {$1::$2}

/* ------------------------------------------------------------------------ */
/*Whencode*/

when_start:
  expr toplevel_after_exp
    { Ast0.wrap((Ast0.wrap(Ast0.Exp($1)))::$2) }
| decl_statement toplevel_after_stm
    { Ast0.wrap($1@$2) }

/* ---------------------------------------------------------------------- */

/* arg expr.  may contain a type or a explist metavariable */
aexpr:
    dargexpr { Ast0.set_arg_exp $1 }
  | TMetaExpList
      { let (nm,lenname,constraints,pure,clt) = $1 in
      let nm = P.clt2mcode nm clt in
      let lenname = P.dolen clt lenname in
      Ast0.wrap(Ast0.MetaExprList(nm,lenname,constraints,pure)) }
  | ctype
      { Ast0.set_arg_exp(Ast0.wrap(Ast0.TypeExp($1))) }

eexpr_list_option:
    empty_list_start(aexpr,TEllipsis)
      { Ast0.wrap
	  ($1
	     (fun _ d -> Ast0.wrap(Ast0.Edots(P.clt2mcode "..." d,None)))
	     (fun c -> Ast0.EComma c)) }

/****************************************************************************/

// IBM C only
exec_list:
    /* empty */ { [] }
  | TDotDot exec_front_ident exec_ident exec_list
      { Ast0.wrap(Ast0.ExecEval(P.clt2mcode ":" $1,$3 $2)) :: $4 }
  | TIdent exec_ident2 exec_list
      { Ast0.wrap(Ast0.ExecToken(P.clt2mcode (fst $1) (snd $1))) ::
	List.map (function x -> Ast0.wrap(Ast0.ExecToken x)) $2 @ $3 }
  | token exec_list { Ast0.wrap(Ast0.ExecToken $1) :: $2 }
  | TEllipsis exec_list
      { Ast0.wrap(Ast0.ExecDots(P.clt2mcode "..." $1)) :: $2 }

exec_front_ident:
    ident { Ast0.wrap(Ast0.Ident($1)) }
  | TMetaIdExp
     { let (nm,constraints,pure,ty,clt) = $1 in
     Ast0.wrap
       (Ast0.MetaExpr(P.clt2mcode nm clt,constraints,ty,Ast.ID,pure,None)) }
  | TMetaExp
     { let (nm,constraints,pure,ty,clt,bitfield) = $1 in
     let bitfield' = Common.map_option (P.dolen clt) bitfield in
     Ast0.wrap
       (Ast0.MetaExpr
	  (P.clt2mcode nm clt,constraints,ty,Ast.ANY,pure,bitfield')) }

exec_ident:
     { function prev -> prev }
 | TDot   disj_ident exec_ident
     { function prev ->
       $3 (Ast0.wrap(Ast0.RecordAccess(prev, P.clt2mcode "." $1, $2))) }
 | TPtrOp disj_ident exec_ident
     { function prev ->
       $3 (Ast0.wrap(Ast0.RecordPtAccess(prev, P.clt2mcode "->" $1,
				     $2))) }

exec_ident2:
     { [] }
 | TDot   TIdent exec_ident2
     { (P.clt2mcode "." $1) :: (P.clt2mcode (fst $2) (snd $2)) :: $3 }
 | TPtrOp TIdent exec_ident2
     { (P.clt2mcode "." $1) :: (P.clt2mcode (fst $2) (snd $2)) :: $3 }

token:
    TPlus { P.clt2mcode "+" $1 }
  | TMinus { P.clt2mcode "-" $1 }
  | TMul { P.clt2mcode "*" $1 }
  | TEqEq { P.clt2mcode "==" $1 }
  | TNotEq { P.clt2mcode "!=" $1 }
  | TDmOp { P.clt2mcode (arithOp(fst $1)) (snd $1) }
  | TShLOp { P.clt2mcode (arithOp(fst $1)) (snd $1) }
  | TShROp { P.clt2mcode (arithOp(fst $1)) (snd $1) }
  | TLogOp { P.clt2mcode (logicalOp(fst $1)) (snd $1) }
  | TOr { P.clt2mcode "|" $1 }
  | TXor { P.clt2mcode "+" $1 }
  | TAnd { P.clt2mcode "&" $1 }
  | TOrLog { P.clt2mcode "||" $1 }
  | TAndLog { P.clt2mcode "&&" $1 }
  | TOBrace { P.clt2mcode "{" $1 }
  | TCBrace { P.clt2mcode "}" $1 }
  | TOCro { P.clt2mcode "[" $1 }
  | TCCro { P.clt2mcode "]" $1 }
  | TEq { P.clt2mcode "=" $1 }
  | TWhy { P.clt2mcode "?" $1 }
  | TBang { P.clt2mcode "!" $1 }
  | TOPar { P.clt2mcode "(" $1 }
  | TCPar { P.clt2mcode ")" $1 }
  | TIf { P.clt2mcode "if" $1 }
  | TElse { P.clt2mcode "else" $1 }

/****************************************************************************/

// non-empty lists - drop separator
%inline comma_list(elem):
  l=separated_nonempty_list(TComma,elem) { l }

midzero_list(elem,aft):
  a=elem b=list(mzl(aft))
     { let (mids,code) = List.split b in (mids,(a::code)) }

mzl(elem):
  a=TMid0 b=elem { (P.id2mcode a, b) }

andzero_list(elem,aft):
  a=elem b=nonempty_list(azl(aft))
     { let (mids,code) = List.split b in (mids,(a::code)) }

azl(elem):
  a=TAnd0 b=elem { (P.id2mcode a, b) }

edots_when(dotter,when_grammar):
    d=dotter                                      { (d,None) }
  | d=dotter t=TWhen e=TNotEq w=when_grammar TLineEnd
    { (d, Some (P.clt2mcode "when" t, P.clt2mcode "!=" e,w)) }

whens(when_grammar,simple_when_grammar,any_strict):
    t=TWhen e=TNotEq w=when_grammar TLineEnd
      { [Ast0.WhenNot (P.clt2mcode "when" t, P.clt2mcode "!=" e, w)] }
  | t=TWhen e=TEq w=simple_when_grammar TLineEnd
      { [Ast0.WhenAlways (P.clt2mcode "when" t, P.clt2mcode "=" e, w)] }
  | t=TWhen l=comma_list(any_strict) TLineEnd
      { List.map (function x -> Ast0.WhenModifier(P.clt2mcode "when" t,x)) l }
  | t=TWhenTrue ee=TNotEq e = eexpr TLineEnd
      { [Ast0.WhenNotTrue (P.clt2mcode "when" t, P.clt2mcode "!=" ee, e)] }
  | t=TWhenFalse ee=TNotEq e = eexpr TLineEnd
      { [Ast0.WhenNotFalse (P.clt2mcode "when" t, P.clt2mcode "!=" ee, e)] }

any_strict:
    TAny    { Ast.WhenAny }
  | TStrict { Ast.WhenStrict }
  | TForall { Ast.WhenForall }
  | TExists { Ast.WhenExists }

/*****************************************************************************
*
*
*****************************************************************************/

iso_main:
  TIsoExpression e1=eexpr el=list(iso(eexpr)) EOF
    { let fn x = Ast0.ExprTag x in P.iso_adjust fn fn e1 el }
| TIsoArgExpression e1=eexpr el=list(iso(eexpr)) EOF
    { let fn x = Ast0.ArgExprTag x in P.iso_adjust fn fn e1 el }
| TIsoTestExpression e1=eexpr el=list(iso(eexpr)) EOF
    { let fn x = Ast0.TestExprTag x in P.iso_adjust fn fn e1 el }
| TIsoToTestExpression e1=eexpr el=list(iso(eexpr)) EOF
    { let ffn x = Ast0.ExprTag x in
      let fn x =  Ast0.TestExprTag x in
      P.iso_adjust ffn fn e1 el }
| TIsoStatement s1=iso_statement sl=list(iso(iso_statement)) EOF
    { let fn x = Ast0.StmtTag x in P.iso_adjust fn fn s1 sl }
| TIsoType t1=ctype tl=list(iso(ctype)) EOF
    { let fn x = Ast0.TypeCTag x in P.iso_adjust fn fn t1 tl }
| TIsoTopLevel e1=nest_start el=list(iso(nest_start)) EOF
    { let fn x = Ast0.DotsStmtTag x in P.iso_adjust fn fn e1 el }
| TIsoDeclaration d1=decl_var dl=list(iso(decl_var)) EOF
    { let check_one = function
	[x] -> x
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       "only one variable per declaration in an isomorphism rule") in
    let d1 = check_one d1 in
    let dl =
      List.map
	(function
	    Common.Left x -> Common.Left(check_one x)
	  | Common.Right x -> Common.Right(check_one x))
	dl in
    let fn x = Ast0.DeclTag x in P.iso_adjust fn fn d1 dl }

iso(term):
    TIso t=term { Common.Left t }
  | TRightIso t=term { Common.Right t }

/*****************************************************************************
*
*
*****************************************************************************/

never_used: TDirective { () }
  | TAttr_             { () }
  | TPArob TMetaPos    { () }
  | TScriptData        { () }
  | TAnalysis          { () }
  | TWhitespace        { () }
  | TCppEscapedNewline { () }

script_meta_main:
    py=pure_ident TMPtVirg
  { ((Some (P.id2name py), None), None, Ast.NoMVInit) }
  | py=pure_ident script_name_decl_ext TMPtVirg
  { ((Some (P.id2name py), None), Some (fst $2), snd $2) }
  | TOPar TUnderscore TComma ast=pure_ident TCPar script_name_decl TMPtVirg
  { ((None, Some (P.id2name ast)), Some $6, Ast.NoMVInit) }
  | TOPar str=pure_ident TComma TUnderscore TCPar script_name_decl_ext TMPtVirg
  { ((Some (P.id2name str), None), Some (fst $6), snd $6) }
  | TOPar str=pure_ident TComma ast=pure_ident TCPar script_name_decl TMPtVirg
  { ((Some (P.id2name str), Some (P.id2name ast)), Some $6, Ast.NoMVInit) }

script_name_decl:
    TShLOp checked_meta_name { $2 }

checked_meta_name:
    TRuleName TDot cocci=pure_ident
      { let nm = P.id2name cocci in
        let mv = Parse_aux.lookup $1 nm in
        (($1, nm), mv) }
  | TVirtual TDot cocci=pure_ident
      { let nm = P.id2name cocci in
	 Iteration.parsed_virtual_identifiers :=
	   Common.union_set [nm]
	     !Iteration.parsed_virtual_identifiers;
        let name = ("virtual", nm) in
        let mv = Ast.MetaIdDecl(Ast.NONE,name) in
        (name,mv) }
  | TMerge TDot cocci=pure_ident
      { let nm = P.id2name cocci in
        let name = ("merge", nm) in
        let mv = Ast.MetaIdDecl(Ast.NONE,name) in
        (name, mv) }

script_name_decl_ext:
    script_name_decl { ($1,Ast.NoMVInit) }
  | script_name_decl TEq TString
    { let (nm,mv) = $1 in
      match mv with
	Ast.MetaPosDecl _ ->
	  raise
	    (Semantic_cocci.Semantic
	       "default value of position variable should be a list")
      | _ ->
	  let (s,clt) = $3 in
	  ($1,Ast.MVInitString s) }
  | script_name_decl TEq TOCro TCCro
    { let (nm,mv) = $1 in
      match mv with
	Ast.MetaPosDecl _ ->
	  ($1,Ast.MVInitPosList) (* just empty, so nothing to record *)
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       "default value of non-position variable should be a string") }

script_meta_virt_nofresh_main:
    py=pure_ident script_virt_name_decl TMPtVirg
  { ((Some (P.id2name py), None), Some $2, Ast.NoMVInit) }

script_virt_name_decl:
    TShLOp TVirtual TDot cocci=pure_ident
      { let nm = P.id2name cocci in
	 Iteration.parsed_virtual_identifiers :=
	   Common.union_set [nm]
	     !Iteration.parsed_virtual_identifiers;
        let name = ("virtual", nm) in
        let mv = Ast.MetaIdDecl(Ast.NONE,name) in
        (name,mv) }
  | TShLOp TMerge TDot cocci=pure_ident
      { let nm = P.id2name cocci in
        let name = ("merge", nm) in
        let mv = Ast.MetaIdDecl(Ast.NONE,name) in
        (name,mv) }

%inline
attr_list:
                        { [] }
 | a=Tattr f=full_attr_list {P.id2mcode a::f}

full_attr_list:
                        { [] }
 | Tattr full_attr_list {P.id2mcode $1::$2}

anything: /* used for script code */
   TIdentifier { "identifier" }
 | TExpression { "expression" }
 | TStatement { "statement" }
 | TFunction { "function" }
 | TType { "type" }
 | TParameter { "parameter" }
 | TIdExpression { "idexpression" }
 | TInitialiser { "initialiser" }
 | TDeclaration { "declaration" }
 | TField { "field" }
 | TMetavariable { "metavariable" }
 | TSymbol { "symbol" }
 | TOperator { "operator" }
 | TBinary { "binary" }
 | TAssignment { "assignment" }
 | Tlist { "list" }
 | TFresh { "fresh" }
 | TConstant { "constant" }
 | TError { "error" }
 | TWords { "words" }
 | TGenerated { "generated" }
 | TFormat { "format" }
 | TLocal { "local" }
 | TGlobal { "global" }
 | TTypedef { "typedef" }
 | TAttribute { "attribute" }
 | TDeclarer { "declarer" }
 | TIterator { "iterator" }
 | TName { "name" }
 | TPosition { "position" }
 | TAnalysis { "analysis" }
 | TPosAny { "any" }
 | TUsing { "using" }
 | TDisable { "disable" }
 | TExtends { "extends" }
 | TDepends { "depends" }
 | TOn { "on" }
 | TFile { "file" }
 | TIn { "in" }
 | TInitialize { "initialize" }
 | TFinalize { "finalize" }
 | TVirtual { "virtual" }
 | TMerge { "merge" }
 | TRuleName { $1 }
 | TScript { "script" }

 | Tchar { "char" }
 | Tshort { "short" }
 | Tint { "int" }
 | Tdouble { "double" }
 | Tfloat { "float" }
 | Tlong { "long" }
 | Tsize_t { "size_t" }
 | Tssize_t { "ssize_t" }
 | Tptrdiff_t { "ptrdiff_t" }
 | Tvoid { "void" }
 | Tstruct { "struct" }
 | Tunion { "union" }
 | Tenum { "enum" }
 | Tunsigned { "unsigned" }
 | Tsigned { "signed" }

 | Tstatic { "static" }
 | Tauto { "auto" }
 | Tregister { "register" }
 | Textern { "extern" }
 | Tinline { "inline" }
 | Ttypedef { "typedef" }
 | Tconst { "const" }
 | Tvolatile { "volatile" }
 | Tattr { fst $1 }

 | TVAEllipsis { "......" }
 | TIf { "if" }
 | TElse { "else" }
 | TWhile { "while" }
 | TFor { "for" }
 | TDo { "do" }
 | TSwitch { "switch" }
 | TCase { "case" }
 | TDefault { "default" }
 | TReturn { "return" }
 | TBreak { "break" }
 | TContinue { "continue" }
 | TGoto { "goto" }
 | TSizeof { "sizeof" }
 | Tdecimal { "decimal" }
 | Texec { "EXEC" }
 | TIdent { fst $1 }
 | TTypeId { fst $1 }
 | TDeclarerId { fst $1 }
 | TIteratorId { fst $1 }
 | TSymId { fst $1 }

 | local_meta { snd $1 }

 | TEllipsis { "..." }
 | TOEllipsis { "<..." }
 | TCEllipsis { "...>" }
 | TPOEllipsis { "<+..." }
 | TPCEllipsis { "...+>" }
 | TWhen { "when" }

 | TWhy { "?" }
 | TDotDot { ":" }
 | TBang { "!" }
 | TOPar { "(" }
 | TCPar { ")" }

 | TInc { "++" }
 | TDec { "--" }

 | TString { Printf.sprintf "\"%s\"" (fst $1) }
 | TChar { fst $1 }
 | TFloat { fst $1 }
 | TInt { fst $1 }
 | TDecimalCst { let (x,_,_,_) = $1 in x }

 | TOrLog { "||" }
 | TAndLog { "&&" }
 | TOr { "|" }
 | TXor { "^" }
 | TAnd { "&" }
 | TEqEq { "==" }
 | TNotEq { "!=" }
 | TTildeEq { "=~" }
 | TTildeExclEq { "!~" }
 | TSub { "<=" }
 | TLogOp
     { match fst $1 with
       Ast.SupEq -> ">=" | Ast.InfEq -> "<="
     | Ast.Inf -> "<" | Ast.Sup -> ">"
     | _ -> failwith "bad log op" }
 | TShLOp { "<<" }
 | TShROp { ">>" }
 | TDmOp
     { match fst $1 with
       Ast.Div -> "/" | Ast.Mod -> "%" | _ -> failwith "bad op" }
 | TPlus { "+" }
 | TMinus { "-" }
 | TMul { "*" }
 | TTilde { "~" }

 | TOCro {"[" }
 | TCCro { "]" }

 | TPtrOp { "->" }

 | TMPtVirg { ";" }
 | TCppConcatOp { "##" }
 | TEq { "=" }
 | TDot { "." }
 | TComma { "," }
 | TPtVirg { ";" }
 | TOpAssign
     { match fst $1 with
       Ast.Minus -> "-=" | Ast.Plus -> "+=" | Ast.Mul -> "*=" | Ast.Div -> "/="
     | Ast.Mod -> "%" | Ast.And -> "&=" | Ast.Or -> "|=" | Ast.Xor -> "^="
     | Ast.Max -> ">?=" | Ast.Min -> "<?=" | Ast.DecLeft -> "<<="
     | Ast.DecRight -> ">>=" }

 | TIso { "<=>" }
 | TRightIso { "=>" }

 | TUnderscore { "_" }
