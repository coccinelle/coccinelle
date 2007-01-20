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

(*let warning s v = 
  if !Flag.verbose_parsing 
  then Common.warning s v
  else v*)

let make_info line logical_line offset =
  { Ast0.line_start = line; Ast0.line_end = line;
    Ast0.logical_start = logical_line; Ast0.logical_end = logical_line;
    Ast0.attachable_start = true; Ast0.attachable_end = true;
    Ast0.mcode_start = []; Ast0.mcode_end = [];
    Ast0.column = -1; Ast0.offset = offset }

let clt2info (_,line,logical_line,offset) = make_info line logical_line offset

let clt2mcode str = function
    (Data.MINUS,line,lline,offset)       ->
      (str,Ast0.NONE,make_info line lline offset,
       Ast0.MINUS(ref([],Ast0.default_token_info)))
  | (Data.OPTMINUS,line,lline,offset)    ->
      (str,Ast0.OPT,make_info line lline offset,
       Ast0.MINUS(ref([],Ast0.default_token_info)))
  | (Data.UNIQUEMINUS,line,lline,offset) ->
      (str,Ast0.UNIQUE,make_info line lline offset,
       Ast0.MINUS(ref([],Ast0.default_token_info)))
  | (Data.MULTIMINUS,line,lline,offset) ->
      (str,Ast0.MULTI,make_info line lline offset,
       Ast0.MINUS(ref([],Ast0.default_token_info)))
  | (Data.PLUS,line,lline,offset)        ->
      (str,Ast0.NONE,make_info line lline offset,Ast0.PLUS)
  | (Data.CONTEXT,line,lline,offset)     ->
      (str,Ast0.NONE,make_info line lline offset,
       Ast0.CONTEXT(ref(Ast.NOTHING,
			Ast0.default_token_info,Ast0.default_token_info)))
  | (Data.OPT,line,lline,offset)         ->
      (str,Ast0.OPT,make_info line lline offset,
       Ast0.CONTEXT(ref(Ast.NOTHING,
			Ast0.default_token_info,Ast0.default_token_info)))
  | (Data.UNIQUE,line,lline,offset)      ->
      (str,Ast0.UNIQUE,make_info line lline offset,
       Ast0.CONTEXT(ref(Ast.NOTHING,
			Ast0.default_token_info,Ast0.default_token_info)))
  | (Data.MULTI,line,lline,offset)      ->
      (str,Ast0.MULTI,make_info line lline offset,
       Ast0.CONTEXT(ref(Ast.NOTHING,
			Ast0.default_token_info,Ast0.default_token_info)))

let id2name   (name, clt) = name
let id2clt    (name, clt) = clt
let id2mcode  (name, clt) = clt2mcode name clt

let mkdots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Dots(clt2mcode str dot, whencode))
  | "ooo" -> Ast0.wrap(Ast0.Circles(clt2mcode str dot, whencode))
  | "***" -> Ast0.wrap(Ast0.Stars(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkedots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Edots(clt2mcode str dot, whencode))
  | "ooo" -> Ast0.wrap(Ast0.Ecircles(clt2mcode str dot, whencode))
  | "***" -> Ast0.wrap(Ast0.Estars(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkidots str (dot,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Idots(clt2mcode str dot, whencode))
  | _ -> failwith "cannot happen"

let mkpdots str dot =
  match str with
    "..." -> Ast0.wrap(Ast0.Pdots(clt2mcode str dot))
  | "ooo" -> Ast0.wrap(Ast0.Pcircles(clt2mcode str dot))
  | _ -> failwith "cannot happen"

let arith_op ast_op left op right =
  Ast0.wrap
    (Ast0.Binary(left, clt2mcode (Ast.Arith ast_op) op, right))

let logic_op ast_op left op right =
  Ast0.wrap
    (Ast0.Binary(left, clt2mcode (Ast.Logical ast_op) op, right))

let make_cv cv ty =
  match cv with None -> ty | Some x -> Ast0.wrap (Ast0.ConstVol(x,ty))

let top_dots l =
  let circle x =
    match Ast0.unwrap x with Ast0.Circles(_) -> true | _ -> false in
  let star x =
    match Ast0.unwrap x with Ast0.Stars(_) -> true | _ -> false in
  if List.exists circle l
  then Ast0.wrap(Ast0.CIRCLES(l))
  else
    if List.exists star l
    then Ast0.wrap(Ast0.STARS(l))
    else Ast0.wrap(Ast0.DOTS(l))

(* here the offset is that of the first in the sequence of *s, not that of
each * individually *)
let pointerify ty m =
  List.fold_left
    (function inner ->
      function cur ->
	Ast0.wrap(Ast0.Pointer(inner,clt2mcode "*" cur)))
    ty m

let ty_pointerify ty m =
  List.fold_left
    (function inner -> function cur -> Type_cocci.Pointer(inner))
    ty m

(* Left is <=>, Right is =>.  Collect <=>s. *)
let iso_adjust fn first rest =
  let rec loop = function
      [] -> [[]]
    | (Common.Left x)::rest ->
	(match loop rest with
	  front::after -> (fn x::front)::after
	| _ -> failwith "not possible")
    | (Common.Right x)::rest ->
	(match loop rest with
	  front::after -> []::(fn x::front)::after
	| _ -> failwith "not possible") in
  match loop rest with
    front::after -> (fn first::front)::after
  | _ -> failwith "not possible"

let metatypes = (Hashtbl.create(10) : (string,unit) Hashtbl.t)
let typenames = (Hashtbl.create(10) : (string,unit) Hashtbl.t)
%}


%token EOF

%token TIdentifier TExpression TStatement TFunction TLocal TType TParameter
%token TText Tlist TFresh TConstant TError TWords TWhy0 TPlus0 TBang0 TPure
%token TTypedef

%token<Data.clt> Tchar Tshort Tint Tdouble Tfloat Tlong
%token<Data.clt> Tvoid Tstruct Tunion
%token<Data.clt> Tunsigned Tsigned

%token<Data.clt> Tstatic Tauto Tregister Textern
%token<Data.clt> Tconst Tvolatile

%token <Data.clt> TIf TElse TWhile TFor TDo TReturn
%token <Data.clt> TBreak TContinue
%token <Data.clt> TSizeof
%token <Data.clt> TFunDecl
%token <string * Data.clt> TIdent TTypeId
%token <string * Data.pure * Data.clt> TMetaId TMetaType TMetaErr
%token <string * Data.pure * Data.clt> TMetaParam TMetaParamList
%token <string * Data.pure * Data.clt> TMetaStm TMetaStmList
%token <string * Data.pure * Data.clt> TMetaFunc TMetaLocalFunc
%token <string * Data.pure * Data.clt> TMetaExpList TMetaText
%token <string * Data.pure * Type_cocci.typeC list option *
          Data.clt> TMetaExp TMetaConst
%token TArobArob

%token <Data.clt> TEllipsis TOEllipsis TCEllipsis
%token <Data.clt> TWhen TLineEnd
%token <Data.clt> TCircles TOCircles TCCircles
%token <Data.clt> TStars TOStars TCStars

%token <Data.clt> TWhy TDotDot TBang TOPar TOPar0
%token <Data.clt> TMid0 TCPar TCPar0

%token <string * Data.clt> TInclude
%token <Data.clt> TDefine
%token <string * Data.clt> TMinusFile TPlusFile

%token <Data.clt> TInc TDec

%token <string * Data.clt> TString TChar TFloat TInt

%token <Data.clt> TOrLog
%token <Data.clt> TAndLog
%token <Data.clt> TOr
%token <Data.clt> TXor
%token <Data.clt> TAnd 
%token <Data.clt> TEqEq TNotEq
%token <Data.clt> TInf TSup TInfEq TSupEq 
%token <Data.clt> TShl TShr
%token <Data.clt> TPlus TMinus
%token <Data.clt> TMul TDiv TMod 

%token <Data.clt> TOBrace TCBrace
%token <Data.clt> TOCro TCCro

%token <Data.clt> TPtrOp

%token <Data.clt> TEq TDot TComma TPtVirg
%token <Ast_cocci.assignOp * Data.clt> TAssign

%token TIso TRightIso TIsoExpression TIsoStatement TIsoDeclaration

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
%left TInf TSup TInfEq TSupEq 
%left TShl TShr
%left TPlus TMinus
%left TMul TDiv TMod 

%start minus_main 
%type <Ast0_cocci.rule> minus_main

%start plus_main 
%type <Ast0_cocci.rule> plus_main

%start meta_main
%type <Ast_cocci.metavar list> meta_main

%start iso_main
%type <Ast0_cocci.anything list list> iso_main

%%

minus_main: minus_body EOF { $1 } | minus_body TArobArob { $1 }
plus_main: plus_body EOF { $1 } | plus_body TArobArob { $1 }
meta_main: metadec* TArobArob { List.concat($1) }

/*****************************************************************************
*
*
*****************************************************************************/

metadec:
  ar=arity ispure=pure
  kindfn=metakind ids=comma_list(pure_ident_or_meta_ident) TPtVirg
  { List.concat (List.map (function nm -> kindfn ar nm ispure) ids) }

pure: TPure { true } | /* empty */ { false }

%inline metakind:
  TIdentifier
    { (function arity -> function name -> function pure ->
      !Data.add_id_meta name pure; [Ast.MetaIdDecl(arity,name)]) }
| TFresh TIdentifier
    { (function arity -> function name -> function pure ->
      !Data.add_id_meta name pure; [Ast.MetaFreshIdDecl(arity,name)]) }
| TType
    { (function arity -> function name -> function pure ->
      Hashtbl.add metatypes name ();
      !Data.add_type_meta name pure; [Ast.MetaTypeDecl(arity,name)]) } 
| TParameter
    { (function arity -> function name -> function pure ->
      !Data.add_param_meta name pure; [Ast.MetaParamDecl(arity,name)]) }
| TParameter Tlist
    { (function arity -> function name -> function pure ->
      !Data.add_paramlist_meta name pure;[Ast.MetaParamListDecl(arity,name)])} 
| TError
    { (function arity -> function name -> function pure ->
      !Data.add_err_meta name pure; [Ast.MetaErrDecl(arity,name)]) }
| TExpression
    { (function arity -> function name -> function pure ->
      !Data.add_exp_meta None name pure; [Ast.MetaExpDecl(arity,name)]) }
| TExpression Tlist
    { (function arity -> function name -> function pure ->
      !Data.add_explist_meta name pure; [Ast.MetaExpListDecl(arity,name)]) }
| TExpression m=nonempty_list(TMul)
    { (function arity -> function name -> function pure ->
      !Data.add_exp_meta (Some [ty_pointerify Type_cocci.Unknown m]) name pure;
      [Ast.MetaExpDecl(arity,name)]) }
| TStatement
    { (function arity -> function name -> function pure ->
      !Data.add_stm_meta name pure; [Ast.MetaStmDecl(arity,name)]) }
| TStatement Tlist
    { (function arity -> function name -> function pure ->
      !Data.add_stmlist_meta name pure; [Ast.MetaStmListDecl(arity,name)]) }
| TFunction
    { (function arity -> function name -> function pure ->
      !Data.add_func_meta name pure; [Ast.MetaFuncDecl(arity,name)]) }
| TLocal TFunction
    { (function arity -> function name -> function pure ->
      !Data.add_local_func_meta name pure;
      [Ast.MetaLocalFuncDecl(arity,name)]) }
| vl=meta_exp_type // no error if use $1 but doesn't type check
    { (function arity -> function name -> function pure ->
      !Data.add_exp_meta (Some vl) name pure; [Ast.MetaExpDecl(arity,name)]) }
| TConstant ty=ioption(const_meta_exp_type)
    { (function arity -> function name -> function pure ->
      !Data.add_const_meta ty name pure; [Ast.MetaConstDecl(arity,name)]) }
| TText
    { (function arity -> function name -> function pure ->
      !Data.add_text_meta name pure; [Ast.MetaTextDecl(arity,name)]) }
| TTypedef
    { (function arity -> function name -> function pure ->
      if arity = Ast.NONE && pure = false
      then (Hashtbl.add typenames name (); !Data.add_type_name name; [])
      else failwith "bad typedef") }

meta_exp_type:
  pure_ident m=list(TMul)
    { let name = id2name $1 in
      let fail _ =
	try let _ = Hashtbl.find typenames name in Type_cocci.TypeName name
	with
	  Not_found ->
	    failwith
	      (Printf.sprintf "%s: bad type for expression metavariable"
		 name) in
      [ty_pointerify
	(if !Data.in_iso
	then
	(* unbound type variables only allowed in isomorphisms *)
	  try
	    let _ = Hashtbl.find metatypes name in Type_cocci.MetaType name
	  with Not_found -> fail()
	else fail())
	m] }
| ctype
    { [Ast0_cocci.ast0_type_to_type $1] }
| TOBrace comma_list(ctype) TCBrace m=list(TMul)
    { List.map (function x -> ty_pointerify (Ast0_cocci.ast0_type_to_type x) m)
	$2 }

const_meta_exp_type:
  mtype
    { [Ast0_cocci.ast0_type_to_type $1] }
| TOBrace comma_list(ctype) TCBrace
    { List.map Ast0_cocci.ast0_type_to_type $2 }

arity: TBang0 { Ast.UNIQUE }
     | TWhy0  { Ast.OPT }
     | TPlus0 { Ast.MULTI }
     | /* empty */ { Ast.NONE }

generic_ctype:
       t=Tvoid
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.VoidType t, None)) }
     | q=ioption(ctype_qualif) ty=Tchar
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.CharType ty, q)) }
     | q=ioption(ctype_qualif) ty=Tshort
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.ShortType ty, q)) }
     | q=ioption(ctype_qualif) ty=Tint
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.IntType ty, q)) }
     | t=Tdouble
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.DoubleType t, None)) }
     | t=Tfloat
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.FloatType t, None)) }
     | q=ioption(ctype_qualif) ty=Tlong
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.LongType ty, q)) }
     | s=struct_or_union i=ident
	 { Ast0.wrap(Ast0.StructUnionName(s, i)) }
     | s=struct_or_union i=ident l=TOBrace d=list(struct_decl) r=TCBrace
	 { Ast0.wrap(Ast0.StructUnionDef(s, i, clt2mcode "{" l,
					 d, clt2mcode "}" r)) }
     | p=TTypeId { Ast0.wrap(Ast0.TypeName(id2mcode p)) }

struct_or_union:
       s=Tstruct { clt2mcode Ast.Struct s }
     | u=Tunion  { clt2mcode Ast.Union u }

struct_decl:
       t=ctype d=d_ident pv=TPtVirg
	 { let (id,fn) = d in
	 Ast0.wrap(Ast0.UnInit(None,fn t,id,clt2mcode ";" pv)) }
     | cv=ioption(const_vol) i=pure_ident d=d_ident pv=TPtVirg
	 { let (id,fn) = d in
	 let idtype = make_cv cv (Ast0.wrap (Ast0.TypeName(id2mcode i))) in
	 Ast0.wrap(Ast0.UnInit(None,fn idtype,id,clt2mcode ";" pv)) }

mtype: // no metavariable, for constant metavariable declarations
       cv=ioption(const_vol) ty=generic_ctype m=list(TMul)
	 { make_cv cv (pointerify ty m) }

ctype:
       cv=ioption(const_vol) ty=generic_ctype m=list(TMul)
	 { pointerify (make_cv cv ty) m }
     | cv=ioption(const_vol) ty=TMetaType m=list(TMul)
	 { let (nm,pure,clt) = ty in
	 let ty = Ast0.wrap(Ast0.MetaType(clt2mcode nm clt,pure)) in
	 pointerify (make_cv cv ty) m }

fn_ctype: // allows metavariables
       ty=generic_ctype m=list(TMul)
	 { pointerify ty m }
     | ty=TMetaType m=list(TMul)
	 { let (nm,pure,clt) = ty in
	 let ty = Ast0.wrap(Ast0.MetaType(clt2mcode nm clt,pure)) in
	 pointerify ty m }

ctype_qualif:
       Tunsigned   { clt2mcode Ast.Unsigned $1 }
     | Tsigned     { clt2mcode Ast.Signed $1 }

/*****************************************************************************/

/* have to inline everything to avoid conflicts? switch to proper
declarations, statements, and expressions for the subterms */

minus_body: 
    f=loption(filespec) i=list(includes)
    b=loption(minus_function_decl_statement_or_expression)
    ew=loption(error_words)
    { Top_level.top_level (f@i@b@ew) }

plus_body: 
    f=loption(filespec) i=list(includes)
    b=loption(plus_function_decl_statement_or_expression)
    ew=loption(error_words)
    { Top_level.top_level (f@i@b@ew) }

filespec:
  TMinusFile TPlusFile
    { [Ast0.wrap
	  (Ast0.FILEINFO(id2mcode $1,
			 id2mcode $2))] }

includes:
  TInclude
    { Ast0.wrap
	(Ast0.DECL
	   (Ast0.wrap
	      (Ast0.Include(clt2mcode "#include" (id2clt $1),
			    id2mcode $1)))) }
| TDefine ident TEllipsis
    { Ast0.wrap
	(Ast0.DECL
	   (Ast0.wrap
	      (Ast0.Define(clt2mcode "#define" $1, $2,
			   Ast0.wrap(Ast0.Ddots(clt2mcode "..." $3)))))) }
| TDefine ident TMetaText
    { let (nm,pure,clt) = $3 in
      Ast0.wrap
	(Ast0.DECL
	   (Ast0.wrap
	      (Ast0.Define(clt2mcode "#define" $1, $2,
			   Ast0.wrap(Ast0.DMetaId(clt2mcode nm clt,pure)))))) }

/*****************************************************************************/

fundecl:
  s=ioption(storage) t=option(fn_ctype)
  TFunDecl i=func_ident lp=TOPar d=decl_list rp=TCPar
  lb=TOBrace b=pre_post_decl_statement_and_expression_opt rb=TCBrace
      { Ast0.wrap(Ast0.FunDecl((Ast0.default_info(),Ast0.context_befaft()),
			       s, t, i,
			       clt2mcode "(" lp, d,
			       clt2mcode ")" rp,
			       clt2mcode "{" lb, b,
			       clt2mcode "}" rb)) }

storage:
         s=Tstatic      { clt2mcode Ast.Static s }
       | s=Tauto        { clt2mcode Ast.Auto s }
       | s=Tregister    { clt2mcode Ast.Register s }
       | s=Textern      { clt2mcode Ast.Extern s }

decl: ctype ident
	{ Ast0.wrap(Ast0.Param($2, $1)) }
    | TMetaParam
	{ let (nm,pure,clt) = $1 in
	Ast0.wrap(Ast0.MetaParam(clt2mcode nm clt,pure)) }

const_vol:
      Tconst       { clt2mcode Ast.Const $1 }
    | Tvolatile    { clt2mcode Ast.Volatile $1 }

/*****************************************************************************/

statement:
  TMetaStm
    { let (nm,pure,clt) = $1 in
    Ast0.wrap(Ast0.MetaStmt(clt2mcode nm clt,pure)) }
| expr TPtVirg
    { Ast0.wrap(Ast0.ExprStatement ($1, clt2mcode ";" $2)) }
| TIf TOPar eexpr TCPar single_statement %prec TIf
    { Ast0.wrap(Ast0.IfThen(clt2mcode "if" $1,
			    clt2mcode "(" $2,$3,
			    clt2mcode ")" $4,$5,
			    (Ast0.default_info(),Ast0.context_befaft()))) }
| TIf TOPar eexpr TCPar single_statement TElse single_statement
    { Ast0.wrap(Ast0.IfThenElse(clt2mcode "if" $1,
				clt2mcode "(" $2,$3,
				clt2mcode ")" $4,$5,
				clt2mcode "else" $6,$7,
				(Ast0.default_info(),Ast0.context_befaft()))) }
| fr=TFor lp=TOPar e1=option(eexpr) sc1=TPtVirg e2=option(eexpr) sc2=TPtVirg
    e3=option(eexpr) rp=TCPar s=single_statement
    { Ast0.wrap(Ast0.For(clt2mcode "for" fr,
			 clt2mcode "(" lp,e1,
			 clt2mcode ";" sc1,e2,
			 clt2mcode ";" sc2,e3,
			 clt2mcode ")" rp,s,
			 (Ast0.default_info(),Ast0.context_befaft()))) }
| TWhile TOPar eexpr TCPar single_statement
    { Ast0.wrap(Ast0.While(clt2mcode "while" $1,
			   clt2mcode "(" $2,$3,
			   clt2mcode ")" $4,$5,
			   (Ast0.default_info(),Ast0.context_befaft()))) }
| TDo single_statement TWhile TOPar eexpr TCPar TPtVirg
    { Ast0.wrap(Ast0.Do(clt2mcode "do" $1,$2,
			clt2mcode "while" $3,
			clt2mcode "(" $4,$5,
			clt2mcode ")" $6,
			clt2mcode ";" $7)) }
| TReturn eexpr TPtVirg
    { Ast0.wrap(Ast0.ReturnExpr(clt2mcode "return" $1,$2,
				clt2mcode ";" $3)) }
| TReturn TPtVirg
    { Ast0.wrap(Ast0.Return(clt2mcode "return" $1,
			    clt2mcode ";" $2)) }
| TBreak TPtVirg
    { Ast0.wrap(Ast0.Break(clt2mcode "break" $1,
			   clt2mcode ";" $2)) }
| TContinue TPtVirg
    { Ast0.wrap(Ast0.Continue(clt2mcode "continue" $1,
			      clt2mcode ";" $2)) }
| TOBrace pre_post_decl_statement_and_expression_opt TCBrace
    { Ast0.wrap(Ast0.Seq(clt2mcode "{" $1,$2,
			 clt2mcode "}" $3)) }
| TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
    { Ast0.wrap(Ast0.Nest(clt2mcode "<..." $1,
			  Ast0.wrap(Ast0.DOTS(b (mkdots "..."))),
			  clt2mcode "...>" $3, None)) }
| TOCircles b=statement_dots(TCircles) TCCircles
    { Ast0.wrap(Ast0.Nest(clt2mcode "<ooo" $1,
			  Ast0.wrap(Ast0.CIRCLES(b (mkdots "ooo"))),
			  clt2mcode "ooo>" $3, None)) }
| TOStars b=statement_dots(TStars) TCStars
    { Ast0.wrap(Ast0.Nest(clt2mcode "<***" $1,
			  Ast0.wrap(Ast0.STARS(b (mkdots "***"))),
			  clt2mcode "***>" $3, None)) }
| TOEllipsis TWhen TNotEq w=pre_post_decl_statement_or_expression TLineEnd
    b=statement_dots(TEllipsis) c=TCEllipsis
    { Ast0.wrap(Ast0.Nest(clt2mcode "<..." $1,
			  Ast0.wrap(Ast0.DOTS(b (mkdots "..."))),
			  clt2mcode "...>" c, Some w)) }
| TOCircles TWhen TNotEq w=pre_post_decl_statement_or_expression TLineEnd
    b=statement_dots(TCircles) c=TCCircles
    { Ast0.wrap(Ast0.Nest(clt2mcode "<ooo" $1,
			  Ast0.wrap(Ast0.CIRCLES(b (mkdots "ooo"))),
			  clt2mcode "ooo>" c, Some w)) }
| TOStars TWhen TNotEq w=pre_post_decl_statement_or_expression TLineEnd
    b=statement_dots(TStars) c=TCStars
    { Ast0.wrap(Ast0.Nest(clt2mcode "<***" $1,
			  Ast0.wrap(Ast0.STARS(b (mkdots "***"))),
			  clt2mcode "***>" c, Some w)) }

/* a statement that fits into a single rule_elem.  should nests be included?
what about statement metavariables? */
rule_elem_statement:
  expr TPtVirg
    { Ast0.wrap(Ast0.ExprStatement ($1, clt2mcode ";" $2)) }
| TReturn eexpr TPtVirg
    { Ast0.wrap(Ast0.ReturnExpr(clt2mcode "return" $1,$2,
				clt2mcode ";" $3)) }
| TReturn TPtVirg
    { Ast0.wrap(Ast0.Return(clt2mcode "return" $1,
			    clt2mcode ";" $2)) }
| TBreak TPtVirg
    { Ast0.wrap(Ast0.Break(clt2mcode "break" $1,
			   clt2mcode ";" $2)) }
| TContinue TPtVirg
    { Ast0.wrap(Ast0.Continue(clt2mcode "continue" $1,
			      clt2mcode ";" $2)) }
| TOPar0 midzero_list(rule_elem_statement) TCPar0
    { let (mids,code) = $2 in
    Ast0.wrap
      (Ast0.Disj(clt2mcode "(" $1,
		 List.map (function x -> Ast0.wrap(Ast0.DOTS([x]))) code,
		 mids, clt2mcode ")" $3)) }

statement_dots(dotter):
  r=no_dot_start_end(exp_decl_statement_list,
		     dots_when(dotter,pre_post_decl_statement_or_expression,
			       rule_elem_statement))
  { function dot_builder ->
    List.concat (r (function x -> [dot_builder x])) }

/* a statement on its own */
single_statement:
    statement                         { $1 }
  | TOPar0 midzero_list(statement) TCPar0
      /* degenerate case, elements are single statements and thus don't
	contain dots */
      { let (mids,code) = $2 in
        Ast0.wrap
	  (Ast0.Disj(clt2mcode "(" $1,
		     List.map (function x -> Ast0.wrap(Ast0.DOTS([x]))) code,
		     mids, clt2mcode ")" $3)) }

/* In the following, an identifier as a type is not fully supported.  Indeed,
the language is ambiguous: what is foo * bar; */
/* The AST DisjDecl cannot be generated because it would be ambiguous with
a disjunction on a statement with a declaration in each branch */
decl_var:
    t=ctype pv=TPtVirg
      { [Ast0.wrap(Ast0.TyDecl(t,clt2mcode ";" pv))] }
  | s=ioption(storage) t=ctype d=comma_list(d_ident) pv=TPtVirg
      { List.map
	  (function (id,fn) ->
	    Ast0.wrap(Ast0.UnInit(s,fn t,id,clt2mcode ";" pv)))
	  d }
  | s=ioption(storage) t=ctype d=d_ident q=TEq e=initialize pv=TPtVirg
      { let (id,fn) = d in
      [Ast0.wrap(Ast0.Init(s,fn t,id,clt2mcode "=" q,e,clt2mcode ";" pv))] }
  | s=ioption(storage) cv=ioption(const_vol) i=pure_ident
      d=comma_list(d_ident) pv=TPtVirg
      { List.map
	  (function (id,fn) ->
	    let idtype = make_cv cv (Ast0.wrap (Ast0.TypeName(id2mcode i))) in
	    Ast0.wrap(Ast0.UnInit(s,fn idtype,id,clt2mcode ";" pv)))
	  d }
  | s=ioption(storage) cv=ioption(const_vol) i=pure_ident d=d_ident q=TEq
      e=initialize pv=TPtVirg
      { let (id,fn) = d in
      !Data.add_type_name (id2name i);
      let idtype = make_cv cv (Ast0.wrap (Ast0.TypeName(id2mcode i))) in
      [Ast0.wrap(Ast0.Init(s,fn idtype,id,clt2mcode "=" q,e,
			   clt2mcode ";" pv))] }

d_ident:
    ident
      { ($1,function x -> x) }
  | a=ident l=TOCro i=option(eexpr) r=TCCro
      { (a,function x ->
	Ast0.wrap(Ast0.Array(x,clt2mcode "[" l,i,
			     clt2mcode "]" r))) }

initialize:
    eexpr
      { Ast0.wrap(Ast0.InitExpr($1)) }
  | TOBrace initialize_list TCBrace
      { Ast0.wrap(Ast0.InitList(clt2mcode "{" $1,$2,clt2mcode "}" $3)) }
  | TOBrace TCBrace
      { Ast0.wrap
	  (Ast0.InitList(clt2mcode "{" $1,Ast0.wrap(Ast0.DOTS []),
			 clt2mcode "}" $2)) }

initialize2:
  /*arithexpr and not eexpr because can have ambiguity with comma*/
  /*dots and nests probably not allowed at top level, haven't looked into why*/
  arith_expr(eexpr,invalid) { Ast0.wrap(Ast0.InitExpr($1)) }
| TOBrace initialize_list TCBrace
    { Ast0.wrap(Ast0.InitList(clt2mcode "{" $1,$2,clt2mcode "}" $3)) }
| TOBrace TCBrace
    { Ast0.wrap
	(Ast0.InitList(clt2mcode "{" $1,Ast0.wrap(Ast0.DOTS []),
		       clt2mcode "}" $2)) }
           /* gccext:, labeled elements */
| TDot ident TEq initialize2
    { Ast0.wrap(Ast0.InitGccDotName(clt2mcode "." $1,$2,clt2mcode "=" $3,$4)) }
| ident TDotDot initialize2
    { Ast0.wrap(Ast0.InitGccName($1,clt2mcode ":" $2,$3)) } /* in old kernel */
| TOCro eexpr TCCro TEq initialize2
    { Ast0.wrap(Ast0.InitGccIndex(clt2mcode "[" $1,$2,clt2mcode "]" $3,
				  clt2mcode "=" $4,$5)) }
| TOCro eexpr TEllipsis eexpr TCCro TEq initialize2
    { Ast0.wrap(Ast0.InitGccRange(clt2mcode "[" $1,$2,clt2mcode "..." $3,
				  $4,clt2mcode "]" $5,clt2mcode "=" $6,$7)) }

initialize_list:
   initialize_list_start { Ast0.wrap(Ast0.DOTS($1)) }

initialize_list_start:
  initialize2        { [$1] }
| initialize2 TComma { [$1(*;Ast0.wrap(Ast0.IComma(clt2mcode "," $2))*)] }
| initialize2 TComma initialize_list_start
    { $1(*::Ast0.wrap(Ast0.IComma(clt2mcode "," $2))*)::$3 }
| d=edots_when(TEllipsis,initialize)
      r=list(comma_initializers(edots_when(TEllipsis,initialize)))
    { (mkidots "..." d)::
      (List.concat(List.map (function x -> x (mkidots "...")) r)) }

comma_initializers(dotter):
  TComma
    { function dot_builder ->
      [(*Ast0.wrap(Ast0.IComma(clt2mcode "," c))*)] }
| TComma d=dotter
    { function dot_builder ->
      [(*Ast0.wrap(Ast0.IComma(clt2mcode "," c));*) dot_builder d] }
| TComma i=initialize2
    { function dot_builder ->
      [(*Ast0.wrap(Ast0.IComma(clt2mcode "," c));*) i] }


/* a statement that is part of a list */
decl_statement:
    TMetaStmList
      { let (nm,pure,clt) = $1 in
      [Ast0.wrap(Ast0.MetaStmt(clt2mcode nm clt,pure))] }
  | decl_var
      { List.map
	  (function x ->
	    Ast0.wrap
	      (Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),x)))
	  $1 }
  | statement { [$1] }
  | TOPar0 pre_post_decl_statement_and_expression_opt_mid TCPar0
      { let (first,rest) = $2 in
        let (mids,code) = List.split rest in
	let code = first :: code in
	if List.for_all
	    (function x ->
	      match Ast0.unwrap x with Ast0.DOTS([]) -> true | _ -> false)
	    code
      then []
      else [Ast0.wrap(Ast0.Disj(clt2mcode "(" $1,
				code, mids,
				clt2mcode ")" $3))] }

/*****************************************************************************/

/* The following cannot contain <... ...> at the top level.  This can only
be allowed as an expression when the expression is delimited on both sides
by expression-specific markers.  In that case, the rule eexpr is used, which
allows <... ...> anywhere.  Hopefully, this will not be too much of a problem
in practice. */
expr:  basic_expr(expr,invalid) { $1 }
/* allows ... and nests */
eexpr: basic_expr(eexpr,dot_expressions) { $1 }
/* allows nests but not .... */
dexpr: basic_expr(eexpr,nest_expressions) { $1 }

invalid:
  TInvalid { failwith "not matchable" }

dot_expressions:
  TEllipsis { Ast0.wrap(Ast0.Edots(clt2mcode "..." $1,None)) }
| nest_expressions { $1 }

nest_expressions:
  TOEllipsis expr_dots(TEllipsis) TCEllipsis
    { Ast0.wrap(Ast0.NestExpr(clt2mcode "<..." $1,
			      Ast0.wrap(Ast0.DOTS($2 (mkedots "..."))),
			      clt2mcode "...>" $3, None)) }
| TOCircles expr_dots(TCircles) TCCircles
    { Ast0.wrap(Ast0.NestExpr(clt2mcode "<ooo" $1,
			      Ast0.wrap(Ast0.CIRCLES($2 (mkedots "ooo"))),
			      clt2mcode "ooo>" $3, None)) }
| TOStars expr_dots(TStars) TCStars
    { Ast0.wrap(Ast0.NestExpr(clt2mcode "<***" $1,
			      Ast0.wrap(Ast0.STARS($2 (mkedots "***"))),
			      clt2mcode "***>" $3, None)) }
| TOEllipsis TWhen TNotEq w=eexpr TLineEnd e=expr_dots(TEllipsis) c=TCEllipsis
    { Ast0.wrap(Ast0.NestExpr(clt2mcode "<..." $1,
			      Ast0.wrap(Ast0.DOTS(e (mkedots "..."))),
			      clt2mcode "...>" c, Some w)) }
| TOCircles TWhen TNotEq w=eexpr TLineEnd e=expr_dots(TCircles) c=TCCircles
    { Ast0.wrap(Ast0.NestExpr(clt2mcode "<ooo" $1,
			      Ast0.wrap(Ast0.CIRCLES(e (mkedots "ooo"))),
			      clt2mcode "ooo>" c, Some w)) }
| TOStars TWhen TNotEq w=eexpr TLineEnd e=expr_dots(TStars) c=TCStars
    { Ast0.wrap(Ast0.NestExpr(clt2mcode "<***" $1,
			      Ast0.wrap(Ast0.STARS(e (mkedots "***"))),
			      clt2mcode "***>" c, Some w)) }

basic_expr(recurser,primary_extra):
  assign_expr(recurser,primary_extra)                        { $1 }

assign_expr(r,pe):
    cond_expr(r,pe)                        { $1 }
  | unary_expr(r,pe) TAssign assign_expr(r,pe)
      { let (op,clt) = $2 in
      Ast0.wrap(Ast0.Assignment($1,clt2mcode op clt,$3)) }
  | unary_expr(r,pe) TEq assign_expr(r,pe)
      { Ast0.wrap
	  (Ast0.Assignment
	     ($1,clt2mcode Ast.SimpleAssign $2,$3)) }

cond_expr(r,pe):
    arith_expr(r,pe)                         { $1 }
  | l=arith_expr(r,pe) w=TWhy t=option(eexpr) dd=TDotDot r=cond_expr(r,pe)
      { Ast0.wrap(Ast0.CondExpr (l, clt2mcode "?" w, t,
				 clt2mcode ":" dd, r)) }

arith_expr(r,pe):
    cast_expr(r,pe)                         { $1 }
  | arith_expr(r,pe) TMul    arith_expr(r,pe)
      { arith_op Ast.Mul $1 $2 $3 }
  | arith_expr(r,pe) TDiv    arith_expr(r,pe)
      { arith_op Ast.Div $1 $2 $3 }
  | arith_expr(r,pe) TMod    arith_expr(r,pe)
      { arith_op Ast.Mod $1 $2 $3 }
  | arith_expr(r,pe) TPlus   arith_expr(r,pe)
      { arith_op Ast.Plus $1 $2 $3 }
  | arith_expr(r,pe) TMinus  arith_expr(r,pe)
      { arith_op Ast.Minus $1 $2 $3 }
  | arith_expr(r,pe) TShl    arith_expr(r,pe)
      { arith_op Ast.DecLeft $1 $2 $3 }
  | arith_expr(r,pe) TShr    arith_expr(r,pe)
      { arith_op Ast.DecRight $1 $2 $3}
  | arith_expr(r,pe) TInf    arith_expr(r,pe)
      { logic_op Ast.Inf $1 $2 $3 }
  | arith_expr(r,pe) TSup    arith_expr(r,pe)
      { logic_op Ast.Sup $1 $2 $3 }
  | arith_expr(r,pe) TInfEq  arith_expr(r,pe)
      { logic_op Ast.InfEq $1 $2 $3 }
  | arith_expr(r,pe) TSupEq  arith_expr(r,pe)
      { logic_op Ast.SupEq $1 $2 $3 }
  | arith_expr(r,pe) TEqEq   arith_expr(r,pe)
      { logic_op Ast.Eq $1 $2 $3 }
  | arith_expr(r,pe) TNotEq  arith_expr(r,pe)
      { logic_op Ast.NotEq $1 $2 $3 }
  | arith_expr(r,pe) TAnd    arith_expr(r,pe)
      { arith_op Ast.And $1 $2 $3 }
  | arith_expr(r,pe) TOr     arith_expr(r,pe)
      { arith_op Ast.Or $1 $2 $3 }
  | arith_expr(r,pe) TXor    arith_expr(r,pe)
      { arith_op Ast.Xor $1 $2 $3 }
  | arith_expr(r,pe) TAndLog arith_expr(r,pe)
      { logic_op Ast.AndLog $1 $2 $3 }
  | arith_expr(r,pe) TOrLog  arith_expr(r,pe)
      { logic_op Ast.OrLog $1 $2 $3 }

cast_expr(r,pe):
    unary_expr(r,pe)                      { $1 }
  | TOPar ctype TCPar cast_expr(r,pe)
      { Ast0.wrap(Ast0.Cast (clt2mcode "(" $1, $2,
			     clt2mcode ")" $3, $4)) }

unary_expr(r,pe):
    postfix_expr(r,pe)                   { $1 }
  | TInc unary_expr(r,pe)
      { Ast0.wrap(Ast0.Infix ($2, clt2mcode Ast.Inc $1)) }
  | TDec unary_expr(r,pe)
      { Ast0.wrap(Ast0.Infix ($2, clt2mcode Ast.Dec $1)) }
  | unary_op unary_expr(r,pe)
      { let mcode = $1 in Ast0.wrap(Ast0.Unary($2, mcode)) }
  | TSizeof unary_expr(r,pe)
      { Ast0.wrap(Ast0.SizeOfExpr (clt2mcode "sizeof" $1, $2)) }
  | TSizeof TOPar ctype TCPar 
      { Ast0.wrap(Ast0.SizeOfType (clt2mcode "sizeof" $1,
                                   clt2mcode "(" $2,
                                   $3,
                                   clt2mcode ")" $4)) }
                                   

unary_op: TAnd   { clt2mcode Ast.GetRef $1 }
	| TMul   { clt2mcode Ast.DeRef $1 }
	| TPlus  { clt2mcode Ast.UnPlus $1 }
	| TMinus { clt2mcode Ast.UnMinus $1 }
	| TBang  { clt2mcode Ast.Not $1 }

postfix_expr(r,pe):
   primary_expr(r,pe)                            { $1 }
 | postfix_expr(r,pe) TOCro eexpr TCCro
     { Ast0.wrap(Ast0.ArrayAccess ($1,clt2mcode "[" $2,$3,
				       clt2mcode "]" $4)) }
 | postfix_expr(r,pe) TDot   ident
     { Ast0.wrap(Ast0.RecordAccess($1, clt2mcode "." $2, $3)) }
 | postfix_expr(r,pe) TPtrOp ident
     { Ast0.wrap(Ast0.RecordPtAccess($1, clt2mcode "->" $2,
				     $3)) }
 | postfix_expr(r,pe) TInc
     { Ast0.wrap(Ast0.Postfix ($1, clt2mcode Ast.Inc $2)) }
 | postfix_expr(r,pe) TDec
     { Ast0.wrap(Ast0.Postfix ($1, clt2mcode Ast.Dec $2)) }
 | postfix_expr(r,pe) TOPar eexpr_list_option TCPar
     { Ast0.wrap(Ast0.FunCall($1,clt2mcode "(" $2,$3,
			      clt2mcode ")" $4)) }

primary_expr(recurser,primary_extra):
   func_ident   { Ast0.wrap(Ast0.Ident($1)) }
 | TInt
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (clt2mcode (Ast.Int x) clt)) }
 | TFloat
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (clt2mcode (Ast.Float x) clt)) }
 | TString
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (clt2mcode (Ast.String x) clt)) }
 | TChar
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (clt2mcode (Ast.Char x) clt)) }
 | TMetaConst
     { let (nm,pure,ty,clt) = $1 in
     Ast0.wrap(Ast0.MetaConst(clt2mcode nm clt,ty,pure)) }
 | TMetaErr
     { let (nm,pure,clt) = $1 in
     Ast0.wrap(Ast0.MetaErr(clt2mcode nm clt,pure)) }
 | TMetaExp
     { let (nm,pure,ty,clt) = $1 in
     Ast0.wrap(Ast0.MetaExpr(clt2mcode nm clt,ty,pure)) }
 | TOPar eexpr TCPar
     { Ast0.wrap(Ast0.Paren(clt2mcode "(" $1,$2,
			    clt2mcode ")" $3)) }
 | TOPar0 midzero_list(recurser) TCPar0
     { let (mids,code) = $2 in
       Ast0.wrap(Ast0.DisjExpr(clt2mcode "(" $1,
			       code, mids,
			       clt2mcode ")" $3)) }
 | primary_extra { $1 }

expr_dots(dotter):
    r=no_dot_start_end(dexpr,edots_when(dotter,eexpr)) { r }

/*****************************************************************************/

pure_ident:
     TIdent { $1 }

/* allows redeclaring metavariables.  used in @@ @@ */
pure_ident_or_meta_ident:
       x=pure_ident       { let (nm,clt) = x in nm }
     | x=TMetaId          { let (nm,pure,clt) = x in nm }
     | x=TMetaType        { let (nm,pure,clt) = x in nm }
     | x=TMetaParam       { let (nm,pure,clt) = x in nm }
     | x=TMetaParamList   { let (nm,pure,clt) = x in nm }
     | x=TMetaStm         { let (nm,pure,clt) = x in nm }
     | x=TMetaStmList     { let (nm,pure,clt) = x in nm }
     | x=TMetaFunc        { let (nm,pure,clt) = x in nm }
     | x=TMetaLocalFunc   { let (nm,pure,clt) = x in nm }
     | x=TMetaExpList     { let (nm,pure,clt) = x in nm }
     | x=TMetaConst       { let (name,_,pure,info) = x in name }
     | x=TMetaExp         { let (name,_,pure,info) = x in name }
     | x=TMetaErr         { let (nm,pure,clt) = x in nm }

func_ident: pure_ident
         { Ast0.wrap(Ast0.Id(id2mcode $1)) }
     | TMetaId
         { let (nm,pure,clt) = $1 in
           Ast0.wrap(Ast0.MetaId(clt2mcode nm clt,pure)) }
     | TMetaFunc
         { let (nm,pure,clt) = $1 in
           Ast0.wrap(Ast0.MetaFunc(clt2mcode nm clt,pure)) }
     | TMetaLocalFunc
	 { let (nm,pure,clt) = $1 in
           Ast0.wrap(Ast0.MetaLocalFunc(clt2mcode nm clt,pure)) }

ident: pure_ident
         { Ast0.wrap(Ast0.Id(id2mcode $1)) }
     | TMetaId
         { let (nm,pure,clt) = $1 in
           Ast0.wrap(Ast0.MetaId(clt2mcode nm clt,pure)) }

/*****************************************************************************/

decl_list:
   decl_list_start
     {let circle x =
       match Ast0.unwrap x with Ast0.Pcircles(_) -> true | _ -> false in
     if List.exists circle $1
     then Ast0.wrap(Ast0.CIRCLES($1))
     else Ast0.wrap(Ast0.DOTS($1)) }

decl_list_start:
  decl  { [$1] }
| TMetaParamList
    { let (nm,pure,clt) = $1 in
    [Ast0.wrap(Ast0.MetaParamList(clt2mcode nm clt,pure))] }
| decl TComma decl_list_start
    { $1::Ast0.wrap(Ast0.PComma(clt2mcode "," $2))::$3 }
| TMetaParamList TComma decl_list_start
    { let (nm,pure,clt) = $1 in
    Ast0.wrap(Ast0.MetaParamList(clt2mcode nm clt,pure))::
    Ast0.wrap(Ast0.PComma(clt2mcode "," $2))::$3 }
| TEllipsis list(comma_decls(TEllipsis))
    { Ast0.wrap(Ast0.Pdots(clt2mcode "..." $1))::
      (List.concat(List.map (function x -> x (mkpdots "...")) $2)) }
| TCircles list(comma_decls(TCircles))
    { Ast0.wrap(Ast0.Pdots(clt2mcode "ooo" $1))::
      (List.concat(List.map (function x -> x (mkpdots "ooo")) $2)) }

comma_decls(dotter):
  TComma dotter
    { function dot_builder ->
      [Ast0.wrap(Ast0.PComma(clt2mcode "," $1));
	dot_builder $2] }
| TComma decl
    { function dot_builder ->
      [Ast0.wrap(Ast0.PComma(clt2mcode "," $1)); $2] }
| TComma TMetaParamList
    { function dot_builder ->
      let (nm,pure,clt) = $2 in
      [Ast0.wrap(Ast0.PComma(clt2mcode "," $1));
	Ast0.wrap(Ast0.MetaParamList(clt2mcode nm clt,pure))] }

/* must be a list of declarations or statements, with no ... or expressions
for "and" case */
pure_decl_statement_list:
    nonempty_list(decl_statement)           { List.concat $1 }

/* as above, but allows a single expression - for "or" case */
exp_decl_statement_list:
    expr                                    { [Ast0.wrap(Ast0.Exp($1))] }
  | expr TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
    exp_decl_statement_list
      /* HACK!!! */
    { (Ast0.wrap(Ast0.Exp($1)))::
      (Ast0.wrap(Ast0.Nest(clt2mcode "<..." $2,
			  Ast0.wrap(Ast0.DOTS(b (mkdots "..."))),
			  clt2mcode "...>" $4, None)))::
      $5 }
  | expr TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
      /* HACK!!! */
    { [(Ast0.wrap(Ast0.Exp($1)));
	(Ast0.wrap(Ast0.Nest(clt2mcode "<..." $2,
			     Ast0.wrap(Ast0.DOTS(b (mkdots "..."))),
			     clt2mcode "...>" $4, None)))] }

  | pure_decl_statement_list                { $1 }

fun_exp_decl_statement_list:
    ctype
      /* This rule could be in exp_decl_statement_list, which would allow
         it to be ain a... sequence.  But it is not clear whether that makes
         sense, so for now it is here. */
      { [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Ty($1))))] }
  | expr                 { [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1))))] }
  | expr TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
    fun_exp_decl_statement_list
      /* HACK!!! */
    { (Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1)))))::
      (Ast0.wrap
	 (Ast0.OTHER
	    (Ast0.wrap
	       (Ast0.Nest(clt2mcode "<..." $2,
			  Ast0.wrap(Ast0.DOTS(b (mkdots "..."))),
			  clt2mcode "...>" $4, None)))))::
      $5 }

  | expr TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
      /* HACK!!! */
    { [(Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1)))));
      (Ast0.wrap
	 (Ast0.OTHER
	    (Ast0.wrap
	       (Ast0.Nest(clt2mcode "<..." $2,
			  Ast0.wrap(Ast0.DOTS(b (mkdots "..."))),
			  clt2mcode "...>" $4, None)))))] }

  | f=nonempty_list(fun_decl_statement)        { List.concat f }

fun_decl_statement:
    d=decl_statement { List.map (function x -> Ast0.wrap(Ast0.OTHER x)) d }
  | f=fundecl        { [Ast0.wrap(Ast0.DECL(f))] }

/* ---------------------------------------------------------------------- */

error_words:
    TError TWords TEq TOCro cl=comma_list(dexpr) TCCro
      { [Ast0.wrap(Ast0.ERRORWORDS(cl))] }

/* ---------------------------------------------------------------------- */
/* sequences of statements and expressions */

/* a mix of declarations, statements and expressions.  an expression may
appear by itself.  always nonempty and cannot just be dots.  allows fns too. */

minus_function_decl_statement_or_expression: /* doesn't allow just ... */
    opt_dot_start_end(fun_exp_decl_statement_list,
		      pre_post_decl_statement_or_expression,
		      rule_elem_statement,
		      fun_exp_decl_statement_list)
    { List.concat
	($1 (function x -> function y ->
	      [Ast0.wrap(Ast0.OTHER (mkdots x y))])) }

plus_function_decl_statement_or_expression: /* does allow just ... */
    first=fun_exp_decl_statement_list { first }
  | first=loption(fun_exp_decl_statement_list)
      second=required_dot_start_with_ender(fun_exp_decl_statement_list,
				    pre_post_decl_statement_or_expression,
				    rule_elem_statement,
				    fun_exp_decl_statement_list)
      { List.concat
	   (first ::
	    (second
	       (function x -> function y ->
		 [Ast0.wrap(Ast0.OTHER (mkdots x y))]))) }


/* a mix of declarations, statements and expressions.  an expression may
appear by itself.  always nonempty and cannot just be dots. */

pre_post_decl_statement_or_expression:
  opt_dot_start_end(exp_decl_statement_list,
		    pre_post_decl_statement_or_expression,
		    rule_elem_statement,
		    exp_decl_statement_list)
  { top_dots(List.concat ($1 (function x -> function y -> [mkdots x y]))) }

/* a mix of declarations, statements and expressions.  an expression must
be surrounded by ... */

pre_post_decl_statement_and_expression:
    first=pure_decl_statement_list { top_dots first }
  | first=loption(pure_decl_statement_list)
      second=required_dot_start_with_ender(exp_decl_statement_list,
				    pre_post_decl_statement_or_expression,
				    rule_elem_statement,
				    pure_decl_statement_list)
      { top_dots
	  (List.concat
	     (first::(second (function x -> function y -> [mkdots x y])))) }

pre_post_decl_statement_and_expression_opt:
    /* empty */                             { Ast0.wrap(Ast0.DOTS([])) }
  | pre_post_decl_statement_and_expression  { $1 }

pre_post_decl_statement_and_expression_opt_mid:
    pre_post_decl_statement_and_expression       { ($1,[]) }
  | /* empty */                          { (Ast0.wrap(Ast0.DOTS([])),[]) }
  | pre_post_decl_statement_and_expression TMid0
      pre_post_decl_statement_and_expression_opt_mid
      { let (first,rest) = $3 in
        ($1,(clt2mcode "|" $2,first)::rest) }
  | TMid0
      pre_post_decl_statement_and_expression_opt_mid
      { let (first,rest) = $2 in
        (Ast0.wrap(Ast0.DOTS([])),
	 (clt2mcode "|" $1,first)::rest) }

/* ---------------------------------------------------------------------- */

eexpr_list:
  eexpr_list_start
     {let circle x =
       match Ast0.unwrap x with Ast0.Ecircles(_) -> true | _ -> false in
     let star x =
       match Ast0.unwrap x with Ast0.Estars(_) -> true | _ -> false in
     if List.exists circle $1
     then Ast0.wrap(Ast0.CIRCLES($1))
     else
       if List.exists star $1
       then Ast0.wrap(Ast0.STARS($1))
       else Ast0.wrap(Ast0.DOTS($1)) }

/* arg expr.  may contain a type or a explist metavariable */
aexpr:
    dexpr
      { $1 }
  | TMetaExpList
      { let (nm,pure,clt) = $1 in
      Ast0.wrap(Ast0.MetaExprList(clt2mcode nm clt,pure)) }
  | generic_ctype
      { Ast0.wrap(Ast0.TypeExp($1)) }
  | TMetaType				(**)
      { let (nm,pure,clt) = $1 in
      Ast0.wrap
	(Ast0.TypeExp(Ast0.wrap(Ast0.MetaType(clt2mcode nm clt,pure)))) }

eexpr_list_start:
    aexpr { [$1] }
  | aexpr TComma eexpr_list_start
      { $1::Ast0.wrap(Ast0.EComma(clt2mcode "," $2))::$3 }
  | d=edots_when(TEllipsis,eexpr)
	r=list(comma_args(edots_when(TEllipsis,eexpr)))
      { (mkedots "..." d)::
	(List.concat (List.map (function x -> x (mkedots "...")) r)) }
  | d=edots_when(TCircles,eexpr)
	r=list(comma_args(edots_when(TCircles,eexpr)))
      { (mkedots "ooo" d)::
	(List.concat (List.map (function x -> x (mkedots "ooo")) r)) }
  | d=edots_when(TStars,eexpr)
	r=list(comma_args(edots_when(TStars,eexpr)))
      { (mkedots "***" d)::
	(List.concat (List.map (function x -> x (mkedots "***")) r)) }

comma_args(dotter):
  c=TComma d=dotter
    { function dot_builder ->
      [Ast0.wrap(Ast0.EComma(clt2mcode "," c)); dot_builder d] }
| TComma aexpr
    { function dot_builder ->
      [Ast0.wrap(Ast0.EComma(clt2mcode "," $1)); $2] }

eexpr_list_option: eexpr_list { $1 }
         | /* empty */     { Ast0.wrap(Ast0.DOTS([])) }

/****************************************************************************/

// non-empty lists - drop separator
comma_list(elem):
  separated_nonempty_list(TComma,elem) { $1 }

midzero_list(elem):
  elem list(mzl(elem))
     { let (mids,code) = List.split $2 in (mids,($1::code)) }

mzl(elem):
  TMid0 elem { (clt2mcode "|" $1, $2) }

// SEQ1
// at least one instance of grammar/ender

opt_dot_start_end(grammar,when_grammar,simple_when_grammar,ender):
   start=ender { function dot_builder -> [start] }
 | r=opt_dot_start_end_pattern(grammar,
			       dots_when(TEllipsis,when_grammar,
					 simple_when_grammar),
     ender,opt_dot_end_ellipsis(grammar,when_grammar,
				simple_when_grammar,ender))
   { function dot_builder -> r (dot_builder "...") }
 | r=opt_dot_start_end_pattern(grammar,dots_when(TCircles,when_grammar,
						 simple_when_grammar),
     ender,opt_dot_end_circles(grammar,when_grammar,
			       simple_when_grammar,ender))
   { function dot_builder -> r (dot_builder "ooo") }
 | r=opt_dot_start_end_pattern(grammar,dots_when(TStars,when_grammar,
						 simple_when_grammar),
     ender,opt_dot_end_stars(grammar,when_grammar,
			     simple_when_grammar,ender))
   { function dot_builder -> r (dot_builder "***") }

opt_dot_start_end_pattern(grammar,dotter,ender,continue):
   g=grammar d=dotter
     { function dot_builder -> [g; (dot_builder d)] }
 | g=grammar d=dotter c=continue
     { function dot_builder -> g :: (dot_builder d) :: (c dot_builder) }
 | d=dotter c=continue // continue is never empty
     { function dot_builder -> (dot_builder d) :: (c dot_builder) }

opt_dot_end_ellipsis(grammar,when_grammar,simple_when_grammar,ender):
   g=ender { function dot_builder -> [g] }
 | g=grammar d=dots_when(TEllipsis,when_grammar,simple_when_grammar)
     { function dot_builder -> [g ; dot_builder d ] }
 | g=grammar d=dots_when(TEllipsis,when_grammar,simple_when_grammar)
     r=opt_dot_end_ellipsis(grammar,when_grammar,simple_when_grammar,ender)
     { function dot_builder -> g :: (dot_builder d) :: (r dot_builder) }

opt_dot_end_circles(grammar,when_grammar,simple_when_grammar,ender):
   g=ender { function dot_builder -> [g] }
 | g=grammar d=dots_when(TCircles,when_grammar,simple_when_grammar)
     { function dot_builder -> [g ; dot_builder d ] }
 | g=grammar d=dots_when(TCircles,when_grammar,simple_when_grammar)
     r=opt_dot_end_circles(grammar,when_grammar,simple_when_grammar,ender)
     { function dot_builder -> g :: (dot_builder d) :: (r dot_builder) }

opt_dot_end_stars(grammar,when_grammar,simple_when_grammar,ender):
   g=ender { function dot_builder -> [g] }
 | g=grammar d=dots_when(TStars,when_grammar,simple_when_grammar)
     { function dot_builder -> [g ; dot_builder d ] }
 | g=grammar d=dots_when(TStars,when_grammar,simple_when_grammar)
     r=opt_dot_end_stars(grammar,when_grammar,simple_when_grammar,ender)
     { function dot_builder -> g :: (dot_builder d) :: (r dot_builder) }

// SEQ2, ender optional
required_dot_start_with_ender(grammar,when_grammar,
				      simple_when_grammar,ender):
 | start=dots_when(TEllipsis,when_grammar,simple_when_grammar)
     finish=no_dot_start_ellipsis(grammar,when_grammar,simple_when_grammar,
				  ender)
   { (function dot_builder ->
       (dot_builder "..." start) :: (finish (dot_builder "..."))) }
 | start=dots_when(TCircles,when_grammar,simple_when_grammar)
     finish=no_dot_start_circles(grammar,when_grammar,simple_when_grammar,
				 ender)
   { (function dot_builder ->
       (dot_builder "ooo" start) :: (finish (dot_builder "ooo"))) }
 | start=dots_when(TStars,when_grammar,simple_when_grammar)
     finish=no_dot_start_stars(grammar,when_grammar,simple_when_grammar,ender)
   { (function dot_builder ->
       (dot_builder "***" start) :: (finish (dot_builder "***"))) }

no_dot_start_ellipsis(grammar,when_grammar,simple_when_grammar,ender):
   /* empty */    { function dot_builder -> [] }
 | e=ender
       { function dot_builder -> [e] }
 | g=grammar d=dots_when(TEllipsis,when_grammar,simple_when_grammar) 
       r=no_dot_start_ellipsis(grammar,when_grammar,simple_when_grammar,ender)
       { function dot_builder -> g::(dot_builder d)::(r dot_builder) }

no_dot_start_circles(grammar,when_grammar,simple_when_grammar,ender):
       { function dot_builder -> [] }
 | e=ender
       { function dot_builder -> [e] }
 | g=grammar d=dots_when(TCircles,when_grammar,simple_when_grammar) 
       r=no_dot_start_circles(grammar,when_grammar,simple_when_grammar,ender)
       { function dot_builder -> g::(dot_builder d)::(r dot_builder) }

no_dot_start_stars(grammar,when_grammar,simple_when_grammar,ender):
       { function dot_builder -> [] }
 | e=ender
       { function dot_builder -> [e] }
 | g=grammar d=dots_when(TStars,when_grammar,simple_when_grammar) 
       r=no_dot_start_stars(grammar,when_grammar,simple_when_grammar,ender)
       { function dot_builder -> g::(dot_builder d)::(r dot_builder) }

edots_when(dotter,when_grammar):
    d=dotter                                      { (d,None) }
  | d=dotter TWhen TNotEq w=when_grammar TLineEnd { (d,Some w) }

dots_when(dotter,when_grammar,simple_when_grammar):
    d=dotter                                 { (d,Ast0.NoWhen) }
  | d=dotter TWhen TNotEq w=when_grammar TLineEnd
      { (d,Ast0.WhenNot w) }
  | d=dotter TWhen TEq w=simple_when_grammar TLineEnd
      { (d,Ast0.WhenAlways w) }

// used in NEST
no_dot_start_end(grammar,dotter):
  g=grammar dg=list(pair(dotter,grammar))
  { function dot_builder ->
      g :: (List.concat(List.map (function (d,g) -> [dot_builder d;g]) dg)) }

/*****************************************************************************
*
*
*****************************************************************************/

iso_main:
  TIsoExpression e1=dexpr el=list(iso(dexpr)) EOF
  { iso_adjust (function x -> Ast0.ExprTag x) e1 el }
| TIsoStatement s1=single_statement sl=list(iso(single_statement)) EOF
    { iso_adjust (function x -> Ast0.StmtTag x) s1 sl }
| TIsoDeclaration d1=decl_var dl=list(iso(decl_var)) EOF
    { let check_one = function
	[x] -> x
      | _ ->
	  failwith
	    "only one variable per declaration in an isomorphism rule" in
    let d1 = check_one d1 in
    let dl =
      List.map
	(function
	    Common.Left x -> Common.Left(check_one x)
	  | Common.Right x -> Common.Right(check_one x))
	dl in
    iso_adjust (function x -> Ast0.DeclTag x) d1 dl }

iso(term):
    TIso t=term { Common.Left t }
  | TRightIso t=term { Common.Right t }
