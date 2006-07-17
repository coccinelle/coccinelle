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
    Ast0.mcode_start = None; Ast0.mcode_end = None;
    Ast0.column = -1; Ast0.offset = offset }

let clt2info (_,line,logical_line,offset) = make_info line logical_line offset

(* the second argument is $startofs, which doesn't work, but which would be
nice to use *)
let clt2mcode str _ = function
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
let id2mcode offset (name, clt) = clt2mcode name offset clt

let mkdots str (dot,offset,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Dots(clt2mcode str offset dot, whencode))
  | "ooo" -> Ast0.wrap(Ast0.Circles(clt2mcode str offset dot, whencode))
  | "***" -> Ast0.wrap(Ast0.Stars(clt2mcode str offset dot, whencode))
  | _ -> failwith "cannot happen"

let mkedots str (dot,offset,whencode) =
  match str with
    "..." -> Ast0.wrap(Ast0.Edots(clt2mcode str offset dot, whencode))
  | "ooo" -> Ast0.wrap(Ast0.Ecircles(clt2mcode str offset dot, whencode))
  | "***" -> Ast0.wrap(Ast0.Estars(clt2mcode str offset dot, whencode))
  | _ -> failwith "cannot happen"

let mkpdots str offset dot =
  match str with
    "..." -> Ast0.wrap(Ast0.Pdots(clt2mcode str offset dot))
  | "ooo" -> Ast0.wrap(Ast0.Pcircles(clt2mcode str offset dot))
  | _ -> failwith "cannot happen"

let arith_op ast_op left op opoffset right =
  Ast0.wrap
    (Ast0.Binary(left, clt2mcode (Ast.Arith ast_op) opoffset op, right))

let logic_op ast_op left op opoffset right =
  Ast0.wrap
    (Ast0.Binary(left, clt2mcode (Ast.Logical ast_op) opoffset op, right))

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
let pointerify ty m offset =
  List.fold_left
    (function inner ->
      function cur ->
	Ast0.wrap(Ast0.Pointer(inner,clt2mcode "*" offset cur)))
    ty m

let startofs _ = -1

%}


%token EOF

%token TIdentifier TExpression TStatement TFunction TLocal TType TParameter
%token TWhy0 TPlus0 TBang0 Tlist TFresh TConstant TError TWords

%token<Data.line_type * int * int * int> Tchar Tshort Tint Tdouble Tfloat Tlong
%token<Data.line_type * int * int * int> Tvoid Tstruct Tunion
%token<Data.line_type * int * int * int> Tunsigned Tsigned

%token<Data.line_type * int * int * int> Tstatic Tconst Tvolatile

%token <Data.line_type * int * int * int> TIf TElse TWhile TFor TDo TReturn
%token <Data.line_type * int * int * int> TSizeof
%token <Data.line_type * int * int * int> TFunDecl
%token <string * (Data.line_type * int * int * int)> TIdent
%token <string * (Data.line_type * int * int * int)> TMetaId TMetaType TMetaErr
%token <string * (Data.line_type * int * int * int)> TMetaParam TMetaParamList
%token <string * (Data.line_type * int * int * int)> TMetaStm TMetaStmList
%token <string * (Data.line_type * int * int * int)> TMetaFunc TMetaLocalFunc
%token <string * (Data.line_type * int * int * int)> TMetaExpList
%token <string * Ast0_cocci.typeC list option *
        (Data.line_type*int*int*int)> TMetaExp TMetaConst
%token TArobArob

%token <Data.line_type * int * int * int> TEllipsis TOEllipsis TCEllipsis
%token <Data.line_type * int * int * int> TWhen TLineEnd
%token <Data.line_type * int * int * int> TCircles TOCircles TCCircles
%token <Data.line_type * int * int * int> TStars TOStars TCStars

%token <Data.line_type * int * int * int> TWhy TDotDot TBang TOPar TOPar0 TMid
%token <Data.line_type * int * int * int> TMid0 TCPar TCPar0

%token <string * (Data.line_type * int * int * int)> TInclude
%token <string * (Data.line_type * int * int * int)> TMinusFile TPlusFile

%token <Data.line_type * int * int * int> TInc TDec

%token <string * (Data.line_type * int * int * int)> TString TChar TFloat TInt

%token <Data.line_type * int * int * int> TOrLog
%token <Data.line_type * int * int * int> TAndLog
%token <Data.line_type * int * int * int> TOr
%token <Data.line_type * int * int * int> TXor
%token <Data.line_type * int * int * int> TAnd 
%token <Data.line_type * int * int * int> TEqEq TNotEq
%token <Data.line_type * int * int * int> TInf TSup TInfEq TSupEq 
%token <Data.line_type * int * int * int> TShl TShr
%token <Data.line_type * int * int * int> TPlus TMinus
%token <Data.line_type * int * int * int> TMul TDiv TMod 

%token <Data.line_type * int * int * int> TOBrace TCBrace
%token <Data.line_type * int * int * int> TOCro TCCro

%token <Data.line_type * int * int * int> TPtrOp

%token <Data.line_type * int * int * int> TEq TDot TComma TPtVirg
%token <Ast_cocci.assignOp * (Data.line_type * int * int * int)> TAssign

%token TIso TIsoExpression TIsoStatement TIsoDeclaration

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
%type <Ast0_cocci.anything list> iso_main

%%

minus_main: minus_body EOF { $1 } | minus_body TArobArob { $1 }
plus_main: plus_body EOF { $1 } | plus_body TArobArob { $1 }
meta_main: metadec* TArobArob { List.concat($1) }

/*****************************************************************************
*
*
*****************************************************************************/

metadec:
  ar=arity kindfn=metakind ids=comma_list(pure_ident_or_meta_ident) TPtVirg
  { List.map (function x -> kindfn ar (id2name x)) ids }

%inline metakind:
  TIdentifier
    { (function arity -> function name ->
        !Data.add_id_meta name; Ast.MetaIdDecl(arity,name)) }
| TFresh TIdentifier
    { (function arity -> function name ->
        !Data.add_id_meta name; Ast.MetaFreshIdDecl(arity,name)) }
| TType
    { (function arity -> function name ->
        !Data.add_type_meta name; Ast.MetaTypeDecl(arity,name)) } 
| TParameter
    { (function arity -> function name ->
        !Data.add_param_meta name; Ast.MetaParamDecl(arity,name)) }
| TParameter Tlist
    { (function arity -> function name ->
        !Data.add_paramlist_meta name; Ast.MetaParamListDecl(arity,name)) } 
| TError
    { (function arity -> function name ->
        !Data.add_err_meta name; Ast.MetaErrDecl(arity,name)) }
| TExpression
    { (function arity -> function name ->
        !Data.add_exp_meta None name; Ast.MetaExpDecl(arity,name)) }
| TExpression Tlist
    { (function arity -> function name ->
        !Data.add_explist_meta name; Ast.MetaExpListDecl(arity,name)) }
| TStatement
    { (function arity -> function name ->
        !Data.add_stm_meta name; Ast.MetaStmDecl(arity,name)) }
| TStatement Tlist
    { (function arity -> function name ->
        !Data.add_stmlist_meta name; Ast.MetaStmListDecl(arity,name)) }
| TFunction
    { (function arity -> function name ->
        !Data.add_func_meta name; Ast.MetaFuncDecl(arity,name)) }
| TLocal TFunction
    { (function arity -> function name ->
        !Data.add_local_func_meta name; Ast.MetaLocalFuncDecl(arity,name)) }
| vl=meta_exp_type // no error if use $1 but doesn't type check
    { (function arity -> function name ->
        !Data.add_exp_meta (Some vl) name; Ast.MetaExpDecl(arity,name)) }
| TConstant ty=ioption(const_meta_exp_type)
    { (function arity -> function name ->
        !Data.add_const_meta ty name; Ast.MetaConstDecl(arity,name)) }

meta_exp_type:
  param_ctype                             { [$1] }
| TOBrace comma_list(param_ctype) TCBrace { $2 }

const_meta_exp_type:
  mtype                                   { [$1] }
| TOBrace comma_list(param_ctype) TCBrace { $2 }

arity: TBang0 { Ast.UNIQUE }
     | TWhy0  { Ast.OPT }
     | TPlus0 { Ast.MULTI }
     | /* empty */ { Ast.NONE }

generic_ctype:
       Tvoid
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.VoidType (startofs($1)) $1,
				   None)) }
     | q=ioption(ctype_qualif) ty=Tchar
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.CharType (startofs(ty)) ty,
				   q)) }
     | q=ioption(ctype_qualif) ty=Tshort
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.ShortType (startofs(ty)) ty,
				   q)) }
     | q=ioption(ctype_qualif) ty=Tint
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.IntType (startofs(ty)) ty,
				   q)) }
     | Tdouble
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.DoubleType (startofs($1)) $1,
				   None)) }
     | Tfloat
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.FloatType (startofs($1)) $1,
				   None)) }
     | q=ioption(ctype_qualif) ty=Tlong
         { Ast0.wrap(Ast0.BaseType(clt2mcode Ast.LongType (startofs(ty)) ty,
				   q)) }
     | Tstruct pure_ident
	 { Ast0.wrap(Ast0.StructUnionName(id2mcode (startofs($2)) $2,
					  clt2mcode Ast.Struct (startofs($1))
					    $1)) }
     | Tunion pure_ident
	 { Ast0.wrap(Ast0.StructUnionName(id2mcode (startofs($2)) $2,
					  clt2mcode Ast.Union (startofs($1))
					    $1)) }

mtype: // no metavariable, for constant metavariable declarations
       cv=ioption(const_vol) ty=generic_ctype m=list(TMul)
	 { make_cv cv (pointerify ty m (startofs(m))) }
     | cv=ioption(const_vol) p=pure_ident m=list(TMul)
	 { let nm = Ast0.wrap(Ast0.TypeName(id2mcode (startofs(p)) p)) in
           make_cv cv (pointerify nm m (startofs(m))) }

ctype:
       cv=ioption(const_vol) ty=generic_ctype m=list(TMul)
	 { make_cv cv (pointerify ty m (startofs(m))) }
     | cv=ioption(const_vol) ty=TMetaType m=list(TMul)
	 { let (nm,clt) = ty in
	 let ty =Ast0.wrap(Ast0.MetaType(clt2mcode nm (startofs(ty)) clt)) in
	 make_cv cv (pointerify ty m (startofs(m))) }

param_ctype: // the most general - allows metavariables and type names
       cv=ioption(const_vol) ty=generic_ctype m=list(TMul)
	 { make_cv cv (pointerify ty m (startofs(m))) }
     | cv=ioption(const_vol) p=pure_ident m=list(TMul)
	 { let nm = Ast0.wrap(Ast0.TypeName(id2mcode (startofs(p)) p)) in
           make_cv cv (pointerify nm m (startofs(m))) }
     | cv=ioption(const_vol) ty=TMetaType m=list(TMul)
	 { let (nm,clt) = ty in
	 let ty = Ast0.wrap(Ast0.MetaType(clt2mcode nm (startofs(ty)) clt)) in
	 make_cv cv (pointerify ty m (startofs(m))) }

ctype_qualif:
       Tunsigned   { clt2mcode Ast.Unsigned (startofs($1)) $1 }
     | Tsigned     { clt2mcode Ast.Signed (startofs($1)) $1 }

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
	  (Ast0.FILEINFO(id2mcode (startofs($1)) $1,
			 id2mcode (startofs($2)) $2))] }

includes:
  TInclude
    { Ast0.wrap
	(Ast0.INCLUDE(clt2mcode "#include" (startofs($1)) (id2clt $1),
		      id2mcode (startofs($1)) $1)) }

/*****************************************************************************/

fundecl:
  storage TFunDecl ident TOPar decl_list TCPar
  TOBrace pre_post_decl_statement_and_expression_opt TCBrace
      { Ast0.wrap(Ast0.FunDecl($1, $3,
			       clt2mcode "(" (startofs($4)) $4, $5,
			       clt2mcode ")" (startofs($6)) $6,
			       clt2mcode "{" (startofs($7)) $7, $8,
			       clt2mcode "}" (startofs($9)) $9)) }

storage: Tstatic      { Some (clt2mcode Ast.Static (startofs($1)) $1) }
       | /* empty */  { None }

decl: param_ctype ident
	{ Ast0.wrap(Ast0.Param($2, $1)) }
    | TMetaParam
	{ let (nm,clt) = $1 in
	Ast0.wrap(Ast0.MetaParam(clt2mcode nm (startofs($1)) clt)) }

const_vol:
      Tconst       { clt2mcode Ast.Const (startofs($1)) $1 }
    | Tvolatile    { clt2mcode Ast.Volatile (startofs($1)) $1 }

/*****************************************************************************/

statement:
  TMetaStm
    { let (nm,clt) = $1 in
    Ast0.wrap(Ast0.MetaStmt(clt2mcode nm (startofs($1)) clt)) }
| expr TPtVirg
    { Ast0.wrap(Ast0.ExprStatement ($1, clt2mcode ";" (startofs($2)) $2)) }
| TIf TOPar eexpr TCPar single_statement %prec TIf
    { Ast0.wrap(Ast0.IfThen(clt2mcode "if" (startofs($1)) $1,
			    clt2mcode "(" (startofs($2)) $2,$3,
			    clt2mcode ")" (startofs($4)) $4,$5)) }
| TIf TOPar eexpr TCPar single_statement TElse single_statement
    { Ast0.wrap(Ast0.IfThenElse(clt2mcode "if" (startofs($1)) $1,
				clt2mcode "(" (startofs($2)) $2,$3,
				clt2mcode ")" (startofs($4)) $4,$5,
				clt2mcode "else" (startofs($6)) $6,$7)) }
| fr=TFor lp=TOPar e1=ioption(eexpr) sc1=TPtVirg e2=ioption(eexpr) sc2=TPtVirg
    e3=ioption(eexpr) rp=TCPar s=single_statement
    { Ast0.wrap(Ast0.For(clt2mcode "for" (startofs(fr)) fr,
			 clt2mcode "(" (startofs(lp)) lp,e1,
			 clt2mcode ";" (startofs(sc1)) sc1,e2,
			 clt2mcode ";" (startofs(sc2)) sc2,e3,
			 clt2mcode ")" (startofs(rp)) rp,s)) }
| TWhile TOPar eexpr TCPar single_statement
    { Ast0.wrap(Ast0.While(clt2mcode "while" (startofs($1)) $1,
			   clt2mcode "(" (startofs($2)) $2,$3,
			   clt2mcode ")" (startofs($4)) $4,$5)) }
| TDo single_statement TWhile TOPar eexpr TCPar TPtVirg
    { Ast0.wrap(Ast0.Do(clt2mcode "do" (startofs($1)) $1,$2,
			clt2mcode "while" (startofs($3)) $3,
			clt2mcode "(" (startofs($4)) $4,$5,
			clt2mcode ")" (startofs($6)) $6,
			clt2mcode ";" (startofs($7)) $7)) }
| TReturn eexpr TPtVirg
    { Ast0.wrap(Ast0.ReturnExpr(clt2mcode "return" (startofs($1)) $1,$2,
				clt2mcode ";" (startofs($3)) $3)) }
| TReturn TPtVirg
    { Ast0.wrap(Ast0.Return(clt2mcode "return" (startofs($1)) $1,
			    clt2mcode ";" (startofs($2)) $2)) }
| TOBrace pre_post_decl_statement_and_expression_opt TCBrace
    { Ast0.wrap(Ast0.Seq(clt2mcode "{" (startofs($1)) $1,$2,
			 clt2mcode "}" (startofs($3)) $3)) }
| TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
    { Ast0.wrap(Ast0.Nest(clt2mcode "<..." (startofs($1)) $1,
			  Ast0.wrap(Ast0.DOTS(b (mkdots "..."))),
			  clt2mcode "...>" (startofs($3)) $3)) }
| TOCircles b=statement_dots(TCircles) TCCircles
    { Ast0.wrap(Ast0.Nest(clt2mcode "<ooo" (startofs($1)) $1,
			  Ast0.wrap(Ast0.CIRCLES(b (mkdots "ooo"))),
			  clt2mcode "ooo>" (startofs($3)) $3)) }
| TOStars b=statement_dots(TStars) TCStars
    { Ast0.wrap(Ast0.Nest(clt2mcode "<***" (startofs($1)) $1,
			  Ast0.wrap(Ast0.STARS(b (mkdots "***"))),
			  clt2mcode "***>" (startofs($3)) $3)) }

statement_dots(dotter):
  r=no_dot_start_end(exp_decl_statement_list,
		     dots_when(dotter,pre_post_decl_statement_or_expression))
  { function dot_builder ->
    List.concat (r (function x -> [dot_builder x])) }

/* a statement on its own */
single_statement:
    statement                         { $1 }
  | TOPar0 mid_list(statement) TCPar0
      /* degenerate case, elements are single statements and thus don't
	contain dots */
      { Ast0.wrap
	  (Ast0.Disj(clt2mcode "(" (startofs($1)) $1,
		     List.map (function x -> Ast0.wrap(Ast0.DOTS([x]))) $2,
		     clt2mcode ")" (startofs($3)) $3)) }

/* In the following, an identifier as a type is not fully supported.  Indeed,
the language is ambiguous: what is foo * bar; */
/* The AST DisjDecl cannot be generated because it would be ambiguous with
a disjunction on a statement with a declaration in each branch */
decl_var:
    ctype comma_list(d_ident) TPtVirg
      { List.map
	  (function (id,fn) ->
	    Ast0.wrap(Ast0.UnInit(fn $1,id,clt2mcode ";" (startofs($3)) $3)))
	  $2 }
  | ctype d_ident TEq eexpr TPtVirg
      { let (id,fn) = $2 in
      [Ast0.wrap(Ast0.Init(fn $1,id,clt2mcode "=" (startofs($3)) $3,$4,
			   clt2mcode ";" (startofs($5)) $5))] }
  | cv=ioption(const_vol) i=pure_ident d=d_ident pv=TPtVirg
      { let (id,fn) = d in
      let idtype =
	make_cv cv (Ast0.wrap (Ast0.TypeName(id2mcode (startofs(i)) i))) in
      [Ast0.wrap(Ast0.UnInit(fn idtype,id,clt2mcode ";" (startofs(pv)) pv))] }
  | cv=ioption(const_vol) i=pure_ident d=d_ident q=TEq e=eexpr pv=TPtVirg
      { let (id,fn) = d in
      let idtype =
	make_cv cv (Ast0.wrap (Ast0.TypeName(id2mcode (startofs(i)) i))) in
      [Ast0.wrap(Ast0.Init(fn idtype,id,clt2mcode "=" (startofs(q)) q,e,
			   clt2mcode ";" (startofs(pv)) pv))] }

d_ident:
    ident
      { ($1,function x -> x) }
  | a=ident l=TOCro i=ioption(eexpr) r=TCCro
      { (a,function x ->
	Ast0.wrap(Ast0.Array(x,clt2mcode "[" (startofs(l)) l,i,
			     clt2mcode "]" (startofs(r)) r))) }

/* a statement that is part of a list */
decl_statement:
    TMetaStmList
      { let (nm,clt) = $1 in
      [Ast0.wrap(Ast0.MetaStmt(clt2mcode nm (startofs($1)) clt))] }
  | decl_var
      { List.map (function x -> Ast0.wrap(Ast0.Decl(x))) $1 }
  | statement { [$1] }
  | TOPar0 pre_post_decl_statement_and_expression_opt_mid TCPar0
      { if List.for_all
	  (function x ->
	    match Ast0.unwrap x with Ast0.DOTS([]) -> true | _ -> false)
	  $2
      then []
      else [Ast0.wrap(Ast0.Disj(clt2mcode "(" (startofs($1)) $1,
				$2,
				clt2mcode ")" (startofs($3)) $3))] }

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
  TEllipsis { Ast0.wrap(Ast0.Edots(clt2mcode "..." (startofs($1)) $1,None)) }
| nest_expressions { $1 }

nest_expressions:
  TOEllipsis expr_dots(TEllipsis) TCEllipsis
    { Ast0.wrap(Ast0.NestExpr(clt2mcode "<..." (startofs($1)) $1,
			      Ast0.wrap(Ast0.DOTS($2 (mkedots "..."))),
			      clt2mcode "...>" (startofs($3)) $3)) }
| TOCircles expr_dots(TCircles) TCCircles
    { Ast0.wrap(Ast0.NestExpr(clt2mcode "<ooo" (startofs($1)) $1,
			      Ast0.wrap(Ast0.CIRCLES($2 (mkedots "ooo"))),
			      clt2mcode "ooo>" (startofs($3)) $3)) }
| TOStars expr_dots(TStars) TCStars
    { Ast0.wrap(Ast0.NestExpr(clt2mcode "<***" (startofs($1)) $1,
			      Ast0.wrap(Ast0.STARS($2 (mkedots "***"))),
			      clt2mcode "***>" (startofs($3)) $3)) }

basic_expr(recurser,primary_extra):
  assign_expr(recurser,primary_extra)                        { $1 }

assign_expr(r,pe):
    cond_expr(r,pe)                        { $1 }
  | unary_expr(r,pe) TAssign assign_expr(r,pe)
      { let (op,clt) = $2 in
      Ast0.wrap(Ast0.Assignment($1,clt2mcode op (startofs($2)) clt,$3)) }
  | unary_expr(r,pe) TEq assign_expr(r,pe)
      { Ast0.wrap
	  (Ast0.Assignment
	     ($1,clt2mcode Ast.SimpleAssign (startofs($2)) $2,$3)) }

cond_expr(r,pe):
    arith_expr(r,pe)                         { $1 }
  | l=arith_expr(r,pe) w=TWhy t=ioption(eexpr) dd=TDotDot r=cond_expr(r,pe)
      { Ast0.wrap(Ast0.CondExpr (l, clt2mcode "?" (startofs(w)) w, t,
				 clt2mcode ":" (startofs(dd)) dd, r)) }

arith_expr(r,pe):
    cast_expr(r,pe)                         { $1 }
  | arith_expr(r,pe) TMul    arith_expr(r,pe)
      { arith_op Ast.Mul $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TDiv    arith_expr(r,pe)
      { arith_op Ast.Div $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TMod    arith_expr(r,pe)
      { arith_op Ast.Mod $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TPlus   arith_expr(r,pe)
      { arith_op Ast.Plus $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TMinus  arith_expr(r,pe)
      { arith_op Ast.Minus $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TShl    arith_expr(r,pe)
      { arith_op Ast.DecLeft $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TShr    arith_expr(r,pe)
      { arith_op Ast.DecRight $1 $2 (startofs($2)) $3}
  | arith_expr(r,pe) TInf    arith_expr(r,pe)
      { logic_op Ast.Inf $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TSup    arith_expr(r,pe)
      { logic_op Ast.Sup $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TInfEq  arith_expr(r,pe)
      { logic_op Ast.InfEq $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TSupEq  arith_expr(r,pe)
      { logic_op Ast.SupEq $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TEqEq   arith_expr(r,pe)
      { logic_op Ast.Eq $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TNotEq  arith_expr(r,pe)
      { logic_op Ast.NotEq $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TAnd    arith_expr(r,pe)
      { arith_op Ast.And $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TOr     arith_expr(r,pe)
      { arith_op Ast.Or $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TXor    arith_expr(r,pe)
      { arith_op Ast.Xor $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TAndLog arith_expr(r,pe)
      { logic_op Ast.AndLog $1 $2 (startofs($2)) $3 }
  | arith_expr(r,pe) TOrLog  arith_expr(r,pe)
      { logic_op Ast.OrLog $1 $2 (startofs($2)) $3 }

cast_expr(r,pe):
    unary_expr(r,pe)                      { $1 }
  | TOPar ctype TCPar cast_expr(r,pe)
      { Ast0.wrap(Ast0.Cast (clt2mcode "(" (startofs($1)) $1, $2,
			     clt2mcode ")" (startofs($3)) $3, $4)) }

unary_expr(r,pe):
    postfix_expr(r,pe)                   { $1 }
  | TInc unary_expr(r,pe)
      { Ast0.wrap(Ast0.Infix ($2, clt2mcode Ast.Inc (startofs($1)) $1)) }
  | TDec unary_expr(r,pe)
      { Ast0.wrap(Ast0.Infix ($2, clt2mcode Ast.Dec (startofs($1)) $1)) }
  | unary_op unary_expr(r,pe)
      { let mcode = $1 in Ast0.wrap(Ast0.Unary($2, mcode)) }
  | TSizeof unary_expr(r,pe)
      { Ast0.wrap(Ast0.SizeOfExpr (clt2mcode "sizeof" (startofs($1)) $1, $2)) }
  | TSizeof TOPar ctype TCPar 
      { Ast0.wrap(Ast0.SizeOfType (clt2mcode "sizeof" (startofs($1)) $1,
                                   clt2mcode "(" (startofs($2)) $2,
                                   $3,
                                   clt2mcode ")" (startofs($4)) $4)) }
                                   

unary_op: TAnd   { clt2mcode Ast.GetRef (startofs($1)) $1 }
	| TMul   { clt2mcode Ast.DeRef (startofs($1)) $1 }
	| TPlus  { clt2mcode Ast.UnPlus (startofs($1)) $1 }
	| TMinus { clt2mcode Ast.UnMinus (startofs($1)) $1 }
	| TBang  { clt2mcode Ast.Not (startofs($1)) $1 }

postfix_expr(r,pe):
   primary_expr(r,pe)                            { $1 }
 | postfix_expr(r,pe) TOCro eexpr TCCro
     { Ast0.wrap(Ast0.ArrayAccess ($1,clt2mcode "[" (startofs($2)) $2,$3,
				       clt2mcode "]" (startofs($4)) $4)) }
 | postfix_expr(r,pe) TDot   ident
     { Ast0.wrap(Ast0.RecordAccess($1, clt2mcode "." (startofs($2)) $2, $3)) }
 | postfix_expr(r,pe) TPtrOp ident
     { Ast0.wrap(Ast0.RecordPtAccess($1, clt2mcode "->" (startofs($2)) $2,
				     $3)) }
 | postfix_expr(r,pe) TInc
     { Ast0.wrap(Ast0.Postfix ($1, clt2mcode Ast.Inc (startofs($2)) $2)) }
 | postfix_expr(r,pe) TDec
     { Ast0.wrap(Ast0.Postfix ($1, clt2mcode Ast.Dec (startofs($2)) $2)) }
 | postfix_expr(r,pe) TOPar eexpr_list_option TCPar
     { Ast0.wrap(Ast0.FunCall($1,clt2mcode "(" (startofs($2)) $2,$3,
			      clt2mcode ")" (startofs($4)) $4)) }

primary_expr(recurser,primary_extra):
   ident   { Ast0.wrap(Ast0.Ident($1)) }
 | TInt
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (clt2mcode (Ast.Int x) (startofs($1)) clt)) }
 | TFloat
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (clt2mcode (Ast.Float x) (startofs($1)) clt)) }
 | TString
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (clt2mcode (Ast.String x) (startofs($1)) clt)) }
 | TChar
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (clt2mcode (Ast.Char x) (startofs($1)) clt)) }
 | TMetaConst
     { let (nm,ty,clt) = $1 in
     Ast0.wrap(Ast0.MetaConst(clt2mcode nm (startofs($1)) clt,ty)) }
 | TMetaErr
     { let (nm,clt) = $1 in
     Ast0.wrap(Ast0.MetaErr(clt2mcode nm (startofs($1)) clt)) }
 | TMetaExp
     { let (nm,ty,clt) = $1 in
     Ast0.wrap(Ast0.MetaExpr(clt2mcode nm (startofs($1)) clt,ty)) }
 | TOPar eexpr TCPar
     { Ast0.wrap(Ast0.Paren(clt2mcode "(" (startofs($1)) $1,$2,
			    clt2mcode ")" (startofs($3)) $3)) }
 | TOPar0 midzero_list(recurser) TCPar0
     { Ast0.wrap(Ast0.DisjExpr(clt2mcode "(" (startofs($1)) $1,
			       $2,
			       clt2mcode ")" (startofs($3)) $3)) }
 | primary_extra { $1 }

expr_dots(dotter):
    r=no_dot_start_end(dexpr,dots_when(dotter,eexpr)) { r }

/*****************************************************************************/

pure_ident: TIdent { $1 }

/* allows redeclaring metavariables.  used in @@ @@ */
pure_ident_or_meta_ident:
       x=TIdent           { x }
     | x=TMetaId          { x }
     | x=TMetaType        { x }
     | x=TMetaParam       { x }
     | x=TMetaParamList   { x }
     | x=TMetaStm         { x }
     | x=TMetaStmList     { x }
     | x=TMetaFunc        { x }
     | x=TMetaLocalFunc   { x }
     | x=TMetaExpList     { x }
     | x=TMetaConst       { let (name,_,info) = x in (name,info) }
     | x=TMetaExp         { let (name,_,info) = x in (name,info) }
     | x=TMetaErr         { x }

ident: TIdent
         { Ast0.wrap(Ast0.Id(id2mcode (startofs($1)) $1)) }
     | TMetaId
         { Ast0.wrap(Ast0.MetaId(id2mcode (startofs($1)) $1)) }
     | TMetaFunc
         { Ast0.wrap(Ast0.MetaFunc(id2mcode (startofs($1)) $1)) }
     | TMetaLocalFunc
	 { Ast0.wrap(Ast0.MetaLocalFunc(id2mcode (startofs($1)) $1)) }

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
    { let (nm,clt) = $1 in
    [Ast0.wrap(Ast0.MetaParamList(clt2mcode nm (startofs($1)) clt))] }
| decl TComma decl_list_start
    { $1::Ast0.wrap(Ast0.PComma(clt2mcode "," (startofs($2)) $2))::$3 }
| TMetaParamList TComma decl_list_start
    { let (nm,clt) = $1 in
    Ast0.wrap(Ast0.MetaParamList(clt2mcode nm (startofs($1)) clt))::
    Ast0.wrap(Ast0.PComma(clt2mcode "," (startofs($2)) $2))::$3 }
| TEllipsis list(comma_decls(TEllipsis))
    { Ast0.wrap(Ast0.Pdots(clt2mcode "..." (startofs($1)) $1))::
      (List.concat(List.map (function x -> x (mkpdots "...")) $2)) }
| TCircles list(comma_decls(TCircles))
    { Ast0.wrap(Ast0.Pdots(clt2mcode "ooo" (startofs($1)) $1))::
      (List.concat(List.map (function x -> x (mkpdots "ooo")) $2)) }

comma_decls(dotter):
  TComma dotter
    { function dot_builder ->
      [Ast0.wrap(Ast0.PComma(clt2mcode "," (startofs($1)) $1));
	dot_builder (startofs($2)) $2] }
| TComma decl
    { function dot_builder ->
      [Ast0.wrap(Ast0.PComma(clt2mcode "," (startofs($1)) $1)); $2] }
| TComma TMetaParamList
    { function dot_builder ->
      let (nm,clt) = $2 in
      [Ast0.wrap(Ast0.PComma(clt2mcode "," (startofs($1)) $1));
	Ast0.wrap(Ast0.MetaParamList(clt2mcode nm (startofs($2)) clt))] }

/* must be a list of declarations or statements, with no ... or expressions
for "and" case */
pure_decl_statement_list:
    nonempty_list(decl_statement)           { List.concat $1 }

/* as above, but allows a single expression - for "or" case */
exp_decl_statement_list:
    expr                                    { [Ast0.wrap(Ast0.Exp($1))] }
  | pure_decl_statement_list                { $1 }

fun_exp_decl_statement_list:
    expr                 { [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1))))] }
  | f=nonempty_list(fun_decl_statement)        { List.concat f }

%inline fun_decl_statement:
    d=decl_statement { List.map (function x -> Ast0.wrap(Ast0.OTHER x)) d }
  | f=fundecl        { [Ast0.wrap(Ast0.FUNCTION(f))] }

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
		      fun_exp_decl_statement_list)
    { List.concat
	($1 (function x -> function y ->
	      [Ast0.wrap(Ast0.OTHER (mkdots x y))])) }

plus_function_decl_statement_or_expression: /* does allow just ... */
    first=fun_exp_decl_statement_list { first }
  | first=loption(fun_exp_decl_statement_list)
      second=required_dot_start_with_ender(fun_exp_decl_statement_list,
				    pre_post_decl_statement_or_expression,
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
		    exp_decl_statement_list)
  { top_dots(List.concat ($1 (function x -> function y -> [mkdots x y]))) }

/* a mix of declarations, statements and expressions.  an expression must
be surrounded by ... */

pre_post_decl_statement_and_expression:
    first=pure_decl_statement_list { top_dots first }
  | first=loption(pure_decl_statement_list)
      second=required_dot_start_with_ender(exp_decl_statement_list,
				    pre_post_decl_statement_or_expression,
				    pure_decl_statement_list)
      { top_dots
	  (List.concat
	     (first::(second (function x -> function y -> [mkdots x y])))) }

pre_post_decl_statement_and_expression_opt:
    /* empty */                             { Ast0.wrap(Ast0.DOTS([])) }
  | pre_post_decl_statement_and_expression  { $1 }

pre_post_decl_statement_and_expression_opt_mid:
    pre_post_decl_statement_and_expression       { [$1] }
  | /* empty */                                  { [Ast0.wrap(Ast0.DOTS([]))] }
  | pre_post_decl_statement_and_expression TMid0
      pre_post_decl_statement_and_expression_opt_mid { $1::$3 }
  | TMid0
      pre_post_decl_statement_and_expression_opt_mid
      { Ast0.wrap(Ast0.DOTS([]))::$2 }

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

eexpr_list_start:
    dexpr
      { [$1] }
  | TMetaExpList
      { let (nm,clt) = $1 in
      [Ast0.wrap(Ast0.MetaExprList(clt2mcode nm (startofs($1)) clt))] }
  | dexpr TComma eexpr_list_start
      { $1::Ast0.wrap(Ast0.EComma(clt2mcode "," (startofs($2)) $2))::$3 }
  | TMetaExpList TComma eexpr_list_start
      { let (nm,clt) = $1 in
      Ast0.wrap(Ast0.MetaExprList(clt2mcode nm (startofs($1)) clt))::
      Ast0.wrap(Ast0.EComma(clt2mcode "," (startofs($2)) $2))::$3 }
  | d=dots_when(TEllipsis,eexpr) r=list(comma_args(dots_when(TEllipsis,eexpr)))
      { (mkedots "..." d)::
	(List.concat (List.map (function x -> x (mkedots "...")) r)) }
  | d=dots_when(TCircles,eexpr) r=list(comma_args(dots_when(TCircles,eexpr)))
      { (mkedots "ooo" d)::
	(List.concat (List.map (function x -> x (mkedots "ooo")) r)) }
  | d=dots_when(TStars,eexpr) r=list(comma_args(dots_when(TStars,eexpr)))
      { (mkedots "***" d)::
	(List.concat (List.map (function x -> x (mkedots "***")) r)) }

comma_args(dotter):
  c=TComma d=dotter
    { function dot_builder ->
      [Ast0.wrap(Ast0.EComma(clt2mcode "," (startofs(c)) c)); dot_builder d] }
| TComma dexpr
    { function dot_builder ->
      [Ast0.wrap(Ast0.EComma(clt2mcode "," (startofs($1)) $1)); $2] }
| TComma TMetaExpList
    { function dot_builder ->
      let (nm,clt) = $2 in
      [Ast0.wrap(Ast0.EComma(clt2mcode "," (startofs($1)) $1));
	Ast0.wrap(Ast0.MetaExprList(clt2mcode nm (startofs($2)) clt))] }

eexpr_list_option: eexpr_list { $1 }
         | /* empty */     { Ast0.wrap(Ast0.DOTS([])) }

/****************************************************************************/

// non-empty lists - drop separator
comma_list(elem):
  separated_nonempty_list(TComma,elem) { $1 }

mid_list(elem):
  separated_nonempty_list(TMid,elem) { $1 }

midzero_list(elem):
  separated_nonempty_list(TMid0,elem) { $1 }

// SEQ1
// at least one instance of grammar/ender

opt_dot_start_end(grammar,when_grammar,ender):
   start=ender { function dot_builder -> [start] }
 | r=opt_dot_start_end_pattern(grammar,dots_when(TEllipsis,when_grammar),
     ender,opt_dot_end_ellipsis(grammar,when_grammar,ender))
   { function dot_builder -> r (dot_builder "...") }
 | r=opt_dot_start_end_pattern(grammar,dots_when(TCircles,when_grammar),
     ender,opt_dot_end_circles(grammar,when_grammar,ender))
   { function dot_builder -> r (dot_builder "ooo") }
 | r=opt_dot_start_end_pattern(grammar,dots_when(TStars,when_grammar),
     ender,opt_dot_end_stars(grammar,when_grammar,ender))
   { function dot_builder -> r (dot_builder "***") }

%inline opt_dot_start_end_pattern(grammar,dotter,ender,continue):
   g=grammar d=dotter
     { function dot_builder -> [g; (dot_builder d)] }
 | g=grammar d=dotter c=continue
     { function dot_builder -> g :: (dot_builder d) :: (c dot_builder) }
 | d=dotter c=continue // continue is never empty
     { function dot_builder -> (dot_builder d) :: (c dot_builder) }

opt_dot_end_ellipsis(grammar,when_grammar,ender):
   g=ender { function dot_builder -> [g] }
 | g=grammar d=dots_when(TEllipsis,when_grammar)
     { function dot_builder -> [g ; dot_builder d ] }
 | g=grammar d=dots_when(TEllipsis,when_grammar)
     r=opt_dot_end_ellipsis(grammar,when_grammar,ender)
     { function dot_builder -> g :: (dot_builder d) :: (r dot_builder) }

opt_dot_end_circles(grammar,when_grammar,ender):
   g=ender { function dot_builder -> [g] }
 | g=grammar d=dots_when(TCircles,when_grammar)
     { function dot_builder -> [g ; dot_builder d ] }
 | g=grammar d=dots_when(TCircles,when_grammar)
     r=opt_dot_end_circles(grammar,when_grammar,ender)
     { function dot_builder -> g :: (dot_builder d) :: (r dot_builder) }

opt_dot_end_stars(grammar,when_grammar,ender):
   g=ender { function dot_builder -> [g] }
 | g=grammar d=dots_when(TStars,when_grammar)
     { function dot_builder -> [g ; dot_builder d ] }
 | g=grammar d=dots_when(TStars,when_grammar)
     r=opt_dot_end_stars(grammar,when_grammar,ender)
     { function dot_builder -> g :: (dot_builder d) :: (r dot_builder) }

// SEQ2, ender optional
%inline required_dot_start_with_ender(grammar,when_grammar,ender):
 | start=dots_when(TEllipsis,when_grammar)
     finish=no_dot_start_ellipsis(grammar,when_grammar,ender)
   { (function dot_builder ->
       (dot_builder "..." start) :: (finish (dot_builder "..."))) }
 | start=dots_when(TCircles,when_grammar)
     finish=no_dot_start_circles(grammar,when_grammar,ender)
   { (function dot_builder ->
       (dot_builder "ooo" start) :: (finish (dot_builder "ooo"))) }
 | start=dots_when(TStars,when_grammar)
     finish=no_dot_start_stars(grammar,when_grammar,ender)
   { (function dot_builder ->
       (dot_builder "***" start) :: (finish (dot_builder "***"))) }

no_dot_start_ellipsis(grammar,when_grammar,ender):
   /* empty */    { function dot_builder -> [] }
 | e=ender
       { function dot_builder -> [e] }
 | g=grammar d=dots_when(TEllipsis,when_grammar) 
       r=no_dot_start_ellipsis(grammar,when_grammar,ender)
       { function dot_builder -> g::(dot_builder d)::(r dot_builder) }

no_dot_start_circles(grammar,when_grammar,ender):
       { function dot_builder -> [] }
 | e=ender
       { function dot_builder -> [e] }
 | g=grammar d=dots_when(TCircles,when_grammar) 
       r=no_dot_start_circles(grammar,when_grammar,ender)
       { function dot_builder -> g::(dot_builder d)::(r dot_builder) }

no_dot_start_stars(grammar,when_grammar,ender):
       { function dot_builder -> [] }
 | e=ender
       { function dot_builder -> [e] }
 | g=grammar d=dots_when(TStars,when_grammar) 
       r=no_dot_start_stars(grammar,when_grammar,ender)
       { function dot_builder -> g::(dot_builder d)::(r dot_builder) }

%inline dots_when(dotter,when_grammar):
    d=dotter                                      { (d,startofs(d),None) }
  | d=dotter TWhen TNotEq w=when_grammar TLineEnd { (d,startofs(d),Some w) }

// used in NEST
%inline no_dot_start_end(grammar,dotter):
  g=grammar dg=list(pair(dotter,grammar))
  { function dot_builder ->
      g :: (List.concat(List.map (function (d,g) -> [dot_builder d;g]) dg)) }

/*****************************************************************************
*
*
*****************************************************************************/

iso_main:
  TIsoExpression e1=dexpr el=list(iso(dexpr)) EOF
  { List.map (function x -> Ast0.ExprTag x) (e1::el) }
| TIsoStatement s1=single_statement sl=list(iso(single_statement)) EOF
    { List.map (function x -> Ast0.StmtTag x) (s1::sl) }
| TIsoDeclaration d1=decl_var dl=list(iso(decl_var)) EOF
    { let check_one = function
	[x] -> x
      | _ ->
	  failwith "only one variable per delaration in an isomorphism rule" in
    let res = List.map check_one (d1::dl) in
    List.map (function x -> Ast0.DeclTag x) res }

%inline iso(term):
    TIso t=term { t }
