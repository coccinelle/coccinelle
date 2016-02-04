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

let make_info line logical_line =
  { Ast.line = line; Ast.logical_line = logical_line }

let clt2info (_,line,logical_line) = make_info line logical_line

let clt2mcode str = function
    (Data.MINUS,line,lline)       ->
      (str,Ast0.NONE,
       (Ast.MINUS({Ast.line=line;Ast.logical_line=lline},ref[])))
  | (Data.OPTMINUS,line,lline)    ->
      (str,Ast0.OPT,
       (Ast.MINUS({Ast.line=line;Ast.logical_line=lline},ref[])))
  | (Data.UNIQUEMINUS,line,lline) ->
      (str,Ast0.UNIQUE,
       (Ast.MINUS({Ast.line=line;Ast.logical_line=lline},ref[])))
  | (Data.MULTIMINUS,line,lline) ->
      (str,Ast0.MULTI,
       (Ast.MINUS({Ast.line=line;Ast.logical_line=lline},ref[])))
  | (Data.PLUS,line,lline)        ->
      (str,Ast0.NONE,Ast.PLUS({Ast.line=line;Ast.logical_line=lline}))
  | (Data.CONTEXT,line,lline)     ->
      (str,Ast0.NONE,
       Ast.CONTEXT({Ast.line=line;Ast.logical_line=lline},ref Ast.NOTHING))
  | (Data.OPT,line,lline)         ->
      (str,Ast0.OPT,
       Ast.CONTEXT({Ast.line=line;Ast.logical_line=lline},ref Ast.NOTHING))
  | (Data.UNIQUE,line,lline)      ->
      (str,Ast0.UNIQUE,
       Ast.CONTEXT({Ast.line=line;Ast.logical_line=lline},ref Ast.NOTHING))
  | (Data.MULTI,line,lline)      ->
      (str,Ast0.MULTI,
       Ast.CONTEXT({Ast.line=line;Ast.logical_line=lline},ref Ast.NOTHING))

let id2name   (name, clt) = name
let id2clt    (name, clt) = clt
let id2info   (name, clt) = clt2info clt
let id2mcode  (name, clt) = clt2mcode name clt

let arith_op ast_op left op right =
  Ast0.Binary(left, clt2mcode (Ast.Arith ast_op) op, right)

let logic_op ast_op left op right =
  Ast0.Binary(left, clt2mcode (Ast.Logical ast_op) op, right)

let top_dots l =
  if List.exists (function Ast0.Circles(_) -> true | _ -> false) l
  then Ast0.CIRCLES(l)
  else if List.exists (function Ast0.Stars(_) -> true | _ -> false) l
  then Ast0.STARS(l)
  else Ast0.DOTS(l)

%}


%token EOF

%token TIdentifier TExpression TStatement TFunction TLocal TType TParameter
%token TWhy0 TPlus0 TBang0 Tlist TFresh TConstant TError TWords

%token<Data.line_type * int * int> Tchar Tshort Tint Tdouble Tfloat Tlong Tvoid
%token<Data.line_type * int * int> Tstruct Tunion
%token<Data.line_type * int * int> Tunsigned Tsigned

%token<Data.line_type * int * int> Tstatic Tconst Tvolatile

%token <Data.line_type * int * int> TIf TElse TWhile TFor TDo TReturn
%token <string * (Data.line_type * int * int)> TIdent TFunName TMetaFunName
%token <string * (Data.line_type * int * int)> TMetaId TMetaType TMetaErr
%token <string * (Data.line_type * int * int)> TMetaParam TMetaParamList
%token <string * (Data.line_type * int * int)> TMetaStm TMetaStmList TMetaFunc
%token <string * (Data.line_type * int * int)> TMetaLocalFunc TMetaExpList
%token <string * Ast0_cocci.fullType list option * (Data.line_type*int*int)>
TMetaExp TMetaConst
%token TArobArob

%token <Data.line_type * int * int> TEllipsis TOEllipsis TCEllipsis
%token <Data.line_type * int * int> TWhen
%token TLineEnd
%token <Data.line_type * int * int> TCircles TOCircles TCCircles
%token <Data.line_type * int * int> TStars TOStars TCStars

%token <Data.line_type * int * int> TWhy TDotDot TBang TOPar TOPar0 TMid
%token <Data.line_type * int * int> TMid0 TCPar TCPar0

%token <string * (Data.line_type * int * int)> TInclude TMinusFile TPlusFile

%token <Data.line_type * int * int> TInc TDec

%token <string * (Data.line_type * int * int)> TString TChar TFloat TInt

%token <Data.line_type * int * int> TOrLog
%token <Data.line_type * int * int> TAndLog
%token <Data.line_type * int * int> TOr
%token <Data.line_type * int * int> TXor
%token <Data.line_type * int * int> TAnd
%token <Data.line_type * int * int> TEqEq TNotEq
%token <Data.line_type * int * int> TInf TSup TInfEq TSupEq
%token <Data.line_type * int * int> TShl TShr
%token <Data.line_type * int * int> TPlus TMinus
%token <Data.line_type * int * int> TMul TDiv TMod

%token <Data.line_type * int * int> TOBrace TCBrace
%token <Data.line_type * int * int> TOCro TCCro

%token <Data.line_type * int * int> TPtrOp

%token <Data.line_type * int * int> TEq TDot TComma TPtVirg
%token <Ast_cocci.assignOp * (Data.line_type * int * int)> TAssign

/* operator precedence */
%nonassoc 	TIf
%nonassoc 	TElse

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

%start main
%type <Ast0_cocci.rule> main

%start meta_main
%type <Ast_cocci.metavar list> meta_main



%%

main: body EOF { $1 } | body TArobArob { $1 }
meta_main: meta_var_list_opt TArobArob { $1 }

/*****************************************************************************
*
*
*****************************************************************************/

meta_var:
  arity TIdentifier pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_id_meta name;
	  Ast.MetaIdDecl($1,name))
	$3 }
| arity TFresh TIdentifier pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_id_meta name;
	  Ast.MetaFreshIdDecl($1,name))
	$4 }
| arity TType pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_type_meta name;
	  Ast.MetaTypeDecl($1,name))
	$3 }
| arity TParameter pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_param_meta name;
	  Ast.MetaParamDecl($1,name))
	$3 }
| arity TParameter Tlist pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_paramlist_meta name;
	  Ast.MetaParamListDecl($1,name))
	$4 }
| arity TError pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_err_meta name;
	  Ast.MetaErrDecl($1,name))
	$3 }
| arity TExpression pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_exp_meta None name;
	  Ast.MetaExpDecl($1,name))
	$3 }
| arity TExpression Tlist pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_explist_meta name;
	  Ast.MetaExpListDecl($1,name))
	$4 }
| arity TStatement pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_stm_meta name;
	  Ast.MetaStmDecl($1,name))
	$3 }
| arity TStatement Tlist pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_stmlist_meta name;
	  Ast.MetaStmListDecl($1,name))
	$4 }
| arity TFunction pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_func_meta name;
	  Ast.MetaFuncDecl($1,name))
	$3 }
| arity TLocal TFunction pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_local_func_meta name;
	  Ast.MetaLocalFuncDecl($1,name))
	$4 }
| arity meta_exp_type pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_exp_meta (Some $2) name;
	  Ast.MetaExpDecl($1,name))
	$3 }
| arity TConstant meta_exp_type pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_const_meta (Some $3) name;
	  Ast.MetaConstDecl($1,name))
	$4 }
| arity TConstant pure_ident_or_meta_ident_list TPtVirg
    { List.map
	(function name ->
	  !Data.add_const_meta None name;
	  Ast.MetaConstDecl($1,name))
	$3 }

meta_exp_type:
  ctype                      { [$1] }
| TOBrace ctype_list TCBrace { $2 }

arity: TBang0 { Ast.UNIQUE }
     | TWhy0  { Ast.OPT }
     | TPlus0 { Ast.MULTI }
     | /* empty */ { Ast.NONE }

ctype: Tvoid
         { Ast0.BaseType(clt2mcode Ast.VoidType $1, None) }
     | ctype_qualif Tchar
         { Ast0.BaseType(clt2mcode Ast.CharType $2, $1) }
     | ctype_qualif Tshort
         { Ast0.BaseType(clt2mcode Ast.ShortType $2, $1) }
     | ctype_qualif Tint
         { Ast0.BaseType(clt2mcode Ast.IntType $2, $1) }
     | Tdouble
         { Ast0.BaseType(clt2mcode Ast.DoubleType $1, None) }
     | Tfloat
         { Ast0.BaseType(clt2mcode Ast.FloatType $1, None) }
     | ctype_qualif Tlong
         { Ast0.BaseType(clt2mcode Ast.LongType $2, $1) }
     | Tstruct pure_ident
	 { Ast0.StructUnionName(id2mcode $2,clt2mcode Ast.Struct $1) }
     | Tunion pure_ident
	 { Ast0.StructUnionName(id2mcode $2,clt2mcode Ast.Union $1) }
     | ctype TMul
         { Ast0.Pointer($1,clt2mcode "*" $2) }
     | TMetaType
	 { let (nm,clt) = $1 in Ast0.MetaType(clt2mcode nm clt) }

ctype_qualif:
       Tunsigned
         { Some (clt2mcode Ast.Unsigned $1) }
     | Tsigned
         { Some (clt2mcode Ast.Signed $1) }
     | /* empty */        { None }

param_ctype:
       Tvoid
         { Ast0.BaseType(clt2mcode Ast.VoidType $1, None) }
     | ctype_qualif Tchar
         { Ast0.BaseType(clt2mcode Ast.CharType $2, $1) }
     | ctype_qualif Tshort
         { Ast0.BaseType(clt2mcode Ast.ShortType $2, $1) }
     | ctype_qualif Tint
         { Ast0.BaseType(clt2mcode Ast.IntType $2, $1) }
     | Tdouble
         { Ast0.BaseType(clt2mcode Ast.DoubleType $1, None) }
     | Tfloat
         { Ast0.BaseType(clt2mcode Ast.FloatType $1, None) }
     | ctype_qualif Tlong
         { Ast0.BaseType(clt2mcode Ast.LongType $2, $1) }
     | Tstruct pure_ident
	 { Ast0.StructUnionName(id2mcode $2,clt2mcode Ast.Struct $1) }
     | Tunion pure_ident
	 { Ast0.StructUnionName(id2mcode $2,clt2mcode Ast.Union $1) }
     | pure_ident         { Ast0.TypeName(id2mcode $1) }
     | param_ctype TMul   { Ast0.Pointer($1,clt2mcode "*" $2) }
     | TMetaType
	 { let (nm,clt) = $1 in Ast0.MetaType(clt2mcode nm clt) }

/*****************************************************************************/

/* have to inline everything to avoid conflicts? switch to proper
declarations, statements, and expressions for the subterms */

body: function_decl_statement_or_expression      { Top_level.top_level $1 }
    | /* empty */                                { [] }

/*****************************************************************************/

fundecl:
  storage TFunName TOPar decl_list TCPar
  TOBrace pre_post_decl_statement_and_expression_opt TCBrace
      { Ast0.FunDecl($1, Ast0.Id(id2mcode $2), clt2mcode "(" $3, $4,
		     clt2mcode ")" $5, clt2mcode "{" $6, $7,
		     clt2mcode "}" $8) }
| storage TMetaFunName TOPar decl_list TCPar
  TOBrace pre_post_decl_statement_and_expression_opt TCBrace
      { Ast0.FunDecl($1, Ast0.MetaFunc(id2mcode $2), clt2mcode "(" $3, $4,
		     clt2mcode ")" $5, clt2mcode "{" $6, $7,
		     clt2mcode "}" $8) }

storage: Tstatic      { Some (clt2mcode Ast.Static $1) }
       | /* empty */  { None }

decl: decl_qualif param_ctype ident
	{ Ast0.Param($3, $1, $2) }
    | TMetaParam
	{ let (nm,clt) = $1 in Ast0.MetaParam(clt2mcode nm clt) }

decl_qualif:
      Tconst       { Some (clt2mcode Ast.Const $1) }
    | Tvolatile    { Some (clt2mcode Ast.Volatile $1) }
    | /* empty */  { None }

/*****************************************************************************/

statement:
  TMetaStm
    { let (nm,clt) = $1 in Ast0.MetaStmt(clt2mcode nm clt) }
| expr TPtVirg
    { Ast0.ExprStatement ($1, clt2mcode ";" $2) }
| TIf TOPar eexpr TCPar single_statement %prec TIf
    { Ast0.IfThen(clt2mcode "if" $1,
		  clt2mcode "(" $2,$3,clt2mcode ")" $4,$5) }
| TIf TOPar eexpr TCPar single_statement TElse single_statement
    { Ast0.IfThenElse(clt2mcode "if" $1,
		      clt2mcode "(" $2,$3,clt2mcode ")" $4,$5,
		      clt2mcode "else" $6,$7) }
| TFor TOPar eexpr_opt TPtVirg eexpr_opt TPtVirg eexpr_opt TCPar
    single_statement
    { Ast0.For(clt2mcode "for" $1,clt2mcode "(" $2,$3,
	       clt2mcode ";" $4,$5,clt2mcode ";" $6,$7,clt2mcode ")" $8,$9) }
| TWhile TOPar eexpr TCPar single_statement
    { Ast0.While(clt2mcode "while" $1,
		 clt2mcode "(" $2,$3,clt2mcode ")" $4,$5) }
| TDo single_statement TWhile TOPar eexpr TCPar TPtVirg
    { Ast0.Do(clt2mcode "do" $1,$2,clt2mcode "while" $3,
	      clt2mcode "(" $4,$5,clt2mcode ")" $6, clt2mcode ";" $7) }
| TReturn eexpr TPtVirg
    { Ast0.ReturnExpr(clt2mcode "return" $1,$2,clt2mcode ";" $3) }
| TReturn TPtVirg
    { Ast0.Return(clt2mcode "return" $1,clt2mcode ";" $2) }
| TOBrace pre_post_decl_statement_and_expression_opt TCBrace
    { Ast0.Seq(clt2mcode "{" $1,$2,clt2mcode "}" $3) }
| TOEllipsis decl_statement_or_expression_dots TCEllipsis
    { Ast0.Nest(Ast0.DOTS($2)) }
| TOCircles decl_statement_or_expression_circles TCCircles
    { Ast0.Nest(Ast0.CIRCLES($2)) }
| TOStars decl_statement_or_expression_stars TCStars
    { Ast0.Nest(Ast0.STARS($2)) }

/* In the following, an identifier as a type is not fully supported.  Indeed,
the language is ambiguous: what is foo * bar; */
decl_var: ctype d_ident_list TPtVirg
      { (List.map
	   (function (id,fn) -> Ast0.UnInit(fn $1,id,clt2mcode ";" $3))
	   $2) }
  | ctype d_ident TEq eexpr TPtVirg
      { let (id,fn) = $2 in
      [Ast0.Init(fn $1,id,clt2mcode "=" $3,$4,clt2mcode ";" $5)] }
  | pure_ident d_ident TPtVirg
      { let (id,fn) = $2 in
      [Ast0.UnInit(fn (Ast0.TypeName(id2mcode $1)),id,clt2mcode ";" $3)] }
  | pure_ident d_ident TEq eexpr TPtVirg
      { let (id,fn) = $2 in
      [Ast0.Init(fn(Ast0.TypeName(id2mcode $1)),id,
		 clt2mcode "=" $3,$4,clt2mcode ";" $5)] }

d_ident:
    ident
      { ($1,function x -> x) }
  | ident TOCro eexpr_opt TCCro
      { ($1,function x -> Ast0.Array(x,clt2mcode "[" $2,$3,clt2mcode "]" $4)) }

/* a statement on its own */
single_statement:
    statement                   { $1 }
  | TOPar0 statement_mid TCPar0
      { Ast0.Disj($2) }

/* a statement that is part of a list */
decl_statement:
    TMetaStmList
      { let (nm,clt) = $1 in [Ast0.MetaStmt(clt2mcode nm clt)] }
  | decl_var
      { List.map (function x -> Ast0.Decl(x)) $1 }
  | statement { [$1] }
  | TOPar0 pre_post_decl_statement_and_expression_opt_mid TCPar0
      { if List.for_all (function Ast0.DOTS([]) -> true | _ -> false) $2
      then []
      else [Ast0.Disj($2)] }

/*****************************************************************************/


/*****************************************************************************/
/* The following cannot contain <... ...> at the top level.  This can only
be allowed as an expression when the expression is delimited on both sides
by expression-specific markers.  In that case, the rule eexpr is used, which
allows <... ...> anywhere.  Hopefully, this will not be too much of a problem
in practice. */

expr: assign_expr                             { $1 }

assign_expr:
    cond_expr                        { $1 }
  | unary_expr TAssign assign_expr
      { let (op,clt) = $2 in Ast0.Assignment($1,clt2mcode op clt,$3) }
  | unary_expr TEq assign_expr
      { Ast0.Assignment($1,clt2mcode Ast.SimpleAssign $2,$3)  }

cond_expr: arith_expr                         { $1 }
	 | arith_expr TWhy eexpr_opt TDotDot cond_expr
	     { Ast0.CondExpr ($1, clt2mcode "?" $2, $3, clt2mcode "?" $4, $5) }

arith_expr: cast_expr                         { $1 }
	  | arith_expr TMul    arith_expr { arith_op Ast.Mul $1 $2 $3 }
	  | arith_expr TDiv    arith_expr { arith_op Ast.Div $1 $2 $3 }
	  | arith_expr TMod    arith_expr { arith_op Ast.Mod $1 $2 $3 }
	  | arith_expr TPlus   arith_expr { arith_op Ast.Plus $1 $2 $3 }
	  | arith_expr TMinus  arith_expr { arith_op Ast.Minus $1 $2 $3 }
	  | arith_expr TShl    arith_expr { arith_op Ast.DecLeft $1 $2 $3 }
	  | arith_expr TShr    arith_expr { arith_op Ast.DecRight $1 $2 $3 }
	  | arith_expr TInf    arith_expr { logic_op Ast.Inf $1 $2 $3 }
	  | arith_expr TSup    arith_expr { logic_op Ast.Sup $1 $2 $3 }
	  | arith_expr TInfEq  arith_expr { logic_op Ast.InfEq $1 $2 $3 }
	  | arith_expr TSupEq  arith_expr { logic_op Ast.SupEq $1 $2 $3 }
	  | arith_expr TEqEq   arith_expr { logic_op Ast.Eq $1 $2 $3 }
	  | arith_expr TNotEq  arith_expr { logic_op Ast.NotEq $1 $2 $3 }
	  | arith_expr TAnd    arith_expr { arith_op Ast.And $1 $2 $3 }
	  | arith_expr TOr     arith_expr { arith_op Ast.Or $1 $2 $3 }
	  | arith_expr TXor    arith_expr { arith_op Ast.Xor $1 $2 $3 }
	  | arith_expr TAndLog arith_expr { logic_op Ast.AndLog $1 $2 $3 }
	  | arith_expr TOrLog  arith_expr { logic_op Ast.OrLog $1 $2 $3 }

cast_expr: unary_expr                      { $1 }
	 | TOPar ctype TCPar cast_expr
	     { Ast0.Cast (clt2mcode "(" $1, $2, clt2mcode ")" $3, $4) }

unary_expr: postfix_expr                   { $1 }
	  | TInc unary_expr
               { Ast0.Infix ($2, clt2mcode Ast.Inc $1) }
	  | TDec unary_expr
               { Ast0.Infix ($2, clt2mcode Ast.Dec $1) }
	  | unary_op unary_expr
               { let mcode = $1 in Ast0.Unary($2, mcode) }

unary_op: TAnd   { clt2mcode Ast.GetRef $1 }
	| TMul   { clt2mcode Ast.DeRef $1 }
	| TPlus  { clt2mcode Ast.UnPlus $1 }
	| TMinus { clt2mcode Ast.UnMinus $1 }
	| TBang  { clt2mcode Ast.Not $1 }

postfix_expr: primary_expr                            { $1 }
	    | postfix_expr TOCro eexpr TCCro
                { Ast0.ArrayAccess ($1,clt2mcode "[" $2,$3,clt2mcode "]" $4) }
	    | postfix_expr TDot   ident
		{ Ast0.RecordAccess($1, clt2mcode "." $2, $3) }
	    | postfix_expr TPtrOp ident
		{ Ast0.RecordPtAccess($1, clt2mcode "->" $2, $3) }
	    | postfix_expr TInc
		{ Ast0.Postfix ($1, clt2mcode Ast.Inc $2) }
	    | postfix_expr TDec
		{ Ast0.Postfix ($1, clt2mcode Ast.Dec $2) }
	    | postfix_expr TOPar eexpr_list_opt TCPar
		{ Ast0.FunCall($1,clt2mcode "(" $2,$3,clt2mcode ")" $4) }

primary_expr: ident   { Ast0.Ident($1) }
            | TInt
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.Int x) clt) }
	    | TFloat
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.Float x) clt) }
	    | TString
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.String x) clt) }
	    | TChar
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.Char x) clt) }
	    | TMetaConst
		{ let (nm,ty,clt) = $1 in Ast0.MetaConst(clt2mcode nm clt,ty) }
	    | TMetaErr
		{ let (nm,clt) = $1 in Ast0.MetaErr(clt2mcode nm clt) }
	    | TMetaExp
		{ let (nm,ty,clt) = $1 in Ast0.MetaExpr(clt2mcode nm clt,ty) }
	    | TOPar eexpr TCPar
		{ Ast0.Paren(clt2mcode "(" $1,$2,clt2mcode ")" $3) }
	    | TOPar0 expr_mid TCPar0 { Ast0.DisjExpr($2) }

/*****************************************************************************/

eexpr: eassign_expr                              { $1 }

eassign_expr: econd_expr                         { $1 }
            | eunary_expr TAssign eassign_expr
		{ let (op,clt) = $2 in
	        Ast0.Assignment($1,clt2mcode op clt,$3) }
	    | eunary_expr TEq eassign_expr
		{ Ast0.Assignment($1,clt2mcode Ast.SimpleAssign $2,$3)  }

econd_expr: earith_expr                          { $1 }
	  | earith_expr TWhy eexpr_opt TDotDot econd_expr
	     { Ast0.CondExpr ($1, clt2mcode "?" $2, $3, clt2mcode "?" $4, $5) }

earith_expr: ecast_expr                         { $1 }
	  | earith_expr TMul    earith_expr { arith_op Ast.Mul $1 $2 $3 }
	  | earith_expr TDiv    earith_expr { arith_op Ast.Div $1 $2 $3 }
	  | earith_expr TMod    earith_expr { arith_op Ast.Mod $1 $2 $3 }
	  | earith_expr TPlus   earith_expr { arith_op Ast.Plus $1 $2 $3 }
	  | earith_expr TMinus  earith_expr { arith_op Ast.Minus $1 $2 $3 }
	  | earith_expr TShl    earith_expr { arith_op Ast.DecLeft $1 $2 $3 }
	  | earith_expr TShr    earith_expr { arith_op Ast.DecRight $1 $2 $3 }
	  | earith_expr TInf    earith_expr { logic_op Ast.Inf $1 $2 $3 }
	  | earith_expr TSup    earith_expr { logic_op Ast.Sup $1 $2 $3 }
	  | earith_expr TInfEq  earith_expr { logic_op Ast.InfEq $1 $2 $3 }
	  | earith_expr TSupEq  earith_expr { logic_op Ast.SupEq $1 $2 $3 }
	  | earith_expr TEqEq   earith_expr { logic_op Ast.Eq $1 $2 $3 }
	  | earith_expr TNotEq  earith_expr { logic_op Ast.NotEq $1 $2 $3 }
	  | earith_expr TAnd    earith_expr { arith_op Ast.And $1 $2 $3 }
	  | earith_expr TOr     earith_expr { arith_op Ast.Or $1 $2 $3 }
	  | earith_expr TXor    earith_expr { arith_op Ast.Xor $1 $2 $3 }
	  | earith_expr TAndLog earith_expr { logic_op Ast.AndLog $1 $2 $3 }
	  | earith_expr TOrLog  earith_expr { logic_op Ast.OrLog $1 $2 $3 }

ecast_expr: eunary_expr                      { $1 }
	 | TOPar ctype TCPar ecast_expr
	     { Ast0.Cast (clt2mcode "(" $1, $2, clt2mcode ")" $3, $4) }

eunary_expr: epostfix_expr                   { $1 }
	   | TInc eunary_expr
               { Ast0.Infix ($2, clt2mcode Ast.Inc $1) }
	   | TDec eunary_expr
               { Ast0.Infix ($2, clt2mcode Ast.Dec $1) }
 	   | unary_op eunary_expr
               { let mcode = $1 in Ast0.Unary($2, mcode) }

epostfix_expr: eprimary_expr                            { $1 }
 	     | epostfix_expr TOCro eexpr TCCro
                 { Ast0.ArrayAccess ($1,clt2mcode "[" $2,$3,clt2mcode "]" $4) }
	     | epostfix_expr TDot   ident
		 { Ast0.RecordAccess($1, clt2mcode "." $2, $3) }
	     | epostfix_expr TPtrOp ident
		 { Ast0.RecordPtAccess($1, clt2mcode "->" $2, $3) }
	     | epostfix_expr TInc
		 { Ast0.Postfix ($1, clt2mcode Ast.Inc $2) }
	     | epostfix_expr TDec
		 { Ast0.Postfix ($1, clt2mcode Ast.Dec $2) }
	     | epostfix_expr TOPar eexpr_list_opt TCPar
		 { Ast0.FunCall($1,clt2mcode "(" $2,$3,clt2mcode ")" $4) }

eprimary_expr: ident  { Ast0.Ident($1) }
            | TEllipsis { Ast0.Edots(clt2mcode "..." $1,None) }
            | TInt
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.Int x) clt) }
	    | TFloat
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.Float x) clt) }
	    | TString
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.String x) clt) }
	    | TChar
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.Char x) clt) }
	    | TMetaConst
		{ let (nm,ty,clt) = $1 in Ast0.MetaConst(clt2mcode nm clt,ty) }
	    | TMetaErr
		{ let (nm,clt) = $1 in Ast0.MetaErr(clt2mcode nm clt) }
	    | TMetaExp
		{ let (nm,ty,clt) = $1 in Ast0.MetaExpr(clt2mcode nm clt,ty) }
	    | TOPar eexpr TCPar
		{ Ast0.Paren(clt2mcode "(" $1,$2,clt2mcode ")" $3) }
	    | TOPar0 eexpr_mid TCPar0
		{ Ast0.DisjExpr($2) }
	    | TOEllipsis expr_dots TCEllipsis
		{ Ast0.NestExpr(Ast0.DOTS($2)) }
	    | TOCircles expr_circles TCCircles
		{ Ast0.NestExpr(Ast0.CIRCLES($2)) }
	    | TOStars expr_stars TCStars
		{ Ast0.NestExpr(Ast0.STARS($2)) }

/*****************************************************************************/

dexpr: dassign_expr                              { $1 }

dassign_expr: dcond_expr                         { $1 }
            | dunary_expr TAssign dassign_expr
		{ let (op,clt) = $2 in
	        Ast0.Assignment($1,clt2mcode op clt,$3) }
	    | dunary_expr TEq dassign_expr
		{ Ast0.Assignment($1,clt2mcode Ast.SimpleAssign $2,$3)  }

dcond_expr: darith_expr                          { $1 }
	  | darith_expr TWhy eexpr_opt TDotDot dcond_expr
	     { Ast0.CondExpr ($1, clt2mcode "?" $2, $3, clt2mcode "?" $4, $5) }

darith_expr: dcast_expr                         { $1 }
	  | darith_expr TMul    darith_expr { arith_op Ast.Mul $1 $2 $3 }
	  | darith_expr TDiv    darith_expr { arith_op Ast.Div $1 $2 $3 }
	  | darith_expr TMod    darith_expr { arith_op Ast.Mod $1 $2 $3 }
	  | darith_expr TPlus   darith_expr { arith_op Ast.Plus $1 $2 $3 }
	  | darith_expr TMinus  darith_expr { arith_op Ast.Minus $1 $2 $3 }
	  | darith_expr TShl    darith_expr { arith_op Ast.DecLeft $1 $2 $3 }
	  | darith_expr TShr    darith_expr { arith_op Ast.DecRight $1 $2 $3 }
	  | darith_expr TInf    darith_expr { logic_op Ast.Inf $1 $2 $3 }
	  | darith_expr TSup    darith_expr { logic_op Ast.Sup $1 $2 $3 }
	  | darith_expr TInfEq  darith_expr { logic_op Ast.InfEq $1 $2 $3 }
	  | darith_expr TSupEq  darith_expr { logic_op Ast.SupEq $1 $2 $3 }
	  | darith_expr TEqEq   darith_expr { logic_op Ast.Eq $1 $2 $3 }
	  | darith_expr TNotEq  darith_expr { logic_op Ast.NotEq $1 $2 $3 }
	  | darith_expr TAnd    darith_expr { arith_op Ast.And $1 $2 $3 }
	  | darith_expr TOr     darith_expr { arith_op Ast.Or $1 $2 $3 }
	  | darith_expr TXor    darith_expr { arith_op Ast.Xor $1 $2 $3 }
	  | darith_expr TAndLog darith_expr { logic_op Ast.AndLog $1 $2 $3 }
	  | darith_expr TOrLog  darith_expr { logic_op Ast.OrLog $1 $2 $3 }

dcast_expr: dunary_expr                      { $1 }
	 | TOPar ctype TCPar dcast_expr
	     { Ast0.Cast (clt2mcode "(" $1, $2, clt2mcode ")" $3, $4) }

dunary_expr: dpostfix_expr                   { $1 }
	   | TInc dunary_expr
               { Ast0.Infix ($2, clt2mcode Ast.Inc $1) }
	   | TDec dunary_expr
               { Ast0.Infix ($2, clt2mcode Ast.Dec $1) }
 	   | unary_op dunary_expr
               { let mcode = $1 in Ast0.Unary($2, mcode) }

dpostfix_expr: dprimary_expr                            { $1 }
 	     | dpostfix_expr TOCro eexpr TCCro
                 { Ast0.ArrayAccess ($1,clt2mcode "[" $2,$3,clt2mcode "]" $4) }
	     | dpostfix_expr TDot   ident
		 { Ast0.RecordAccess($1, clt2mcode "." $2, $3) }
	     | dpostfix_expr TPtrOp ident
		 { Ast0.RecordPtAccess($1, clt2mcode "->" $2, $3) }
	     | dpostfix_expr TInc
		 { Ast0.Postfix ($1, clt2mcode Ast.Inc $2) }
	     | dpostfix_expr TDec
		 { Ast0.Postfix ($1, clt2mcode Ast.Dec $2) }
	     | dpostfix_expr TOPar eexpr_list_opt TCPar
		 { Ast0.FunCall($1,clt2mcode "(" $2,$3,clt2mcode ")" $4) }

dprimary_expr: ident  { Ast0.Ident($1) }
            | TInt
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.Int x) clt) }
	    | TFloat
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.Float x) clt) }
	    | TString
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.String x) clt) }
	    | TChar
		{ let (x,clt) = $1 in
		Ast0.Constant (clt2mcode (Ast.Char x) clt) }
	    | TMetaConst
		{ let (nm,ty,clt) = $1 in Ast0.MetaConst(clt2mcode nm clt,ty) }
	    | TMetaErr
		{ let (nm,clt) = $1 in Ast0.MetaErr(clt2mcode nm clt) }
	    | TMetaExp
		{ let (nm,ty,clt) = $1 in Ast0.MetaExpr(clt2mcode nm clt,ty) }
	    | TOPar eexpr TCPar
		{ Ast0.Paren(clt2mcode "(" $1,$2,clt2mcode ")" $3) }
	    | TOPar0 eexpr_mid TCPar0
		{ Ast0.DisjExpr($2) }
	    | TOEllipsis expr_dots TCEllipsis
		{ Ast0.NestExpr(Ast0.DOTS($2)) }
	    | TOCircles expr_circles TCCircles
		{ Ast0.NestExpr(Ast0.CIRCLES($2)) }
	    | TOStars expr_stars TCStars
		{ Ast0.NestExpr(Ast0.STARS($2)) }

expr_dots:
    dexpr                       { [$1] }
  | dexpr TEllipsis expr_dots
      { $1 :: Ast0.Edots(clt2mcode "..." $2,None) :: $3 }
  | dexpr TEllipsis TWhen TNotEq eexpr TLineEnd expr_dots
      { $1 :: Ast0.Edots(clt2mcode "..." $2,Some $5) :: $7 }

expr_circles:
    dexpr                       { [$1] }
  | dexpr TCircles expr_circles
      { $1 :: Ast0.Ecircles(clt2mcode "ooo" $2,None) :: $3 }
  | dexpr TCircles TWhen TNotEq eexpr TLineEnd expr_dots
      { $1 :: Ast0.Ecircles(clt2mcode "ooo" $2,Some $5) :: $7 }

expr_stars:
    dexpr                       { [$1] }
  | dexpr TStars expr_stars
      { $1 :: Ast0.Estars(clt2mcode "***" $2,None) :: $3 }
  | dexpr TStars TWhen TNotEq eexpr TLineEnd expr_dots
      { $1 :: Ast0.Estars(clt2mcode "***" $2,Some $5) :: $7 }

/*****************************************************************************/

pure_ident: TIdent { $1 }

/* allows redeclaring metavariables.  used in @@ @@ */
pure_ident_or_meta_ident:
       TIdent           { $1 }
     | TMetaId          { $1 }
     | TMetaType        { $1 }
     | TMetaParam       { $1 }
     | TMetaParamList   { $1 }
     | TMetaStm         { $1 }
     | TMetaStmList     { $1 }
     | TMetaFunc        { $1 }
     | TMetaLocalFunc   { $1 }
     | TMetaExpList     { $1 }
     | TMetaConst       { let (name,_,info) = $1 in (name,info) }
     | TMetaExp         { let (name,_,info) = $1 in (name,info) }
     | TMetaErr         { $1 }

ident: TIdent           { Ast0.Id(id2mcode $1) }
     | TMetaId          { Ast0.MetaId(id2mcode $1) }
     | TMetaFunc        { Ast0.MetaFunc(id2mcode $1) }
     | TMetaLocalFunc   { Ast0.MetaLocalFunc(id2mcode $1) }

/*****************************************************************************/

meta_var_list: meta_var               { $1 }
             | meta_var meta_var_list { $1@$2 }

meta_var_list_opt: meta_var_list      { $1 }
                 | /* empty */        { [] }

d_ident_list: d_ident                       { [$1] }
            | d_ident TComma d_ident_list   { $1::$3 }

ctype_list: ctype                     { [$1] }
          | ctype TComma ctype_list   { $1::$3 }

pure_ident_or_meta_ident_list:
     pure_ident_or_meta_ident
       { [id2name $1] }
   | pure_ident_or_meta_ident TComma pure_ident_or_meta_ident_list
       { (id2name $1)::$3 }

decl_list:
   decl_list_start
     { if List.exists (function Ast0.Pcircles(_) -> true | _ -> false) $1
     then Ast0.CIRCLES($1)
     else Ast0.DOTS($1) }

decl_list_start:
	   decl                              { [$1] }
         | TMetaParamList
             { let (nm,clt) = $1 in [Ast0.MetaParamList(clt2mcode nm clt)] }
         | TEllipsis
             { [Ast0.Pdots(clt2mcode "..." $1)] }
         | TCircles
             { [Ast0.Pcircles(clt2mcode "ooo" $1)] }
         | decl TComma decl_list_start
	     { $1::Ast0.PComma(clt2mcode "," $2)::$3 }
         | TMetaParamList TComma decl_list_start
             { let (nm,clt) = $1 in
	     Ast0.MetaParamList(clt2mcode nm clt)::
	     Ast0.PComma(clt2mcode "," $2)::$3 }
         | TEllipsis TComma decl_list_dots
	     { Ast0.Pdots(clt2mcode "..." $1)::
	       Ast0.PComma(clt2mcode "," $2)::
	       $3 }
         | TCircles TComma decl_list_circles
	     { Ast0.Pcircles(clt2mcode "ooo" $1)::
	       Ast0.PComma(clt2mcode "," $2)::
	       $3 }

decl_list_dots:
	   decl                              { [$1] }
         | TMetaParamList
             { let (nm,clt) = $1 in [Ast0.MetaParamList(clt2mcode nm clt)] }
         | TEllipsis
             { [Ast0.Pdots(clt2mcode "..." $1)] }
         | decl TComma decl_list_dots
	     { $1::Ast0.PComma(clt2mcode "," $2)::$3 }
         | TMetaParamList TComma decl_list_dots
             { let (nm,clt) = $1 in
	     Ast0.MetaParamList(clt2mcode nm clt)::
	     Ast0.PComma(clt2mcode "," $2)::$3 }
         | TEllipsis TComma decl_list_dots
	     { Ast0.Pdots(clt2mcode "..." $1)::Ast0.PComma(clt2mcode "," $2)::
	       $3 }

decl_list_circles:
	   decl                              { [$1] }
         | TMetaParamList
             { let (nm,clt) = $1 in [Ast0.MetaParamList(clt2mcode nm clt)] }
         | TCircles
             { [Ast0.Pcircles(clt2mcode "ooo" $1)] }
         | decl TComma decl_list_circles
	     { $1::Ast0.PComma(clt2mcode "," $2)::$3 }
         | TMetaParamList TComma decl_list_circles
	     { let (nm,clt) = $1 in
	     Ast0.MetaParamList(clt2mcode nm clt)::
	     Ast0.PComma(clt2mcode "," $2)::$3 }
         | TCircles TComma decl_list_circles
	     { Ast0.Pcircles(clt2mcode "ooo" $1)::
	       Ast0.PComma(clt2mcode "," $2)::
	       $3 }

/* must be a single statement */
statement_mid:
    statement                               { [Ast0.DOTS([$1])] }
  | statement TMid0 statement_mid           { Ast0.DOTS([$1])::$3 }

/* must be a list of declarations or statements, with no ... or expressions
for "and" case */
pure_decl_statement_list:
    decl_statement                          { $1 }
  | decl_statement pure_decl_statement_list { $1@$2 }

/* as above, but allows a single expression - for "or" case */
exp_decl_statement_list:
    expr                                    { [Ast0.Exp($1)] }
  | decl_statement                          { $1 }
  | decl_statement pure_decl_statement_list { $1@$2 }

fun_exp_decl_statement_list:
    expr
      { [Ast0.OTHER(Ast0.Exp($1))] }
  | decl_statement
      { List.map (function x -> Ast0.OTHER x) $1 }
  | fundecl
      { [Ast0.FUNCTION($1)] }
  | TInclude
      { [Ast0.INCLUDE(clt2mcode "#include" (id2clt $1),id2mcode $1)] }
  | TMinusFile TPlusFile
      { [Ast0.FILEINFO(id2mcode $1,id2mcode $2)] }
  | decl_statement fun_exp_decl_statement_list
      { (List.map (function x -> Ast0.OTHER x) $1)@$2 }
  | fundecl fun_exp_decl_statement_list
      { Ast0.FUNCTION($1)::$2 }
  | TInclude fun_exp_decl_statement_list
      { Ast0.INCLUDE(clt2mcode "#include" (id2clt $1),id2mcode $1)::$2 }
  | TMinusFile TPlusFile fun_exp_decl_statement_list
      { Ast0.FILEINFO(id2mcode $1,id2mcode $2)::$3 }

/* ---------------------------------------------------------------------- */

error_words:
    TError TWords TEq TOCro dotless_eexpr_list TCCro
      { Ast0.ERRORWORDS($5) }

/* ---------------------------------------------------------------------- */
/* sequences of statements and expressions */

/* a mix of declarations, statements and expressions.  an expression may
appear by itself.  always nonempty and cannot just be dots. */

function_decl_statement_or_expression:
    error_words /* only at the end */
      { [$1] }
  | fun_exp_decl_statement_list
      { $1 }
  | fun_exp_decl_statement_list TEllipsis
      function_decl_statement_or_expression_dots
      { $1@Ast0.OTHER(Ast0.Dots(clt2mcode "..." $2,None))::$3 }
  | TEllipsis function_decl_statement_or_expression_dots
      { Ast0.OTHER(Ast0.Dots(clt2mcode "..." $1,None))::$2 }
  | fun_exp_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      function_decl_statement_or_expression_dots
      { $1@Ast0.OTHER(Ast0.Dots(clt2mcode "..." $2,Some $5))::$7 }
  | TEllipsis TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      function_decl_statement_or_expression_dots
      { Ast0.OTHER(Ast0.Dots(clt2mcode "..." $2,Some $4))::$6 }
  | fun_exp_decl_statement_list TCircles
      function_decl_statement_or_expression_circles
      { $1@Ast0.OTHER(Ast0.Circles(clt2mcode "ooo" $2,None))::$3 }
  | TCircles function_decl_statement_or_expression_circles
      { Ast0.OTHER(Ast0.Circles(clt2mcode "ooo" $1,None))::$2 }
  | fun_exp_decl_statement_list TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      function_decl_statement_or_expression_circles
      { $1@Ast0.OTHER(Ast0.Circles(clt2mcode "ooo" $2,Some $5))::$7 }
  | TCircles TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      function_decl_statement_or_expression_circles
      { Ast0.OTHER(Ast0.Circles(clt2mcode "ooo" $1,Some $4))::$6 }
  | fun_exp_decl_statement_list TStars
      function_decl_statement_or_expression_stars
      { $1@Ast0.OTHER(Ast0.Stars(clt2mcode "***" $2,None))::$3 }
  | TStars function_decl_statement_or_expression_stars
      { Ast0.OTHER(Ast0.Stars(clt2mcode "***" $1,None))::$2 }
  | fun_exp_decl_statement_list TStars
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      function_decl_statement_or_expression_stars
      { $1@Ast0.OTHER(Ast0.Stars(clt2mcode "***" $2,Some $5))::$7 }
  | TStars TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      function_decl_statement_or_expression_stars
      { Ast0.OTHER(Ast0.Stars(clt2mcode "***" $1,Some $4))::$6 }

function_decl_statement_or_expression_dots:
    /* empty */ { [] }
  | fun_exp_decl_statement_list
      { $1 }
  | fun_exp_decl_statement_list TEllipsis
      function_decl_statement_or_expression_dots
      { $1@Ast0.OTHER(Ast0.Dots(clt2mcode "..." $2,None))::$3 }
  | fun_exp_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      function_decl_statement_or_expression_dots
      { $1@Ast0.OTHER(Ast0.Dots(clt2mcode "..." $2,Some $5))::$7 }

function_decl_statement_or_expression_circles:
    /* empty */ { [] }
  | fun_exp_decl_statement_list
      { $1 }
  | fun_exp_decl_statement_list TCircles
      function_decl_statement_or_expression_circles
      { $1@Ast0.OTHER(Ast0.Circles(clt2mcode "ooo" $2,None))::$3 }
  | fun_exp_decl_statement_list TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      function_decl_statement_or_expression_circles
      { $1@Ast0.OTHER(Ast0.Circles(clt2mcode "ooo" $2,Some $5))::$7 }

function_decl_statement_or_expression_stars:
    /* empty */ { [] }
  | fun_exp_decl_statement_list
      { $1 }
  | fun_exp_decl_statement_list TStars
      function_decl_statement_or_expression_stars
      { $1@Ast0.OTHER(Ast0.Stars(clt2mcode "***" $2,None))::$3 }
  | fun_exp_decl_statement_list TStars
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      function_decl_statement_or_expression_stars
      { $1@Ast0.OTHER(Ast0.Stars(clt2mcode "***" $2,Some $5))::$7 }

decl_statement_or_expression_dots:
    exp_decl_statement_list
      { $1 }
  | exp_decl_statement_list TEllipsis decl_statement_or_expression_dots
      { $1@Ast0.Dots(clt2mcode "..." $2,None)::$3 }
  | exp_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      decl_statement_or_expression_dots
      { $1@Ast0.Dots(clt2mcode "..." $2,Some $5)::$7 }

decl_statement_or_expression_circles:
    exp_decl_statement_list
      { $1 }
  | exp_decl_statement_list TCircles decl_statement_or_expression_circles
      { $1@Ast0.Dots(clt2mcode "..." $2,None)::$3 }
  | exp_decl_statement_list TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      decl_statement_or_expression_circles
      { $1@Ast0.Dots(clt2mcode "..." $2,Some $5)::$7 }

decl_statement_or_expression_stars:
    exp_decl_statement_list
      { $1 }
  | exp_decl_statement_list TStars decl_statement_or_expression_stars
      { $1@Ast0.Stars(clt2mcode "***" $2,None)::$3 }
  | exp_decl_statement_list TStars
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      decl_statement_or_expression_stars
      { $1@Ast0.Stars(clt2mcode "***" $2,Some $5)::$7 }

post_decl_statement_or_expression:
    exp_decl_statement_list
      { $1 }
  | exp_decl_statement_list TEllipsis
      { $1@[Ast0.Dots(clt2mcode "..." $2,None)] }
  | exp_decl_statement_list TEllipsis
       TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      { $1@[Ast0.Dots(clt2mcode "..." $2,Some $5)] }
  | exp_decl_statement_list TCircles
      { $1@[Ast0.Circles(clt2mcode "ooo" $2,None)] }
  | exp_decl_statement_list TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      { $1@[Ast0.Circles(clt2mcode "ooo" $2,Some $5)] }
  | exp_decl_statement_list TStars
      { $1@[Ast0.Stars(clt2mcode "***" $2,None)] }
  | exp_decl_statement_list TStars
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      { $1@[Ast0.Stars(clt2mcode "***" $2,Some $5)] }
  | exp_decl_statement_list TEllipsis post_decl_statement_or_expression_dots
      { $1@Ast0.Dots(clt2mcode "..." $2,None)::$3 }
  | exp_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_or_expression_dots
      { $1@Ast0.Dots(clt2mcode "..." $2,Some $5)::$7 }
  | exp_decl_statement_list TCircles post_decl_statement_or_expression_dots
      { $1@Ast0.Circles(clt2mcode "ooo" $2,None)::$3 }
  | exp_decl_statement_list TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_or_expression_dots
      { $1@Ast0.Circles(clt2mcode "ooo" $2,Some $5)::$7 }
  | exp_decl_statement_list TStars post_decl_statement_or_expression_dots
      { $1@Ast0.Stars(clt2mcode "***" $2,None)::$3 }
  | exp_decl_statement_list TStars
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_or_expression_dots
      { $1@Ast0.Stars(clt2mcode "***" $2,Some $5)::$7 }

post_decl_statement_or_expression_dots:
    exp_decl_statement_list
      { $1 }
  | exp_decl_statement_list TEllipsis
      { $1@[Ast0.Dots(clt2mcode "..." $2,None)] }
  | exp_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      { $1@[Ast0.Dots(clt2mcode "..." $2,Some $5)] }
  | exp_decl_statement_list TEllipsis post_decl_statement_or_expression_dots
      { $1@Ast0.Dots(clt2mcode "..." $2,None)::$3 }
  | exp_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_or_expression_dots
      { $1@Ast0.Dots(clt2mcode "..." $2,Some $5)::$7 }

post_decl_statement_or_expression_circles:
    exp_decl_statement_list
      { $1 }
  | exp_decl_statement_list TCircles
      { $1@[Ast0.Circles(clt2mcode "ooo" $2,None)] }
  | exp_decl_statement_list TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      { $1@[Ast0.Circles(clt2mcode "ooo" $2,Some $5)] }
  | exp_decl_statement_list TCircles post_decl_statement_or_expression_circles
      { $1@Ast0.Circles(clt2mcode "ooo" $2,None)::$3 }
  | exp_decl_statement_list TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_or_expression_circles
      { $1@Ast0.Circles(clt2mcode "ooo" $2,Some $5)::$7 }

post_decl_statement_or_expression_stars:
    exp_decl_statement_list
      { $1 }
  | exp_decl_statement_list TEllipsis
      { $1@[Ast0.Stars(clt2mcode "***" $2,None)] }
  | exp_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      { $1@[Ast0.Stars(clt2mcode "***" $2,Some $5)] }
  | exp_decl_statement_list TEllipsis post_decl_statement_or_expression_stars
      { $1@Ast0.Stars(clt2mcode "***" $2,None)::$3 }
  | exp_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_or_expression_stars
      { $1@Ast0.Stars(clt2mcode "***" $2,Some $5)::$7 }

pre_post_decl_statement_or_expression:
    post_decl_statement_or_expression
     { if List.exists (function Ast0.Circles(_) -> true | _ -> false) $1
     then Ast0.CIRCLES($1)
     else if List.exists (function Ast0.Stars(_) -> true | _ -> false) $1
     then Ast0.STARS($1)
     else Ast0.DOTS($1) }
  | TEllipsis post_decl_statement_or_expression_dots
      { Ast0.DOTS(Ast0.Dots(clt2mcode "..." $1,None)::$2) }
  | TEllipsis TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_or_expression_dots
      { Ast0.DOTS(Ast0.Dots(clt2mcode "..." $1,Some $4)::$6) }
  | TCircles post_decl_statement_or_expression_circles
      { Ast0.CIRCLES(Ast0.Circles(clt2mcode "ooo" $1,None)::$2) }
  | TCircles TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_or_expression_circles
      { Ast0.CIRCLES(Ast0.Circles(clt2mcode "ooo" $1,Some $4)::$6) }
  | TStars post_decl_statement_or_expression_stars
      { Ast0.STARS(Ast0.Stars(clt2mcode "***" $1,None)::$2) }
  | TStars TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_or_expression_stars
      { Ast0.STARS(Ast0.Stars(clt2mcode "***" $1,Some $4)::$6) }

/* a mix of declarations, statements and expressions.  an expression must
be surrounded by ... */

post_decl_statement_and_expression_dots:
    /* empty */              { [] }
  | pure_decl_statement_list { $1 }
  | expr TEllipsis post_decl_statement_and_expression_dots
      { Ast0.Exp($1)::Ast0.Dots(clt2mcode "..." $2,None)::$3 }
  | expr TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_dots
      { Ast0.Exp($1)::Ast0.Dots(clt2mcode "..." $2,Some $5)::$7 }
  | pure_decl_statement_list TEllipsis post_decl_statement_and_expression_dots
      { $1@Ast0.Dots(clt2mcode "..." $2,None)::$3 }
  | pure_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_dots
      { $1@Ast0.Dots(clt2mcode "..." $2,Some $5)::$7 }

post_decl_statement_and_expression_circles:
    /* empty */              { [] }
  | pure_decl_statement_list { $1 }
  | expr TCircles post_decl_statement_and_expression_circles
      { Ast0.Exp($1)::Ast0.Circles(clt2mcode "ooo" $2,None)::$3 }
  | expr TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_circles
      { Ast0.Exp($1)::Ast0.Circles(clt2mcode "ooo" $2,Some $5)::$7 }
  | pure_decl_statement_list TCircles
      post_decl_statement_and_expression_circles
      { $1@Ast0.Circles(clt2mcode "ooo" $2,None)::$3 }
  | pure_decl_statement_list TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_circles
      { $1@Ast0.Circles(clt2mcode "ooo" $2,Some $5)::$7 }

post_decl_statement_and_expression_stars:
    /* empty */              { [] }
  | pure_decl_statement_list { $1 }
  | expr TStars post_decl_statement_and_expression_stars
      { Ast0.Exp($1)::Ast0.Stars(clt2mcode "***" $2,None)::$3 }
  | expr TStars
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_stars
      { Ast0.Exp($1)::Ast0.Stars(clt2mcode "***" $2,Some $5)::$7 }
  | pure_decl_statement_list TStars post_decl_statement_and_expression_stars
      { $1@Ast0.Stars(clt2mcode "***" $2,None)::$3 }
  | pure_decl_statement_list TStars
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_stars
      { $1@Ast0.Stars(clt2mcode "***" $2,Some $5)::$7 }

pre_post_decl_statement_and_expression:
    pure_decl_statement_list
      { top_dots $1 }
  | pure_decl_statement_list TEllipsis post_decl_statement_and_expression_dots
      { Ast0.DOTS($1@Ast0.Dots(clt2mcode "..." $2,None)::$3) }
  | pure_decl_statement_list TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_dots
      { Ast0.DOTS($1@Ast0.Dots(clt2mcode "..." $2,Some $5)::$7) }
  | pure_decl_statement_list TCircles
      post_decl_statement_and_expression_circles
      { Ast0.CIRCLES($1@Ast0.Circles(clt2mcode "ooo" $2,None)::$3) }
  | pure_decl_statement_list TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_circles
      { Ast0.CIRCLES($1@Ast0.Circles(clt2mcode "ooo" $2,Some $5)::$7) }
  | pure_decl_statement_list TStars post_decl_statement_and_expression_stars
      { Ast0.STARS($1@Ast0.Stars(clt2mcode "***" $2,None)::$3) }
  | pure_decl_statement_list TStars
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_stars
      { Ast0.STARS($1@Ast0.Stars(clt2mcode "***" $2,Some $5)::$7) }
  | TEllipsis post_decl_statement_and_expression_dots
      { Ast0.DOTS(Ast0.Dots(clt2mcode "..." $1,None)::$2) }
  | TEllipsis
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_dots
      { Ast0.DOTS(Ast0.Dots(clt2mcode "..." $1,Some $4)::$6) }
  | TCircles post_decl_statement_and_expression_circles
      { Ast0.CIRCLES(Ast0.Circles(clt2mcode "ooo" $1,None)::$2) }
  | TCircles
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_circles
      { Ast0.CIRCLES(Ast0.Circles(clt2mcode "ooo" $1,Some $4)::$6) }
  | TStars post_decl_statement_and_expression_stars
      { Ast0.STARS(Ast0.Stars(clt2mcode "***" $1,None)::$2) }
  | TStars
      TWhen TNotEq pre_post_decl_statement_or_expression TLineEnd
      post_decl_statement_and_expression_stars
      { Ast0.STARS(Ast0.Stars(clt2mcode "***" $1,Some $4)::$6) }

pre_post_decl_statement_and_expression_opt:
    /* empty */                             { Ast0.DOTS([]) }
  | pre_post_decl_statement_and_expression  { $1 }

pre_post_decl_statement_and_expression_opt_mid:
    pre_post_decl_statement_and_expression       { [$1] }
  | /* empty */                                  { [Ast0.DOTS([])] }
  | pre_post_decl_statement_and_expression TMid0
      pre_post_decl_statement_and_expression_opt_mid { $1::$3 }
  | TMid0
      pre_post_decl_statement_and_expression_opt_mid { Ast0.DOTS([])::$2 }

/* ---------------------------------------------------------------------- */

dotless_eexpr_list:
    dexpr
      { [$1] }
  | dexpr TComma dotless_eexpr_list
      { $1::Ast0.EComma(clt2mcode "," $2)::$3 }

eexpr_list:
  eexpr_list_start
     { if List.exists (function Ast0.Ecircles(_) -> true | _ -> false) $1
     then Ast0.CIRCLES($1)
     else if List.exists (function Ast0.Estars(_) -> true | _ -> false) $1
     then Ast0.STARS($1)
     else Ast0.DOTS($1) }

eexpr_list_start:
    dexpr
      { [$1] }
  | TMetaExpList
      { let (nm,clt) = $1 in [Ast0.MetaExprList(clt2mcode nm clt)] }
  | TEllipsis
      { [Ast0.Edots(clt2mcode "..." $1,None)] }
  | TEllipsis TWhen TNotEq eexpr TLineEnd
      { [Ast0.Edots(clt2mcode "..." $1,Some $4)] }
  | TCircles
      { [Ast0.Ecircles(clt2mcode "ooo" $1,None)] }
  | TCircles TWhen TNotEq eexpr TLineEnd
      { [Ast0.Ecircles(clt2mcode "ooo" $1,Some $4)] }
  | TStars
      { [Ast0.Estars(clt2mcode "***" $1,None)] }
  | TStars TWhen TNotEq eexpr TLineEnd
      { [Ast0.Estars(clt2mcode "***" $1,Some $4)] }
  | dexpr TComma eexpr_list_start
      { $1::Ast0.EComma(clt2mcode "," $2)::$3 }
  | TMetaExpList TComma eexpr_list_start
      { let (nm,clt) = $1 in
      Ast0.MetaExprList(clt2mcode nm clt)::Ast0.EComma(clt2mcode "," $2)::$3 }
  | TEllipsis TComma eexpr_list_dots
      { Ast0.Edots(clt2mcode "..." $1,None)::
	Ast0.EComma(clt2mcode "," $2)::$3  }
  | TEllipsis TWhen TNotEq eexpr TLineEnd TComma eexpr_list_dots
      { Ast0.Edots(clt2mcode "..." $1,Some $4)::
	Ast0.EComma(clt2mcode "," $6)::$7  }
  | TCircles TComma eexpr_list_circles
      { Ast0.Ecircles(clt2mcode "ooo" $1,None)::
	Ast0.EComma(clt2mcode "," $2)::$3  }
  | TCircles TWhen TNotEq eexpr TLineEnd TComma eexpr_list_circles
      { Ast0.Ecircles(clt2mcode "ooo" $1,Some $4)::
	Ast0.EComma(clt2mcode "," $6)::$7  }
  | TStars TComma eexpr_list_stars
      { Ast0.Estars(clt2mcode "***" $1,None)::
	Ast0.EComma(clt2mcode "," $2)::$3  }
  | TStars TWhen TNotEq eexpr TLineEnd TComma eexpr_list_stars
      { Ast0.Estars(clt2mcode "***" $1,Some $4)::
	Ast0.EComma(clt2mcode "," $6)::$7  }

eexpr_list_dots:
    dexpr
      { [$1] }
  | TMetaExpList
      { let (nm,clt) = $1 in [Ast0.MetaExprList(clt2mcode nm clt)] }
  | TEllipsis
      { [Ast0.Edots(clt2mcode "..." $1,None)] }
  | TEllipsis TWhen TNotEq eexpr TLineEnd
      { [Ast0.Edots(clt2mcode "..." $1,Some $4)] }
  | dexpr TComma eexpr_list_dots
      { $1::Ast0.EComma(clt2mcode "," $2)::$3 }
  | TMetaExpList TComma eexpr_list_dots
      { let (nm,clt) = $1 in
      Ast0.MetaExprList(clt2mcode nm clt)::Ast0.EComma(clt2mcode "," $2)::$3 }
  | TEllipsis TComma eexpr_list_dots
      { Ast0.Edots(clt2mcode "..." $1,None)::
	Ast0.EComma(clt2mcode "," $2)::$3  }
  | TEllipsis TWhen TNotEq eexpr TLineEnd TComma eexpr_list_dots
      { Ast0.Edots(clt2mcode "..." $1,Some $4)::
	Ast0.EComma(clt2mcode "," $6)::$7  }

eexpr_list_circles:
    dexpr
      { [$1] }
  | TMetaExpList
      { let (nm,clt) = $1 in [Ast0.MetaExprList(clt2mcode nm clt)] }
  | TCircles
      { [Ast0.Ecircles(clt2mcode "ooo" $1,None)] }
  | TCircles TWhen TNotEq eexpr TLineEnd
      { [Ast0.Ecircles(clt2mcode "ooo" $1,Some $4)] }
  | dexpr TComma eexpr_list_circles
      { $1::Ast0.EComma(clt2mcode "," $2)::$3 }
  | TMetaExpList TComma eexpr_list_circles
      { let (nm,clt) = $1 in
      Ast0.MetaExprList(clt2mcode nm clt)::Ast0.EComma(clt2mcode "," $2)::$3 }
  | TCircles TComma eexpr_list_circles
      { Ast0.Ecircles(clt2mcode "ooo" $1,None)::
	Ast0.EComma(clt2mcode "," $2)::$3 }
  | TCircles TWhen TNotEq eexpr TLineEnd TComma eexpr_list_circles
      { Ast0.Ecircles(clt2mcode "ooo" $1,Some $4)::
	Ast0.EComma(clt2mcode "," $6)::$7 }

eexpr_list_stars:
    dexpr
      { [$1] }
  | TMetaExpList
      { let (nm,clt) = $1 in [Ast0.MetaExprList(clt2mcode nm clt)] }
  | TStars
      { [Ast0.Estars(clt2mcode "***" $1,None)] }
  | TStars TWhen TNotEq eexpr TLineEnd
      { [Ast0.Estars(clt2mcode "***" $1,Some $4)] }
  | dexpr TComma eexpr_list_stars
      { $1::Ast0.EComma(clt2mcode "," $2)::$3 }
  | TMetaExpList TComma eexpr_list_stars
      { let (nm,clt) = $1 in
      Ast0.MetaExprList(clt2mcode nm clt)::Ast0.EComma(clt2mcode "," $2)::$3 }
  | TStars TComma eexpr_list_stars
      { Ast0.Estars(clt2mcode "***" $1,None)::
	Ast0.EComma(clt2mcode "," $2)::$3 }
  | TStars TWhen TNotEq eexpr TLineEnd TComma eexpr_list_stars
      { Ast0.Estars(clt2mcode "***" $1,Some $4)::
	Ast0.EComma(clt2mcode "," $6)::$7 }

eexpr_list_opt: eexpr_list { $1 }
         | /* empty */     { Ast0.DOTS([]) }

expr_mid:
    expr                  { [$1] }
  | expr TMid0 expr_mid   { $1::$3 }

eexpr_mid:
    eexpr                 { [$1] }
  | eexpr TMid0 eexpr_mid { $1::$3 }

eexpr_opt: eexpr       { Some ($1) }
         | /* empty */ { None }

