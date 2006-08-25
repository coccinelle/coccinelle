%{
open Common
open Commonop
open Lexer_parser

open Ast_c
open Semantic_c

(* 
 todo: good error message when parse error caused by typedef (see token, see if ident that is a typedef, ...)
       special parse error treatment for missing ; (gcc)
*)

let warning s v = 
  if !Flag_parsing_c.verbose_parsing 
  then Common.warning s v
  else v

(**************************************)
(* parse auxillary function *)
type shortLong      = Short  | Long | LongLong

type decl = {storageD: storage                                               * Ast_c.info list
	    ;typeD: ((sign option) * (shortLong option) * (typeCbis option)) * Ast_c.info list
	    ;qualifD: typeQualifierbis                                       * Ast_c.info list
            (*todo: full_info: parse_info list;  to remember ordering between storage, qualifier, type *)
	    } 
let nullDecl = {storageD = NoSto, [];typeD = (None, None, None), [];qualifD = nullQualif}

let addStorageD  = function 
  | ((x,ii), ({storageD = (NoSto,[])} as v))     -> { v with storageD = (x, [ii]) }
  | ((x,ii), ({storageD = (y, ii2)} as v)) ->  
      if x = y then warning "duplicate storage classes" v
      else raise (Semantic ("multiple storage classes", fake_parse_info))

let addTypeD     = function 
  | ((Left3 Signed,ii)   ,({typeD = ((Some Signed,  b,c),ii2)} as v)) -> warning "duplicate 'signed'"   v
  | ((Left3 UnSigned,ii) ,({typeD = ((Some UnSigned,b,c),ii2)} as v)) -> warning "duplicate 'unsigned'" v
  | ((Left3 _,ii),        ({typeD = ((Some _,b,c),ii2)} as _v)) -> 
      raise (Semantic ("both signed and unsigned specified", fake_parse_info))
  | ((Left3 x,ii),        ({typeD = ((None,b,c),ii2)} as v))   -> {v with typeD = (Some x,b,c),ii ++ ii2}

  | ((Middle3 Short,ii),  ({typeD = ((a,Some Short,c),ii2)} as v)) -> warning "duplicate 'short'" v

   
  (* gccext: long long allowed *)
  | ((Middle3 Long,ii),   ({typeD = ((a,Some Long ,c),ii2)} as v)) -> { v with typeD = (a, Some LongLong, c),ii++ii2 }
  | ((Middle3 Long,ii),   ({typeD = ((a,Some LongLong ,c),ii2)} as v)) -> warning "triplicate 'long'" v

  | ((Middle3 _,ii),      ({typeD = ((a,Some _,c),ii2)} as _v)) -> 
      raise (Semantic ("both long and short specified", fake_parse_info))
  | ((Middle3 x,ii),      ({typeD = ((a,None,c),ii2)} as v))  -> {v with typeD = (a, Some x,c),ii++ii2}

  | ((Right3 t,ii),       ({typeD = ((a,b,Some _),ii2)} as _v)) -> 
      raise (Semantic ("two or more data types", fake_parse_info))
  | ((Right3 t,ii),       ({typeD = ((a,b,None),ii2)} as v))   -> {v with typeD = (a,b, Some t),ii++ii2}


let addQualif = function
  | ({const=true},    ({const=true} as x)) -> warning "duplicate 'const'" x
  | ({volatile=true}, ({volatile=true} as x)) -> warning "duplicate 'volatile'" x
  | ({const=true},    v) -> {v with const=true}
  | ({volatile=true}, v) -> {v with volatile=true}
  | _ -> internal_error "there is no noconst or novolatile keyword"

let addQualifD ((qu,ii), ({qualifD = (v,ii2)} as x)) =
  { x with qualifD = (addQualif (qu, v),ii::ii2) }


(**************************************)
(* stdC: type section, basic integer types (and ritchie)
   to understand the code, just look at the result (right part of the PM) and go back
*)
let (fixDeclSpecForDecl: decl -> (fullType * (storage * info list)))  = function
 {storageD = (st,iist); qualifD = (qu,iiq); typeD = (ty,iit)} -> 
  (((qu, iiq),
   (match ty with 
 | (None,None,None)       -> warning "type defaults to 'int'" (defaultInt, [])
 | (None, None, Some t)   -> (t, iit)
	 
 | (Some sign,   None, (None| Some (BaseType (IntType (Si (_,CInt))))))  -> BaseType(IntType (Si (sign, CInt))), iit
 | ((None|Some Signed),Some x,(None|Some(BaseType(IntType (Si (_,CInt)))))) -> 
     BaseType(IntType (Si (Signed, [Short,CShort; Long, CLong; LongLong, CLongLong] +> List.assoc x))), iit
 | (Some UnSigned, Some x, (None| Some (BaseType (IntType (Si (_,CInt))))))-> 
     BaseType(IntType (Si (UnSigned, [Short,CShort; Long, CLong; LongLong, CLongLong] +> List.assoc x))), iit
 | (Some sign,   None, (Some (BaseType (IntType CChar))))   -> BaseType(IntType (Si (sign, CChar2))), iit
 | (None, Some Long,(Some(BaseType(FloatType CDouble))))    -> BaseType (FloatType (CLongDouble)), iit

 | (Some _,_, Some _) ->  
     raise (Semantic ("signed, unsigned valid only for char and int", fake_parse_info)) (*mine*)
 | (_,Some _,(Some(BaseType(FloatType (CFloat|CLongDouble))))) -> 
     raise (Semantic ("long or short specified with floatint type", fake_parse_info))
 | (_,Some Short,(Some(BaseType(FloatType CDouble)))) ->
     raise (Semantic ("the only valid combination is long double", fake_parse_info))
       
 | (_, Some _, Some _) -> raise (Semantic ("long, short valid only for int or float", fake_parse_info)) (* mine *)
     (* if do short uint i, then gcc say parse error, strange ? it is not a parse error, it is 
	just that we dont allow with typedef either short/long or signed/unsigned 
        IN FACT, with parse_typedef_fix2 (with et() and dt()) now i say too parse error
	so this code is executed only when do short struct {....} and never with a typedef
	cos now we parse short uint i as short ident ident => parse error
	(cos after first short i pass in dt() mode)
     *)
   ))
     ,(st,iist)
  )

let fixDeclSpecForParam = function ({storageD = (st,iist)} as r) -> 
  let ((qu,ty) as v,_st) = fixDeclSpecForDecl r in
  match st with
  | (Sto Register) -> (v, true), iist
  | NoSto -> (v, false), iist
  | _ -> raise (Semantic ("storage class specified for parameter of function", fake_parse_info))

let fixDeclSpecForFuncDef x =
  let (returnType,storage) = fixDeclSpecForDecl x in
  (match storage with
  | (StoTypedef,ii) -> raise (Semantic ("function definition declared 'typedef'", fake_parse_info))
  | x -> (returnType, x)
  )

(* parameter: (this is the context where we give parameter only when in func DEFINITION not in funct DECLARATION)
   we must have a name  
   this function ensure that we give only parameterTypeDecl with well formed Classic constructor
   todo?: do we accept other declaration in ? so I must add them to the compound of the deffunc 
   I dont  have to handle typedef pb here cos C forbid to do VF f { ... } with VF a typedef of func
    cos here we dont see the name of the argument (in the typedef)
*)
let (fixOldCDecl: fullType -> fullType) = fun ty ->
  match snd ty with
  | ((FunctionType (fullt, (params, (b, iib)))),iifunc) -> 
       (* stdC: If the prototype declaration declares a parameter for a function that you 
	  are defining (it is part of a function definition), then you must write a name 
	  within the declarator. Otherwise, you can omit the name.
       *)
      (match params with
      | [((reg, None, ((_qua, (BaseType Void,_)))),_), _] ->  
          ty
      | params -> 
          (params +> List.iter (function 
            | (((b, None, _),  ii1),ii2) -> 
                raise (Semantic ("parameter name omitted", fake_parse_info)) 
	    | _ -> ()
          );
          ty)
      )
  (* todo? can we declare prototype in the decl or structdef, ... => length <> but good kan meme *)
  | _ -> raise (Semantic ("seems this is not a function", fake_parse_info)) (* gcc say parse error but dont see why *)


let fixFunc = function
  | (((s,iis), (nQ, (FunctionType (fullt, (params,bool)),iifunc)), (st,iist)), (cp,iicp)) -> 
      assert (nQ =*= nullQualif);
      (match params with
      | [((reg, None, ((_qua, (BaseType Void,_)))),_), _] ->  ()
      | params -> 
        params +> List.iter (function 
            | (((bool, Some s, fullt), _), _) -> ()
	    | _ -> failwith "internal errror: fixOldCDecl not good"
       ));
      (s, (fullt, (params, bool)), st, cp), ([iis]++iifunc++iicp++iist) (* it must be nullQualif,cos parser construct only this*)
  | _ -> raise (Semantic ("you are trying to do a function definition but you dont give any parameter", fake_parse_info))


(**************************************)
(* parse_typedef_fix2 *)

let dt s () = 
  if !Flag_parsing_c.debug_etdt then pr2 ("<" ^ s); 
  disable_typedef ()

let et s () = 
  if !Flag_parsing_c.debug_etdt then pr2 (">" ^ s);  
  enable_typedef ()


let fix_add_params_ident = function
  | ((s, (nQ, (FunctionType (fullt, (params, bool)),_)), st)) ->  

      (match params with
      | [((reg, None, ((_qua, (BaseType Void,_)))),_), _] ->  ()
      | params -> 
        params +> List.iter (function 
         | (((bool, Some s, fullt), _), _) -> 
            Lexer_parser.add_ident s
	 | _ -> failwith "internal errror: fixOldCDecl not good"
      )) 
  | _ -> ()
    
%}

%token <Ast_c.info> TComment TCommentSpace TCommentCpp TCommentAttrOrMacro 


%token <(string * Ast_c.isWchar) * Ast_c.info> TString
%token <(string * Ast_c.isWchar) * Ast_c.info> TChar
%token <string * Ast_c.info> TIdent TypedefIdent
%token <string * Ast_c.info>   TInt
%token <(string * Ast_c.floatType) * Ast_c.info> TFloat



%token <Ast_c.info> TOPar TCPar TOBrace TCBrace TOCro TCCro 
%token <Ast_c.info> TDot TComma TPtrOp 
%token <Ast_c.info> TInc TDec
%token <Ast_c.assignOp * Ast_c.info> TAssign 
%token <Ast_c.info> TEq
%token <Ast_c.info> TWhy  TTilde TBang
%token <Ast_c.info> TEllipsis
%token <Ast_c.info> TDotDot

%token <Ast_c.info> TPtVirg
%token <Ast_c.info> 
       TOrLog TAndLog TOr TXor TAnd  TEqEq TNotEq TInf TSup TInfEq TSupEq  TShl TShr 
       TPlus TMinus TMul TDiv TMod 

%token <Ast_c.info>
       Tchar Tshort Tint Tdouble Tfloat Tlong Tunsigned Tsigned Tvoid
       Tauto Tregister Textern Tstatic 
       Tconst Tvolatile
       Tstruct Tenum Ttypedef Tunion
       Tbreak Telse Tswitch Tcase Tcontinue Tfor Tdo Tif  Twhile Treturn Tgoto Tdefault
       Tsizeof  
%token <Ast_c.info> Tasm
%token <Ast_c.info> Tattribute

%token <Ast_c.info> THigherOrderMacro THigherOrderExprExprStatement THigherOrderExprStatement THigherOrderExprExprExprStatement


%token <Ast_c.info> EOF


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

%start main external_declaration2 statement expr
%type <Ast_c.program> main
%type <Ast_c.programElement> external_declaration2

%type <Ast_c.statement> statement
%type <Ast_c.expression> expr
%%

main:  translation_unit EOF     { $1 }

/********************************************************************************/
/*
 expression
 statement
 declaration
 main
*/

/********************************************************************************/

expr: assign_expr             { $1 }
    | expr TComma assign_expr { (Sequence ($1,$3), noType),       [$2] }

assign_expr: cond_expr                      { $1 }
           /* bugfix: in C grammar they put unary_expr, but in fact it must be cast_expr, otherwise (int *) xxx = &yy; is not allowed */
           | cast_expr TAssign assign_expr { (Assignment ($1, fst $2, $3),           noType), [snd $2] }
           | cast_expr TEq     assign_expr { (Assignment ($1, SimpleAssign, $3),     noType), [$2] }

cond_expr: arith_expr                             { $1 }
	 | arith_expr TWhy gcc_opt_expr TDotDot cond_expr { (CondExpr ($1, $3, $5),      noType), [$2;$4] } /* gccext: allow optional then part */

gcc_opt_expr: expr { Some $1 }
            | /* empty */ { None  }

arith_expr: cast_expr                     { $1 }
	  | arith_expr TMul    arith_expr { (Binary ($1, Arith Mul,      $3),        noType), [$2] }
	  | arith_expr TDiv    arith_expr { (Binary ($1, Arith Div,      $3),        noType), [$2] }
	  | arith_expr TMod    arith_expr { (Binary ($1, Arith Mod,      $3),        noType), [$2] }
	  | arith_expr TPlus   arith_expr { (Binary ($1, Arith Plus,     $3),        noType), [$2] }
	  | arith_expr TMinus  arith_expr { (Binary ($1, Arith Minus,    $3),        noType), [$2] }
	  | arith_expr TShl    arith_expr { (Binary ($1, Arith DecLeft,  $3),        noType), [$2] }
	  | arith_expr TShr    arith_expr { (Binary ($1, Arith DecRight, $3),        noType), [$2] }
	  | arith_expr TInf    arith_expr { (Binary ($1, Logical Inf,    $3),        noType), [$2] }
	  | arith_expr TSup    arith_expr { (Binary ($1, Logical Sup,    $3),        noType), [$2] }
	  | arith_expr TInfEq  arith_expr { (Binary ($1, Logical InfEq,  $3),        noType), [$2] }
	  | arith_expr TSupEq  arith_expr { (Binary ($1, Logical SupEq,  $3),        noType), [$2] }
	  | arith_expr TEqEq   arith_expr { (Binary ($1, Logical Eq,     $3),        noType), [$2] }
	  | arith_expr TNotEq  arith_expr { (Binary ($1, Logical NotEq,  $3),        noType), [$2] }
	  | arith_expr TAnd    arith_expr { (Binary ($1, Arith And,      $3),        noType), [$2] }
	  | arith_expr TOr     arith_expr { (Binary ($1, Arith Or,       $3),        noType), [$2] }
	  | arith_expr TXor    arith_expr { (Binary ($1, Arith Xor,      $3),        noType), [$2] }
	  | arith_expr TAndLog arith_expr { (Binary ($1, Logical AndLog, $3),        noType), [$2] }
	  | arith_expr TOrLog  arith_expr { (Binary ($1, Logical OrLog,  $3),        noType), [$2] }

cast_expr: unary_expr                        { $1 }
	 | topar2 type_name tcpar2 cast_expr { (Cast ($2, $4),       noType), [$1;$3] }

topar2: TOPar { et "topar2" (); $1  }
tcpar2: TCPar { et "tcpar2" (); $1 (*TODO? et ? sure ? c pas dt plutot ? *) } 

unary_expr: postfix_expr                   { $1 }
	  | TInc unary_expr                { (Infix ($2, Inc),       noType),[$1] }
	  | TDec unary_expr                { (Infix ($2, Dec),       noType),[$1] }
	  | unary_op cast_expr             { (Unary ($2, fst $1),    noType),[snd $1] }
	  | Tsizeof unary_expr             { (SizeOfExpr ($2),       noType),[$1] }
	  | Tsizeof topar2 type_name tcpar2  { (SizeOfType ($3),     noType),[$1;$2;$4] }

unary_op: TAnd   { GetRef,     $1 }
	| TMul   { DeRef,      $1 }
	| TPlus  { UnPlus,     $1 }
	| TMinus { UnMinus,    $1 }
	| TTilde { Tilde,      $1 }
	| TBang  { Not,        $1 }
/*        | TAndLog { GetRef } */   /* ONLY OLD KERNEL, and also many 2.5.39->53 ? , generate s/r conflict,  gccext: have that in old kernel to get address of local declared label?  UGLY */


postfix_expr: primary_expr                                 { $1 }
	    | postfix_expr TOCro expr TCCro                { (ArrayAccess ($1, $3),      noType),[$2;$4] }
	    | postfix_expr TOPar argument_expr_list TCPar  { (FunCall ($1, $3),          noType),[$2;$4] }
	    | postfix_expr TOPar  TCPar                    { (FunCall ($1, []),          noType),[$2;$3] }
	    | postfix_expr TDot   ident                    { (RecordAccess   ($1,fst $3),   noType),[$2;snd $3] }
	    | postfix_expr TPtrOp ident                    { (RecordPtAccess ($1,fst $3),   noType),[$2;snd $3] }
	    | postfix_expr TInc                            { (Postfix ($1, Inc),      noType),[$2] }
	    | postfix_expr TDec                            { (Postfix ($1, Dec),      noType),[$2] }

            /* gccext: */
            | topar2 type_name tcpar2 TOBrace TCBrace                                    { (Constructor, noType),[] }
            | topar2 type_name tcpar2 TOBrace initialize_list gcc_comma_opt TCBrace      { (Constructor, noType),[] }

            /* cppext: */
            | THigherOrderMacro TOPar statement_list TCPar { (MacroCall2 (Right $3),  noType),[$1;$2;$4] }
            | THigherOrderMacro TOPar expr TCPar           { (MacroCall2 (Left $3),  noType),[$1;$2;$4] }

            | THigherOrderExprStatement TOPar expr TComma action_higherordermacro TCPar  
                { 
                  (MacroCall [(Left3 $3, []);(Right3 ($5), [$4])], noType), [$1;$2;$6]
                }
            | THigherOrderExprExprStatement TOPar assign_expr TComma assign_expr TComma action_higherordermacro TCPar 
                { 
                  (MacroCall [(Left3 $3, []);(Left3 $5, [$4]);(Right3 ($7), [$6])], noType), [$1;$2;$8]
                }
            | THigherOrderExprExprStatement TOPar decl_spec3 TComma assign_expr TComma action_higherordermacro TCPar 
                { 
                  let (returnType,storage) = fixDeclSpecForDecl $3 in
                  (MacroCall [(Middle3 (returnType, storage), []);(Left3 $5, [$4]);(Right3 ($7), [$6])], noType), [$1;$2;$8]
                }

            | THigherOrderExprExprExprStatement TOPar assign_expr TComma assign_expr TComma assign_expr TComma action_higherordermacro TCPar { (Constructor, noType), [] }

action_higherordermacro: jump        { ActJump $1 }
                       | Tdo         { ActMisc [$1] }
                       | Textern     { ActMisc [$1] }
                       | /* empty */ { ActMisc [] }
                       | assign_expr { ActExpr $1 }
                       | assign_expr TPtVirg action_higherordermacro { ActExpr2 ($1, $2, $3)  }

argument_expr_list: assign_expr { [Left $1, []] }
	          | argument_expr_list TComma assign_expr { $1 ++ [Left $3,    [$2]] }

                   /* decl_spec and not juste type_spec cos can have  unsigned short for instance => type_spec_list */
                  | decl_spec3 { let (returnType,storage) = fixDeclSpecForDecl $1 in [Right (returnType, storage),  []]  }  /* cppext: */
	          | argument_expr_list TComma decl_spec3  { let (returnType,storage) = fixDeclSpecForDecl $3 in $1 ++ [Right (returnType, storage), [$2]] }  /* cppext: */


decl_spec3: decl_spec { et "decl_spec3" (); $1 }

primary_expr: TIdent  { ((Ident  (fst $1)), noType), [snd $1] }
            | TInt    { (Constant (Int    (fst $1)), noType), [snd $1] }
	    | TFloat  { (Constant (Float  (fst $1)), noType), [snd $1] }
	    | TString { (Constant (String (fst $1)), noType), [snd $1] }
	    | TChar   { (Constant (Char   (fst $1)), noType), [snd $1] }
	    | TOPar expr TCPar { (ParenExpr ($2),  noType), [$1;$3] } /* forunparser: */

            | TString string_list { (Constant (String (fst $1)),  noType), snd $1::$2 } /* gccext: */
/*          | TIdent  string_list { (Constant (Ident (fst $1)), noType), [snd $1] } */  /*  cppext:  ex= printk (KERN_INFO "xxx" UTS_RELEASE)  */
               /* note that can make a bug cos if not good parsing of typedef,  ucharv toto;  is not parsed as a declaration */
            /* gccext: allow statement as expressions via ({ statement }) */
            | TOPar compound TCPar  { (StatementExpr ($2),   noType), [$1;$3] } 




const_expr: cond_expr { $1 (* would like evalInt $1 but require too much info *) }

/* why this ? why not s/ident/TIdent ?  cos there is multiple namespace in C, 
   so a label can have the same name that a typedef, same for field and tags
   hence sometimes the use of ident  insteat of TIdent
 */
ident: TIdent       { $1 }
     | TypedefIdent { $1 }

/********************************************************************************/

statement: labeled         { Labeled        (fst $1),      snd $1 }
	 | compound        { Compound       (fst $1),      snd $1 }
	 | expr_statement  { ExprStatement  (fst $1),      snd $1 }
	 | selection       { Selection      (fst $1),      snd $1 }
	 | iteration       { Iteration      (fst $1),      snd $1 }
	 | jump TPtVirg    { Jump           (fst $1),      snd $1 ++ [$2] }
         /* gccext: */
         | Tasm TOPar asmbody TCPar TPtVirg             { Asm, [] }

labeled: ident            TDotDot statement                      { Label (fst $1, $3),     [snd $1; $2] }
       | Tcase const_expr TDotDot statement                      { Case ($2, $4),          [$1; $3] }
       | Tcase const_expr TEllipsis const_expr TDotDot statement { CaseRange ($2, $4, $6), [$1;$3;$5] } /* gccext: allow range */
       | Tdefault         TDotDot statement                      { Default $3,             [$1; $2] } 

       /* note that case 1: case 2: i++;    would be correctly parsed, but with a Case  (1, (Case (2, i++)))  :(  */

       /* generate conflict 31 shift/Reduce conflict each ,  mais ca va, ca fait ce qu'il faut */
       /* gccext:  allow toto: } */
       | ident            TDotDot { Label (fst $1, (ExprStatement None, [])), [snd $1; $2] }
       | Tcase const_expr TDotDot { Case ($2, (ExprStatement None, [])),      [$1;$3] }   
       | Tdefault         TDotDot { Default (ExprStatement None, []),         [$1; $2] }  




compound: tobrace compound2 tcbrace { $2, [$1; $3]  }

tobrace: TOBrace                     {  new_scope (); $1(* to do scoped typedef *) }
tcbrace: TCBrace                     {  del_scope (); $1 }

/*
compound2:                           { ([],[]) }
        |  statement_list            { ([], $1) }
        |  decl_list                 { ($1, []) }
        |  decl_list statement_list  { ($1,$2) }

decl_list: decl           { [$1]   }
	 | decl_list decl { $1 ++ [$2] }

statement_list: statement { [$1] }
	      | statement_list statement { $1 ++ [$2] }

*/
/* cppext: (because of cpp),  mix decl/statement */
statement_list: statement { [$1] }
	      | statement_list statement { $1 ++ [$2] }

compound2:  { ([]) }
        | compound3 { ($1) }

compound3: 
        | stat_or_decl { [$1] }                          
        | compound3 stat_or_decl { $1 ++ [$2] }

stat_or_decl: decl      { Decl $1, [] }
            | statement { $1 }






expr_statement: TPtVirg      { None,    [$1] }
	      |	expr TPtVirg { Some $1, [$2] }

selection: Tif TOPar expr TCPar statement                 { If ($3, $5, (ExprStatement None, [])),   [$1;$2;$4] }
	 | Tif TOPar expr TCPar statement Telse statement { If ($3, $5, $7),                         [$1;$2;$4;$6] }
	 | Tswitch TOPar expr TCPar statement             { Switch ($3,$5),                          [$1;$2;$4]  }

iteration: Twhile TOPar expr TCPar statement                             { While ($3,$5),                [$1;$2;$4] }
	 | Tdo statement Twhile TOPar expr TCPar TPtVirg                 { DoWhile ($2,$5),              [$1;$3;$4;$6;$7] }
	 | Tfor TOPar expr_statement expr_statement TCPar statement      { For ($3,$4,(None, []),$6),    [$1;$2;$5]}
	 | Tfor TOPar expr_statement expr_statement expr TCPar statement { For ($3,$4,(Some $5, []),$7), [$1;$2;$6] }


jump: Tgoto ident  { Goto (fst $2),  [$1;snd $2] } 
    | Tcontinue    { Continue,       [$1] }
    | Tbreak       { Break,          [$1] }
    | Treturn      { Return,         [$1] } 
    | Treturn expr { ReturnExpr $2,  [$1] }







/* gccext: */
asmbody: string_list colon_asm_list  { }
       | string_list { } /* in old kernel */


string_list: string_elem { [$1] }
           | string_list string_elem { $1 ++ [$2] } 

string_elem: TString { snd $1 }
           /* cppext: */
           /* can cause some strange behaviour ... */
           | TIdent { snd $1 } 
           | TIdent TOPar TIdent TCPar { snd $1 } 


colon_asm_list: colon_asm {}
              | colon_asm_list colon_asm  {}

colon_asm: TDotDot colon_option_list {}

colon_option_list: colon_option {} 
                 | colon_option_list TComma colon_option {}

colon_option: TString {}
            | TString TOPar assign_expr TCPar {} 
            | /* empty */ {}



/********************************************************************************/

/*------------------------------------------------------------------------------*/
decl: decl2  { et "decl" (); $1 }

decl2: decl_spec TPtVirg                   { let (returnType,storage) = fixDeclSpecForDecl $1 
                                             in DeclList ([(None, returnType, fst storage),[]],  ($2::snd storage))} 
     | decl_spec init_declarator_list TPtVirg 
	{ let (returnType,storage) = fixDeclSpecForDecl $1 in
          DeclList (
	       ($2 +> List.map (fun ((((s,iis),f), ini), iivirg) -> 
                 let ini, iini = 
                   match ini with
                   | None -> None, []
                   | Some (ini, iini) -> Some ini, [iini]
                 in
		 if fst storage = StoTypedef 
		 then begin 
                   Lexer_parser.add_typedef s;
                   (Some ((s, ini), iis::iini), f returnType, StoTypedef),
                   iivirg 
                 end
		 else 
                   (Some ((s, ini), iis::iini), f returnType, fst storage),
                   iivirg
  	         )
	       ),  ($3::snd storage))
	} 
     /* | init_declarator_list TPtVirg { failwith "todo" } */

/*------------------------------------------------------------------------------*/
decl_spec: decl_spec2                   { dt "declspec" (); $1  }


decl_spec2: storage_class_spec            { {nullDecl with storageD = (fst $1, [snd $1]) } }
	  | type_spec                     { addTypeD ($1,nullDecl) }
	  | type_qualif                   { {nullDecl with qualifD  = (fst $1, [snd $1]) } }
          | storage_class_spec decl_spec2 { addStorageD ($1, $2) }
	  | type_spec          decl_spec2 { addTypeD    ($1, $2) }
	  | type_qualif        decl_spec2 { addQualifD  ($1, $2) }
/* can simplity by putting all in _opt ? must have at least one otherwise decl_list is ambiguous ? (no cos have ';' between decl) */


storage_class_spec: Tstatic      { Sto Static,  $1 }
	          | Textern      { Sto Extern,  $1 }
		  | Tauto        { Sto Auto,    $1 }
		  | Tregister    { Sto Register,$1 }
		  | Ttypedef     { StoTypedef,  $1 }

type_spec: type_spec2            { dt "type" (); $1   }

type_spec2: 
	 | Tvoid                { Right3 (BaseType Void),            [$1] }
         | Tchar                { Right3 (BaseType (IntType CChar)), [$1]}
	 | Tint                 { Right3 (BaseType (IntType (Si (Signed,CInt)))), [$1]}
	 | Tfloat               { Right3 (BaseType (FloatType CFloat)),  [$1]}
	 | Tdouble              { Right3 (BaseType (FloatType CDouble)), [$1] }
	 | Tshort               { Middle3 Short,  [$1]}
	 | Tlong                { Middle3 Long,   [$1]}
	 | Tsigned              { Left3 Signed,   [$1]}
	 | Tunsigned            { Left3 UnSigned, [$1]}
	 | struct_or_union_spec { Right3 (fst $1), snd $1 }
	 | enum_spec            { Right3 (fst $1), snd $1 }
	 | TypedefIdent         { Right3 (TypeName (fst $1)), [snd $1]}
         /* parse_typedef_fix1: cant put: TIdent {} cos it make the grammar ambigue, generate lots of conflicts => we must 
 	     use some tricks: we make the lexer and parser cooperate, cf lexerParser.ml
	    parse_typedef_fix2: this is not enough, and you must use parse_typedef_fix2 to fully manage 
	    typedef pb in grammar	  
         */

type_qualif: Tconst    { {const=true  ; volatile=false}, $1 }
	   | Tvolatile { {const=false ; volatile=true},  $1 }

/*------------------------------------------------------------------------------*/
struct_or_union_spec: s_or_u_spec2 { dt "su" (); $1 }

s_or_u_spec2: struct_or_union ident TOBrace struct_decl_list_gcc TCBrace gcc_attr_opt { StructUnion (Some (fst $2), (fst $1,$4)),       [snd $1;snd $2;$3;$5]  }
            | struct_or_union       TOBrace struct_decl_list_gcc TCBrace gcc_attr_opt { StructUnion (None, (fst $1,$3)), [snd $1;$2;$4] }
	    | struct_or_union ident                                  
		{ StructUnionName ((fst $2), fst $1), [snd $1;snd $2] }
								     
struct_or_union: struct_or_union2 { et "su" (); $1 }

struct_or_union2: Tstruct { Struct, $1 }
    	        | Tunion  { Union, $1 }

struct_decl_list_gcc: struct_decl_list gcc_opt_virg        { $1 } /* gccext: allow double ;; at end too */
                    | /* empty */       { [] } /* gccext: allow empty struct */


gcc_opt_virg: TPtVirg { }
            |  { }

gcc_attr_opt: /* empty */ { }
            | Tattribute TOPar TOPar argument_expr_list TCPar TCPar { }


struct_decl_list: struct_decl                   { [$1] }
	        | struct_decl_list struct_decl  { $1 ++ [$2] }
	        | struct_decl_list TPtVirg struct_decl  { $1 ++ [$3] } /* gccext: allow double ;; */


struct_decl: struct_decl2  { et "struct" (); $1 }

struct_decl2: spec_qualif_list struct_declarator_list TPtVirg 
                { let (returnType,storage) = fixDeclSpecForDecl $1 in
                  (if fst storage <> NoSto then internal_error "parsing dont allow this";

                   FieldDeclList ($2 +> (List.map (fun (f, iivirg) ->     f returnType, iivirg))),    [$3] )
                  (* dont need to check if typedef or func initialised cos grammar dont allow typedef nor 
                  initialiser in struct 
                  *)
                }

             |  spec_qualif_list TPtVirg 
                { 
                  (* gccext: allow empty elements if it is a structdef or enumdef *)
                  let (returnType,storage) = fixDeclSpecForDecl $1 in
                  (if fst storage <> NoSto then internal_error "parsing dont allow this";
                   FieldDeclList [(Simple (None, returnType), []) , []], [$2]
                )
               
                }


struct_declarator_list: struct_declarator                               { [$1,           []] }
		      | struct_declarator_list TComma struct_declarator { $1 ++ [$3,     [$2]] }

struct_declarator: declarator                    { (fun x -> Simple   (Some (fst (fst $1)), (snd $1) x),   [snd (fst $1)]) }
		 | dotdot const_expr2            { (fun x -> BitField (None, x, $2),              [$1]) }
		 | declarator dotdot const_expr2 { (fun x -> BitField (Some (fst (fst $1)), ((snd $1) x), $3),      [snd (fst $1);$2]) }

dotdot: TDotDot  { et "dotdot" (); $1 }
const_expr2: const_expr { dt "const_expr2" (); $1 }

/*------------------------------------------------------------------------------*/
enum_spec: Tenum        TOBrace enumerator_list gcc_comma_opt TCBrace { Enum (None,    $3),           [$1;$2;$5] ++ $4 }
         | Tenum ident  TOBrace enumerator_list gcc_comma_opt TCBrace { Enum (Some (fst $2), $4),     [$1; snd $2; $3;$6] ++ $5 }
         | Tenum ident                                                { EnumName (fst $2),       [$1; snd $2] }

enumerator_list: enumerator                        { [$1,          []]   }
	       | enumerator_list TComma enumerator { $1 ++ [$3,    [$2]] }

gcc_comma_opt: TComma {  [$1] } /* gccext:  which allow a trailing ',' in enum (as in perl) */
             | /* */  {  []  }

enumerator: idente                 { (fst $1, None),      [snd $1]    }
          | idente  TEq const_expr { (fst $1, Some $3),   [snd $1; $2] }


idente: ident { Lexer_parser.add_ident (fst $1); $1 }
/*------------------------------------------------------------------------------*/

spec_qualif_list: spec_qualif_list2            {  dt "spec_qualif" (); $1 }		    

/* cant put decl_spec cos no storage is allowed for field struct */
spec_qualif_list2: type_spec                    { addTypeD ($1, nullDecl) }
		 | type_qualif                  { {nullDecl with qualifD  = (fst $1, [snd $1]) } }
		 | type_spec   spec_qualif_list { addTypeD ($1,$2)   }
		 | type_qualif spec_qualif_list { addQualifD ($1,$2) }

 	     
/*------------------------------------------------------------------------------*/
init_declarator_list: init_declarator                             { [$1,   []] }
	            | init_declarator_list TComma init_declarator { $1 ++ [$3,     [$2]] }

init_declarator: init_declarator2 gcc_attr_opt  { dt "init" (); $1 }

init_declarator2:  declaratori                  { ($1, None) }
	        |  declaratori teq initialize   { ($1, Some ($3, $2)) }

declaratori: declarator  { Lexer_parser.add_ident (fst (fst $1)); $1 }
teq: TEq  { et "teq" (); $1 }


/*------------------------------------------------------------------------------*/
/* declarator return a couple: (name, partial type (a function to be applied to return type)) */
/*-----------------------------------------------------------------------------*/
declarator: pointer direct_d { (fst $2, fun x -> x +> $1 +> (snd $2)  ) 
           (* when int* f(int) we must return Func(Pointer int,int) and not Pointer (Func(int,int) *)
			     }
          | direct_d         { $1  }


pointer: TMul                          { fun x -> (nQ,         (Pointer x,         [$1])) }
       | TMul type_qualif_list         { fun x -> ($2.qualifD, (Pointer x,         [$1])) }
       | TMul pointer                  { fun x -> (nQ,         (Pointer ($2 x),    [$1])) }
       | TMul type_qualif_list pointer { fun x -> ($2.qualifD, (Pointer ($3 x),    [$1])) }

type_qualif_list: type_qualif                  { {nullDecl with qualifD = (fst $1, [snd $1])} }
		| type_qualif_list type_qualif { addQualifD ($2,$1) }

direct_d: 
   TIdent                                  { ($1, fun x -> x) }
 | TOPar declarator TCPar                  { (*old: $2*) (fst $2, fun x -> (nQ, (ParenType ((snd $2) x), [$1;$3]))) }/* forunparser: */ 
 | direct_d tocro            tccro         { (fst $1,fun x->(snd $1) (nQ,(Array (None,x),                          [$2;$3]))) }
 | direct_d tocro const_expr tccro         { (fst $1,fun x->(snd $1) (nQ,(Array (Some $3,x),                       [$2;$4])))}
 | direct_d topar            tcpar         { (fst $1,fun x->(snd $1) (nQ,(FunctionType (x,(([],(false, [])))),[$2;$3])))}
 | direct_d topar parameter_type_list tcpar{ (fst $1,fun x->(snd $1) (nQ,(FunctionType (x, $3),            [$2;$4]))) }

tocro: TOCro { et "tocro" ();$1 }
tccro: TCCro { dt "tccro" ();$1 }

topar: TOPar { new_scope ();et "topar" (); Lexer_parser._lexer_hint := Some ParameterDeclaration; $1  }
tcpar: TCPar { del_scope ();dt "tcpar" (); Lexer_parser._lexer_hint := None; $1  }




parameter_type_list: parameter_list                  { ($1, (false, []))}
		   | parameter_list TComma TEllipsis { ($1, (true,  [$2;$3])) }

parameter_list: parameter_decl                       { [$1, []] }
	      | parameter_list TComma parameter_decl { $1 ++ [$3,  [$2]] }

parameter_decl: parameter_decl2 { et "param" ();  $1 }

parameter_decl2: decl_spec declaratorp          { let ((returnType,hasreg),iihasreg) = fixDeclSpecForParam $1 
                                                 in (hasreg, Some (fst (fst $2)), ((snd $2) returnType)),     (iihasreg ++ [snd (fst $2)]) }
	      |	 decl_spec abstract_declarator  { let ((returnType,hasreg), iihasreg) = fixDeclSpecForParam $1 
		                                 in (hasreg, None, ($2 returnType)),      (iihasreg ++ []) }
	      |	 decl_spec                      { let ((returnType,hasreg), iihasreg) = fixDeclSpecForParam $1 
		                                 in (hasreg, None, returnType),           (iihasreg ++ []) }

declaratorp: declarator { Lexer_parser.add_ident (fst (fst $1)); $1 }


/*------------------------------------------------------------------------------*/
type_name: spec_qualif_list                     
	     { let (returnType, _) = fixDeclSpecForDecl $1 in  returnType }
	 | spec_qualif_list abstract_declarator 
	     { let (returnType, _) = fixDeclSpecForDecl $1 in $2 returnType }

abstract_declarator: pointer                            { $1 }
	           |         direct_abstract_declarator { $1 }
		   | pointer direct_abstract_declarator { fun x -> x +> $2 +> $1 }

direct_abstract_declarator: 
   TOPar abstract_declarator TCPar                   { (* old: $2 *) (fun x -> (nQ, (ParenType ($2 x), [$1;$3]))) }/* forunparser: */ 

 | TOCro            TCCro                            { fun x ->   (nQ, (Array (None, x),      [$1;$2]))}
 | TOCro const_expr TCCro                            { fun x ->   (nQ, (Array (Some $2, x),   [$1;$3]))}
 | direct_abstract_declarator TOCro            TCCro { fun x ->$1 (nQ, (Array (None, x),      [$2;$3])) }
 | direct_abstract_declarator TOCro const_expr TCCro { fun x ->$1 (nQ, (Array (Some $3,x),    [$2;$4])) }
 | TOPar TCPar                                       { fun x ->   (nQ, (FunctionType (x, ([], (false,  []))),   [$1;$2])) }
 | TOPar parameter_type_list TCPar                   { fun x ->   (nQ, (FunctionType (x, $2),           [$1;$3]))}
 | direct_abstract_declarator TOPar TCPar            { fun x ->$1 (nQ, (FunctionType (x, (([], (false, [])))),[$2;$3])) }
 | direct_abstract_declarator TOPar parameter_type_list TCPar { fun x -> $1 (nQ, (FunctionType (x, $3), [$2;$4])) }
			  
/*------------------------------------------------------------------------------*/
initialize: assign_expr                                           { InitExpr $1,                [] }
          | TOBrace initialize_list gcc_comma_opt  TCBrace        { InitList (List.rev $2),     [$1]++$3++[$4] }
          | TOBrace TCBrace                                       { InitList [],       [$1;$2] } /* gccext: */

/*opti: this time use the List.rev tricks cos quite critical */
initialize_list: initialize2                        { [$1,   []] }
	       | initialize_list TComma initialize2 { ($3,  [$2])::$1 }

initialize2: arith_expr                                         { InitExpr $1,   [] } /* gccext:, arithexpr and no assign_expr cos can have ambiguity with comma */
          | TOBrace initialize_list gcc_comma_opt TCBrace       { InitList (List.rev $2),   [$1]++$3++[$4] }
          | TOBrace TCBrace                                     { InitList [],  [$1;$2]  }

           /* gccext:, labeled elements */
          | TDot ident TEq initialize2                          { InitGcc (fst $2, $4),     [$1;snd $2; $3] } 
          | ident TDotDot initialize2                           { InitGcc (fst $1, $3),     [snd $1; $2] } /* in old kernel */
          | TOCro const_expr TCCro TEq initialize2              { InitGccIndex ($2, $5),    [$1;$3;$4] }
          | TOCro const_expr TEllipsis const_expr TCCro TEq initialize2   { InitGccRange ($2, $4, $7),  [$1;$3;$5;$6] }

/********************************************************************************/

translation_unit: 
                | external_declaration                  { Lexer_parser._lexer_hint := Some Toplevel;   [$1] }
	        | translation_unit external_declaration { Lexer_parser._lexer_hint := Some Toplevel; $1 ++ [$2] }

external_declaration: 
                    | function_definition               { Definition (fixFunc $1) }
		    | decl                              { Declaration $1 }
                    | TIdent TOPar argument_expr_list TCPar TPtVirg  { SpecialDeclMacro (fst $1, $3,    [snd $1;$2;$4;$5]) } /* cppext: */
                    | TIdent TOPar argument_expr_list TCPar  { EmptyDef [] } /* seems dont work */

function_definition: start_fun compound      { del_scope(); ($1, $2) }

start_fun: start_fun2                        { new_scope(); fix_add_params_ident $1; Lexer_parser._lexer_hint := None;  $1 }
start_fun2: decl_spec declaratorfd           { let (returnType,storage) = fixDeclSpecForFuncDef $1 in
                                               (fst $2, fixOldCDecl ((snd $2) returnType) , storage) }


declaratorfd: declarator { et "declaratorfd" (); $1 }


/********************************************************************************/

external_declaration2: 
         | external_declaration                         { $1 }

         /* can have asm declaration at toplevel */
         | Tasm TOPar asmbody TCPar TPtVirg             { EmptyDef [] } 
         
         /* in ~/kernels/src/linux-2.5.2/drivers/isdn/hisax/isdnl3.c sometimes the function ends with }; instead of just } */
         | TPtVirg    { EmptyDef [$1] } 

         | EOF        { FinalDef $1 } 


