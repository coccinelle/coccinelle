module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)
(* Modified code *)

type arity = OPT | UNIQUE | MULTI | NONE

type token_info = { tline_start : int; tline_end : int }
let default_token_info = { tline_start = -1; tline_end = -1 }

(* MIXED is like CONTEXT, since sometimes MIXED things have to revert to
CONTEXT - see insert_plus.ml *)
type mcodekind =
    MINUS       of (Ast.anything list list * token_info) ref
  | PLUS
  | CONTEXT     of (Ast.anything Ast.befaft * token_info * token_info) ref
  | MIXED       of (Ast.anything Ast.befaft * token_info * token_info) ref

type info = { line_start : int; line_end : int;
	      logical_start : int; logical_end : int;
	      attachable_start : bool; attachable_end : bool;
	      mcode_start : mcodekind option; mcode_end : mcodekind option;
	      column : int; offset : int }

type 'a mcode = 'a * arity * info * mcodekind
type 'a wrap = 'a * info * int ref * mcodekind ref (* int ref is an index *)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

type 'a base_dots =
    DOTS of 'a list
  | CIRCLES of 'a list
  | STARS of 'a list

type 'a dots = 'a base_dots wrap

(* --------------------------------------------------------------------- *)
(* Identifier *)

type base_ident =
    Id of string mcode
  | MetaId of string mcode
  | MetaFunc of string mcode
  | MetaLocalFunc of string mcode
  | OptIdent      of ident
  | UniqueIdent   of ident
  | MultiIdent    of ident (* only allowed in nests *)

and ident = base_ident wrap

(* --------------------------------------------------------------------- *)
(* Expression *)

type base_expression = 
    Ident          of ident
  | Constant       of Ast.constant mcode
  | FunCall        of expression * string mcode (* ( *) *
                      expression dots * string mcode (* ) *)
  | Assignment     of expression * Ast.assignOp mcode * expression
  | CondExpr       of expression * string mcode (* ? *) * expression option *
	              string mcode (* : *) * expression
  | Postfix        of expression * Ast.fixOp mcode
  | Infix          of expression * Ast.fixOp mcode
  | Unary          of expression * Ast.unaryOp mcode
  | Binary         of expression * Ast.binaryOp mcode * expression
  | Paren          of string mcode (* ( *) * expression *
                      string mcode (* ) *)
  | ArrayAccess    of expression * string mcode (* [ *) * expression *
	              string mcode (* ] *)
  | RecordAccess   of expression * string mcode (* . *) * ident
  | RecordPtAccess of expression * string mcode (* -> *) * ident
  | Cast           of string mcode (* ( *) * typeC * string mcode (* ) *) *
                      expression
  | SizeOfExpr     of string mcode (* sizeof *) * expression
  | SizeOfType     of string mcode (* sizeof *) * string mcode (* ( *) *
                      typeC * string mcode (* ) *)
  | MetaConst      of string mcode * typeC list option
  | MetaErr        of string mcode
  | MetaExpr       of string mcode * typeC list option
  | MetaExprList   of string mcode (* only in arg lists *)
  | EComma         of string mcode (* only in arg lists *)
  | DisjExpr       of string mcode * expression list * string mcode
  | NestExpr       of string mcode * expression dots * string mcode
  | Edots          of string mcode (* ... *) * expression option
  | Ecircles       of string mcode (* ooo *) * expression option
  | Estars         of string mcode (* *** *) * expression option
  | OptExp         of expression
  | UniqueExp      of expression
  | MultiExp       of expression (* only allowed in nests *)

and expression = base_expression wrap * typeC

(* --------------------------------------------------------------------- *)
(* Types *)

and base_typeC = 
    ConstVol        of Ast.const_vol mcode * typeC
  | BaseType        of Ast.baseType mcode * Ast.sign mcode option
  | Pointer         of typeC * string mcode (* * *)
  | Array           of typeC * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | StructUnionName of tagged_string * Ast.structUnion mcode
  | TypeName        of string mcode
  | MetaType        of string mcode
  | OptType         of typeC
  | UniqueType      of typeC
  | MultiType       of typeC
  | Unknown         (* for metavariables *)

and tagged_string = string mcode

and typeC = base_typeC wrap

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

type base_declaration =
    Init of typeC * ident * string mcode (*=*) * expression *
	string mcode (*;*)
  | UnInit of typeC * ident * string mcode (* ; *)
  | DisjDecl   of string mcode * declaration list * string mcode
  | OptDecl    of declaration
  | UniqueDecl of declaration
  | MultiDecl  of declaration (* only allowed in nests *)

and declaration = base_declaration wrap

(* --------------------------------------------------------------------- *)
(* Parameter *)

type base_parameterTypeDef =
    VoidParam     of typeC
  | Param         of ident * typeC
  | MetaParam     of string mcode
  | MetaParamList of string mcode
  | PComma        of string mcode
  | Pdots         of string mcode (* ... *)
  | Pcircles      of string mcode (* ooo *)
  | OptParam      of parameterTypeDef
  | UniqueParam   of parameterTypeDef

and parameterTypeDef = base_parameterTypeDef wrap

and parameter_list = parameterTypeDef dots

(* --------------------------------------------------------------------- *)
(* Statement*)

type base_statement =
    Decl          of declaration
  | Seq           of string mcode (* { *) * statement dots *
 	             string mcode (* } *)
  | ExprStatement of expression * string mcode (*;*)
  | IfThen        of string mcode (* if *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
	             statement
  | IfThenElse    of string mcode (* if *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
	             statement * string mcode (* else *) * statement
  | While         of string mcode (* while *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
	             statement
  | Do            of string mcode (* do *) * statement *
                     string mcode (* while *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
                     string mcode (* ; *)
  | For           of string mcode (* for *) * string mcode (* ( *) *
                     expression option * string mcode (*;*) *
	             expression option * string mcode (*;*) *
                     expression option * string mcode (* ) *) * statement
  | Return        of string mcode (* return *) * string mcode (* ; *)
  | ReturnExpr    of string mcode (* return *) * expression *
	             string mcode (* ; *)
  | MetaStmt      of string mcode  
  | MetaStmtList  of string mcode  (* only in statement lists *)
  | Exp           of expression  (* only in dotted statement lists *)
  | Disj          of string mcode * statement dots list * string mcode
  | Nest          of string mcode * statement dots * string mcode
  | Dots          of string mcode (* ... *) * statement dots option
  | Circles       of string mcode (* ooo *) * statement dots option
  | Stars         of string mcode (* *** *) * statement dots option
  | FunDecl of Ast.storage mcode option * ident (* name *) *
	string mcode (* ( *) * parameter_list * string mcode (* ) *) *
	string mcode (* { *) * statement dots *
	string mcode (* } *)
  | OptStm   of statement
  | UniqueStm of statement
  | MultiStm  of statement (* only allowed in nests *)

and statement = base_statement wrap

(* --------------------------------------------------------------------- *)
(* Top-level code *)

type base_top_level =
    FUNCTION of statement
  | DECL of declaration
  | INCLUDE of string mcode (* #include *) * string mcode (* file *)
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | ERRORWORDS of expression list
  | CODE of statement dots
  | OTHER of statement (* temporary, disappears after top_level.ml *)

and top_level = base_top_level wrap
and rule = top_level list

(* --------------------------------------------------------------------- *)

type anything =
    DotsExprTag of expression dots
  | DotsParamTag of parameterTypeDef dots
  | DotsStmtTag of statement dots
  | IdentTag of ident
  | ExprTag of expression
  | TypeCTag of typeC
  | ParamTag of parameterTypeDef
  | DeclTag of declaration
  | StmtTag of statement
  | TopTag of top_level

let dotsExpr x = DotsExprTag x
let dotsParam x = DotsParamTag x
let dotsStmt x = DotsStmtTag x
let ident x = IdentTag x
let expr x = ExprTag x
let typeC x = TypeCTag x
let param x = ParamTag x
let decl x = DeclTag x
let stmt x = StmtTag x
let top x = TopTag x

(* --------------------------------------------------------------------- *)
(* Avoid cluttering the parser.  Calculated in compute_lines.ml. *)

let default_info _ = (* why is this a function? *)
  { line_start = -1; line_end = -1;
    logical_start = -1; logical_end = -1;
    attachable_start = true; attachable_end = true;
    mcode_start = None; mcode_end = None;
    column = -1; offset = -1 }

let default_befaft _ =
  MIXED(ref (Ast.NOTHING,default_token_info,default_token_info))
let context_befaft _ =
  CONTEXT(ref (Ast.NOTHING,default_token_info,default_token_info))

let wrap x = (x,default_info(),ref (-1),ref (default_befaft()))
let context_wrap x = (x,default_info(),ref (-1),ref (context_befaft()))
let unwrap (x,_,_,_) = x
let unwrap_mcode (x,_,_,_) = x
let rewrap (_,info,index,mcodekind) x = (x,info,index,mcodekind)
let copywrap (_,info,index,mcodekind) x =
  (x,
   { line_start = info.line_start; line_end = info.line_end;
     logical_start = info.logical_start; logical_end = info.logical_end;
     attachable_start = info.attachable_start;
     attachable_end = info.attachable_end;
     mcode_start = info.mcode_start; mcode_end = info.mcode_end;
     column = info.column; offset = info.offset },
   ref !index,ref !mcodekind)
let get_info (_,info,_,_) = info
let get_index (_,_,index,_) = !index
let set_index (_,_,index,_) i = index := i
let get_mcodekind (_,_,_,mcodekind) = !mcodekind
let set_mcodekind (_,_,_,mcodekind) mk = mcodekind := mk

(* --------------------------------------------------------------------- *)

(* unique indices, for mcode and tree nodes *)
let index_counter = ref 0
let fresh_index _ = let cur = !index_counter in index_counter := cur + 1; cur

(* --------------------------------------------------------------------- *)

let undots d =
  match unwrap d with
  | DOTS    e -> e
  | CIRCLES e -> e
  | STARS   e -> e
