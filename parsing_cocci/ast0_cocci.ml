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
	      mcode_start : mcodekind list; mcode_end : mcodekind list;
	      column : int; offset : int }

type 'a mcode = 'a * arity * info * mcodekind
(* int ref is an index *)
type 'a wrap = 'a * info * int ref * mcodekind ref
      * Type_cocci.typeC option ref (* only for expressions *)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

and 'a base_dots =
    DOTS of 'a list
  | CIRCLES of 'a list
  | STARS of 'a list

and 'a dots = 'a base_dots wrap

(* --------------------------------------------------------------------- *)
(* Identifier *)

and base_ident =
    Id of string mcode
  | MetaId of string mcode * bool (* true if inherited *)
  | MetaFunc of string mcode * bool (* true if inherited *)
  | MetaLocalFunc of string mcode * bool (* true if inherited *)
  | OptIdent      of ident
  | UniqueIdent   of ident
  | MultiIdent    of ident (* only allowed in nests *)

and ident = base_ident wrap

(* --------------------------------------------------------------------- *)
(* Expression *)

and base_expression = 
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
  | MetaConst      of string mcode * Type_cocci.typeC list option *
	              bool (* true if inherited *)
  | MetaErr        of string mcode * bool (* true if inherited *)
  | MetaExpr       of string mcode * Type_cocci.typeC list option *
	              bool (* true if inherited *)
  | MetaExprList   of string mcode (* only in arg lists *) *
	              bool (* true if inherited *)
  | EComma         of string mcode (* only in arg lists *)
  | DisjExpr       of string mcode * expression list * string mcode
  | NestExpr       of string mcode * expression dots * string mcode *
	              expression option
  | Edots          of string mcode (* ... *) * expression option
  | Ecircles       of string mcode (* ooo *) * expression option
  | Estars         of string mcode (* *** *) * expression option
  | OptExp         of expression
  | UniqueExp      of expression
  | MultiExp       of expression (* only allowed in nests *)

and expression = base_expression wrap

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
  | MetaType        of string mcode * bool (* true if inherited *)
  | OptType         of typeC
  | UniqueType      of typeC
  | MultiType       of typeC

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
  | MetaParam     of string mcode * bool (* true if inherited *)
  | MetaParamList of string mcode * bool (* true if inherited *)
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
	             statement * (info * mcodekind) (* after info *)
  | IfThenElse    of string mcode (* if *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
	             statement * string mcode (* else *) * statement *
	             (info * mcodekind)
  | While         of string mcode (* while *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
	             statement * (info * mcodekind) (* after info *)
  | Do            of string mcode (* do *) * statement *
                     string mcode (* while *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
                     string mcode (* ; *)
  | For           of string mcode (* for *) * string mcode (* ( *) *
                     expression option * string mcode (*;*) *
	             expression option * string mcode (*;*) *
                     expression option * string mcode (* ) *) * statement *
	             (info * mcodekind) (* after info *)
  | Return        of string mcode (* return *) * string mcode (* ; *)
  | ReturnExpr    of string mcode (* return *) * expression *
	             string mcode (* ; *)
  | MetaStmt      of string mcode  * bool (* true if inherited *)
  | MetaStmtList  of string mcode * bool (* only in statement lists *)
  | Exp           of expression  (* only in dotted statement lists *)
  | Disj          of string mcode * statement dots list * string mcode
  | Nest          of string mcode * statement dots * string mcode *
	             statement dots option
  | Dots          of string mcode (* ... *) * statement dots option
  | Circles       of string mcode (* ooo *) * statement dots option
  | Stars         of string mcode (* *** *) * statement dots option
  | FunDecl of Ast.storage mcode option * typeC option * ident (* name *) *
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
    mcode_start = []; mcode_end = [];
    column = -1; offset = -1 }

let default_befaft _ =
  MIXED(ref (Ast.NOTHING,default_token_info,default_token_info))
let context_befaft _ =
  CONTEXT(ref (Ast.NOTHING,default_token_info,default_token_info))

let wrap x = (x,default_info(),ref (-1),ref (default_befaft()),ref None)
let context_wrap x =
  (x,default_info(),ref (-1),ref (context_befaft()),ref None)
let unwrap (x,_,_,_,_) = x
let unwrap_mcode (x,_,_,_) = x
let rewrap (_,info,index,mcodekind,ty) x =
  (x,info,index,mcodekind,ty)
let copywrap (_,info,index,mcodekind,ty) x =
  (x,
   { line_start = info.line_start; line_end = info.line_end;
     logical_start = info.logical_start; logical_end = info.logical_end;
     attachable_start = info.attachable_start;
     attachable_end = info.attachable_end;
     mcode_start = info.mcode_start; mcode_end = info.mcode_end;
     column = info.column; offset = info.offset },
   ref !index,ref !mcodekind,ref !ty)
let get_info (_,info,_,_,_) = info
let get_index (_,_,index,_,_) = !index
let set_index (_,_,index,_,_) i = index := i
let get_mcodekind (_,_,_,mcodekind,_) = !mcodekind
let set_mcodekind (_,_,_,mcodekind,_) mk = mcodekind := mk
let set_type (_,_,_,_,ty) t = ty := t
let get_type (_,_,_,_,ty) = !ty

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

(* --------------------------------------------------------------------- *)

let rec ast0_type_to_type ty =
  match unwrap ty with
    ConstVol(cv,ty) -> Type_cocci.ConstVol(const_vol cv,ast0_type_to_type ty)
  | BaseType(bty,None) ->
      Type_cocci.BaseType(baseType bty,None)
  | BaseType(bty,Some sgn) ->
      Type_cocci.BaseType(baseType bty,Some (sign sgn))
  | Pointer(ty,_) -> Type_cocci.Pointer(ast0_type_to_type ty)
  | Array(ety,_,_,_) -> Type_cocci.Array(ast0_type_to_type ety)
  | StructUnionName(tag,su) ->
      Type_cocci.StructUnionName(unwrap_mcode tag,structUnion su)
  | TypeName(name) -> Type_cocci.TypeName(unwrap_mcode name)
  | MetaType(name,_) -> Type_cocci.MetaType(unwrap_mcode name)
  | OptType(ty) | UniqueType(ty) | MultiType(ty) ->
      ast0_type_to_type ty

and baseType t =
  match unwrap_mcode t with
    Ast.VoidType -> Type_cocci.VoidType
  | Ast.CharType -> Type_cocci.CharType
  | Ast.ShortType -> Type_cocci.ShortType
  | Ast.IntType -> Type_cocci.IntType
  | Ast.DoubleType -> Type_cocci.DoubleType
  | Ast.FloatType -> Type_cocci.FloatType
  | Ast.LongType -> Type_cocci.LongType

and structUnion t =
  match unwrap_mcode t with
    Ast.Struct -> Type_cocci.Struct
  | Ast.Union -> Type_cocci.Union

and sign t =
  match unwrap_mcode t with
    Ast.Signed -> Type_cocci.Signed
  | Ast.Unsigned -> Type_cocci.Unsigned

and const_vol t =
  match unwrap_mcode t with
    Ast.Const -> Type_cocci.Const
  | Ast.Volatile -> Type_cocci.Volatile
