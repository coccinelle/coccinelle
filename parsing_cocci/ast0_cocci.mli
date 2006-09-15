(* --------------------------------------------------------------------- *)
(* Modified code *)

type arity = OPT | UNIQUE | MULTI | NONE

type token_info = { tline_start : int; tline_end : int }
val default_token_info : token_info

type mcodekind =
    MINUS       of (Ast_cocci.anything list list * token_info) ref
  | PLUS
  | CONTEXT     of (Ast_cocci.anything Ast_cocci.befaft *
		      token_info * token_info) ref
  | MIXED       of (Ast_cocci.anything Ast_cocci.befaft *
		      token_info * token_info) ref

type info = { line_start : int; line_end : int;
	      logical_start : int; logical_end : int;
	      attachable_start : bool; attachable_end : bool;
	      mcode_start : mcodekind list; mcode_end : mcodekind list;
	      column : int; offset : int }

type 'a mcode = 'a * arity * info * mcodekind
type 'a wrap = 'a * info * int ref * mcodekind ref
      * Type_cocci.typeC option ref (* only for expressions *)
      * dots_bef_aft

and dots_bef_aft = NoDots | BetweenDots of statement

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
  | MetaId of string mcode
  | MetaFunc of string mcode
  | MetaLocalFunc of string mcode
  | OptIdent      of ident
  | UniqueIdent   of ident
  | MultiIdent    of ident (* only allowed in nests *)

and ident = base_ident wrap

(* --------------------------------------------------------------------- *)
(* Expression *)

and base_expression = 
    Ident          of ident
  | Constant       of Ast_cocci.constant mcode
  | FunCall        of expression * string mcode (* ( *) *
                      expression dots * string mcode (* ) *)
  | Assignment     of expression * Ast_cocci.assignOp mcode * expression
  | CondExpr       of expression * string mcode (* ? *) * expression option *
	              string mcode (* : *) * expression
  | Postfix        of expression * Ast_cocci.fixOp mcode
  | Infix          of expression * Ast_cocci.fixOp mcode
  | Unary          of expression * Ast_cocci.unaryOp mcode
  | Binary         of expression * Ast_cocci.binaryOp mcode * expression
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
  | MetaConst      of string mcode * Type_cocci.typeC list option
  | MetaErr        of string mcode
  | MetaExpr       of string mcode * Type_cocci.typeC list option
  | MetaExprList   of string mcode (* only in arg lists *)
  | EComma         of string mcode (* only in arg lists *)
  | DisjExpr       of string mcode * expression list * string mcode list *
	              string mcode
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
    ConstVol        of Ast_cocci.const_vol mcode * typeC
  | BaseType        of Ast_cocci.baseType mcode * Ast_cocci.sign mcode option
  | Pointer         of typeC * string mcode (* * *)
  | Array           of typeC * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | StructUnionName of tagged_string * Ast_cocci.structUnion mcode
  | TypeName        of string mcode
  | MetaType        of string mcode
  | OptType         of typeC
  | UniqueType      of typeC
  | MultiType       of typeC

and tagged_string = string mcode

and typeC = base_typeC wrap

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and base_declaration =
    Init of typeC * ident * string mcode (*=*) * expression *
	string mcode (*;*)
  | UnInit of typeC * ident * string mcode (* ; *)
  | DisjDecl   of string mcode * declaration list * string mcode list *
	          string mcode
  | OptDecl    of declaration
  | UniqueDecl of declaration
  | MultiDecl  of declaration (* only allowed in nests *)

and declaration = base_declaration wrap

(* --------------------------------------------------------------------- *)
(* Parameter *)

and base_parameterTypeDef =
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

and base_statement =
    Decl          of declaration
  | Seq           of string mcode (* { *) * statement dots *
 	             string mcode (* } *)
  | ExprStatement of expression * string mcode (*;*)
  | IfThen        of string mcode (* if *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
	             statement * (info * mcodekind)
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
  | Break         of string mcode (* break *) * string mcode (* ; *)
  | Continue      of string mcode (* continue *) * string mcode (* ; *)
  | Return        of string mcode (* return *) * string mcode (* ; *)
  | ReturnExpr    of string mcode (* return *) * expression *
	             string mcode (* ; *)
  | MetaStmt      of string mcode
  | MetaStmtList  of string mcode (* only in statement lists *)
  | Exp           of expression  (* only in dotted statement lists *)
  | Disj          of string mcode * statement dots list * string mcode list *
	             string mcode
  | Nest          of string mcode * statement dots * string mcode *
	             statement dots option
  | Dots          of string mcode (* ... *) *
                     (statement dots,statement) whencode
  | Circles       of string mcode (* ooo *) *
	             (statement dots,statement) whencode
  | Stars         of string mcode (* *** *) *
	             (statement dots,statement) whencode
  | FunDecl of
        Ast_cocci.storage mcode option * typeC option * ident (* name *) *
	string mcode (* ( *) * parameter_list * string mcode (* ) *) *
	string mcode (* { *) * statement dots *
	string mcode (* } *)
  | OptStm   of statement
  | UniqueStm of statement
  | MultiStm  of statement (* only allowed in nests *)

and ('a,'b) whencode =
    NoWhen
  | WhenNot of 'a
  | WhenAlways of 'b

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

val dotsExpr : expression dots -> anything
val dotsParam : parameterTypeDef dots -> anything
val dotsStmt : statement dots -> anything
val ident : ident -> anything
val expr : expression -> anything
val typeC : typeC -> anything
val param : parameterTypeDef -> anything
val decl : declaration -> anything
val stmt : statement -> anything
val top : top_level -> anything

(* --------------------------------------------------------------------- *)

val undots : 'a dots -> 'a list

(* --------------------------------------------------------------------- *)
(* Avoid cluttering the parser.  Calculated in compute_lines.ml. *)

val default_info : unit -> info
val default_befaft : unit -> mcodekind
val context_befaft : unit -> mcodekind
val wrap : 'a -> 'a wrap
val context_wrap : 'a -> 'a wrap
val unwrap : 'a wrap -> 'a
val unwrap_mcode : 'a mcode -> 'a
val rewrap : 'a wrap -> 'b -> 'b wrap
val copywrap : 'a wrap -> 'a -> 'a wrap
val get_info : 'a wrap -> info
val get_index : 'a wrap -> int
val set_index : 'a wrap -> int -> unit
val get_mcodekind : 'a wrap -> mcodekind
val get_mcodekind_ref : 'a wrap -> mcodekind ref
val set_mcodekind : 'a wrap -> mcodekind -> unit
val set_type : 'a wrap -> Type_cocci.typeC option -> unit
val get_type : 'a wrap -> Type_cocci.typeC option
val set_dots_bef_aft : statement -> dots_bef_aft -> statement
val get_dots_bef_aft : 'a wrap -> dots_bef_aft
val fresh_index : unit -> int

val ast0_type_to_type : typeC -> Type_cocci.typeC
