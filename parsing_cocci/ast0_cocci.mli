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

(* for iso metavariables, true if they can only match nonmodified, unitary
   metavariables
   for SP metavariables, true if the metavariable is unitary (valid up to
   isomorphism phase only) *)
and pure = bool

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
  | MetaId        of string mcode * pure
  | MetaFunc      of string mcode * pure
  | MetaLocalFunc of string mcode * pure
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
  | TypeExp        of typeC
  | MetaConst      of string mcode *
	              Type_cocci.typeC list option * pure
  | MetaErr        of string mcode * pure
  | MetaExpr       of string mcode *
	              Type_cocci.typeC list option * pure
  | MetaExprList   of string mcode(*only in arglists*) * pure
  | EComma         of string mcode (* only in arglists *)
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
  | StructUnionName of Ast_cocci.structUnion mcode * ident (* name *)
  | StructUnionDef  of Ast_cocci.structUnion mcode * ident (* name *) *
	string mcode (* { *) * declaration list * string mcode (* } *)
  | TypeName        of string mcode
  | MetaType        of string mcode * pure
  | OptType         of typeC
  | UniqueType      of typeC
  | MultiType       of typeC

and typeC = base_typeC wrap

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and base_declaration =
    Init       of Ast_cocci.storage mcode option * typeC * ident *
	string mcode (*=*) * initialiser * string mcode (*;*)
  | UnInit     of Ast_cocci.storage mcode option * typeC * ident *
	string mcode (* ; *)
  | TyDecl of typeC * string mcode (* ; *)
  | DisjDecl   of string mcode * declaration list * string mcode list *
	          string mcode
  | OptDecl    of declaration
  | UniqueDecl of declaration
  | MultiDecl  of declaration (* only allowed in nests *)

and declaration = base_declaration wrap

(* --------------------------------------------------------------------- *)
(* Initializers *)

and base_initialiser =
    InitExpr of expression 
  | InitList of string mcode (*{*) * initialiser_list * string mcode (*}*)
  | InitGccDotName of
      string mcode (*.*) * ident (* name *) * string mcode (*=*) *
	initialiser (* gccext: *)
  | InitGccName of ident (* name *) * string mcode (*:*) *
	initialiser
  | InitGccIndex of
      string mcode (*[*) * expression * string mcode (*]*) *
	string mcode (*=*) * initialiser
  | InitGccRange of
      string mcode (*[*) * expression * string mcode (*...*) *
        expression * string mcode (*]*) * string mcode (*=*) * initialiser
  | IComma of string mcode
  | Idots  of string mcode (* ... *) * initialiser option (* whencode *)
  | OptIni    of initialiser
  | UniqueIni of initialiser
  | MultiIni  of initialiser

and initialiser = base_initialiser wrap

and initialiser_list = initialiser dots

(* --------------------------------------------------------------------- *)
(* Parameter *)

and base_parameterTypeDef =
    VoidParam     of typeC
  | Param         of ident * typeC
  | MetaParam     of string mcode * pure
  | MetaParamList of string mcode * pure
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
    Decl          of (info * mcodekind) (* before the decl *) * declaration
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
  | MetaStmt      of string mcode * pure
  | MetaStmtList  of string mcode (* only in statement lists *) * pure
  | Exp           of expression  (* only in dotted statement lists *)
  | Ty            of typeC (* only at top level *)
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
  | FunDecl of (info * mcodekind) (* before the function decl *) *
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
(* CPP code *)

and base_meta =
    Include of string mcode (* #include *) * string mcode (* file *)
  | Define of string mcode (* #define *) * ident (* name *) * define_body
  | OptMeta        of meta
  | UniqueMeta     of meta
  | MultiMeta      of meta

and meta = base_meta wrap

and base_define_body =
    DMetaId of string mcode * pure
  | Ddots   of string mcode (* ... *)

and define_body = base_define_body wrap

(* --------------------------------------------------------------------- *)
(* Top-level code *)

type base_top_level =
    FUNCTION of statement
  | DECL of (info * mcodekind) (* before the decl *) * declaration
  | META of meta
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | ERRORWORDS of expression list
  | CODE of statement dots
  | OTHER of statement (* temporary, disappears after top_level.ml *)

and top_level = base_top_level wrap
and rule = top_level list

(* --------------------------------------------------------------------- *)

type anything =
    DotsExprTag of expression dots
  | DotsInitTag of initialiser dots
  | DotsParamTag of parameterTypeDef dots
  | DotsStmtTag of statement dots
  | IdentTag of ident
  | ExprTag of expression
  | TypeCTag of typeC
  | ParamTag of parameterTypeDef
  | InitTag of initialiser
  | DeclTag of declaration
  | StmtTag of statement
  | MetaTag of meta
  | TopTag of top_level

val dotsExpr : expression dots -> anything
val dotsInit : initialiser dots -> anything
val dotsParam : parameterTypeDef dots -> anything
val dotsStmt : statement dots -> anything
val ident : ident -> anything
val expr : expression -> anything
val typeC : typeC -> anything
val param : parameterTypeDef -> anything
val ini : initialiser -> anything
val decl : declaration -> anything
val stmt : statement -> anything
val meta : meta -> anything
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
val get_mcode_mcodekind : 'a mcode -> mcodekind
val get_mcodekind_ref : 'a wrap -> mcodekind ref
val set_mcodekind : 'a wrap -> mcodekind -> unit
val set_type : 'a wrap -> Type_cocci.typeC option -> unit
val get_type : 'a wrap -> Type_cocci.typeC option
val set_dots_bef_aft : statement -> dots_bef_aft -> statement
val get_dots_bef_aft : 'a wrap -> dots_bef_aft
val fresh_index : unit -> int
val set_mcode_data : 'a -> 'a mcode -> 'a mcode

val ast0_type_to_type : typeC -> Type_cocci.typeC
