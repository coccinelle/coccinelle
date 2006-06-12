(* --------------------------------------------------------------------- *)
(* Modified code *)

type arity = OPT | UNIQUE | MULTI | NONE

type 'a mcode = 'a * arity * Ast_cocci.mcodekind

type logline = Good of int | Bad of int
type line_info = {logical_start : logline; logical_end : logline}

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

type 'a base_dots =
    DOTS of 'a list
  | CIRCLES of 'a list
  | STARS of 'a list

type 'a dots = 'a base_dots * line_info

(* --------------------------------------------------------------------- *)
(* Identifier *)

type ident =
    Id of string mcode
  | MetaId of string mcode
  | MetaFunc of string mcode
  | MetaLocalFunc of string mcode
  | OptIdent      of ident
  | UniqueIdent   of ident
  | MultiIdent    of ident (* only allowed in nests *)

(* --------------------------------------------------------------------- *)
(* Expression *)

type base_expression = 
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
  | Cast           of string mcode (* ( *) * fullType * string mcode (* ) *) *
                      expression
  | MetaConst      of string mcode * fullType list option
  | MetaErr        of string mcode
  | MetaExpr       of string mcode * fullType list option
  | MetaExprList   of string mcode (* only in arg lists *)
  | EComma         of string mcode (* only in arg lists *)
  | DisjExpr       of expression list
  | NestExpr       of expression dots
  | Edots          of string mcode (* ... *) * expression option
  | Ecircles       of string mcode (* ooo *) * expression option
  | Estars         of string mcode (* *** *) * expression option
  | OptExp         of expression
  | UniqueExp      of expression
  | MultiExp       of expression (* only allowed in nests *)

and expression = base_expression * line_info

(* --------------------------------------------------------------------- *)
(* Types *)

and base_fullType =
    Type            of Ast_cocci.const_vol mcode option * typeC
  | OptType         of fullType
  | UniqueType      of fullType
  | MultiType       of fullType

and base_typeC = 
    BaseType        of Ast_cocci.baseType mcode * Ast_cocci.sign mcode option
  | Pointer         of fullType * string mcode (* * *)
  | Array           of fullType * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | StructUnionName of tagged_string * Ast_cocci.structUnion mcode
  | TypeName        of string mcode
  | MetaType        of string mcode

and tagged_string = string mcode

and fullType = base_fullType * line_info
and typeC = base_typeC * line_info

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

type base_declaration =
    Init of fullType * ident * string mcode (*=*) * expression *
	string mcode (*;*)
  | UnInit of fullType * ident * string mcode (* ; *)
  | OptDecl    of declaration
  | UniqueDecl of declaration
  | MultiDecl  of declaration (* only allowed in nests *)

and declaration = base_declaration * line_info

(* --------------------------------------------------------------------- *)
(* Parameter *)

type base_parameterTypeDef =
    VoidParam     of fullType
  | Param         of ident * fullType
  | MetaParam     of string mcode
  | MetaParamList of string mcode
  | PComma        of string mcode
  | Pdots         of string mcode (* ... *)
  | Pcircles      of string mcode (* ooo *)
  | OptParam      of parameterTypeDef
  | UniqueParam   of parameterTypeDef

and parameterTypeDef = base_parameterTypeDef * line_info

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
  | Disj          of statement dots list
  | Nest          of statement dots
  | Dots          of string mcode (* ... *) * statement dots option
  | Circles       of string mcode (* ooo *) * statement dots option
  | Stars         of string mcode (* *** *) * statement dots option
  | FunDecl of Ast_cocci.storage mcode option * ident (* name *) *
	string mcode (* ( *) * parameter_list * string mcode (* ) *) *
	string mcode (* { *) * statement dots *
	string mcode (* } *)
  | OptStm   of statement
  | UniqueStm of statement
  | MultiStm  of statement (* only allowed in nests *)

and statement = base_statement * line_info

(* --------------------------------------------------------------------- *)
(* Top-level code *)

type top_level =
    FUNCTION of statement
  | DECL of declaration
  | INCLUDE of string mcode (* #include *) * string mcode (* file *)
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | ERRORWORDS of expression list
  | CODE of statement dots
  | OTHER of statement (* temporary, disappears after top_level.ml *)

type rule = top_level list

(* --------------------------------------------------------------------- *)
(* Avoid cluttering the parser.  Calculated in compute_lines.ml. *)

val wrap : 'a -> 'a * line_info
val unwrap : 'a * line_info -> 'a
val rewrap : 'a * line_info -> 'a -> 'a * line_info
