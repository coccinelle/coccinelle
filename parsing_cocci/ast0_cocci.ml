module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)
(* Modified code *)

type line_type = CONTEXT | MINUS | PLUS
type arity = OPT | UNIQUE | MULTI | NONE

type 'a mcode =
    'a * line_type * arity * int (* real_line *) * int (* logical_line *)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

type 'a dots =
    DOTS of 'a list
  | CIRCLES of 'a list
  | STARS of 'a list

(* --------------------------------------------------------------------- *)
(* Identifier *)

type ident =
    Id of string mcode
  | MetaId of string mcode
  | MetaFunc of string mcode
  | MetaLocalFunc of string mcode

(* --------------------------------------------------------------------- *)
(* Expression *)

type expression = 
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
  | Cast           of string mcode (* ( *) * fullType * string mcode (* ) *) *
                      expression
  | MetaConst      of string mcode * fullType list option
  | MetaExpr       of string mcode * fullType list option
  | MetaExprList   of string mcode (* only in arg lists *)
  | EComma         of string mcode (* only in arg lists *)
  | DisjExpr       of expression list
  | NestExpr       of expression dots
  | Edots          of string mcode (* ... *) * expression option
  | Ecircles       of string mcode (* ooo *) * expression option
  | Estars         of string mcode (* *** *) * expression option

(* --------------------------------------------------------------------- *)
(* Types *)

and fullType = typeC
and typeC = 
    BaseType        of Ast.baseType mcode * Ast.sign mcode option
  | Pointer         of fullType * string mcode (* * *)
  | Array           of fullType * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | StructUnionName of tagged_string * Ast.structUnion mcode
  | TypeName        of string mcode
  | MetaType        of string mcode

and tagged_string = string mcode

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

type declaration =
    Init of fullType * ident * string mcode (*=*) * expression *
	string mcode (*;*)
  | UnInit of fullType * ident * string mcode (* ; *)

(* --------------------------------------------------------------------- *)
(* Parameter *)

type parameterTypeDef =
    VoidParam     of fullType
  | Param         of ident * Ast.value_qualif mcode option * fullType
  | MetaParam     of string mcode
  | MetaParamList of string mcode
  | PComma        of string mcode
  | Pdots         of string mcode (* ... *)
  | Pcircles      of string mcode (* ooo *)

and parameter_list = parameterTypeDef dots

(* --------------------------------------------------------------------- *)
(* Statement*)

type statement =
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
  | FunDecl of Ast.storage mcode option * ident (* name *) *
	string mcode (* ( *) * parameter_list * string mcode (* ) *) *
	string mcode (* { *) * statement dots *
	string mcode (* } *)

(* --------------------------------------------------------------------- *)
(* Top-level code *)

type top_level =
    FUNCTION of statement
  | DECL of declaration
  | INCLUDE of string mcode (* #include *) * string mcode (* file *)
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | CODE of statement dots
  | OTHER of statement (* temporary, disappears after top_level.ml *)

type rule = top_level list
