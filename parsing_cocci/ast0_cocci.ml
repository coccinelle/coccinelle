module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)
(* Modified code *)

type arity = OPT | UNIQUE | MULTI | NONE

type 'a mcode = 'a * arity * Ast.mcodekind

type logline = Good of int | Bad of int
type line_info = {logical_start : logline; logical_end : logline}

(* --------------------------------------------------------------------- *)

type token_info =
    (* a tree of all minus tokens *)
    AllMinus
    (* a context node where all subtrees have the same set of context tokens
       in the minus and plus trees *)
  | BindContext (* the context children *)
    (* neither of the above cases *)
  | Neither

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

type 'a base_dots =
    DOTS of 'a list
  | CIRCLES of 'a list
  | STARS of 'a list

type 'a dots = 'a base_dots * line_info * token_info ref

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

and ident = base_ident * line_info * token_info ref

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

and expression = base_expression * line_info * token_info ref

(* --------------------------------------------------------------------- *)
(* Types *)

and base_fullType =
    Type            of Ast.const_vol mcode option * typeC
  | OptType         of fullType
  | UniqueType      of fullType
  | MultiType       of fullType

and base_typeC = 
    BaseType        of Ast.baseType mcode * Ast.sign mcode option
  | Pointer         of fullType * string mcode (* * *)
  | Array           of fullType * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | StructUnionName of tagged_string * Ast.structUnion mcode
  | TypeName        of string mcode
  | MetaType        of string mcode

and tagged_string = string mcode

and fullType = base_fullType * line_info * token_info ref
and typeC = base_typeC * line_info * token_info ref

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

and declaration = base_declaration * line_info * token_info ref

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

and parameterTypeDef = base_parameterTypeDef * line_info * token_info ref

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
  | FunDecl of Ast.storage mcode option * ident (* name *) *
	string mcode (* ( *) * parameter_list * string mcode (* ) *) *
	string mcode (* { *) * statement dots *
	string mcode (* } *)
  | OptStm   of statement
  | UniqueStm of statement
  | MultiStm  of statement (* only allowed in nests *)

and statement = base_statement * line_info * token_info ref

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

and top_level = base_top_level * line_info * token_info ref
and rule = top_level list

(* --------------------------------------------------------------------- *)
(* Avoid cluttering the parser.  Calculated in compute_lines.ml. *)

let wrap x =
  (x,{logical_start = Good (-1); logical_end = Good (-1)},ref Neither)
let unwrap (x,_,_) = x
let rewrap (_,info,tinfo) x = (x,info,tinfo)
let starting_line (_,info,_) = info.logical_start
let ending_line (_,info,_) = info.logical_end
let get_info (_,info,_) = info
let get_tinfo (_,_,tinfo) = !tinfo
let set_tinfo (_,_,tinfo) ti = tinfo := ti

(* --------------------------------------------------------------------- *)

let undots d =
  match unwrap d with
  | DOTS    e -> e
  | CIRCLES e -> e
  | STARS   e -> e
