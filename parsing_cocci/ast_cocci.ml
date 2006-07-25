(* --------------------------------------------------------------------- *)
(* Modified code *)

type info = { line : int; column : int }
type line = int
type 'a wrap = ('a * line)

type 'a befaft =
    BEFORE      of 'a list list
  | AFTER       of 'a list list
  | BEFOREAFTER of 'a list list * 'a list list
  | NOTHING

type 'a mcode = 'a * info * mcodekind
 and mcodekind =
    MINUS       of anything list list
  | CONTEXT     of anything befaft
  | PLUS

(* --------------------------------------------------------------------- *)
(* Metavariables *)

and arity = UNIQUE | OPT | MULTI | NONE

and metavar =
    MetaIdDecl of arity * string (* name *)
  | MetaFreshIdDecl of arity * string (* name *)
  | MetaTypeDecl of arity * string (* name *)
  | MetaParamDecl of arity * string (* name *)
  | MetaParamListDecl of arity * string (* name *)
  | MetaConstDecl of arity * string (* name *)
  | MetaErrDecl of arity * string (* name *)
  | MetaExpDecl of arity * string (* name *)
  | MetaExpListDecl of arity * string (* name *)
  | MetaStmDecl of arity * string (* name *)
  | MetaStmListDecl of arity * string (* name *)
  | MetaFuncDecl of arity * string (* name *)
  | MetaLocalFuncDecl of arity * string (* name *)

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
  | Constant       of constant mcode
  | FunCall        of expression * string mcode (* ( *) *
                      expression dots * string mcode (* ) *)
  | Assignment     of expression * assignOp mcode * expression
  | CondExpr       of expression * string mcode (* ? *) * expression option *
	              string mcode (* : *) * expression
  | Postfix        of expression * fixOp mcode
  | Infix          of expression * fixOp mcode
  | Unary          of expression * unaryOp mcode
  | Binary         of expression * binaryOp mcode * expression
  | ArrayAccess    of expression * string mcode (* [ *) * expression *
	              string mcode (* ] *)
  | RecordAccess   of expression * string mcode (* . *) * ident
  | RecordPtAccess of expression * string mcode (* -> *) * ident
  | Cast           of string mcode (* ( *) * fullType * string mcode (* ) *) *
                      expression
  | SizeOfExpr     of string mcode (* sizeof *) * expression
  | SizeOfType     of string mcode (* sizeof *) * string mcode (* ( *) *
                      fullType * string mcode (* ) *)

  | Paren          of string mcode (* ( *) * expression *
                      string mcode (* ) *)

  | MetaConst      of string mcode * Type_cocci.typeC list option
  | MetaErr        of string mcode
  | MetaExpr       of string mcode * Type_cocci.typeC list option
  | MetaExprList   of string mcode (* only in arg lists *)

  | EComma         of string mcode (* only in arg lists *)

  | DisjExpr       of expression list
  | NestExpr       of expression dots

  (* can appear in arg lists, and also inside Nest, as in:
   if(< ... X ... Y ...>)
   In the following, the expression option is the WHEN  *)
  | Edots          of string mcode (* ... *) * expression option
  | Ecircles       of string mcode (* ooo *) * expression option
  | Estars         of string mcode (* *** *) * expression option

  | OptExp         of expression
  | UniqueExp      of expression
  | MultiExp       of expression (* only allowed in nests *)

and expression = base_expression wrap

and  unaryOp = GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not
and  assignOp = SimpleAssign | OpAssign of arithOp
and  fixOp = Dec | Inc

and  binaryOp = Arith of arithOp | Logical of logicalOp
and  arithOp =
    Plus | Minus | Mul | Div | Mod | DecLeft | DecRight | And | Or | Xor
and  logicalOp = Inf | Sup | InfEq | SupEq | Eq | NotEq | AndLog | OrLog

and constant =
    String of string
  | Char   of string
  | Int    of string
  | Float  of string

(* --------------------------------------------------------------------- *)
(* Types *)

and base_fullType =
    Type            of const_vol mcode option * typeC
  | OptType         of fullType
  | UniqueType      of fullType
  | MultiType       of fullType

and base_typeC = 
    BaseType        of baseType mcode * sign mcode option
  | Pointer         of fullType * string mcode (* * *)
  | Array           of fullType * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | StructUnionName of tagged_string * structUnion mcode
  | TypeName        of string mcode

  | MetaType        of string mcode

and fullType = base_fullType wrap
and typeC = base_typeC wrap

and tagged_string = string mcode
     
and baseType = VoidType | CharType | ShortType | IntType | DoubleType
| FloatType | LongType

and structUnion = Struct | Union

and sign = Signed | Unsigned

and const_vol = Const | Volatile

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and base_declaration =
    Init of fullType * ident * string mcode (*=*) * expression *
	string mcode (*;*)
  | UnInit of fullType * ident * string mcode (* ; *)
  | DisjDecl   of declaration list

  | OptDecl    of declaration
  | UniqueDecl of declaration
  | MultiDecl  of declaration (* only allowed in nests *)

and declaration = base_declaration wrap

(* --------------------------------------------------------------------- *)
(* Parameter *)

and base_parameterTypeDef =
    VoidParam     of fullType
  | Param         of ident * fullType

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
(* Function declaration *)

and storage = Static

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and base_rule_elem =
    FunHeader     of storage mcode option * ident (* name *) *
	             string mcode (* ( *) * parameter_list *
                     string mcode (* ) *)
  | Decl          of declaration

  | SeqStart      of string mcode (* { *)
  | SeqEnd        of string mcode (* } *)

  | ExprStatement of expression * string mcode (*;*)
  | IfHeader      of string mcode (* if *) * string mcode (* ( *) *
	             expression * string mcode (* ) *)
  | Else          of string mcode (* else *)
  | WhileHeader   of string mcode (* while *) * string mcode (* ( *) *
	             expression * string mcode (* ) *)
  | DoHeader      of string mcode (* do *)
  | WhileTail     of string mcode (* while *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
                     string mcode (* ; *)
  | ForHeader     of string mcode (* for *) * string mcode (* ( *) *
                     expression option * string mcode (*;*) *
	             expression option * string mcode (*;*) *
                     expression option * string mcode (* ) *)
  | Return        of string mcode (* return *) * string mcode (* ; *)
  | ReturnExpr    of string mcode (* return *) * expression *
	             string mcode (* ; *)

  | MetaRuleElem  of string mcode  
  | MetaStmt      of string mcode  
  | MetaStmtList  of string mcode  

  | Exp           of expression

and rule_elem = base_rule_elem wrap

and base_statement =
    Seq           of rule_elem (* { *) * statement dots * rule_elem (* } *)
  | IfThen        of rule_elem (* header *) * statement
  | IfThenElse    of rule_elem (* header *) * statement *
	             rule_elem (* else *) * statement
  | While         of rule_elem (* header *) * statement
  | Do            of rule_elem (* do *) * statement * rule_elem (* tail *)
  | For           of rule_elem (* header *) * statement
  | Atomic        of rule_elem
  | Disj          of statement dots list
  | Nest          of statement dots
  | FunDecl       of rule_elem (* header *) * rule_elem (* { *) *
     	             statement dots * rule_elem (* } *)
  | Dots          of
      string mcode (* ... *) * statement dots list * statement list
  | Circles       of
      string mcode (* ooo *) * statement dots list * statement list
  | Stars         of
      string mcode (* *** *) * statement dots list * statement list
  | OptStm        of statement
  | UniqueStm     of statement
  | MultiStm      of statement (* only allowed in nests *)

and statement = base_statement wrap

and base_top_level =
    FUNCTION of statement
  | DECL of declaration
  | INCLUDE of string mcode (* #include *) * string mcode (* file *)
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | ERRORWORDS of expression list
  | CODE of statement dots

and top_level = base_top_level wrap

and rule = top_level list

and rule_with_metavars = metavar list * rule

and anything =
    FullTypeTag         of fullType
  | BaseTypeTag         of baseType
  | StructUnionTag      of structUnion
  | SignTag             of sign
  | IdentTag            of ident
  | ExpressionTag       of expression
  | ConstantTag         of constant
  | UnaryOpTag          of unaryOp
  | AssignOpTag         of assignOp
  | FixOpTag            of fixOp
  | BinaryOpTag         of binaryOp
  | ArithOpTag          of arithOp
  | LogicalOpTag        of logicalOp
  | DeclarationTag      of declaration
  | ParameterTypeDefTag of parameterTypeDef
  | StorageTag          of storage
  | Rule_elemTag        of rule_elem
  | StatementTag        of statement
  | ConstVolTag         of const_vol
  | Token               of string
  | Code                of top_level
  | ExprDotsTag         of expression dots
  | ParamDotsTag        of parameterTypeDef dots
  | StmtDotsTag         of statement dots
  | TypeCTag            of typeC
  | ParamTag            of parameterTypeDef

(* --------------------------------------------------------------------- *)

let rewrap (_,l) x = (x,l)
let unwrap (x,_) = x
let get_line (_,l) = l

(* --------------------------------------------------------------------- *)

let make_meta_rule_elem s d =
  (MetaRuleElem(s,{ line = 0; column = 0 },d), 0)

(* --------------------------------------------------------------------- *)

let undots x =
  match unwrap x with
    DOTS    e -> e
  | CIRCLES e -> e
  | STARS   e -> e
