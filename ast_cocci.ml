type info = { line : int; logical_line : int }

(* --------------------------------------------------------------------- *)
(* Modified code *)

type 'a befaft =
    BEFORE      of 'a list list
  | AFTER       of 'a list list
  | BEFOREAFTER of 'a list list * 'a list list
  | NOTHING

type 'a mcode =
    MINUS       of 'a * info * anything list list ref
  | PLUS        of 'a * info
  | CONTEXT     of 'a * info * anything befaft ref

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
  | MetaExpDecl of arity * string (* name *)
  | MetaExpListDecl of arity * string (* name *)
  | MetaStmDecl of arity * string (* name *)
  | MetaStmListDecl of arity * string (* name *)
  | MetaFuncDecl of arity * string (* name *)
  | MetaLocalFuncDecl of arity * string (* name *)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

and 'a dots =
    DOTS of 'a list
  | CIRCLES of 'a list
  | STARS of 'a list

(* --------------------------------------------------------------------- *)
(* Identifier *)

and ident =
    Id of string mcode
  | MetaId of string mcode
  | MetaFunc of string mcode
  | MetaLocalFunc of string mcode
  | OptIdent      of ident
  | UniqueIdent   of ident
  | MultiIdent    of ident (* only allowed in nests *)

(* --------------------------------------------------------------------- *)
(* Expression *)

and expression = 
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
  | OptExp         of expression
  | UniqueExp      of expression
  | MultiExp       of expression (* only allowed in nests *)

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

and fullType = typeC
and typeC = 
    BaseType        of baseType mcode * sign mcode option
  | Pointer         of fullType * string mcode (* * *)
  | Array           of fullType * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | StructUnionName of tagged_string * structUnion mcode
  | TypeName        of string mcode
  | MetaType        of string mcode
  | OptType         of typeC
  | UniqueType      of typeC
  | MultiType       of typeC

and tagged_string = string mcode
     
and baseType = VoidType | CharType | ShortType | IntType | DoubleType
| FloatType | LongType

and structUnion = Struct | Union

and sign = Signed | Unsigned

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and declaration =
    Init of fullType * ident * string mcode (*=*) * expression *
	string mcode (*;*)
  | UnInit of fullType * ident * string mcode (* ; *)
  | OptDecl    of declaration
  | UniqueDecl of declaration
  | MultiDecl  of declaration (* only allowed in nests *)

(* --------------------------------------------------------------------- *)
(* Parameter *)

and parameterTypeDef =
    VoidParam     of fullType
  | Param         of ident * value_qualif mcode option * fullType
  | MetaParam     of string mcode
  | MetaParamList of string mcode
  | PComma        of string mcode
  | Pdots         of string mcode (* ... *)
  | Pcircles      of string mcode (* ooo *)
  | OptParam      of parameterTypeDef
  | UniqueParam   of parameterTypeDef

and parameter_list = parameterTypeDef dots

and value_qualif = Const | Volatile

(* --------------------------------------------------------------------- *)
(* Function declaration *)

and storage = Static

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and rule_elem =
    FunDecl       of storage mcode option * ident (* name *) *
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
  | Do            of string mcode (* do *)
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
  | MetaStmt      of string mcode  
  | MetaStmtList  of string mcode  
  | Disj          of rule_elem dots list
  | Nest          of rule_elem dots
  | Exp           of expression
  | Dots          of string mcode (* ... *) * rule_elem dots option
  | Circles       of string mcode (* ooo *) * rule_elem dots option
  | Stars         of string mcode (* *** *) * rule_elem dots option
  | OptRuleElem   of rule_elem list
  | UniqueRuleElem of rule_elem list
  | MultiRuleElem  of rule_elem list (* only allowed in nests *)


and top_level =
    FUNCTION of rule_elem dots
  | DECL of declaration
  | INCLUDE of string mcode (* #include *) * string mcode (* file *)
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | CODE of rule_elem dots

and rule = top_level list

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
  | ValueQualifTag      of value_qualif
  | Token               of string
  | Code                of top_level
