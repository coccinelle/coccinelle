(* --------------------------------------------------------------------- *)
(* Modified code *)

type info = { line : int; column : int }
type line = int
type 'a wrap = ('a * line * string list (*free vars*) * dots_bef_aft)

and 'a befaft =
    BEFORE      of 'a list list
  | AFTER       of 'a list list
  | BEFOREAFTER of 'a list list * 'a list list
  | NOTHING

and 'a mcode = 'a * info * mcodekind
 and mcodekind =
    MINUS       of anything list list
  | CONTEXT     of anything befaft
  | PLUS

and dots_bef_aft = NoDots | BetweenDots of statement * int (*index of let var*)

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

and inherited = bool (* true if inherited *)

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

  | MetaId of string mcode * inherited (* true if inherited *)
  | MetaFunc of string mcode * inherited
  | MetaLocalFunc of string mcode * inherited

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

  | MetaConst      of string mcode * Type_cocci.typeC list option * inherited
  | MetaErr        of string mcode * inherited
  | MetaExpr       of string mcode * Type_cocci.typeC list option * inherited
  | MetaExprList   of string mcode * inherited (* only in arg lists *)

  | EComma         of string mcode (* only in arg lists *)

  | DisjExpr       of expression list
  | NestExpr       of expression dots * expression option

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

  | MetaType        of string mcode * inherited

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
    Init of storage mcode option * fullType * ident * string mcode (*=*) *
	initialiser * string mcode (*;*)
  | UnInit of storage mcode option * fullType * ident * string mcode (* ; *)
  | DisjDecl of declaration list

  | MetaDecl of string mcode * inherited

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
    VoidParam     of fullType
  | Param         of ident * fullType

  | MetaParam     of string mcode * inherited
  | MetaParamList of string mcode * inherited

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
    FunHeader     of bool (* true if all minus *) *
	             storage mcode option * fullType option *
	             ident (* name *) *
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
  | Break         of string mcode (* break *) * string mcode (* ; *)
  | Continue      of string mcode (* continue *) * string mcode (* ; *)
  | Return        of string mcode (* return *) * string mcode (* ; *)
  | ReturnExpr    of string mcode (* return *) * expression *
	             string mcode (* ; *)

  | MetaRuleElem  of string mcode * inherited
  | MetaStmt      of string mcode * metaStmtInfo * inherited
  | MetaStmtList  of string mcode * inherited

  | Exp           of expression

and metaStmtInfo =
    NotSequencible | SequencibleAfterDots of dots_whencode list | Sequencible

and rule_elem = base_rule_elem wrap

and base_statement =
    Seq           of rule_elem (* { *) * statement dots * bool *
	             statement dots * rule_elem (* } *)
  | IfThen        of rule_elem (* header *) * statement * mcodekind
  | IfThenElse    of rule_elem (* header *) * statement *
	             rule_elem (* else *) * statement * mcodekind
  | While         of rule_elem (* header *) * statement * mcodekind
  | Do            of rule_elem (* do *) * statement * rule_elem (* tail *)
  | For           of rule_elem (* header *) * statement * mcodekind
  | Atomic        of rule_elem
  | Disj          of statement dots list
  | Nest          of statement dots * (statement dots,statement) whencode *
	             dots_whencode list
  | FunDecl       of rule_elem (* header *) * rule_elem (* { *) *
     	             statement dots * bool * statement dots * rule_elem (* } *)
  | Dots          of string mcode (* ... *) *
	             (statement dots,statement) whencode *
	             dots_whencode list
  | Circles       of string mcode (* ooo *) *
	             (statement dots,statement) whencode *
	             dots_whencode list
  | Stars         of string mcode (* *** *) *
	             (statement dots,statement) whencode *
	             dots_whencode list
  | OptStm        of statement
  | UniqueStm     of statement
  | MultiStm      of statement (* only allowed in nests *)

and ('a,'b) whencode =
    NoWhen
  | WhenNot of 'a
  | WhenAlways of 'b

(* only used with asttoctl *)
and dots_whencode =
    WParen of rule_elem * string (*pren_var*)
  | Other of statement
  | Other_dots of statement dots

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
  | InitTag             of initialiser
  | ParameterTypeDefTag of parameterTypeDef
  | StorageTag          of storage
  | Rule_elemTag        of rule_elem
  | StatementTag        of statement
  | ConstVolTag         of const_vol
  | Token               of string
  | Code                of top_level
  | ExprDotsTag         of expression dots
  | ParamDotsTag        of parameterTypeDef dots
  | InitDotsTag         of initialiser dots
  | StmtDotsTag         of statement dots
  | TypeCTag            of typeC
  | ParamTag            of parameterTypeDef

(* --------------------------------------------------------------------- *)

let rewrap (_,l,fvs,d) x = (x,l,fvs,d)
let unwrap (x,_,_,_) = x
let unwrap_mcode (x,_,_) = x
let get_line (_,l,_,_) = l
let get_fvs (_,_,fvs,_) = fvs
let get_dots_bef_aft (_,_,_,d) = d

(* --------------------------------------------------------------------- *)

let make_meta_rule_elem s d =
  (MetaRuleElem((s,{ line = 0; column = 0 },d),false), 0, [s], NoDots)

let make_meta_decl s d =
  (MetaDecl((s,{ line = 0; column = 0 },d),false), 0, [s], NoDots)

(* --------------------------------------------------------------------- *)

let undots x =
  match unwrap x with
    DOTS    e -> e
  | CIRCLES e -> e
  | STARS   e -> e
