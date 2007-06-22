(* --------------------------------------------------------------------- *)
(* Modified code *)

type info = { line : int; column : int;
	      strbef : string list; straft : string list }
type line = int
type meta_name = string * string
type 'a wrap =
    ('a * line * meta_name list (*free vars*) * meta_name list (*fresh vars*) *
       meta_name list (*inherited vars*) * meta_name list (*witness vars*) *
       dots_bef_aft)

and 'a befaft =
    BEFORE      of 'a list list
  | AFTER       of 'a list list
  | BEFOREAFTER of 'a list list * 'a list list
  | NOTHING

and 'a mcode = 'a * info * mcodekind
 (* pos is an offset indicating where in the C code the mcodekind has an
 effect *)
 and mcodekind =
    MINUS       of pos * anything list list
  | CONTEXT     of pos * anything befaft
  | PLUS
 and pos = NoPos | DontCarePos | FixPos of (int * int)

and dots_bef_aft = NoDots | BetweenDots of statement * int (*index of let var*)

and inherited = Type_cocci.inherited
and keep_binding = Type_cocci.keep_binding

and end_info =
    meta_name list (*free vars*) * meta_name list (*inherited vars*) *
      meta_name list (*witness vars*) * mcodekind

(* --------------------------------------------------------------------- *)
(* Metavariables *)

and arity = UNIQUE | OPT | MULTI | NONE

and metavar =
    MetaIdDecl of arity * (string * string) (* name *)
  | MetaFreshIdDecl of arity * (string * string) (* name *)
  | MetaTypeDecl of arity * (string * string) (* name *)
  | MetaParamDecl of arity * (string * string) (* name *)
  | MetaParamListDecl of arity * (string * string) (* name *)
  | MetaConstDecl of
      arity * (string * string) (* name *) * Type_cocci.typeC list option
  | MetaErrDecl of arity * (string * string) (* name *)
  | MetaExpDecl of
      arity * (string * string) (* name *) * Type_cocci.typeC list option
  | MetaExpListDecl of arity * (string * string) (* name *)
  | MetaStmDecl of arity * (string * string) (* name *)
  | MetaStmListDecl of arity * (string * string) (* name *)
  | MetaFuncDecl of arity * (string * string) (* name *)
  | MetaLocalFuncDecl of arity * (string * string) (* name *)

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

  | MetaId        of (string * string) mcode * keep_binding * inherited
  | MetaFunc      of (string * string) mcode * keep_binding * inherited
  | MetaLocalFunc of (string * string) mcode * keep_binding * inherited

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
  | TypeExp        of fullType

  | Paren          of string mcode (* ( *) * expression *
                      string mcode (* ) *)

  | MetaConst      of (string * string) mcode * keep_binding *
	              Type_cocci.typeC list option * inherited
  | MetaErr        of (string * string) mcode * keep_binding * inherited
  | MetaExpr       of (string * string) mcode * keep_binding *
	              Type_cocci.typeC list option * inherited
  | MetaExprList   of (string * string) mcode * keep_binding *
	              inherited (* only in arg lists *)

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
  | DisjType        of fullType list (* only after iso *)
  | OptType         of fullType
  | UniqueType      of fullType
  | MultiType       of fullType

and base_typeC = 
    BaseType        of baseType mcode * sign mcode option
  | ImplicitInt     of sign mcode
  | Pointer         of fullType * string mcode (* * *)
  | FunctionPointer of fullType *
	          string mcode(* ( *)*string mcode(* * *)*string mcode(* ) *)*
                  string mcode (* ( *)*parameter_list*string mcode(* ) *)
  | FunctionType     of bool (* true if all minus for dropping return type *) *
                   fullType option *
	           string mcode (* ( *) * parameter_list *
                   string mcode (* ) *)
  | Array           of fullType * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | StructUnionName of structUnion mcode * ident (* name *)
  | StructUnionDef  of fullType (* either StructUnionName or metavar *) *
	string mcode (* { *) * declaration dots * string mcode (* } *)
  | TypeName        of string mcode

  | MetaType        of (string * string) mcode * keep_binding * inherited

and fullType = base_fullType wrap
and typeC = base_typeC wrap
     
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
  | TyDecl of fullType * string mcode (* ; *)
  | MacroDecl of string mcode (* name *) * string mcode (* ( *) *
        expression dots * string mcode (* ) *) * string mcode (* ; *)
  | DisjDecl   of declaration list
  | Ddots    of string mcode (* ... *) * declaration option (* whencode *)

  | MetaDecl of (string * string) mcode * keep_binding * inherited

  | OptDecl    of declaration
  | UniqueDecl of declaration
  | MultiDecl  of declaration (* only allowed in nests *)

and declaration = base_declaration wrap

(* --------------------------------------------------------------------- *)
(* Initializers *)

and base_initialiser =
    InitExpr of expression 
  | InitList of string mcode (*{*) * initialiser list * string mcode (*}*) *
	initialiser list (* whencode: elements that shouldn't appear in init *)
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
  | IComma of string mcode (* , *)
  | OptIni    of initialiser
  | UniqueIni of initialiser
  | MultiIni  of initialiser

and initialiser = base_initialiser wrap

(* --------------------------------------------------------------------- *)
(* Parameter *)

and base_parameterTypeDef =
    VoidParam     of fullType
  | Param         of fullType * ident option

  | MetaParam     of (string * string) mcode * keep_binding * inherited
  | MetaParamList of (string * string) mcode * keep_binding * inherited

  | PComma        of string mcode

  | Pdots         of string mcode (* ... *)
  | Pcircles      of string mcode (* ooo *)

  | OptParam      of parameterTypeDef
  | UniqueParam   of parameterTypeDef

and parameterTypeDef = base_parameterTypeDef wrap

and parameter_list = parameterTypeDef dots

(* --------------------------------------------------------------------- *)
(* Function declaration *)

and storage = Static | Auto | Register | Extern

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and base_rule_elem =
    FunHeader     of mcodekind (* before the function header *) *
	             bool (* true if all minus, for dropping static, etc *) *
	             fninfo list * ident (* name *) *
	             string mcode (* ( *) * parameter_list *
                     string mcode (* ) *)
  | Decl          of mcodekind (* before the decl *) * 
                     bool (* true if all minus *) * declaration

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
  | SwitchHeader  of string mcode (* switch *) * string mcode (* ( *) *
	             expression * string mcode (* ) *)
  | Break         of string mcode (* break *) * string mcode (* ; *)
  | Continue      of string mcode (* continue *) * string mcode (* ; *)
  | Goto             (* not in the source language *)
  | Return        of string mcode (* return *) * string mcode (* ; *)
  | ReturnExpr    of string mcode (* return *) * expression *
	             string mcode (* ; *)

  | MetaRuleElem  of (string * string) mcode * keep_binding * inherited
  | MetaStmt      of (string * string) mcode * keep_binding * metaStmtInfo *
	             inherited
  | MetaStmtList  of (string * string) mcode * keep_binding * inherited

  | Exp           of expression
  | Ty            of fullType (* only at top level *)
  | Include       of string mcode (*#include*) * inc_file mcode (*file *)
  | DefineHeader  of string mcode (* #define *) * ident (* name *) *
	             string mcode list option (*params*)
  | Case          of string mcode (* case *) * expression * string mcode (*:*)
  | Default       of string mcode (* default *) * string mcode (*:*)

and fninfo =
    FStorage of storage mcode
  | FType of fullType
  | FInline of string mcode
  | FAttr of string mcode

and metaStmtInfo =
    NotSequencible | SequencibleAfterDots of dots_whencode list | Sequencible

and rule_elem = base_rule_elem wrap

and base_statement =
    Seq           of rule_elem (* { *) * statement dots * bool *
	             statement dots * rule_elem (* } *)
  | IfThen        of rule_elem (* header *) * statement * end_info
  | IfThenElse    of rule_elem (* header *) * statement *
	             rule_elem (* else *) * statement * end_info
  | While         of rule_elem (* header *) * statement * end_info
  | Do            of rule_elem (* do *) * statement * rule_elem (* tail *)
  | For           of rule_elem (* header *) * statement * end_info
  | Switch        of rule_elem (* header *) * rule_elem (* { *) *
	             case_line list * rule_elem (* } *)
  | Atomic        of rule_elem
  | Disj          of statement dots list
  | Nest          of statement dots * (statement dots,statement) whencode *
	             dots_whencode list
  | FunDecl       of rule_elem (* header *) * rule_elem (* { *) *
     	             statement dots * bool * statement dots * rule_elem (* } *)
  | Define        of rule_elem (* header *) * statement dots
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

and dots_whencode =
    WParen of rule_elem * meta_name (*pren_var*)
  | Other of statement
  | Other_dots of statement dots

and statement = base_statement wrap

and base_case_line =
    CaseLine of rule_elem (* case/default header *) * statement dots
  | OptCase of case_line

and case_line = base_case_line wrap

and inc_file =
    Local of inc_elem list
  | NonLocal of inc_elem list

and inc_elem =
    IncPath of string
  | IncDots

and base_top_level =
    DECL of statement
  | CODE of statement dots
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | ERRORWORDS of expression list

and top_level = base_top_level wrap

and rule = string * dependency list * top_level list

and dependency = Dep of string | AntiDep of string

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
  | StorageTag          of storage
  | IncFileTag          of inc_file
  | Rule_elemTag        of rule_elem
  | StatementTag        of statement
  | CaseLineTag         of case_line
  | ConstVolTag         of const_vol
  | Token               of string * info option
  | Code                of top_level
  | ExprDotsTag         of expression dots
  | ParamDotsTag        of parameterTypeDef dots
  | StmtDotsTag         of statement dots
  | DeclDotsTag         of declaration dots
  | TypeCTag            of typeC
  | ParamTag            of parameterTypeDef
  | SgrepStartTag       of string
  | SgrepEndTag         of string

val mkToken : string -> anything

val undots : 'a dots -> 'a list

val rewrap : 'a wrap -> 'b -> 'b wrap
val unwrap : 'a wrap -> 'a
val unwrap_mcode : 'a mcode -> 'a
val get_line : 'a wrap -> line
val get_mcode_line : 'a mcode -> line
val get_fvs : 'a wrap -> meta_name list
val get_fresh : 'a wrap -> meta_name list
val get_inherited : 'a wrap -> meta_name list
val get_saved : 'a wrap -> meta_name list
val get_dots_bef_aft : statement -> dots_bef_aft
val rewrap_dots_bef_aft : statement -> dots_bef_aft -> statement

val get_meta_name : metavar -> meta_name

val no_info : info

val make_meta_rule_elem :
    string -> mcodekind ->
      (meta_name list * meta_name list * meta_name list) ->
      rule_elem

val make_meta_decl :
    string -> mcodekind ->
      (meta_name list * meta_name list * meta_name list) ->
      declaration

val make_term : 'a -> 'a wrap
val make_mcode : 'a -> 'a mcode
