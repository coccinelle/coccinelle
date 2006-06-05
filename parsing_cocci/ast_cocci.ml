type info = { line : int; logical_line : int }

(* --------------------------------------------------------------------- *)
(* Modified code *)

type 'a befaft =
    BEFORE      of 'a list list
  | AFTER       of 'a list list
  | BEFOREAFTER of 'a list list * 'a list list
  | NOTHING

type 'a mcode = 'a * mcodekind
 and mcodekind =
    MINUS       of info * anything list list ref
  | PLUS        of info
  | CONTEXT     of info * anything befaft ref

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
  | ArrayAccess    of expression * string mcode (* [ *) * expression *
	              string mcode (* ] *)
  | RecordAccess   of expression * string mcode (* . *) * ident
  | RecordPtAccess of expression * string mcode (* -> *) * ident
  | Cast           of string mcode (* ( *) * fullType * string mcode (* ) *) *
                      expression

  | Paren          of string mcode (* ( *) * expression *
                      string mcode (* ) *)

  | MetaConst      of string mcode * fullType list option
  | MetaErr        of string mcode
  | MetaExpr       of string mcode * fullType list option
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
  | ERRORWORDS of expression list
  | CODE of rule_elem dots

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
  | ValueQualifTag      of value_qualif
  | Token               of string
  | Code                of top_level


let get_real_line = function
    MINUS(info,_) -> info.line
  | PLUS(info) -> info.line
  | CONTEXT(info,_) -> info.line

let get_logical_line = function
    MINUS(info,_) -> info.logical_line
  | PLUS(info) -> info.logical_line
  | CONTEXT(info,_) -> info.logical_line

(* --------------------------------------------------------------------- *)

let modif_option f = function
    None -> false
  | Some x -> f x

let modif_mcode (_,x) =
  match x with
    MINUS(_,l) -> true
  | CONTEXT(_,x) when !x = NOTHING -> false
  | CONTEXT(_,_) -> true
  | _ -> failwith "not possible"

let modif_dots f = function DOTS(l) | CIRCLES(l) | STARS(l) -> List.exists f l

let rec modif_ident = function
    Id(name) -> modif_mcode name
  | MetaId(name) -> modif_mcode name
  | MetaFunc(name) -> modif_mcode name
  | MetaLocalFunc(name) -> modif_mcode name
  | OptIdent(id) -> modif_ident id
  | UniqueIdent(id) -> modif_ident id
  | MultiIdent(id) -> modif_ident id



and modif_expression = function
    Ident(id) -> modif_ident id
  | Constant(const) -> modif_mcode const
  | FunCall(fn,lp,args,rp) ->
      modif_expression fn or modif_mcode lp or
	modif_dots modif_expression args or modif_mcode rp
  | Assignment(left,op,right) ->
      modif_expression left or modif_mcode op or modif_expression right
  | CondExpr(exp1,why,exp2,colon,exp3) ->
      modif_expression exp1 or modif_mcode why or
	modif_option modif_expression exp2 or
	modif_mcode colon or modif_expression exp3
  | Postfix(exp,op) -> modif_expression exp or modif_mcode op
  | Infix(exp,op) -> modif_mcode op or modif_expression exp
  | Unary(exp,op) -> modif_mcode op or modif_expression exp
  | Binary(left,op,right) ->
      modif_expression left or modif_mcode op or modif_expression right
  | Paren(lp,exp,rp) ->
      modif_mcode lp or modif_expression exp or modif_mcode rp
  | ArrayAccess(exp1,lb,exp2,rb) ->
      modif_expression exp1 or modif_mcode lb or modif_expression exp2
	or modif_mcode rb
  | RecordAccess(exp,pt,field) ->
      modif_expression exp or modif_mcode pt or modif_ident field
  | RecordPtAccess(exp,ar,field) ->
      modif_expression exp or modif_mcode ar or modif_ident field
  | Cast(lp,ty,rp,exp) ->
      modif_mcode lp or modif_ty ty or modif_mcode rp or modif_expression exp
  | MetaConst(name,_) -> modif_mcode name
  | MetaErr(name) -> modif_mcode name
  | MetaExpr(name,_) -> modif_mcode name
  | MetaExprList(name) -> modif_mcode name
  | EComma(cm) -> modif_mcode cm
  | DisjExpr(exp_list) -> List.exists modif_expression exp_list
  | NestExpr(expr_dots) -> modif_dots modif_expression expr_dots
  | Edots(dots,_) | Ecircles(dots,_) | Estars(dots,_) -> modif_mcode dots
  | OptExp(exp) | UniqueExp(exp) | MultiExp(exp) -> modif_expression exp

and modif_ty = function
    BaseType(ty,sgn) -> modif_mcode ty or modif_option modif_mcode sgn
  | Pointer(ty,star) -> modif_ty ty or modif_mcode star
  | Array(ty,lb,size,rb) ->
      modif_ty ty or modif_mcode lb or modif_option modif_expression size or
      modif_mcode rb
  | StructUnionName(name,kind) -> modif_mcode kind or modif_mcode name
  | TypeName(name)-> modif_mcode name
  | MetaType(name)-> modif_mcode name
  | OptType(ty) -> modif_ty ty
  | UniqueType(ty) -> modif_ty ty
  | MultiType(ty) -> modif_ty ty

let rec modif_declaration = function
    Init(ty,id,eq,exp,sem) ->
      modif_ty ty or modif_ident id or modif_mcode eq or
	modif_expression exp or modif_mcode sem
  | UnInit(ty,id,sem) ->
      modif_ty ty or modif_ident id or modif_mcode sem
  | OptDecl(decl) -> modif_declaration decl
  | UniqueDecl(decl) -> modif_declaration decl
  | MultiDecl(decl) -> modif_declaration decl

let rec modif_parameterTypeDef = function
    VoidParam(ty) -> modif_ty ty
  | Param(id,None,ty) -> modif_ty ty or modif_ident id
  | Param(id,Some vs,ty) -> modif_mcode vs or modif_ty ty or modif_ident id
  | MetaParam(name) -> modif_mcode name
  | MetaParamList(name) -> modif_mcode name
  | PComma(cm) -> modif_mcode cm
  | Pdots(dots) -> modif_mcode dots
  | Pcircles(dots) -> modif_mcode dots
  | OptParam(param) -> modif_parameterTypeDef param
  | UniqueParam(param) -> modif_parameterTypeDef param

let modif_parameter_list = modif_dots modif_parameterTypeDef

let rec contains_modif = function
    FunDecl(stg,name,lp,params,rp) ->
      modif_option modif_mcode stg or modif_ident name or
	modif_mcode lp or modif_parameter_list params or modif_mcode rp
  | Decl(decl) ->  modif_declaration decl
  | SeqStart(brace) -> modif_mcode brace
  | SeqEnd(brace) -> modif_mcode brace
  | ExprStatement(exp,sem) -> modif_expression exp or modif_mcode sem
  | IfHeader(iff,lp,exp,rp) ->
      modif_mcode iff or modif_mcode lp or modif_expression exp or
	  modif_mcode rp
  | Else(els) -> modif_mcode els
  | WhileHeader(whl,lp,exp,rp) ->
      modif_mcode whl or modif_mcode lp or modif_expression exp or
	modif_mcode rp
  | Do(d) -> modif_mcode d
  | WhileTail(whl,lp,exp,rp,sem) ->
      modif_mcode whl or modif_mcode lp or modif_expression exp or
	modif_mcode rp or modif_mcode sem
  | ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
      modif_mcode fr or modif_mcode lp or
	modif_option modif_expression e1 or modif_mcode sem1 or
	modif_option modif_expression e2 or modif_mcode sem2 or
	modif_option modif_expression e3 or modif_mcode rp
  | Return(ret,sem) ->
       modif_mcode ret or modif_mcode sem
  | ReturnExpr(ret,exp,sem) ->
      modif_mcode ret or modif_expression exp or modif_mcode sem
  | MetaStmt(name) -> modif_mcode name
  | MetaStmtList(name) -> modif_mcode name
  | Disj(rule_elem_dots_list) ->
      List.exists (modif_dots contains_modif) rule_elem_dots_list
  | Nest(rule_elem_dots) -> modif_dots contains_modif rule_elem_dots
  | Exp(exp) ->  modif_expression exp
  | Dots(dots,_) | Circles(dots,_) | Stars(dots,_) -> modif_mcode dots
  | OptRuleElem(re) | UniqueRuleElem(re) | MultiRuleElem(re) ->
      List.exists contains_modif re
