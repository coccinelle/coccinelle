module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)
(* Modified code *)

type arity = OPT | UNIQUE | MULTI | NONE

type token_info = { tline_start : int; tline_end : int }
let default_token_info = { tline_start = -1; tline_end = -1 }

(* MIXED is like CONTEXT, since sometimes MIXED things have to revert to
CONTEXT - see insert_plus.ml *)
type mcodekind =
    MINUS       of (Ast.anything list list * token_info) ref
  | PLUS
  | CONTEXT     of (Ast.anything Ast.befaft * token_info * token_info) ref
  | MIXED       of (Ast.anything Ast.befaft * token_info * token_info) ref

type info = { line_start : int; line_end : int;
	      logical_start : int; logical_end : int;
	      attachable_start : bool; attachable_end : bool;
	      mcode_start : mcodekind list; mcode_end : mcodekind list;
	      column : int; offset : int }

type 'a mcode = 'a * arity * info * mcodekind
(* int ref is an index *)
type 'a wrap = 'a * info * int ref * mcodekind ref
      * Type_cocci.typeC option ref (* only for expressions *)
      * dots_bef_aft (* only for statements *)

and dots_bef_aft = NoDots | BetweenDots of statement

(* for iso metavariables, true if they can only match nonmodified terms with
   all metavariables unitary
   for SP metavariables, true if the metavariable is unitary (valid up to
   isomorphism phase only) *)
and pure = Impure | Pure | Context (* pure and only context *)

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
  | Cast           of string mcode (* ( *) * typeC * string mcode (* ) *) *
                      expression
  | SizeOfExpr     of string mcode (* sizeof *) * expression
  | SizeOfType     of string mcode (* sizeof *) * string mcode (* ( *) *
                      typeC * string mcode (* ) *)
  | TypeExp        of typeC (* type name used as an expression, only in args *)
  | MetaConst      of string mcode *
	              Type_cocci.typeC list option * pure
  | MetaErr        of string mcode * pure
  | MetaExpr       of string mcode *
	              Type_cocci.typeC list option * pure
  | MetaExprList   of string mcode (* only in arg lists *) * pure
  | EComma         of string mcode (* only in arg lists *)
  | DisjExpr       of string mcode * expression list *
	              string mcode list (* the |s *) * string mcode
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
    ConstVol        of Ast.const_vol mcode * typeC
  | BaseType        of Ast.baseType mcode * Ast.sign mcode option
  | ImplicitInt     of Ast.sign mcode
  | Pointer         of typeC * string mcode (* * *)
  | FunctionPointer of typeC *
	          string mcode(* ( *)*string mcode(* * *)*string mcode(* ) *)*
                  string mcode (* ( *)*parameter_list*string mcode(* ) *)
  | FunctionType    of typeC option *
	               string mcode (* ( *) * parameter_list *
                       string mcode (* ) *)
  | Array           of typeC * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | StructUnionName of Ast.structUnion mcode * ident (* name *)
  | StructUnionDef  of typeC (* either StructUnionName or metavar *) *
	string mcode (* { *) * declaration dots * string mcode (* } *)
  | TypeName        of string mcode
  | MetaType        of string mcode * pure
  | DisjType        of string mcode * typeC list * (* only after iso *)
                       string mcode list (* the |s *)  * string mcode
  | OptType         of typeC
  | UniqueType      of typeC
  | MultiType       of typeC

and typeC = base_typeC wrap

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and base_declaration =
    Init of Ast.storage mcode option * typeC * ident * string mcode (*=*) *
	initialiser * string mcode (*;*)
  | UnInit of Ast.storage mcode option * typeC * ident * string mcode (* ; *)
  | TyDecl of typeC * string mcode (* ; *)
  | MacroDecl of string mcode (* name *) * string mcode (* ( *) *
        expression dots * string mcode (* ) *) * string mcode (* ; *)
  | DisjDecl   of string mcode * declaration list *
                  string mcode list (* the |s *)  * string mcode
  (* Ddots is for a structure declaration *)
  | Ddots      of string mcode (* ... *) * declaration option (* whencode *)
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
  | IComma of string mcode (* , *)
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
  | Param         of typeC * ident option
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
	             statement * (info * mcodekind) (* after info *)
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
  | Switch        of string mcode (* switch *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) * string mcode (* { *) *
	             case_line list * string mcode (* } *)
  | Break         of string mcode (* break *) * string mcode (* ; *)
  | Continue      of string mcode (* continue *) * string mcode (* ; *)
  | Return        of string mcode (* return *) * string mcode (* ; *)
  | ReturnExpr    of string mcode (* return *) * expression *
	             string mcode (* ; *)
  | MetaStmt      of string mcode * pure
  | MetaStmtList  of string mcode(*only in statement lists*) * pure
  | Exp           of expression  (* only in dotted statement lists *)
  | Ty            of typeC (* only at top level *)
  | Disj          of string mcode * statement dots list *
	             string mcode list (* the |s *)  * string mcode
  | Nest          of string mcode * statement dots * string mcode *
	             statement dots option
  | Dots          of string mcode (* ... *) *
                     (statement dots,statement) whencode
  | Circles       of string mcode (* ooo *) *
	             (statement dots,statement) whencode
  | Stars         of string mcode (* *** *) *
	             (statement dots,statement) whencode
  | FunDecl of (info * mcodekind) (* before the function decl *) *
	Ast.storage mcode option * typeC option * ident (* name *) *
	string mcode (* ( *) * parameter_list * string mcode (* ) *) *
	string mcode (* { *) * statement dots *
	string mcode (* } *)
  | Include of string mcode (* #include *) * string mcode (* file *)
  | Define of string mcode (* #define *) * ident (* name *) *
	string mcode list option (*params*) * define_body
  | OptStm   of statement
  | UniqueStm of statement
  | MultiStm  of statement (* only allowed in nests *)

and ('a,'b) whencode =
    NoWhen
  | WhenNot of 'a
  | WhenAlways of 'b

and statement = base_statement wrap

and base_case_line =
    Default of string mcode (* default *) * string mcode (*:*) * statement dots
  | Case of string mcode (* case *) * expression * string mcode (*:*) *
	statement dots
  | OptCase of case_line

and case_line = base_case_line wrap

(* --------------------------------------------------------------------- *)
(* CPP code *)

and base_define_body =
    DMetaId of string mcode * pure
  | DStm of statement dots

and define_body = base_define_body wrap

(* --------------------------------------------------------------------- *)
(* Top-level code *)

type base_top_level =
    DECL of statement
  | CODE of statement dots
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | ERRORWORDS of expression list
  | OTHER of statement (* temporary, disappears after top_level.ml *)

and top_level = base_top_level wrap
and rule = top_level list

(* --------------------------------------------------------------------- *)

type anything =
    DotsExprTag of expression dots
  | DotsInitTag of initialiser dots
  | DotsParamTag of parameterTypeDef dots
  | DotsStmtTag of statement dots
  | DotsDeclTag of declaration dots
  | IdentTag of ident
  | ExprTag of expression
  | TypeCTag of typeC
  | ParamTag of parameterTypeDef
  | InitTag of initialiser
  | DeclTag of declaration
  | StmtTag of statement
  | CaseLineTag of case_line
  | TopTag of top_level

let dotsExpr x = DotsExprTag x
let dotsParam x = DotsParamTag x
let dotsInit x = DotsInitTag x
let dotsStmt x = DotsStmtTag x
let dotsDecl x = DotsDeclTag x
let ident x = IdentTag x
let expr x = ExprTag x
let typeC x = TypeCTag x
let param x = ParamTag x
let ini x = InitTag x
let decl x = DeclTag x
let stmt x = StmtTag x
let case_line x = CaseLineTag x
let top x = TopTag x

(* --------------------------------------------------------------------- *)
(* Avoid cluttering the parser.  Calculated in compute_lines.ml. *)

let default_info _ = (* why is this a function? *)
  { line_start = -1; line_end = -1;
    logical_start = -1; logical_end = -1;
    attachable_start = true; attachable_end = true;
    mcode_start = []; mcode_end = [];
    column = -1; offset = -1 }

let default_befaft _ =
  MIXED(ref (Ast.NOTHING,default_token_info,default_token_info))
let context_befaft _ =
  CONTEXT(ref (Ast.NOTHING,default_token_info,default_token_info))

let wrap x = (x,default_info(),ref (-1),ref (default_befaft()),ref None,NoDots)
let context_wrap x =
  (x,default_info(),ref (-1),ref (context_befaft()),ref None,NoDots)
let unwrap (x,_,_,_,_,_) = x
let unwrap_mcode (x,_,_,_) = x
let rewrap (_,info,index,mcodekind,ty,dots) x =
  (x,info,index,mcodekind,ty,dots)
let rewrap_mcode (_,arity,info,mcodekind) x = (x,arity,info,mcodekind)
let copywrap (_,info,index,mcodekind,ty,dots) x =
  (x,
   { line_start = info.line_start; line_end = info.line_end;
     logical_start = info.logical_start; logical_end = info.logical_end;
     attachable_start = info.attachable_start;
     attachable_end = info.attachable_end;
     mcode_start = info.mcode_start; mcode_end = info.mcode_end;
     column = info.column; offset = info.offset },
   ref !index,ref !mcodekind,ref !ty,dots)
let get_info (_,info,_,_,_,_) = info
let get_index (_,_,index,_,_,_) = !index
let set_index (_,_,index,_,_,_) i = index := i
let get_mcodekind (_,_,_,mcodekind,_,_) = !mcodekind
let get_mcode_mcodekind (_,_,_,mcodekind) = mcodekind
let get_mcodekind_ref (_,_,_,mcodekind,_,_) = mcodekind
let set_mcodekind (_,_,_,mcodekind,_,_) mk = mcodekind := mk
let set_type (_,_,_,_,ty,_) t = ty := t
let get_type (_,_,_,_,ty,_) = !ty
let get_dots_bef_aft (_,_,_,_,_,d) = d
let set_dots_bef_aft (a,b,c,d,e,_) dots_bef_aft = (a,b,c,d,e,dots_bef_aft)
let set_mcode_data data (_,ar,info,mc) = (data,ar,info,mc)

(* --------------------------------------------------------------------- *)

(* unique indices, for mcode and tree nodes *)
let index_counter = ref 0
let fresh_index _ = let cur = !index_counter in index_counter := cur + 1; cur

(* --------------------------------------------------------------------- *)

let undots d =
  match unwrap d with
  | DOTS    e -> e
  | CIRCLES e -> e
  | STARS   e -> e

(* --------------------------------------------------------------------- *)

let rec ast0_type_to_type ty =
  match unwrap ty with
    ConstVol(cv,ty) -> Type_cocci.ConstVol(const_vol cv,ast0_type_to_type ty)
  | BaseType(bty,None) ->
      Type_cocci.BaseType(baseType bty,None)
  | BaseType(bty,Some sgn) ->
      Type_cocci.BaseType(baseType bty,Some (sign sgn))
  | ImplicitInt(sgn) ->
      let bty = Type_cocci.IntType in
      Type_cocci.BaseType(bty,Some (sign sgn))
  | Pointer(ty,_) -> Type_cocci.Pointer(ast0_type_to_type ty)
  | FunctionPointer(ty,_,_,_,_,params,_) ->
      Type_cocci.FunctionPointer(ast0_type_to_type ty)
  | FunctionType _ -> failwith "not supported"
  | Array(ety,_,_,_) -> Type_cocci.Array(ast0_type_to_type ety)
  | StructUnionName(su,tag) ->
      (match unwrap tag with
	Id(tag) ->
	  Type_cocci.StructUnionName(structUnion su,false,unwrap_mcode tag)
      | MetaId(tag,_) ->
	  (Printf.printf
	     "warning: struct/union with a metavariable name detected.\n";
	   Printf.printf
	     "For type checking assuming the name of the metavariable is the name of the type\n";
	   Type_cocci.StructUnionName(structUnion su,true,unwrap_mcode tag))
      | _ -> failwith "unexpected struct/union type name")
  | StructUnionDef(ty,_,_,_) -> ast0_type_to_type ty
  | TypeName(name) -> Type_cocci.TypeName(unwrap_mcode name)
  | MetaType(name,_) ->
      Type_cocci.MetaType(unwrap_mcode name,Type_cocci.Unitary,false)
  | DisjType(_,types,_,_) -> failwith "unexpected DisjType"
  | OptType(ty) | UniqueType(ty) | MultiType(ty) ->
      ast0_type_to_type ty

and baseType t =
  match unwrap_mcode t with
    Ast.VoidType -> Type_cocci.VoidType
  | Ast.CharType -> Type_cocci.CharType
  | Ast.ShortType -> Type_cocci.ShortType
  | Ast.IntType -> Type_cocci.IntType
  | Ast.DoubleType -> Type_cocci.DoubleType
  | Ast.FloatType -> Type_cocci.FloatType
  | Ast.LongType -> Type_cocci.LongType

and structUnion t =
  match unwrap_mcode t with
    Ast.Struct -> Type_cocci.Struct
  | Ast.Union -> Type_cocci.Union

and sign t =
  match unwrap_mcode t with
    Ast.Signed -> Type_cocci.Signed
  | Ast.Unsigned -> Type_cocci.Unsigned

and const_vol t =
  match unwrap_mcode t with
    Ast.Const -> Type_cocci.Const
  | Ast.Volatile -> Type_cocci.Volatile

(* --------------------------------------------------------------------- *)
(* this function is a rather minimal attempt.  the problem is that information
has been lost.  but since it is only used for metavariable types in the isos,
perhaps it doesn't matter *)
let ty_rewrap_mcode x = (x,NONE,default_info(),context_befaft())

exception TyConv

let rec reverse_type ty =
  match ty with
    Type_cocci.ConstVol(cv,ty) ->
      ConstVol(reverse_const_vol cv,wrap(reverse_type ty))
  | Type_cocci.BaseType(bty,None) ->
      BaseType(reverse_baseType bty,None)
  | Type_cocci.BaseType(bty,Some sgn) ->
      BaseType(reverse_baseType bty,Some (reverse_sign sgn))
  | Type_cocci.Pointer(ty) ->
      Pointer(wrap(reverse_type ty),ty_rewrap_mcode "*")
  | Type_cocci.StructUnionName(su,mv,tag) ->
      if mv
      then
	StructUnionName(reverse_structUnion su,
			wrap(MetaId(ty_rewrap_mcode tag,Impure)))
      else
	StructUnionName(reverse_structUnion su, wrap(Id(ty_rewrap_mcode tag)))
  | Type_cocci.TypeName(name) -> TypeName(ty_rewrap_mcode name)
  | Type_cocci.MetaType(name,_,_) ->
      MetaType(ty_rewrap_mcode name,Impure(*not really right*))
  | _ -> raise TyConv

and reverse_baseType t =
  ty_rewrap_mcode
    (match t with
      Type_cocci.VoidType -> Ast.VoidType
    | Type_cocci.CharType -> Ast.CharType
    | Type_cocci.BoolType -> Ast.IntType
    | Type_cocci.ShortType -> Ast.ShortType
    | Type_cocci.IntType -> Ast.IntType
    | Type_cocci.DoubleType -> Ast.DoubleType
    | Type_cocci.FloatType -> Ast.FloatType
    | Type_cocci.LongType -> Ast.LongType)

and reverse_structUnion t =
  ty_rewrap_mcode
    (match t with
      Type_cocci.Struct -> Ast.Struct
    | Type_cocci.Union -> Ast.Union)

and reverse_sign t =
  ty_rewrap_mcode
    (match t with
      Type_cocci.Signed -> Ast.Signed
    | Type_cocci.Unsigned -> Ast.Unsigned)

and reverse_const_vol t =
  ty_rewrap_mcode
    (match t with
      Type_cocci.Const -> Ast.Const
    | Type_cocci.Volatile -> Ast.Volatile)

(* --------------------------------------------------------------------- *)

let lub_pure x y =
  match (x,y) with
    (Impure,_) | (_,Impure) -> Impure
  | (Pure,_) | (_,Pure) -> Pure
  | _ -> Context
