type posl = int * int
type virtual_position = Common.parse_info * int
type parse_info =
    OriginTok of Common.parse_info
  | FakeTok of string * virtual_position
  | ExpandedTok of Common.parse_info * virtual_position
  | AbstractLineTok of Common.parse_info
type danger = DangerStart | DangerEnd | Danger | NoDanger
type info = {
  pinfo : parse_info;
  cocci_tag : (Ast_cocci.mcodekind * metavars_binding list) option ref;
  comments_tag : comments_around ref;
  mutable annots_tag : Token_annot.annots;
  danger : danger ref;
}
and il = info list
and 'a wrap = 'a * il
and 'a wrap2 = 'a * il
and 'a wrap3 = 'a * il
and name =
    RegularName of string wrap
  | CppConcatenatedName of string wrap wrap2 list
  | CppVariadicName of string wrap
  | CppIdentBuilder of string wrap * string wrap wrap2 list
and fullType = typeQualifier * typeC
and typeC = typeCbis wrap
and typeCbis =
    NoType
  | BaseType of baseType
  | Pointer of fullType
  | Array of constExpression option * fullType
  | Decimal of constExpression * constExpression option
  | FunctionType of functionType
  | Enum of string option * enumType
  | StructUnion of structUnion * string option * structType
  | EnumName of string
  | StructUnionName of structUnion * string
  | TypeName of name * fullType option
  | FieldType of fullType * name * constExpression option
  | ParenType of fullType
  | TypeOfExpr of expression
  | TypeOfType of fullType
  | AutoType (* c++ >= 11 *)
and baseType =
    Void
  | IntType of intType
  | FloatType of floatType
  | SizeType
  | SSizeType
  | PtrDiffType
and intType = CChar | Si of signed
and signed = sign * base
and base = CChar2 | CShort | CInt | CLong | CLongLong
and sign = Signed | UnSigned
and floatType =
    CFloat | CDouble | CLongDouble | CFloatComplex | CDoubleComplex | CLongDoubleComplex
and structUnion = Struct | Union
and structType = field list
and field =
    DeclarationField of field_declaration
  | EmptyField of info
  | MacroDeclField of (string * argument wrap2 list) wrap
  | CppDirectiveStruct of cpp_directive
  | IfdefStruct of ifdef_directive
and field_declaration = FieldDeclList of fieldkind wrap2 list wrap
and fieldkind =
    Simple of name option * fullType
  | BitField of name option * fullType * info * constExpression
and enumType = oneEnumType wrap2 list
and oneEnumType = name * (info * constExpression) option
and functionType = fullType * (parameterType wrap2 list * bool wrap)
and parameterType = {
  p_namei : name option;
  p_register : bool wrap;
  p_type : fullType;
  p_attr : attribute list;
}
and typeQualifier = typeQualifierbis wrap
and typeQualifierbis = { const : bool; volatile : bool; }
and attribute = attributebis wrap
and attributebis = Attribute of attr_arg
and attr_arg = attr_arg_bis wrap
and attr_arg_bis = AttrName of string
and expression = (expressionbis * exp_info ref) wrap3
and exp_info = exp_type option * test
and exp_type = fullType * local
and local =
    LocalVar of parse_info
  | StaticLocalVar of parse_info
  | NotLocalVar
and test = Test | NotTest
and expressionbis =
    Ident of name
  | Constant of constant
  | StringConstant of string_fragment list * string * isWchar
  | FunCall of expression * argument wrap2 list
  | CondExpr of expression * expression option * expression
  | Sequence of expression * expression
  | Assignment of expression * assignOp * expression
  | Postfix of expression * fixOp
  | Infix of expression * fixOp
  | Unary of expression * unaryOp
  | Binary of expression * binaryOp * expression
  | ArrayAccess of expression * expression
  | RecordAccess of expression * name
  | RecordPtAccess of expression * name
  | SizeOfExpr of expression
  | SizeOfType of fullType
  | Cast of fullType * attribute list * expression
  | StatementExpr of compound wrap
  | Constructor of fullType * initialiser
  | ParenExpr of expression
  | New of argument wrap2 list option * argument
  | Delete of bool (* true if [] *) * expression
  | Defined of name
and argument = (expression, weird_argument) Common.either
and weird_argument = ArgType of parameterType | ArgAction of action_macro
and action_macro = ActMisc of il
and constant =
    String of (string * isWchar)
  | MultiString of string list
  | Char of (string * isWchar)
  | Int of (string * intType)
  | Float of (string * floatType)
  | DecimalConst of (string * string * string)
and isWchar = IsWchar | IsUchar | Isuchar | Isu8char | IsChar
and unaryOp = GetRef | DeRef | UnPlus | UnMinus | Tilde | Not | GetRefLabel
and assignOpbis = SimpleAssign | OpAssign of arithOp
and assignOp = assignOpbis wrap
and fixOp = Dec | Inc
and binaryOpbis = Arith of arithOp | Logical of logicalOp
and binaryOp = binaryOpbis wrap
and arithOp =
    Plus
  | Minus
  | Mul
  | Div
  | Mod
  | DecLeft
  | DecRight
  | And
  | Or
  | Xor
  | Max
  | Min
and logicalOp = Inf | Sup | InfEq | SupEq | Eq | NotEq | AndLog | OrLog
and constExpression = expression
and string_fragment = string_fragment_bis wrap
and string_fragment_bis =
    ConstantFragment of string
  | FormatFragment of string_format
and string_format = string_format_bis wrap
and string_format_bis = ConstantFormat of string
and statement = statementbis wrap3
and statementbis =
    Labeled of labeled
  | Compound of compound
  | ExprStatement of exprStatement
  | Selection of selection
  | Iteration of iteration
  | Jump of jump
  | Decl of declaration
  | Asm of asmbody
  | NestedFunc of definition
  | MacroStmt
  | Exec of exec_code list
  | IfdefStmt1 of ifdef_directive list * statement list
and labeled =
    Label of name * statement
  | Case of expression * statement
  | CaseRange of expression * expression * statement
  | Default of statement
and compound = statement_sequencable list
and statement_sequencable =
    StmtElem of statement
  | CppDirectiveStmt of cpp_directive
  | IfdefStmt of ifdef_directive
  | IfdefStmt2 of ifdef_directive list * statement_sequencable list list
and exprStatement = expression option
and declOrExpr = ForDecl of declaration | ForExp of expression option wrap
and selection =
    If of expression * statement * statement
  | Switch of expression * statement
  | Ifdef_Ite of expression * statement * statement
  | Ifdef_Ite2 of expression * statement * statement * statement
and iteration =
    While of expression * statement
  | DoWhile of statement * expression
  | For of declOrExpr * exprStatement wrap * exprStatement wrap * statement
  | MacroIteration of string * argument wrap2 list * statement
and jump =
    Goto of name
  | Continue
  | Break
  | Return
  | ReturnExpr of expression
  | GotoComputed of expression
and asmbody = il * colon wrap list
and colon = Colon of colon_option wrap2 list
and colon_option = colon_option_bis wrap
and colon_option_bis = ColonMisc | ColonExpr of expression
and exec_code_bis = ExecEval of expression | ExecToken
and exec_code = exec_code_bis wrap
and declaration =
    DeclList of onedecl wrap2 list wrap
  | MacroDecl of
      (storagebis * string * argument wrap2 list * attribute list * bool)
        wrap
  | MacroDeclInit of
      (storagebis * string * argument wrap2 list * initialiser) wrap
and onedecl = {
  v_namei : (name * v_init) option;
  v_type : fullType;
  v_type_bis : fullType option ref;
  v_storage : storage;
  v_local : local_decl;
  v_attr : attribute list;
  v_endattr : attribute list;
}
and v_init =
    NoInit
  | ValInit of info * initialiser
  | ConstrInit of argument wrap2 list wrap
and storage = storagebis * bool
and storagebis = NoSto | StoTypedef | Sto of storageClass
and storageClass = Auto | Static | Register | Extern
and local_decl = LocalDecl | NotLocalDecl
and initialiser = initialiserbis wrap
and initialiserbis =
    InitExpr of expression
  | InitList of initialiser wrap2 list
  | InitDesignators of designator list * initialiser
  | InitFieldOld of string * initialiser
  | InitIndexOld of expression * initialiser
and designator = designatorbis wrap
and designatorbis =
    DesignatorField of string
  | DesignatorIndex of expression
  | DesignatorRange of expression * expression
and definition = definitionbis wrap
and definitionbis = {
  f_name : name;
  f_type : functionType;
  f_storage : storage;
  f_body : compound;
  f_attr : attribute list;
  f_old_c_style : declaration list option;
}
and cpp_directive =
    Define of define
  | Include of includ
  | Pragma of (name * string wrap list) wrap
  | OtherDirective of il
and define = string wrap * (define_kind * define_val)
and define_kind =
    DefineVar
  | DefineFunc of string wrap wrap2 list wrap
  | Undef
and define_val =
    DefineExpr of expression
  | DefineStmt of statement
  | DefineType of fullType
  | DefineDoWhileZero of (statement * expression) wrap
  | DefineFunction of definition
  | DefineInit of initialiser
  | DefineMulti of statement list
  | DefineText of string wrap
  | DefineEmpty
  | DefineTodo
and includ = {
  i_include : inc_file wrap;
  i_rel_pos : include_rel_pos option ref;
  i_overall_rel_pos : include_rel_pos option ref;
  i_is_in_ifdef : bool;
  i_content : (Common.filename * program) option;
}
and inc_file =
    Local of inc_elem list
  | NonLocal of inc_elem list
  | Weird of string
and inc_elem = string
and include_rel_pos = {
  first_of : string list list;
  last_of : string list list;
}
and ifdef_directive = IfdefDirective of (ifdefkind * matching_tag) wrap
and ifdefkind =
    Ifdef of ifdef_guard
  | IfdefElseif of ifdef_guard
  | IfdefElse
  | IfdefEndif
and ifdef_guard =
    Gifdef of macro_symbol
  | Gifndef of macro_symbol
  | Gif_str of Lexing.position * string
  | Gif of expression
  | Gnone
and macro_symbol = string
and matching_tag = IfdefTag of (int * int)
and toplevel =
    Declaration of declaration
  | Definition of definition
  | CppTop of cpp_directive
  | IfdefTop of ifdef_directive
  | MacroTop of string * argument wrap2 list * il
  | EmptyDef of il
  | NotParsedCorrectly of il
  | FinalDef of info
  | Namespace of toplevel list * il
and program = toplevel list
and metavars_binding =
    (Ast_cocci.meta_name, metavar_binding_kind) Common.assoc
and metavar_binding_kind =
    MetaIdVal of string
  | MetaFuncVal of string
  | MetaLocalFuncVal of string
  | MetaExprVal of expression * Ast_cocci.meta_name list * stripped
  | MetaExprListVal of argument wrap2 list
  | MetaParamVal of parameterType
  | MetaParamListVal of parameterType wrap2 list
  | MetaTypeVal of fullType
  | MetaInitVal of initialiser
  | MetaInitListVal of initialiser wrap2 list
  | MetaDeclVal of declaration * declaration
  | MetaFieldVal of field
  | MetaFieldListVal of field list
  | MetaStmtVal of statement * statement * stripped
  | MetaStmtListVal of statement_sequencable list * stripped
  | MetaDParamListVal of string wrap wrap2 list
  | MetaFmtVal of string_format
  | MetaAttrArgVal of attr_arg
  | MetaFragListVal of string_fragment list
  | MetaAssignOpVal of assignOp
  | MetaBinaryOpVal of binaryOp
  | MetaPosVal of (Ast_cocci.fixpos * Ast_cocci.fixpos)
  | MetaPosValList of
      (Common.filename * string * (posl * posl) option * posl * posl) list
  | MetaComValList   of (Token_c.comment_like_token list *
			   Token_c.comment_like_token list *
			   Token_c.comment_like_token list) list
  | MetaListlenVal of int
  | MetaNoVal
and stripped = WITH_TYPES | WITHOUT_TYPES
and comments_around = {
  mbefore : Token_c.comment_like_token list;
  mafter : Token_c.comment_like_token list;
  mbefore2 : comment_and_relative_pos list;
  mafter2 : comment_and_relative_pos list;
}
and comment_and_relative_pos = { minfo : Common.parse_info; mpos : int; }
and comment = Common.parse_info
and com = comment list ref
val nullQualif : typeQualifierbis * 'a list
val nQ : typeQualifierbis * 'a list
val defaultInt : typeCbis
val noType : unit -> ('a option * test) ref
val noInstr : statementbis * 'a list
val noTypedefDef : unit -> 'a option
val emptyMetavarsBinding : metavars_binding
val emptyAnnotCocci : Ast_cocci.mcodekind * metavars_binding list
val emptyAnnot : (Ast_cocci.mcodekind * metavars_binding list) option
val mcode_and_env_of_cocciref :
  (Ast_cocci.mcodekind * metavars_binding list) option ref ->
  Ast_cocci.mcodekind * metavars_binding list
val emptyComments : comments_around
val noRelPos : unit -> include_rel_pos option ref
val noInIfdef : unit -> bool ref
val no_virt_pos : Common.parse_info * int
val fakeInfo : 'a -> info
val noii : 'a list
val noattr : 'a list
val noi_content : (Common.filename * program) option
val unwrap : 'a * 'b -> 'a
val unwrap2 : 'a * 'b -> 'a
val unwrap_expr : ('a * 'b) * 'c -> 'a
val rewrap_expr : ('a * 'b) * 'c -> 'd -> ('d * 'b) * 'c
val unwrap_typeC : 'a * ('b * 'c) -> 'b
val rewrap_typeC : 'a * ('b * 'c) -> 'd -> 'a * ('d * 'c)
val unwrap_typeCbis : 'a * 'b -> 'a
val unwrap_st : 'a * 'b -> 'a
val mk_e : 'a -> 'b -> ('a * ('c option * test) ref) * 'b
val mk_e_bis : 'a -> 'b -> 'c -> ('a * 'b) * 'c
val mk_ty : 'a -> 'b -> (typeQualifierbis * 'c list) * ('a * 'b)
val mk_tybis : 'a -> 'b -> 'a * 'b
val mk_st : 'a -> 'b -> 'a * 'b
val get_ii_typeC_take_care : 'a * 'b -> 'b
val get_ii_st_take_care : 'a * 'b -> 'b
val get_ii_expr_take_care : 'a * 'b -> 'b
val get_st_and_ii : 'a * 'b -> 'a * 'b
val get_ty_and_ii : 'a * ('b * 'c) -> 'a * ('b * 'c)
val get_e_and_ii : 'a * 'b -> 'a * 'b
val get_type_expr : ('a * 'b ref) * 'c -> 'b
val set_type_expr : ('a * 'b ref) * 'c -> 'b -> unit
val get_onlytype_expr : ('a * (('b * 'c) option * 'd) ref) * 'e -> 'b option
val get_onlylocal_expr : ('a * (('b * 'c) option * 'd) ref) * 'e -> 'c option
val rewrap_str : string -> info -> info
val rewrap_charpos : int -> info -> info
val rewrap_col : int -> info -> info
val rewrap_pinfo : parse_info -> info -> info
val get_pi : parse_info -> Common.parse_info
val get_opi : parse_info -> Common.parse_info
val str_of_info : info -> string
val get_info : (Common.parse_info -> 'a) -> info -> 'a
val get_orig_info : (Common.parse_info -> 'a) -> info -> 'a
val make_expanded : info -> info
val pos_of_info : info -> int
val opos_of_info : info -> int
val line_of_info : info -> int
val col_of_info : info -> int
val file_of_info : info -> Common.filename
val mcode_of_info : info -> Ast_cocci.mcodekind
val pinfo_of_info : info -> parse_info
val parse_info_of_info : info -> Common.parse_info
val strloc_of_info : info -> string
val is_fake : info -> bool
val is_origintok : info -> bool
type posrv = Real of Common.parse_info | Virt of virtual_position
val compare_pos : info -> info -> int
val equal_posl : 'a * 'b -> 'a * 'b -> bool
val compare_posl : int * int -> int * int -> int
val info_to_fixpos : info -> Ast_cocci.fixpos
val is_test : expression -> bool
val al_info : int -> info -> info
val semi_al_info : info -> info
val magic_real_number : int
val real_al_info : info -> info
val al_comments : bool -> comments_around -> comments_around
val al_info_cpp : int -> info -> info
val semi_al_info_cpp : info -> info
val real_al_info_cpp : bool -> info -> info
val split_comma : 'a wrap2 list -> ('a, il) Common.either list
val unsplit_comma : ('a, il) Common.either list -> 'a wrap2 list
val split_nocomma : 'a list -> ('a, il) Common.either list
val unsplit_nocomma : ('a, il) Common.either list -> 'a list
val s_of_inc_file : inc_file -> string
val s_of_inc_file_bis : inc_file -> string
val fieldname_of_fieldkind : fieldkind -> name option
val s_of_attr : (attributebis * 'a) list -> string
val str_of_name : name -> string
val get_s_and_ii_of_name : name -> string * il
val get_s_and_info_of_name : name -> string * info
val info_of_name : name -> info
val ii_of_name : name -> il
val get_local_ii_of_expr_inlining_ii_of_name :
  (expressionbis * 'a) * il -> il
val get_local_ii_of_tybis_inlining_ii_of_name : typeCbis * il -> il
val info_of_type : 'a * (typeCbis * il) -> parse_info option
val get_local_ii_of_st_inlining_ii_of_name :
  statementbis * info list -> info list
val name_of_parameter : parameterType -> string option
val put_annot_info :
  info -> Token_annot.annot_key -> Token_annot.annot_val -> unit
val get_annot_info :
  info -> Token_annot.annot_key -> Token_annot.annot_val option
val get_comments_before : info -> Token_c.comment_like_token list
val get_comments_after : info -> Token_c.comment_like_token list
val string_of_toplevel : toplevel -> string
val string_of_inc_file : inc_file -> string
