module Token_c :
  sig
    type cppcommentkind = Token_c.cppcommentkind =
	CppDirective
      | CppIfDirective of ifdef
      | CppAttr
      | CppMacro
      | CppPassingNormal
      | CppPassingCosWouldGetError
      | CppPassingExplicit
    and ifdef = Token_c.ifdef = IfDef | IfDef0 | Else | Endif | Other
    type info = Common.parse_info
    type token = token_tag * info
    and token_tag = Token_c.token_tag =
	TCommentSpace
      | TCommentNewline
      | TComment
      | TCommentCpp of cppcommentkind
    type comment_like_token = token
    val str_of_token : 'a * Common.parse_info -> string
  end
module Ast_c :
  sig
    type posl = int * int
    type virtual_position = Common.parse_info * int
    type befaft =
      Ast_c.befaft = Before | After (* reason for fake tok *)
    type parse_info =
      Ast_c.parse_info =
        OriginTok of Common.parse_info
      | FakeTok of string * virtual_position * befaft
      | ExpandedTok of Common.parse_info * virtual_position
      | AbstractLineTok of Common.parse_info
    type danger = Ast_c.danger = DangerStart | DangerEnd | Danger | NoDanger
    type info =
      Ast_c.info = {
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
      Ast_c.name =
        RegularName of string wrap
      | Operator of bool wrap
      | QualName of name wrap2 (* the :: separators *) list
      | CppConcatenatedName of string wrap wrap2 list
      | CppVariadicName of string wrap
      | CppIdentBuilder of string wrap * string wrap wrap2 list
    and fullType = typeQualifier * attribute list * typeC
    and typeC = typeCbis wrap
    and typeCbis =
      Ast_c.typeCbis =
        NoType
      | BaseType of baseType
      | Pointer of fullType
      | Array of constExpression option * fullType
      | Decimal of constExpression * constExpression option
      | FunctionType of functionType
      | EnumDef of fullType * fullType option * enumType
      | StructUnion of structUnion * string option * info option (* C++, final *) *
	    base_class wrap2 list (* C++ *) * structType (* new scope *)
      | EnumName of structUnion option * string option
      | StructUnionName of structUnion * string
      | TypeName of name
      | NamedType of name * fullType option
      | QualifiedType of fullType option * name
      | FieldType of fullType * name * constExpression option
      | ParenType of fullType
      | TypeOfExpr of expression
      | TypeOfType of fullType
      | AutoType (* c++ >= 11 *)
      | TemplateType of fullType * argument wrap2 (* , *) list (* C++ *)
    and baseType =
      Ast_c.baseType =
        Void
      | IntType of intType
      | FloatType of floatType
      | SizeType
      | SSizeType
      | PtrDiffType
    and intType = Ast_c.intType = CChar | Si of signed
    and signed = sign * base
    and base = Ast_c.base = CChar2 | CShort | CInt | CLong | CLongLong
    and sign = Ast_c.sign = Signed | UnSigned
    and floatType =
	Ast_c.floatType =
	CFloat | CDouble | CLongDouble | CFloatComplex | CDoubleComplex
      | CLongDoubleComplex | CUnknownComplex
    and structUnion = Ast_c.structUnion = Struct | Union | Class
    and structType = field list
    and field =
      Ast_c.field =
        DeclarationField of field_declaration
      | EmptyField of info
      | FunctionField of definition
      | AccSpec of info list
      | ConstructDestructField of c_plus_plus_constructor
      | MacroDeclField of (string * argument wrap2 list * attribute list) wrap
      | MacroDeclFieldInit of (string * argument wrap2 list * attribute list * initialiser) wrap
      | CppDirectiveStruct of cpp_directive
      | IfdefStruct of ifdef_directive
    and field_declaration =
      Ast_c.field_declaration =
        FieldDeclList of fieldkind wrap2 list wrap
    and fieldkind =
      Ast_c.fieldkind =
	Simple   of storage * attribute list * (name * v_init) option * fullType * attribute list (* endattrs *)
      | BitField of name option * fullType * info * constExpression
    and enumType = oneEnumType wrap2 list
    and oneEnumType = name * (info * constExpression) option
    and functionType = fullType * (parameterType wrap2 list * bool wrap)
    and parameterType =
      Ast_c.parameterType = {
      p_namei : name option;
      p_register : bool wrap;
      p_type : fullType;
      p_endattr : attribute list;
    }
    and typeQualifier = typeQualifierbis wrap
    and typeQualifierbis =
      Ast_c.typeQualifierbis = {
      const : bool;
      volatile : bool;
      restrict : bool;
    }
    and attribute = attributebis wrap
    and attributebis = Ast_c.attributebis =
	Attribute of attr_arg
      | GccAttribute of argument wrap2 list
      | CxxAttribute of argument wrap2 list
      | CxxAttributeUsing of name * argument wrap2 list
    and attr_arg = attr_arg_bis wrap
    and attr_arg_bis =
      Ast_c.attr_arg_bis =
        MacroAttr of string
      | MacroAttrArgs of string * argument wrap2 list
    and expression = (expressionbis * exp_info ref) wrap3
    and exp_info = exp_type option * test
    and exp_type = fullType * local
    and local =
      Ast_c.local =
        LocalVar of parse_info
      | StaticLocalVar of parse_info
      | NotLocalVar
    and test = Ast_c.test = Test | NotTest
    and expressionbis =
      Ast_c.expressionbis =
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
      | ArrayAccess of expression * argument wrap2 list
      | RecordAccess of expression * name
      | RecordPtAccess of expression * name
      | QualifiedAccess of fullType option * name
      | SizeOfExpr of expression
      | SizeOfType of fullType
      | Cast of fullType * expression
      | StatementExpr of compound wrap
      | Constructor of fullType * initialiser
      | ParenExpr of expression
      | New of argument wrap2 list option * fullType * argument wrap2 list option
      | Delete of bool * expression
      | TemplateInst of expression * argument wrap2 list
      | TupleExpr of initialiser
      | Defined of name
    and argument = (expression, weird_argument) Common.either
    and weird_argument =
      Ast_c.weird_argument =
        ArgType of parameterType
      | ArgAction of action_macro
    and action_macro = Ast_c.action_macro = ActMisc of il
    and constant =
      Ast_c.constant =
        String of (string * isWchar)
      | MultiString of string list
      | Char of (string * isWchar)
      | Int of (string * intType)
      | Float of (string * floatType)
      | DecimalConst of (string * string * string)
    and isWchar = Ast_c.isWchar = IsWchar | IsUchar | Isuchar | Isu8char | IsChar
    and unaryOp =
      Ast_c.unaryOp =
        GetRef
      | DeRef
      | UnPlus
      | UnMinus
      | Tilde
      | Not
      | GetRefLabel
    and assignOpbis = Ast_c.assignOpbis = SimpleAssign | OpAssign of arithOp
    and assignOp = assignOpbis wrap
    and fixOp = Ast_c.fixOp = Dec | Inc
    and binaryOpbis =
      Ast_c.binaryOpbis =
        Arith of arithOp
      | Logical of logicalOp
    and binaryOp = binaryOpbis wrap
    and arithOp =
      Ast_c.arithOp =
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
    and logicalOp =
      Ast_c.logicalOp =
        Inf
      | Sup
      | InfEq
      | SupEq
      | Eq
      | NotEq
      | AndLog
      | OrLog
    and constExpression = expression
    and string_fragment = string_fragment_bis wrap
    and string_fragment_bis =
      Ast_c.string_fragment_bis =
        ConstantFragment of string
      | FormatFragment of string_format
    and string_format = string_format_bis wrap
    and string_format_bis =
      Ast_c.string_format_bis =
        ConstantFormat of string
    and statement = statementbis wrap3
    and statementbis =
      Ast_c.statementbis =
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
      Ast_c.labeled =
        Label of name * statement
      | Case of expression * statement
      | CaseRange of expression * expression * statement
      | Default of statement
    and compound = statement_sequencable list
    and statement_sequencable =
      Ast_c.statement_sequencable =
        StmtElem of statement
      | CppDirectiveStmt of cpp_directive
      | IfdefStmt of ifdef_directive
      | IfdefStmt2 of ifdef_directive list * statement_sequencable list list
    and exprStatement = expression option
    and declOrExpr =
      Ast_c.declOrExpr =
	ForDecl of declaration * exprStatement wrap * exprStatement wrap
      | ForExp of
	  expression option wrap * exprStatement wrap * exprStatement wrap
      | ForRange of declaration * initialiser
    and whileDeclOrExpr =
      Ast_c.whileDeclOrExpr =
        WhileDecl of declaration
      | WhileExp of expression
    and selection =
      Ast_c.selection =
        If of expression * statement * statement
      | Switch of expression * statement
      | Ifdef_Ite of expression * statement * statement
      | Ifdef_Ite2 of expression * statement * statement * statement
      | TryCatch of statement * (parameterType * statement) wrap list
    and iteration =
      Ast_c.iteration =
        While of whileDeclOrExpr * statement
      | DoWhile of statement * expression
      | For of declOrExpr * statement
      | MacroIteration of string * argument wrap2 list * statement
      | ScopedGuard of argument wrap2 (* , *) list * statement
    and jump =
      Ast_c.jump =
        Goto of name
      | Continue
      | Break
      | Return
      | ReturnExpr of expression
      | GotoComputed of expression
    and asmbody = il * colon wrap list
    and colon = Ast_c.colon = Colon of colon_option wrap2 list
    and colon_option = colon_option_bis wrap
    and colon_option_bis =
      Ast_c.colon_option_bis =
        ColonMisc
      | ColonExpr of expression
    and exec_code_bis =
      Ast_c.exec_code_bis =
        ExecEval of expression
      | ExecToken
    and exec_code = exec_code_bis wrap
    and declaration =
      Ast_c.declaration =
        DeclList of (onedecl wrap2 list * bool) wrap
      | MacroDecl of
          (storagebis * attribute list * string *
	     argument wrap2 list * attribute list * bool)
            wrap
      | MacroDeclInit of
          (storagebis * attribute list * string *
	     argument wrap2 list * attribute list * initialiser) wrap
    and onedecl =
      Ast_c.onedecl = {
      v_namei : (name * v_init) option;
      v_type : fullType;
      v_type_bis : fullType option ref;
      v_storage : storage;
      v_local : local_decl;
      v_attr : attribute list;
      v_endattr : attribute list;
    }
    and v_init =
      Ast_c.v_init =
        NoInit
      | ValInit of initialiser wrap
    and storage = storagebis * bool * align
    and storagebis =
      Ast_c.storagebis =
        NoSto
      | StoTypedef
      | Sto of storageClass
    and storageClass = Ast_c.storageClass = Auto | Static | Register | Extern
    and align =
      Ast_c.align =
        NoAlign | Align of argument
    and local_decl = Ast_c.local_decl = LocalDecl | NotLocalDecl
    and initialiser = initialiserbis wrap
    and initialiserbis =
      Ast_c.initialiserbis =
        InitExpr of expression
      | InitList of initialiser wrap2 list
      | InitListNoBrace of initialiser wrap2 (* , *) list
      | InitDesignators of designator list * initialiser
      | InitFieldOld of string * initialiser
      | InitIndexOld of expression * initialiser
    and designator = designatorbis wrap
    and designatorbis =
      Ast_c.designatorbis =
        DesignatorField of string
      | DesignatorIndex of expression
      | DesignatorRange of expression * expression
    and definition = definitionbis wrap
    and definitionbis =
      Ast_c.definitionbis = {
      f_name : name;
      f_type : functionType;
      f_storage : storage;
      f_constr_inherited: expression wrap2 list;
      f_body : compound;
      f_endattr : attribute list;
      f_old_c_style : declaration list option;
    }

    and c_plus_plus_constructor = c_plus_plus_constructorbis wrap
    and c_plus_plus_constructorbis =
      Ast_c.c_plus_plus_constructorbis =
      | ConstructorDecl of
	  bool wrap * string * (parameterType wrap2 list * bool wrap) * bool wrap
      | DestructorDecl  of
	  bool wrap * string * (parameterType wrap2 list * bool wrap) * bool wrap
      | ConstructorDef  of
	  bool wrap * string * (parameterType wrap2 list * bool wrap) *
	    (constr_init wrap2 (* , *) list) wrap * bool wrap * compound
      | DestructorDef   of
	  bool wrap * string * (parameterType wrap2 list * bool wrap) * bool wrap * compound

    and constr_init = (name * argument wrap2 (* , *) list) wrap

    and base_class = base_class_bis wrap
    and base_class_bis =
      Ast_c.base_class_bis =
	ClassName of name
      | CPublic of name
      | CProtected of name
      | CPrivate of name

    and cpp_directive =
      Ast_c.cpp_directive =
        Define of define
      | Include of includ
      | Pragma of (name * info) wrap
      | OtherDirective of il
      | UsingTypename of (name * fullType) wrap
      | UsingNamespace of name wrap
      | UsingMember of name wrap

    and define = string wrap * (define_kind * define_val)
    and define_kind =
      Ast_c.define_kind =
        DefineVar
      | DefineFunc of string wrap wrap2 list wrap
      | Undef
    and define_val =
      Ast_c.define_val =
        DefineExpr of expression
      | DefineStmt of statement
      | DefineType of fullType
      | DefineAttr of attribute list
      | DefineDoWhileZero of (statement * expression) wrap
      | DefineFunction of definition
      | DefineInit of initialiser
      | DefineMulti of statement list
      | DefineText of string wrap
      | DefineEmpty
      | DefineTodo
    and includ =
      Ast_c.includ = {
      i_include : inc_file wrap;
      i_rel_pos : include_rel_pos option ref;
      i_overall_rel_pos : include_rel_pos option ref;
      i_is_in_ifdef : bool;
      i_content : (Common.filename * program) option;
    }
    and inc_file =
      Ast_c.inc_file =
        Local of inc_elem list
      | NonLocal of inc_elem list
      | Weird of string
    and inc_elem = string
    and include_rel_pos =
      Ast_c.include_rel_pos = {
      first_of : string list list;
      last_of : string list list;
    }
    and ifdef_directive =
      Ast_c.ifdef_directive =
        IfdefDirective of (ifdefkind * matching_tag) wrap
    and ifdefkind =
      Ast_c.ifdefkind =
        Ifdef of ifdef_guard
      | IfdefElseif of ifdef_guard
      | IfdefElse
      | IfdefEndif
    and ifdef_guard =
      Ast_c.ifdef_guard =
        Gifdef of macro_symbol
      | Gifndef of macro_symbol
      | Gif_str of Lexing.position * string
      | Gif of expression
      | Gnone
    and macro_symbol = string
    and matching_tag = Ast_c.matching_tag = IfdefTag of (int * int)
    and toplevel =
      Ast_c.toplevel =
        Declaration of declaration
      | Definition of definition
      | CppTop of cpp_directive
      | IfdefTop of ifdef_directive
      | MacroTop of string * argument wrap2 list * il
      | EmptyDef of il
      | NotParsedCorrectly of il
      | FinalDef of info
      | Namespace of toplevel list * il
      | TemplateDefinition of templateParameterType wrap2 list * toplevel * il
    and templateParameterType =
      Ast_c.templateParameterType =
	TypenameOrClassParam of (name * fullType option) wrap
      | VarNameParam of (fullType * name * initialiser option) wrap
      | TemplateParam of (templateParameterType wrap2 list * templateParameterType) wrap
    and program = toplevel list
    and metavars_binding =
        (Ast_cocci.meta_name, metavar_binding_kind) Common.assoc
    and newlines =
      Ast_c.newlines =
	Keep | Compress
    and metavar_binding_kind =
      Ast_c.metavar_binding_kind =
        MetaIdVal of string
      | MetaFuncVal of string
      | MetaLocalFuncVal of string
      | MetaExprVal of expression * expression * Ast_cocci.meta_name list * stripped
      | MetaExprListVal of argument wrap2 list * argument wrap2 list
      | MetaParamVal of parameterType * parameterType
      | MetaParamListVal of parameterType wrap2 list * parameterType wrap2 list
      | MetaTemplateParamVal
	of templateParameterType * templateParameterType
      | MetaTemplateParamListVal
	of templateParameterType wrap2 list * templateParameterType wrap2 list
      | MetaTypeVal of fullType * fullType
      | MetaInitVal of initialiser * initialiser
      | MetaInitListVal of newlines * initialiser wrap2 list * initialiser wrap2 list
      | MetaDeclVal of declaration * declaration
      | MetaFieldVal of field * field
      | MetaFieldListVal of field list * field list
      | MetaStmtVal of statement * statement * stripped
      | MetaStmtListVal of statement_sequencable list * statement_sequencable list * stripped
      | MetaDParamListVal of string wrap wrap2 list
      | MetaFmtVal of string_format
      | MetaAttrArgVal of attr_arg * attr_arg
      | MetaFragListVal of string_fragment list
      | MetaAssignOpVal of assignOp
      | MetaBinaryOpVal of binaryOp
      | MetaPragmaInfoVal of info
      | MetaPosVal of (Ast_cocci.fixpos * Ast_cocci.fixpos)
      | MetaPosValList of
          (Common.filename * string * (posl * posl) option * posl * posl)
          list
      | MetaComValList of (Token_c.comment_like_token list *
			   Token_c.comment_like_token list *
			   Token_c.comment_like_token list) list
      | MetaListlenVal of int
      | MetaNoVal
    and stripped = Ast_c.stripped = WITH_TYPES | WITHOUT_TYPES
    and comments_around =
      Ast_c.comments_around = {
      mbefore : Token_c.comment_like_token list;
      mafter : Token_c.comment_like_token list;
      mbefore2 : comment_and_relative_pos list;
      mafter2 : comment_and_relative_pos list;
    }
    and comment_and_relative_pos =
      Ast_c.comment_and_relative_pos = {
      minfo : Common.parse_info;
      mpos : int;
    }
    and comment = Common.parse_info
    and com = comment list ref
    val nullQualif : typeQualifierbis * 'a list
    val nQ : typeQualifierbis * 'a list
    val defaultInt : typeCbis
    val noType : unit -> ('a option * test) ref
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
    val fakeInfo : befaft -> info
    val noii : 'a list
    val noattr : 'a list
    val noi_content : (Common.filename * program) option
    val unwrap : 'a * 'b -> 'a
    val unwrap2 : 'a * 'b -> 'a
    val unwrap_expr : ('a * 'b) * 'c -> 'a
    val rewrap_expr : ('a * 'b) * 'c -> 'd -> ('d * 'b) * 'c
    val unwrap_typeC : fullType -> typeCbis
    val rewrap_typeC : fullType -> typeCbis -> fullType
    val unwrap_st : 'a * 'b -> 'a
    val mk_e : 'a -> 'b -> ('a * ('c option * test) ref) * 'b
    val mk_e_bis : 'a -> 'b -> 'c -> ('a * 'b) * 'c
    val mk_ty : 'a -> 'b -> (typeQualifierbis * 'c list) * attribute list * ('a * 'b)
    val mk_tybis : 'a -> 'b -> 'a * 'b
    val mk_st : 'a -> 'b -> 'a * 'b
    val get_ii_typeC_take_care : 'a * 'b -> 'b
    val get_ii_st_take_care : 'a * 'b -> 'b
    val get_st_and_ii : 'a * 'b -> 'a * 'b
    val get_ty_and_ii : 'a * attribute list * ('b * 'c) -> 'b * 'c
    val get_e_and_ii : 'a * 'b -> 'a * 'b
    val get_type_expr : ('a * 'b ref) * 'c -> 'b
    val set_type_expr : ('a * 'b ref) * 'c -> 'b -> unit
    val get_onlytype_expr :
      ('a * (('b * 'c) option * 'd) ref) * 'e -> 'b option
    val rewrap_str : string -> info -> info
    val rewrap_charpos : int -> info -> info
    val rewrap_col : int -> info -> info
    val rewrap_pinfo : parse_info -> info -> info
    val get_opi : parse_info -> Common.parse_info
    val str_of_info : info -> string
    val get_info : (Common.parse_info -> 'a) -> info -> 'a
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
    type posrv =
      Ast_c.posrv =
        Real of Common.parse_info
      | Virt of virtual_position
    val compare_pos : info -> info -> int
    val equal_posl : 'a * 'b -> 'a * 'b -> bool
    val compare_posl : int * int -> int * int -> int
    val info_to_fixpos : info -> Ast_cocci.fixpos
    val is_test : expression -> bool
    val al_info : int -> info -> info
    val semi_al_info : info -> info
    val real_al_info : info -> info
    val al_info_cpp : int -> info -> info
    val semi_al_info_cpp : info -> info
    val real_al_info_cpp : bool -> info -> info
    val split_comma : 'a wrap2 list -> ('a, il) Common.either list
    val unsplit_comma : ('a, il) Common.either list -> 'a wrap2 list
    val split_nocomma : 'a list -> ('a, il) Common.either list
    val unsplit_nocomma : ('a, il) Common.either list -> 'a list
    val s_of_inc_file : inc_file -> string
    val s_of_inc_file_bis : inc_file -> string
    val s_of_attr : (attributebis * info list) list -> string
    val str_of_name : name -> string
    val get_s_and_info_of_name : name -> string * info
    val info_of_name : name -> info
    val info_of_type : 'a * attribute list * (typeCbis * il) -> parse_info option
    val name_of_parameter : parameterType -> string option
    val put_annot_info :
      info -> Token_annot.annot_key -> Token_annot.annot_val -> unit
    val get_annot_info :
      info -> Token_annot.annot_key -> Token_annot.annot_val option
    val get_comments_before : info -> Token_c.comment_like_token list
    val get_comments_after : info -> Token_c.comment_like_token list
  end
module Parse_c :
  sig
    type program2 = toplevel2 list
    and extended_program2 =
        toplevel2 list *
        (string, Lexer_parser.identkind) Common.scoped_h_env *
        (string, Cpp_token_c.define_def) Hashtbl.t
    and toplevel2 = Ast_c.toplevel * info_item
    and info_item = string * Parser_c.token list
    type 'a generic_parse_info =
	'a Parse_c.generic_parse_info = {
      filename : string;
      ranges : Parse_c.line_restriction list option;
      parse_trees : 'a;
      statistics : Parsing_stat.parsing_stat;
    }
    type parse_info = program2 generic_parse_info
    type extended_parse_info = extended_program2 generic_parse_info
    val _defs : (string, Cpp_token_c.define_def) Hashtbl.t ref
    val _defs_builtins : (string, Cpp_token_c.define_def) Hashtbl.t ref
    val init_defs_macros : Common.filename -> unit
    val init_defs_builtins : Common.filename -> unit
    val parse_c_and_cpp :
      bool -> bool -> Common.filename -> program2 * Parsing_stat.parsing_stat
    val parse_c_and_cpp_keep_typedefs :
      (string, Lexer_parser.identkind) Common.scoped_h_env option ->
      (string, Cpp_token_c.define_def) Hashtbl.t option ->
      bool ->
      bool ->
      Common.filename -> extended_parse_info * extended_parse_info list
    val parse_cache :
      (string, Lexer_parser.identkind) Common.scoped_h_env option ->
      bool ->
      bool ->
      Common.filename ->
      bool -> extended_parse_info * extended_parse_info list
    val extract_macros :
      Common.filename -> (string, Cpp_token_c.define_def) Common.assoc
    val tokens : ?profile:bool -> Common.filename -> Parser_c.token list
    val tokens_of_string :
	string -> Lexing.position option -> Parser_c.token list
    val parse : Common.filename -> Ast_c.program
    val type_of_string : string -> Ast_c.fullType
    val statement_of_string : string -> Ast_c.statement
    val cstatement_of_string : string -> Ast_c.statement
    val cexpression_of_string : string -> Ast_c.expression
    val print_commentized : Parser_c.token list -> unit
    val program_of_program2 : program2 -> Ast_c.program
    type parse_error_function =
        int ->
        Parser_c.token list -> int * int -> string array -> int -> unit
  end
module Parser_c :
  sig
    type token =
      Parser_c.token =
        TUnknown of Ast_c.info
      | TCommentSpace of Ast_c.info
      | TCommentNewline of Ast_c.info
      | TComment of Ast_c.info
      | TInt of ((string * (Ast_c.sign * Ast_c.base)) * Ast_c.info)
      | TFloat of ((string * Ast_c.floatType) * Ast_c.info)
      | TChar of ((string * Ast_c.isWchar) * Ast_c.info)
      | TString of ((string * Ast_c.isWchar) * Ast_c.info)
      | TQuote of ((string * Ast_c.isWchar) * Ast_c.info)
      | TPct of Ast_c.info
      | TFormat of (string * Ast_c.info)
      | TSubString of (string * Ast_c.info)
      | TDecimal of ((string * string * string) * Ast_c.info)
      | TIdent of (string * Ast_c.info)
      | TKRParam of (string * Ast_c.info)
      | Tconstructorname of (string * Ast_c.info)
      | TypedefIdent of (string * Ast_c.info)
      | TOPar of Ast_c.info
      | TCPar of Ast_c.info
      | TOBrace of Ast_c.info
      | TCBrace of Ast_c.info
      | TOCro of Ast_c.info
      | TCCro of Ast_c.info
      | TOCroCro of Ast_c.info
      | TDot of Ast_c.info
      | TColonColon of Ast_c.info
      | TComma of Ast_c.info
      | TNoComma of Ast_c.info
      | TPtrOp of Ast_c.info
      | TQualType of Ast_c.info
      | TQualExp of Ast_c.info
      | TQualId of Ast_c.info
      | TInc of Ast_c.info
      | TDec of Ast_c.info
      | TAssign of Ast_c.assignOp
      | TEq of Ast_c.info
      | TWhy of Ast_c.info
      | TTilde of Ast_c.info
      | TBang of Ast_c.info
      | TEllipsis of Ast_c.info
      | TDotDot of Ast_c.info
      | TPtVirg of Ast_c.info
      | TOrLog of Ast_c.info
      | TAndLog of Ast_c.info
      | TOr of Ast_c.info
      | TXor of Ast_c.info
      | TAnd of Ast_c.info
      | TEqEq of Ast_c.info
      | TNotEq of Ast_c.info
      | TInf of Ast_c.info
      | TSup of Ast_c.info
      | TInf3 of Ast_c.info
      | TSup3 of Ast_c.info
      | TInfEq of Ast_c.info
      | TSupEq of Ast_c.info
      | TShl of Ast_c.info
      | TShr of Ast_c.info
      | TPlus of Ast_c.info
      | TMinus of Ast_c.info
      | TMul of Ast_c.info
      | TDiv of Ast_c.info
      | TMod of Ast_c.info
      | TMax of Ast_c.info
      | TMin of Ast_c.info
      | Tchar of Ast_c.info
      | Tshort of Ast_c.info
      | Tint of Ast_c.info
      | Tdouble of Ast_c.info
      | Tfloat of Ast_c.info
      | Tcomplex of Ast_c.info
      | Tlong of Ast_c.info
      | Tunsigned of Ast_c.info
      | Tsigned of Ast_c.info
      | Tvoid of Ast_c.info
      | Tsize_t of Ast_c.info
      | Tssize_t of Ast_c.info
      | Tptrdiff_t of Ast_c.info
      | TautoType of Ast_c.info
      | Tauto of Ast_c.info
      | Tregister of Ast_c.info
      | Textern of Ast_c.info
      | Tstatic of Ast_c.info
      | Talignas of Ast_c.info
      | Ttypedef of Ast_c.info
      | Tconst of Ast_c.info
      | Tvolatile of Ast_c.info
      | Tstruct of Ast_c.info
      | Tunion of Ast_c.info
      | Tenum of Ast_c.info
      | Tdecimal of Ast_c.info
      | Texec of Ast_c.info
      | Ttemplate of Ast_c.info
      | Tbreak of Ast_c.info
      | Telse of Ast_c.info
      | Tswitch of Ast_c.info
      | Tcase of Ast_c.info
      | Tcontinue of Ast_c.info
      | Tfor of Ast_c.info
      | Tdo of Ast_c.info
      | Ttry of (Ast_c.info)
      | Tif of Ast_c.info
      | Twhile of Ast_c.info
      | Tcatch of Ast_c.info
      | Treturn of Ast_c.info
      | Tgoto of Ast_c.info
      | Tscopedguard of Ast_c.info
      | Tdefault of Ast_c.info
      | Tsizeof of Ast_c.info
      | Tnew of Ast_c.info
      | Tdelete of Ast_c.info
      | Tusing of Ast_c.info
      | Tdefined of Ast_c.info
      | TOParCplusplusInit of Ast_c.info
      | Tnamespace of Ast_c.info
      | Tcpp_struct of Ast_c.info
      | Tcpp_union of Ast_c.info
      | Tclass of Ast_c.info
      | Tprivate of Ast_c.info
      | Tpublic of Ast_c.info
      | Tprotected of Ast_c.info
      | Toperator of Ast_c.info
      | TTemplateStart of Ast_c.info
      | TTemplateEnd of Ast_c.info
      | TTemplateEndSup of Ast_c.info
      | TTemplateEndTemplateEnd of Ast_c.info
      | TTemplateEndTemplateEndTemplateEnd of Ast_c.info
      | Tfinal of Ast_c.info
      | Tvirtual of Ast_c.info
      | Ttypename of Ast_c.info
      | Trestrict of Ast_c.info
      | Tasm of Ast_c.info
      | Tattribute of Ast_c.info
      | TattributeNoarg of Ast_c.info
      | Tinline of Ast_c.info
      | Ttypeof of Ast_c.info
      | TDefine of Ast_c.info
      | TCppEscapedNewline of Ast_c.info
      | TCppConcatOp of Ast_c.info
      | TOParDefine of Ast_c.info
      | TOBraceDefineInit of Ast_c.info
      | TIdentDefine of (string * Ast_c.info)
      | TDefEOL of Ast_c.info
      | TInclude of (string * string * bool ref * Ast_c.info)
      | TIncludeStart of (Ast_c.info * bool ref)
      | TIncludeFilename of (string * Ast_c.info)
      | TIfdef of (Ast_c.ifdef_guard * (int * int) option ref * Ast_c.info)
      | TIfdefelif of
          (Ast_c.ifdef_guard * (int * int) option ref * Ast_c.info)
      | TIfdefelse of ((int * int) option ref * Ast_c.info)
      | TEndif of ((int * int) option ref * Ast_c.info)
      | TIfdefBool of (bool * (int * int) option ref * Ast_c.info)
      | TIfdefMisc of (bool * (int * int) option ref * Ast_c.info)
      | TIfdefVersion of (bool * (int * int) option ref * Ast_c.info)
      | TUifdef of Ast_c.info
      | TUelseif of Ast_c.info
      | TUendif of Ast_c.info
      | TUndef of Ast_c.info
      | TPrePragma of (Ast_c.info*Ast_c.info*string*Ast_c.info*
			 Ast_c.info*Ast_c.info)
      | TPragma of (Ast_c.info)
      | TPragmaString of (Ast_c.info)
      | TCppDirectiveOther of Ast_c.info
      | TMacroAttr of (string * Ast_c.info)
      | TMacroAttrArgs of (string * Ast_c.info)
      | TMacroStmt of (string * Ast_c.info)
      | TMacroIdStmt of (string * Ast_c.info)
      | TMacroIdentBuilder of (string * Ast_c.info)
      | TMacroString of (string * Ast_c.info)
      | TMacroDecl of (string * Ast_c.info)
      | TMacroDeclConst of Ast_c.info
      | TMacroIterator of (string * Ast_c.info)
      | TCommentSkipTagStart of Ast_c.info
      | TCommentSkipTagEnd of Ast_c.info
      | TCParEOL of Ast_c.info
      | TAction of Ast_c.info
      | TCommentMisc of Ast_c.info
      | TCommentCpp of (Token_c.cppcommentkind * Ast_c.info)
      | EOF of Ast_c.info
      | TTODO of Ast_c.info
    val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast_c.program
    val celem : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast_c.toplevel
    val statement :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast_c.statement
    val expr : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast_c.expression
    val type_name :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast_c.fullType
  end
module Lexer_c :
  sig
    val pr2 : string -> unit
    val pr2_once : string -> unit
    exception Lexical of string
    val tok : Lexing.lexbuf -> string
    val token : Lexing.lexbuf -> Parser_c.token
    val char : Lexing.lexbuf -> string
    val string : Lexing.lexbuf -> string
    val comment : Lexing.lexbuf -> string
  end
module Pretty_print_c :
  sig
    type type_with_ident = Ast_c.fullType -> (unit -> unit) -> unit
    type type_with_ident_rest = Ast_c.fullType -> (unit -> unit) -> unit
    type 'a printer = 'a -> unit
    type pretty_printers =
      Pretty_print_c.pretty_printers = {
      expression : Ast_c.expression printer;
      assignOp : Ast_c.assignOp printer;
      binaryOp : Ast_c.binaryOp printer;
      arg_list : Ast_c.argument Ast_c.wrap2 list printer;
      arg : Ast_c.argument printer;
      statement : Ast_c.statement printer;
      statement_seq_list : Ast_c.statement_sequencable list printer;
      decl : Ast_c.declaration printer;
      field : Ast_c.field printer;
      field_list : Ast_c.field list printer;
      init : Ast_c.initialiser printer;
      init_list : (Ast_c.newlines * Ast_c.initialiser Ast_c.wrap2 list) printer;
      param : Ast_c.parameterType printer;
      paramlist : Ast_c.parameterType Ast_c.wrap2 list printer;
      template_param : Ast_c.templateParameterType printer;
      template_paramlist : Ast_c.templateParameterType Ast_c.wrap2 list printer;
      dparamlist : string Ast_c.wrap Ast_c.wrap2 list printer;
      ty : Ast_c.fullType printer;
      type_with_ident : type_with_ident;
      base_type       : Ast_c.fullType printer;
      type_with_ident_rest : type_with_ident_rest;
      toplevel : Ast_c.toplevel printer;
      fragment : Ast_c.string_fragment printer;
      fragment_list : Ast_c.string_fragment list printer;
      format : Ast_c.string_format printer;
      attribute : Ast_c.attribute printer;
      attr_arg : Ast_c.attr_arg printer;
      flow : Control_flow_c.node printer;
      name : Ast_c.name printer;
    }
    val mk_pretty_printers :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer ->
      pr_nl:unit printer ->
      pr_indent:unit printer ->
      pr_outdent:unit printer -> pr_unindent:unit printer -> pretty_printers
    val pp_expression_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.expression printer
    val pp_assignOp_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.assignOp printer
    val pp_binaryOp_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.binaryOp printer
    val pp_arg_list_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.argument Ast_c.wrap2 list printer
    val pp_arg_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.argument printer
    val pp_decl_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.declaration printer
    val pp_field_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.field printer
    val pp_field_list_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.field list printer
    val pp_statement_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.statement printer
    val pp_statement_seq_list_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> pr_nl:unit printer -> Ast_c.statement_sequencable list printer
    val pp_param_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.parameterType printer
    val pp_param_list_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.parameterType Ast_c.wrap2 list printer
    val pp_define_param_list_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> string Ast_c.wrap Ast_c.wrap2 list printer
    val pp_type_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.fullType printer
    val pp_init_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.initialiser printer
    val pp_init_list_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer ->
	(Ast_c.newlines * Ast_c.initialiser Ast_c.wrap2 list) printer
    val pp_string_fragment_list_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.string_fragment list printer
    val pp_string_format_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.string_format printer
    val pp_program_gen :
      pr_elem:Ast_c.info printer ->
      pr_space:unit printer -> Ast_c.toplevel printer
    val pp_expression_simple : Ast_c.expression printer
    val pp_assignOp_simple : Ast_c.assignOp printer
    val pp_binaryOp_simple : Ast_c.binaryOp printer
    val pp_init_simple : Ast_c.initialiser printer
    val pp_type_simple : Ast_c.fullType printer
    val pp_decl_simple : Ast_c.declaration printer
    val pp_field_simple : Ast_c.field printer
    val pp_statement_simple : Ast_c.statement printer
    val pp_statement_seq_list_simple :
      Ast_c.statement_sequencable list printer
    val pp_toplevel_simple : Ast_c.toplevel printer
    val pp_string_fragment_simple : Ast_c.string_fragment printer
    val pp_string_format_simple : Ast_c.string_format printer
    val debug_info_of_node :
      Control_flow_c.G.key -> Control_flow_c.cflow -> string
    val string_of_expression : Ast_c.expression -> string
    val string_of_fullType : Ast_c.fullType -> string
  end
module Lib_parsing_c :
  sig
    val pr2 : string -> unit
    val pr2_once : string -> unit
    val al_expr : Ast_c.expression -> Ast_c.expression
    val al_declaration : Ast_c.declaration -> Ast_c.declaration
    val al_field : Ast_c.field -> Ast_c.field
    val al_statement : Ast_c.statement -> Ast_c.statement
    val al_statement_seq_list :
      Ast_c.statement_sequencable list -> Ast_c.statement_sequencable list
    val al_type : Ast_c.fullType -> Ast_c.fullType
    val al_init : Ast_c.initialiser -> Ast_c.initialiser
    val al_inits :
      Ast_c.initialiser Ast_c.wrap2 list ->
      Ast_c.initialiser Ast_c.wrap2 list
    val al_param : Ast_c.parameterType -> Ast_c.parameterType
    val al_params :
      Ast_c.parameterType Ast_c.wrap2 list ->
      Ast_c.parameterType Ast_c.wrap2 list
    val al_define_params :
      string Ast_c.wrap Ast_c.wrap2 list ->
      string Ast_c.wrap Ast_c.wrap2 list
    val al_arguments :
      Ast_c.argument Ast_c.wrap2 list -> Ast_c.argument Ast_c.wrap2 list
    val al_fields : Ast_c.field list -> Ast_c.field list
    val al_string_format : Ast_c.string_format -> Ast_c.string_format
    val al_string_fragments :
      Ast_c.string_fragment list -> Ast_c.string_fragment list
    val al_ii : Ast_c.info list -> Ast_c.info list
    val al_inh_expr : Ast_c.expression -> Ast_c.expression
    val al_inh_declaration : Ast_c.declaration -> Ast_c.declaration
    val al_inh_field : Ast_c.field -> Ast_c.field
    val al_inh_field_list : Ast_c.field list -> Ast_c.field list
    val al_inh_statement : Ast_c.statement -> Ast_c.statement
    val al_inh_statement_seq_list :
      Ast_c.statement_sequencable list -> Ast_c.statement_sequencable list
    val al_inh_type : Ast_c.fullType -> Ast_c.fullType
    val al_inh_init : Ast_c.initialiser -> Ast_c.initialiser
    val al_inh_inits :
      Ast_c.initialiser Ast_c.wrap2 list ->
      Ast_c.initialiser Ast_c.wrap2 list
    val al_inh_arguments :
      Ast_c.argument Ast_c.wrap2 list -> Ast_c.argument Ast_c.wrap2 list
    val al_inh_string_format : Ast_c.string_format -> Ast_c.string_format
    val al_inh_string_fragments :
      Ast_c.string_fragment list -> Ast_c.string_fragment list
    val semi_al_expr : Ast_c.expression -> Ast_c.expression
    val semi_al_declaration : Ast_c.declaration -> Ast_c.declaration
    val semi_al_field : Ast_c.field -> Ast_c.field
    val semi_al_fields : Ast_c.field list -> Ast_c.field list
    val semi_al_statement : Ast_c.statement -> Ast_c.statement
    val semi_al_statement_seq_list :
      Ast_c.statement_sequencable list -> Ast_c.statement_sequencable list
    val semi_al_type : Ast_c.fullType -> Ast_c.fullType
    val semi_al_init : Ast_c.initialiser -> Ast_c.initialiser
    val semi_al_inits :
      Ast_c.initialiser Ast_c.wrap2 list ->
      Ast_c.initialiser Ast_c.wrap2 list
    val semi_al_param : Ast_c.parameterType -> Ast_c.parameterType
    val semi_al_params :
      Ast_c.parameterType Ast_c.wrap2 list ->
      Ast_c.parameterType Ast_c.wrap2 list
    val semi_al_define_params :
      string Ast_c.wrap Ast_c.wrap2 list ->
      string Ast_c.wrap Ast_c.wrap2 list
    val semi_al_arguments :
      Ast_c.argument Ast_c.wrap2 list -> Ast_c.argument Ast_c.wrap2 list
    val semi_al_string_format : Ast_c.string_format -> Ast_c.string_format
    val semi_al_string_fragments :
      Ast_c.string_fragment list -> Ast_c.string_fragment list
    val real_al_expr : Ast_c.expression -> Ast_c.expression
    val real_al_statement : Ast_c.statement -> Ast_c.statement
    val real_al_statement_seq_list :
      Ast_c.statement_sequencable list -> Ast_c.statement_sequencable list
    val ii_of_def : Ast_c.definition -> Ast_c.info list
    val ii_of_decl : Ast_c.declaration -> Ast_c.info list
    val ii_of_field : Ast_c.field -> Ast_c.info list
    val ii_of_node : Control_flow_c.node -> Ast_c.info list
    val ii_of_expr : Ast_c.expression -> Ast_c.info list
    val ii_of_assignOp : Ast_c.assignOp -> Ast_c.info list
    val ii_of_binaryOp : Ast_c.binaryOp -> Ast_c.info list
    val ii_of_stmt : Ast_c.statement -> Ast_c.info list
    val ii_of_stmtseq : Ast_c.statement_sequencable -> Ast_c.info list
    val ii_of_stmtseqlist :
      Ast_c.statement_sequencable list -> Ast_c.info list
    val ii_of_args :
      (Ast_c.argument, Ast_c.il) Common.either list -> Ast_c.info list
    val ii_of_type : Ast_c.fullType -> Ast_c.info list
    val ii_of_ini : Ast_c.initialiser -> Ast_c.info list
    val ii_of_inis :
      (Ast_c.initialiser, Ast_c.il) Common.either list -> Ast_c.info list
    val ii_of_param : Ast_c.parameterType -> Ast_c.info list
    val ii_of_params :
      (Ast_c.parameterType, Ast_c.il) Common.either list -> Ast_c.info list
    val ii_of_enum_fields :
      (Ast_c.oneEnumType, Ast_c.il) Common.either list -> Ast_c.info list
    val ii_of_struct_fields : Ast_c.field list -> Ast_c.info list
    val ii_of_cst :
      (Ast_c.constant, string) Common.either Ast_c.wrap -> Ast_c.info list
    val ii_of_fragments :
      (Ast_c.string_fragment, Ast_c.il) Common.either list -> Ast_c.info list
    val ii_of_format : Ast_c.string_format -> Ast_c.info list
    val ii_of_define_params :
      (string Ast_c.wrap, Ast_c.il) Common.either list -> Ast_c.info list
    val ii_of_ident_list :
      (Ast_c.name, Ast_c.il) Common.either list -> Ast_c.info list
    val ii_of_exec_code_list :
      (Ast_c.exec_code, Ast_c.il) Common.either list -> Ast_c.info list
    val ii_of_attrs :
      (Ast_c.attribute, Ast_c.il) Common.either list -> Ast_c.info list
    val ii_of_toplevel : Ast_c.toplevel -> Ast_c.info list
    val max_min_ii_by_pos : Ast_c.info list -> Ast_c.info * Ast_c.info
    val info_to_fixpos : Ast_c.info -> Ast_cocci.fixpos
    val max_min_by_pos :
      Ast_c.info list -> Ast_cocci.fixpos * Ast_cocci.fixpos
    val lin_col_by_pos :
      Ast_c.info list ->
      Common.filename * string * Ast_c.posl * Ast_c.posl
    val min_pinfo_of_node : Control_flow_c.node -> Common.parse_info
    val stmt_elems_of_sequencable :
      Ast_c.statement_sequencable list -> Ast_c.statement list
  end
module Visitor_c :
  sig
    type visitor_c =
      Visitor_c.visitor_c = {
      kexpr :
        (Ast_c.expression -> unit) * visitor_c -> Ast_c.expression -> unit;
      kassignOp :
        (Ast_c.assignOp -> unit) * visitor_c -> Ast_c.assignOp -> unit;
      kbinaryOp :
        (Ast_c.binaryOp -> unit) * visitor_c -> Ast_c.binaryOp -> unit;
      kstatement :
        (Ast_c.statement -> unit) * visitor_c -> Ast_c.statement -> unit;
      ktype : (Ast_c.fullType -> unit) * visitor_c -> Ast_c.fullType -> unit;
      kdecl :
        (Ast_c.declaration -> unit) * visitor_c -> Ast_c.declaration -> unit;
      konedecl_opt : bool -> (Ast_c.onedecl -> unit) * visitor_c ->
	Ast_c.onedecl -> unit;
      kparam :
        (Ast_c.parameterType -> unit) * visitor_c ->
        Ast_c.parameterType -> unit;
      kdef :
        (Ast_c.definition -> unit) * visitor_c -> Ast_c.definition -> unit;
      kcondes :
	(Ast_c.c_plus_plus_constructor -> unit) * visitor_c -> Ast_c.c_plus_plus_constructor -> unit;
      kname : (Ast_c.name -> unit) * visitor_c -> Ast_c.name -> unit;
      kini :
        (Ast_c.initialiser -> unit) * visitor_c -> Ast_c.initialiser -> unit;
      kfield : (Ast_c.field -> unit) * visitor_c -> Ast_c.field -> unit;
      kcppdirective :
        (Ast_c.cpp_directive -> unit) * visitor_c ->
        Ast_c.cpp_directive -> unit;
      kifdefdirective :
        (Ast_c.ifdef_directive -> unit) * visitor_c ->
        Ast_c.ifdef_directive -> unit;
      kdefineval :
        (Ast_c.define_val -> unit) * visitor_c -> Ast_c.define_val -> unit;
      kstatementseq :
        (Ast_c.statement_sequencable -> unit) * visitor_c ->
        Ast_c.statement_sequencable -> unit;
      knode :
        (Control_flow_c.node -> unit) * visitor_c ->
        Control_flow_c.node -> unit;
      ktoplevel :
        (Ast_c.toplevel -> unit) * visitor_c -> Ast_c.toplevel -> unit;
      kfragment :
        (Ast_c.string_fragment -> unit) * visitor_c ->
        Ast_c.string_fragment -> unit;
      kformat :
        (Ast_c.string_format -> unit) * visitor_c ->
        Ast_c.string_format -> unit;
      kinfo : (Ast_c.info -> unit) * visitor_c -> Ast_c.info -> unit;
    }
    val default_visitor_c : visitor_c
    val vk_expr : visitor_c -> Ast_c.expression -> unit
    val vk_assignOp : visitor_c -> Ast_c.assignOp -> unit
    val vk_binaryOp : visitor_c -> Ast_c.binaryOp -> unit
    val vk_statement : visitor_c -> Ast_c.statement -> unit
    val vk_statement_sequencable :
      visitor_c -> Ast_c.statement_sequencable -> unit
    val vk_statement_sequencable_list :
      visitor_c -> Ast_c.statement_sequencable list -> unit
    val vk_type : visitor_c -> Ast_c.fullType -> unit
    val vk_decl : visitor_c -> Ast_c.declaration -> unit
    val vk_decl_list : visitor_c -> Ast_c.declaration list -> unit
    val vk_onedecl_opt : bool -> visitor_c -> Ast_c.onedecl -> unit
    val vk_onedecl : visitor_c -> Ast_c.onedecl -> unit
    val vk_ini : visitor_c -> Ast_c.initialiser -> unit
    val vk_ini_list : visitor_c -> Ast_c.initialiser Ast_c.wrap2 list -> unit
    val vk_inis_splitted :
      visitor_c -> (Ast_c.initialiser, Ast_c.il) Common.either list -> unit
    val vk_name : visitor_c -> Ast_c.name -> unit
    val vk_def : visitor_c -> Ast_c.definition -> unit
    val vk_node : visitor_c -> Control_flow_c.node -> unit
    val vk_string_fragment : visitor_c -> Ast_c.string_fragment -> unit
    val vk_string_fragments : visitor_c -> Ast_c.string_fragment list -> unit
    val vk_string_fragments_splitted :
      visitor_c ->
      (Ast_c.string_fragment, Ast_c.il) Common.either list -> unit
    val vk_string_format : visitor_c -> Ast_c.string_format -> unit
    val vk_info : visitor_c -> Ast_c.info -> unit
    val vk_toplevel : visitor_c -> Ast_c.toplevel -> unit
    val vk_program : visitor_c -> Ast_c.program -> unit
    val vk_argument : visitor_c -> Ast_c.argument -> unit
    val vk_argument_list :
      visitor_c -> Ast_c.argument Ast_c.wrap2 list -> unit
    val vk_args_splitted :
      visitor_c -> (Ast_c.argument, Ast_c.il) Common.either list -> unit
    val vk_param : visitor_c -> Ast_c.parameterType -> unit
    val vk_param_list :
      visitor_c -> Ast_c.parameterType Ast_c.wrap2 list -> unit
    val vk_params_splitted :
      visitor_c -> (Ast_c.parameterType, Ast_c.il) Common.either list -> unit
    val vk_struct_field : visitor_c -> Ast_c.field -> unit
    val vk_struct_fields : visitor_c -> Ast_c.field list -> unit
    val vk_struct_fieldkinds :
      visitor_c -> Ast_c.fieldkind Ast_c.wrap list -> unit
    val vk_enum_fields : visitor_c -> Ast_c.enumType -> unit
    val vk_enum_fields_splitted :
      visitor_c -> (Ast_c.oneEnumType, Ast_c.il) Common.either list -> unit
    val vk_cst :
      visitor_c -> (Ast_c.constant, string) Common.either Ast_c.wrap -> unit
    val vk_define_params :
      visitor_c -> string Ast_c.wrap Ast_c.wrap2 list -> unit
    val vk_define_params_splitted :
      visitor_c -> (string Ast_c.wrap, Ast_c.il) Common.either list -> unit
    val vk_ident_list_splitted :
      visitor_c -> (Ast_c.name, Ast_c.il) Common.either list -> unit
    val vk_exec_code_list_splitted :
      visitor_c -> (Ast_c.exec_code, Ast_c.il) Common.either list -> unit
    val vk_attrs_splitted :
      visitor_c -> (Ast_c.attribute, Ast_c.il) Common.either list -> unit
    type 'a inout = 'a -> 'a
    type visitor_c_s =
      Visitor_c.visitor_c_s = {
      kexpr_s :
        Ast_c.expression inout * visitor_c_s -> Ast_c.expression inout;
      kassignOp_s :
        Ast_c.assignOp inout * visitor_c_s -> Ast_c.assignOp inout;
      kbinaryOp_s :
        Ast_c.binaryOp inout * visitor_c_s -> Ast_c.binaryOp inout;
      kstatement_s :
        Ast_c.statement inout * visitor_c_s -> Ast_c.statement inout;
      ktype_s : Ast_c.fullType inout * visitor_c_s -> Ast_c.fullType inout;
      kdecl_s :
        Ast_c.declaration inout * visitor_c_s -> Ast_c.declaration inout;
      kdef_s : Ast_c.definition inout * visitor_c_s -> Ast_c.definition inout;
      kcondes_s: Ast_c.c_plus_plus_constructor inout * visitor_c_s -> Ast_c.c_plus_plus_constructor inout;
      kname_s : Ast_c.name inout * visitor_c_s -> Ast_c.name inout;
      kini_s :
        Ast_c.initialiser inout * visitor_c_s -> Ast_c.initialiser inout;
      kcppdirective_s :
        Ast_c.cpp_directive inout * visitor_c_s -> Ast_c.cpp_directive inout;
      kifdefdirective_s :
        Ast_c.ifdef_directive inout * visitor_c_s ->
        Ast_c.ifdef_directive inout;
      kdefineval_s :
        Ast_c.define_val inout * visitor_c_s -> Ast_c.define_val inout;
      kstatementseq_s :
        Ast_c.statement_sequencable inout * visitor_c_s ->
        Ast_c.statement_sequencable inout;
      kstatementseq_list_s :
        Ast_c.statement_sequencable list inout * visitor_c_s ->
        Ast_c.statement_sequencable list inout;
      knode_s :
        Control_flow_c.node inout * visitor_c_s -> Control_flow_c.node inout;
      ktoplevel_s :
        Ast_c.toplevel inout * visitor_c_s -> Ast_c.toplevel inout;
      kfragment_s :
        Ast_c.string_fragment inout * visitor_c_s ->
        Ast_c.string_fragment inout;
      kformat_s :
        Ast_c.string_format inout * visitor_c_s -> Ast_c.string_format inout;
      kinfo_s : Ast_c.info inout * visitor_c_s -> Ast_c.info inout;
    }
    val default_visitor_c_s : visitor_c_s
    val vk_expr_s : visitor_c_s -> Ast_c.expression -> Ast_c.expression
    val vk_assignOp_s : visitor_c_s -> Ast_c.assignOp -> Ast_c.assignOp
    val vk_binaryOp_s : visitor_c_s -> Ast_c.binaryOp -> Ast_c.binaryOp
    val vk_argument_s : visitor_c_s -> Ast_c.argument -> Ast_c.argument
    val vk_statement_s : visitor_c_s -> Ast_c.statement -> Ast_c.statement
    val vk_statement_sequencable_s :
      visitor_c_s ->
      Ast_c.statement_sequencable -> Ast_c.statement_sequencable
    val vk_statement_sequencable_list_s :
      visitor_c_s ->
      Ast_c.statement_sequencable list -> Ast_c.statement_sequencable list
    val vk_type_s : visitor_c_s -> Ast_c.fullType -> Ast_c.fullType
    val vk_decl_s : visitor_c_s -> Ast_c.declaration -> Ast_c.declaration
    val vk_onedecl_opt_s : bool -> visitor_c_s -> Ast_c.onedecl -> Ast_c.onedecl
    val vk_onedecl_s : visitor_c_s -> Ast_c.onedecl -> Ast_c.onedecl
    val vk_decl_list_s :
      visitor_c_s -> Ast_c.declaration list -> Ast_c.declaration list
    val vk_ini_s : visitor_c_s -> Ast_c.initialiser -> Ast_c.initialiser
    val vk_inis_splitted_s :
      visitor_c_s ->
      (Ast_c.initialiser, Ast_c.il) Common.either list ->
      (Ast_c.initialiser, Ast_c.il) Common.either list
    val vk_def_s : visitor_c_s -> Ast_c.definition -> Ast_c.definition
    val vk_name_s : visitor_c_s -> Ast_c.name -> Ast_c.name
    val vk_toplevel_s : visitor_c_s -> Ast_c.toplevel -> Ast_c.toplevel
    val vk_string_fragment_s :
      visitor_c_s -> Ast_c.string_fragment -> Ast_c.string_fragment
    val vk_string_fragments_s :
      visitor_c_s -> Ast_c.string_fragment list -> Ast_c.string_fragment list
    val vk_string_fragments_splitted_s :
      visitor_c_s ->
      (Ast_c.string_fragment, Ast_c.il) Common.either list ->
      (Ast_c.string_fragment, Ast_c.il) Common.either list
    val vk_string_format_s :
      visitor_c_s -> Ast_c.string_format -> Ast_c.string_format
    val vk_info_s : visitor_c_s -> Ast_c.info -> Ast_c.info
    val vk_ii_s : visitor_c_s -> Ast_c.info list -> Ast_c.info list
    val vk_node_s : visitor_c_s -> Control_flow_c.node -> Control_flow_c.node
    val vk_program_s : visitor_c_s -> Ast_c.program -> Ast_c.program
    val vk_arguments_s :
      visitor_c_s ->
      Ast_c.argument Ast_c.wrap2 list -> Ast_c.argument Ast_c.wrap2 list
    val vk_inis_s :
      visitor_c_s ->
      Ast_c.initialiser Ast_c.wrap2 list ->
      Ast_c.initialiser Ast_c.wrap2 list
    val vk_args_splitted_s :
      visitor_c_s ->
      (Ast_c.argument, Ast_c.il) Common.either list ->
      (Ast_c.argument, Ast_c.il) Common.either list
    val vk_params_s :
      visitor_c_s ->
      Ast_c.parameterType Ast_c.wrap2 list ->
      Ast_c.parameterType Ast_c.wrap2 list
    val vk_params_splitted_s :
      visitor_c_s ->
      (Ast_c.parameterType, Ast_c.il) Common.either list ->
      (Ast_c.parameterType, Ast_c.il) Common.either list
    val vk_param_s :
      visitor_c_s -> Ast_c.parameterType -> Ast_c.parameterType
    val vk_define_params_s :
      visitor_c_s ->
      string Ast_c.wrap Ast_c.wrap2 list ->
      string Ast_c.wrap Ast_c.wrap2 list
    val vk_define_params_splitted_s :
      visitor_c_s ->
      (string Ast_c.wrap, Ast_c.il) Common.either list ->
      (string Ast_c.wrap, Ast_c.il) Common.either list
    val vk_ident_list_splitted_s :
      visitor_c_s ->
      (Ast_c.name, Ast_c.il) Common.either list ->
      (Ast_c.name, Ast_c.il) Common.either list
    val vk_enum_fields_s : visitor_c_s -> Ast_c.enumType -> Ast_c.enumType
    val vk_enum_fields_splitted_s :
      visitor_c_s ->
      (Ast_c.oneEnumType, Ast_c.il) Common.either list ->
      (Ast_c.oneEnumType, Ast_c.il) Common.either list
    val vk_struct_field_s : visitor_c_s -> Ast_c.field -> Ast_c.field
    val vk_struct_fields_s :
      visitor_c_s -> Ast_c.field list -> Ast_c.field list
    val vk_exec_code_list_splitted_s :
      visitor_c_s ->
      (Ast_c.exec_code, Ast_c.il) Common.either list ->
      (Ast_c.exec_code, Ast_c.il) Common.either list
    val vk_attrs_splitted_s :
      visitor_c_s ->
      (Ast_c.attribute, Ast_c.il) Common.either list ->
      (Ast_c.attribute, Ast_c.il) Common.either list
    val vk_cst_s :
      visitor_c_s -> (Ast_c.constant, string) Common.either Ast_c.wrap inout
  end
module Regexp :
  sig
    type regexp = Regexp.regexp
    val pcre_support : bool ref
    val regexp : string -> regexp
    val string_match : regexp -> string -> bool
  end
module Cocciconfig :
  sig
    val version : string
    val path : string
    val std_iso : string ref
    val std_h : string ref
    val dynlink_is_native : bool
    val get_temp_dir_name : string
    val configure_flags : string
    val ocaml_version : string
    val python_interpreter : string ref
  end
module Flag :
  sig
    val sgrep_mode2 : bool ref
    val show_misc : bool ref
    val show_transinfo : bool ref
    val show_trying : bool ref
    val track_iso_usage : bool ref
    val worth_trying_opt : bool ref
    type scanner =
      Flag.scanner =
        IdUtils
      | Glimpse
      | CocciGrep
      | GitGrep
      | PatchDiff
      | PatchDiffRange of string * string
      | NoScanner
    val scanner : scanner ref
    val pyoutput : string ref
    val ocamlc : string ref
    val ocamlopt : string ref
    val ocamldep : string ref
    val ocamlfind : string ref
    val ocaml_lib_search_path : string list ref
    val patch : string option ref
    val make_hrule : string option ref
    val hrule_per_file : bool ref
    val currentfile : string option ref
    val currentfiles : string list ref
    val current_element : string ref
    val current_element_pos : (Ast_c.posl * Ast_c.posl) Lazy.t ref
    val dir : string ref
    val defined_virtual_rules : string list ref
    val defined_virtual_env : (string * string) list ref
    val set_defined_virtual_rules : string -> unit
    type c_plus_plus = Flag.c_plus_plus = Off | On of int option
    val c_plus_plus : c_plus_plus ref
    val ibm : bool ref
    val include_headers : bool ref
    val no_include_cache : bool ref
    val parmap_cores      : int option ref
    val parmap_chunk_size : int option ref
    exception UnreadableFile of string
    val cocci_attribute_names : string list ref
    val add_cocci_attribute_names : string -> unit
  end
module Flag_parsing_c :
  sig
    val path : string ref
    val std_h : string ref
    val common_h : string ref
    val cpp_i_opts : string list ref
    val cpp_d_opts : string list ref
    val show_parsing_error : bool ref
    val verbose_lexing : bool ref
    val verbose_parsing : bool ref
    val verbose_type : bool ref
    val verbose_cfg : bool ref
    val verbose_annotater : bool ref
    val verbose_unparsing : bool ref
    val verbose_visit : bool ref
    val verbose_cpp_ast : bool ref
    val verbose_includes : bool ref
    val filter_msg : bool ref
    val filter_msg_define_error : bool ref
    val filter_define_error : bool ref
    val filter_passed_level : int ref
    val pretty_print_type_info : bool ref
    val pretty_print_comment_info : bool ref
    val pretty_print_typedef_value : bool ref
    val show_flow_labels : bool ref
    val debug_lexer : bool ref
    val debug_etdt : bool ref
    val debug_typedef : bool ref
    val debug_cpp : bool ref
    val debug_cpp_ast : bool ref
    val debug_unparsing : bool ref
    val debug_cfg : bool ref
    val check_annotater : bool ref
    val label_strategy_2 : bool ref
    val cpp_directive_passing : bool ref
    val ifdef_directive_passing : bool ref
    val ifdef_to_if : bool ref
    val disable_multi_pass : bool ref
    val disable_add_typedef : bool ref
    val if0_passing : bool ref
    val add_typedef_root : bool ref
    val exts_ITU : bool ref
    val add : string list ref -> string -> unit
    val defined : string list ref
    val undefined : string list ref
    val diff_lines : string option ref
    val use_cache : bool ref
    val cache_prefix : string option ref
    val cache_limit : int option ref
    val int_thresholds :
        (int * int * string * string * string * string) option ref
    val set_int_bits : int -> unit
    val set_long_bits : int -> unit
    type spacing = Flag_parsing_c.spacing = LINUX | SMPL
    val spacing : spacing ref
    val indent : int ref
    val set_linux_spacing : 'a -> unit
    val set_smpl_spacing : 'a -> unit
    val max_width : int ref
    val no_loops : bool ref
    val no_gotos : bool ref
    val keep_comments : bool ref
    val parsing_header_for_types : bool ref
    val force_kr : bool ref
    val prevent_kr : bool ref
  end
module Iteration :
  sig
    type init_info =
        (string * string) * (string list * (string * string) list)
    val initialization_stack : init_info list ref
    val base_file_list : string list ref
    val parsed_virtual_rules : string list ref
    val parsed_virtual_identifiers : string list ref
    type pending_info = string list * string list * (string * string) list
    val add_pending_instance :
      string list option * string list * (string * string) list * bool ->
      unit
    val get_pending_instance : unit -> pending_info option
  end
module Commands :
  sig
    val ocamlfind_cmd : string
    val ocamlc_cmd : string
    val ocamlopt_cmd : string
    val ocamldep_cmd : string
  end
module Common :
  sig
    val debugger : bool ref
    type prof = Common.prof = PALL | PNONE | PSOME of string list
    val profile : prof ref
    val show_trace_profile : bool ref
    val verbose_level : int ref
    val disable_pr2_once : bool ref
    val save_tmp_files : bool ref
    type filename = string
    type dirname = string
    module BasicType : sig type filename = string end
    module Infix :
      sig
        val ( +> ) : 'a -> ('a -> 'b) -> 'b
        val ( =~ ) : string -> string -> bool
        val ( ==~ ) : string -> Str.regexp -> bool
      end
    val _tab_level_print : int ref
    val indent_do : (unit -> 'a) -> 'a
    val reset_pr_indent : unit -> unit
    val _prefix_pr : string ref
    val pr : string -> unit
    val pr_no_nl : string -> unit
    val pr_xxxxxxxxxxxxxxxxx : unit -> unit
    val _chan_pr2 : out_channel option ref
    val print_to_stderr : bool ref
    val pr2 : string -> unit
    val pr2_no_nl : string -> unit
    val pr2_xxxxxxxxxxxxxxxxx : unit -> unit
    val pr2_gen : 'a -> unit
    val _already_printed : (string, bool) Hashtbl.t
    val pr2_once : string -> unit
    val clear_pr2_once : unit -> unit
    val mk_pr2_wrappers : bool ref -> (string -> unit) * (string -> unit)
    val redirect_stdout_opt : filename option -> (unit -> 'a) -> 'a
    val redirect_stdout_stderr : filename -> (unit -> unit) -> unit
    val redirect_stdin : filename -> (unit -> 'a) -> 'a
    val redirect_stdin_opt : filename option -> (unit -> 'a) -> 'a
    val spf : ('a, unit, string) format -> 'a
    val _chan : out_channel ref
    val log : string -> unit
    val log2 : string -> unit
    val pause : unit -> unit
    val _trace_var : int ref
    val add_var : unit -> unit
    val dec_var : unit -> unit
    val get_var : unit -> int
    val print_n : int -> string -> unit
    val printerr_n : int -> string -> unit
    val _debug : bool ref
    val debug : (unit -> unit) -> unit
    val _profile_table : (string, float ref * int ref) Hashtbl.t ref
    val profile_code : string -> (unit -> 'a) -> 'a
    val profile_diagnostic : unit -> string
    val reset_profile : unit -> unit
    val profile_code_exclusif : string -> (unit -> 'a) -> 'a
    val profile_code_inside_exclusif_ok : string -> (unit -> 'a) -> 'a
    val report_if_take_time : int -> string -> (unit -> 'a) -> 'a
    val example : bool -> unit
    val example2 : string -> bool -> unit
    val assert_equal : 'a -> 'a -> unit
    val _list_bool : (string * bool) list ref
    val example3 : string -> bool -> unit
    val test_all : unit -> unit
    type score_result = Common.score_result = Ok | Pb of string | PbKnown of string
    type score = (string, score_result) Hashtbl.t
    type score_list = (string * score_result) list
    val empty_score : unit -> score
    val load_score : string -> unit -> score
    val save_score : score -> string -> unit
    val regression_testing : score -> filename -> unit
    val regression_testing_vs : score -> score -> score
    val total_scores : score -> int * int * int
    val print_total_score : score -> unit
    type 'a gen = unit -> 'a
    val ig : int gen
    val lg : 'a gen -> 'a list gen
    val pg : 'a gen -> 'b gen -> ('a * 'b) gen
    val polyg : int gen
    val ng : string gen
    val oneofl : 'a list -> 'a gen
    val oneof : 'a gen list -> 'a gen
    val always : 'a -> 'a gen
    val frequency : (int * 'a gen) list -> 'a gen
    val frequencyl : (int * 'a) list -> 'a gen
    val laws : string -> ('a -> bool) -> 'a gen -> 'a option
    val statistic_number : 'a list -> (int * 'a) list
    val statistic : 'a list -> (int * 'a) list
    val laws2 :
      string -> ('a -> bool * 'b) -> 'a gen -> 'a option * (int * 'b) list
    val get_value : filename -> 'a
    val read_value : filename -> 'a
    val write_value : 'a -> filename -> unit
    val marshal__to_string : 'a -> Marshal.extern_flags list -> string
    val marshal__from_string : string -> int -> 'a
    val _counter : int ref
    val _counter2 : int ref
    val _counter3 : int ref
    val counter : unit -> int
    val counter2 : unit -> int
    val counter3 : unit -> int
    type timestamp = int
    val print_between : (unit -> unit) -> ('a -> unit) -> 'a list -> unit
    val pp_do_in_box : (unit -> unit) -> unit
    val pp_do_in_zero_box : (unit -> unit) -> unit
    val pp : string -> unit
    val format_to_string : (unit -> unit) -> string
    val adjust_pp_with_indent : (unit -> unit) -> unit
    val adjust_pp_with_indent_and_header : string -> (unit -> unit) -> unit
    val ( +> ) : 'a -> ('a -> 'b) -> 'b
    val ( +!> ) : 'a ref -> ('a -> 'a) -> unit
    val ( $ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
    val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
    val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
    val id : 'a -> 'a
    val applyn : int -> ('a -> 'a) -> 'a -> 'a
    class ['a] shared_variable_hook :
      'a ->
      object
        val mutable data : 'a
        val mutable registered : (unit -> unit) list
        method get : 'a
        method modify : ('a -> 'a) -> unit
        method register : (unit -> unit) -> unit
        method set : 'a -> unit
      end
    val fixpoint : ('a -> 'a) -> 'a -> 'a
    val fixpoint_for_object :
      ((< equal : 'a -> bool; .. > as 'a) -> 'a) -> 'a -> 'a
    val add_hook :
      ('a -> ('a -> 'b) -> 'b) ref -> ('a -> ('a -> 'b) -> 'b) -> unit
    val add_hook_action : ('a -> unit) -> ('a -> unit) list ref -> unit
    val run_hooks_action : 'a -> ('a -> unit) list ref -> unit
    type 'a mylazy = unit -> 'a
    val save_excursion : 'a ref -> (unit -> 'b) -> 'b
    val unwind_protect : (unit -> 'a) -> (exn -> 'b) -> 'a
    val finalize : (unit -> 'a) -> (unit -> 'b) -> 'a
    val memoized : ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b
    val cache_computation :
      ?verbose:bool ->
      ?use_cache:bool -> filename -> string -> (unit -> 'a) -> 'a
    val cache_computation_robust :
      filename ->
      string -> filename list * 'a -> string -> (unit -> 'b) -> 'b
    val cache_computation_robust_in_dir :
      string option ->
      filename ->
      string -> filename list * 'a -> string -> (unit -> 'b) -> 'b
    val once : ('a -> unit) -> 'a -> unit
    val main_boilerplate : (unit -> unit) -> unit
    exception Todo
    exception Impossible of int
    exception Here
    exception ReturnExn
    exception Multi_found
    exception WrongFormat of string
    val internal_error : string -> 'a
    val warning : string -> 'a -> 'a
    val error_cant_have : 'a -> 'b
    type evotype = unit
    val evoval : evotype
    val check_stack_size : int -> unit
    val check_stack_nbfiles : int -> unit
    val _init_gc_stack : unit
    type arg_spec_full = Arg.key * Arg.spec * Arg.doc
    type cmdline_options = arg_spec_full list
    type options_with_title = string * string * arg_spec_full list
    type cmdline_sections = options_with_title list
    val parse_options :
      cmdline_options -> Arg.usage_msg -> string array -> string list
    val usage : Arg.usage_msg -> cmdline_options -> unit
    val short_usage : Arg.usage_msg -> short_opt:cmdline_options -> unit
    val long_usage :
      Arg.usage_msg ->
      short_opt:cmdline_options -> long_opt:cmdline_sections -> unit
    val arg_align2 : cmdline_options -> cmdline_options
    type flag_spec = Arg.key * Arg.spec * Arg.doc
    type action_spec = Arg.key * Arg.doc * action_func
    and action_func = string list -> unit
    type cmdline_actions = action_spec list
    exception WrongNumberOfArguments
    val mk_action_0_arg : (unit -> unit) -> action_func
    val mk_action_1_arg : (string -> unit) -> action_func
    val mk_action_2_arg : (string -> string -> unit) -> action_func
    val mk_action_n_arg : (string list -> unit) -> action_func
    val options_of_actions : string ref -> cmdline_actions -> cmdline_options
    val action_list : cmdline_actions -> Arg.key list
    val do_action : Arg.key -> string list -> cmdline_actions -> unit
    val ( ||| ) : 'a -> 'a -> 'a
    val ( ==> ) : bool -> bool -> bool
    val string_of_char : char -> string
    val string_of_chars : char list -> string
    val is_single : char -> bool
    val is_symbol : char -> bool
    val is_space : char -> bool
    val is_upper : char -> bool
    val is_lower : char -> bool
    val is_digit : char -> bool
    val cbetween : char -> char -> char -> bool
    val ( /! ) : int -> int -> int
    val do_n : int -> (unit -> unit) -> unit
    val foldn : ('a -> int -> 'a) -> 'a -> int -> 'a
    val pi : float
    val pi2 : float
    val pi4 : float
    val deg_to_rad : float -> float
    val clampf : float -> float
    val square : float -> float
    val power : int -> int -> int
    val between : 'a -> 'a -> 'a -> bool
    val between_strict : int -> int -> int -> bool
    val prime1 : int -> int option
    val prime : int -> int option
    val sum : int list -> int
    val product : int list -> int
    val decompose : int -> int list
    val sqr : float -> float
    type compare = Common.compare = Equal | Inf | Sup
    val ( <=> ) : 'a -> 'a -> compare
    val ( <==> ) : 'a -> 'a -> int
    type uint = int
    val int_of_base : string -> int -> int
    val int_of_stringbits : string -> int
    val int_of_octal : string -> int
    val ( += ) : int ref -> int -> unit
    val ( -= ) : int ref -> int -> unit
    val pourcent : int -> int -> int
    type 'a numdict =
      'a Common.numdict =
        NumDict of
          (('a -> 'a -> 'a) * ('a -> 'a -> 'a) * ('a -> 'a -> 'a) *
           ('a -> 'a))
    val add : 'a numdict -> 'a -> 'a -> 'a
    val mul : 'a numdict -> 'a -> 'a -> 'a
    val div : 'a numdict -> 'a -> 'a -> 'a
    val neg : 'a numdict -> 'a -> 'a
    val numd_int : int numdict
    val numd_float : float numdict
    module ArithFloatInfix :
      sig
        val ( + ) : float -> float -> float
        val ( - ) : float -> float -> float
        val ( / ) : float -> float -> float
        val ( * ) : float -> float -> float
        val ( +.. ) : int -> int -> int
        val ( -.. ) : int -> int -> int
        val ( /.. ) : int -> int -> int
        val ( *.. ) : int -> int -> int
        val ( += ) : float ref -> float -> unit
      end
    val _init_random : unit
    val random_list : 'a list -> 'a
    val randomize_list : 'a list -> 'a list
    type 'a pair = 'a * 'a
    type 'a triple = 'a * 'a * 'a
    val fst3 : 'a * 'b * 'c -> 'a
    val snd3 : 'a * 'b * 'c -> 'b
    val thd3 : 'a * 'b * 'c -> 'c
    val pair : ('a -> 'b) -> 'a * 'a -> 'b * 'b
    val snd : 'a * 'b -> 'b
    val fst : 'a * 'b -> 'a
    val double : 'a -> 'a * 'a
    val swap : 'a * 'b -> 'b * 'a
    val tuple_of_list1 : 'a list -> 'a
    val tuple_of_list2 : 'a list -> 'a * 'a
    val tuple_of_list3 : 'a list -> 'a * 'a * 'a
    val tuple_of_list4 : 'a list -> 'a * 'a * 'a * 'a
    val tuple_of_list5 : 'a list -> 'a * 'a * 'a * 'a * 'a
    val tuple_of_list6 : 'a list -> 'a * 'a * 'a * 'a * 'a * 'a
    type ('a, 'b) either = ('a, 'b) Common.either = Left of 'a | Right of 'b
    type ('a, 'b, 'c) either3 =
      ('a, 'b, 'c) Common.either3 =
        Left3 of 'a
      | Middle3 of 'b
      | Right3 of 'c
    val just : 'a option -> 'a
    val some : 'a option -> 'a
    val fmap : ('a -> 'b) -> 'a option -> 'b option
    val map_option : ('a -> 'b) -> 'a option -> 'b option
    val equal_option : ('a -> 'b -> bool) -> 'a option -> 'b option -> bool
    val default : 'a -> ('b -> 'a) -> 'b option -> 'a
    val do_option : ('a -> unit) -> 'a option -> unit
    val optionise : (unit -> 'a) -> 'a option
    val some_or : 'a option -> 'a -> 'a
    val partition_either :
      ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
    val filter_some : 'a option list -> 'a list
    val map_filter : ('a -> 'b option) -> 'a list -> 'b list
    val tail_map_filter : ('a -> 'b option) -> 'a list -> 'b list
    val find_some : ('a -> 'b option) -> 'a list -> 'b
    type bool3 = Common.bool3 = True3 | False3 | TrueFalsePb3 of string
    val _shareds : (string, string) Hashtbl.t
    val shared_string : string -> string
    val chop : string -> string
    val chop_dirsymbol : string -> string
    val ( <!!> ) : string -> int * int -> string
    val ( <!> ) : string -> int -> char
    val quote : string -> string
    val edit_distance : string -> string -> int
    val md5sum_of_string : string -> string
    val regexp_alpha : Str.regexp
    val regexp_int : Str.regexp
    val regexp_word : Str.regexp
    val _memo_compiled_regexp : (string, Str.regexp) Hashtbl.t
    val ( =~ ) : string -> string -> bool
    val ( ==~ ) : string -> Str.regexp -> bool
    val regexp_match : string -> string -> string
    val matched : int -> string -> string
    val matched1 : string -> string
    val matched2 : string -> string * string
    val matched3 : string -> string * string * string
    val matched4 : string -> string * string * string * string
    val matched5 : string -> string * string * string * string * string
    val matched6 :
      string -> string * string * string * string * string * string
    val matched7 :
      string -> string * string * string * string * string * string * string
    val string_match_substring : Str.regexp -> string -> bool
    val split : string -> string -> string list
    val split_list_regexp :
      string -> string list -> (string * string list) list
    val all_match : string -> string -> string list
    val filesuffix : filename -> string
    val fileprefix : filename -> string
    val adjust_ext_if_needed : filename -> string -> filename
    val filename_of_db : string * filename -> filename
    val dbe_of_filename : filename -> string * string * string
    val dbe_of_filename_safe :
      filename -> (string * string * string, string * string) either
    val filename_of_dbe : string * string * string -> filename
    val normalize_path : filename -> filename
    val is_relative : filename -> bool
    type langage = Common.langage = English | Francais | Deutsch
    type month =
      Common.month =
        Jan
      | Feb
      | Mar
      | Apr
      | May
      | Jun
      | Jul
      | Aug
      | Sep
      | Oct
      | Nov
      | Dec
    type year = Common.year = Year of int
    type day = Common.day = Day of int
    type date_dmy = Common.date_dmy = DMY of day * month * year
    type hour = Common.hour = Hour of int
    type minute = Common.minute = Min of int
    type second = Common.second = Sec of int
    type time_hms = Common.time_hms = HMS of hour * minute * second
    type full_date = date_dmy * time_hms
    type days = Common.days = Days of int
    type time_dmy = Common.time_dmy = TimeDMY of day * month * year
    type float_time = float
    val int_of_month : month -> int
    val month_of_string : string -> month
    val string_of_month : month -> string
    val string_of_unix_time : ?langage:langage -> Unix.tm -> string
    val rough_days_since_jesus : date_dmy -> days
    val rough_days_between_dates : date_dmy -> date_dmy -> days
    val is_more_recent : date_dmy -> date_dmy -> bool
    val this_year : unit -> int
    val list_of_string : string -> char list
    val lines : string -> string list
    val unlines : string list -> string
    val words : string -> string list
    val unwords : string list -> string
    val split_space : string -> string list
    val lines_with_nl : string -> string list
    val nblines : string -> int
    val cat : filename -> string list
    val cat_orig : filename -> string list
    val cat_array : filename -> string array
    val echo : string -> string
    val process_output_to_list : string -> string list
    val cmd_to_list : string -> string list
    val cmd_to_list_and_status : string -> string list * Unix.process_status
    val opt_to_list : 'a option -> 'a list
    val opt_get : 'a option -> 'a
    val file_to_stdout : string -> unit
    val file_to_stderr : string -> unit
    val command2 : string -> unit
    val _batch_mode : bool ref
    val command2_y_or_no : string -> bool
    val command2_y_or_no_exit_if_no : string -> unit
    val mkdir : ?mode:Unix.file_perm -> string -> unit
    val write_file : file:filename -> string -> unit
    val filesize : filename -> int
    val filemtime : filename -> float
    val lfile_exists : filename -> bool
    val is_directory : filename -> bool
    val readdir_to_kind_list : string -> Unix.file_kind -> string list
    val readdir_to_dir_list : string -> string list
    val readdir_to_file_list : string -> string list
    val readdir_to_link_list : string -> string list
    val readdir_to_dir_size_list : string -> (string * int) list
    val glob : string -> filename list
    val files_of_dir_or_files : string -> string list -> filename list
    val files_of_dir_or_files_no_vcs : string -> string list -> filename list
    val with_open_outfile :
      filename -> ((string -> unit) * out_channel -> 'a) -> 'a
    val with_open_infile : filename -> (in_channel -> 'a) -> 'a
    val with_open_outfile_append :
      filename -> ((string -> unit) * out_channel -> 'a) -> 'a
    val with_open_stringbuf : ((string -> unit) * Buffer.t -> unit) -> string
    exception Timeout
    val timeout_function : string -> int -> (unit -> 'a) -> 'a
    val timeout_function_opt : string -> int option -> (unit -> 'a) -> 'a
    val remove_file : string -> unit
    val _temp_files_created : string list ref
    val temp_files : string ref
    val new_temp_file : string -> string -> filename
    val erase_temp_files : unit -> unit
    exception UnixExit of int
    val exn_to_real_unixexit : (unit -> 'a) -> 'a
    val map_eff_rev : ('a -> 'b) -> 'a list -> 'b list
    val acc_map : ('a -> 'b) -> 'a list -> 'b list
    val zip : 'a list -> 'b list -> ('a * 'b) list
    val combine4 :
      'a list -> 'b list -> 'c list -> 'd list -> ('a * 'b * 'c * 'd) list
    val zip_safe : 'a list -> 'b list -> ('a * 'b) list
    val unzip : ('a * 'b) list -> 'a list * 'b list
    val take : int -> 'a list -> 'a list
    val take_safe : int -> 'a list -> 'a list
    val take_until : ('a -> bool) -> 'a list -> 'a list
    val take_while : ('a -> bool) -> 'a list -> 'a list
    val drop : int -> 'a list -> 'a list
    val drop_while : ('a -> bool) -> 'a list -> 'a list
    val drop_until : ('a -> bool) -> 'a list -> 'a list
    val span : ('a -> bool) -> 'a list -> 'a list * 'a list
    val skip_until : ('a list -> bool) -> 'a list -> 'a list
    val skipfirst : 'a -> 'a list -> 'a list
    val groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list
    val exclude_but_keep_attached :
      ('a -> bool) -> 'a list -> ('a * 'a list) list
    val group_by_post :
      ('a -> bool) -> 'a list -> ('a list * 'a) list * 'a list
    val group_by_pre :
      ('a -> bool) -> 'a list -> 'a list * ('a * 'a list) list
    val group_by_mapped_key : ('a -> 'b) -> 'a list -> ('b * 'a list) list
    val group_assoc_bykey_eff : ('a * 'b) list -> ('a * 'b list) list
    val splitAt : int -> 'a list -> 'a list * 'a list
    val split_when : ('a -> bool) -> 'a list -> 'a list * 'a * 'a list
    val split_gen_when :
      ('a list -> 'a list option) -> 'a list -> 'a list list
    val pack : int -> 'a list -> 'a list list
    val enum : int -> int -> int list
    val repeat : 'a -> int -> 'a list
    val generate : int -> 'a -> 'a list
    val index_list : 'a list -> ('a * int) list
    val index_list_1 : 'a list -> ('a * int) list
    val filter_index : (int -> 'a -> bool) -> 'a list -> 'a list
    val fold_left_with_index : ('a -> 'b -> int -> 'a) -> 'a -> 'b list -> 'a
    val nth : 'a list -> int -> 'a
    val rang : 'a -> 'a list -> int
    val last_n : int -> 'a list -> 'a list
    val cons : 'a -> 'a list -> 'a list
    val uncons : 'a list -> 'a * 'a list
    val head_middle_tail : 'a list -> 'a * 'a list * 'a
    val last : 'a list -> 'a
    val list_init : 'a list -> 'a list
    val list_last : 'a list -> 'a
    val removelast : 'a list -> 'a list
    val inits : 'a list -> 'a list list
    val tails : 'a list -> 'a list list
    val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
    val fold_k :
      ('a -> 'b -> ('a -> 'a) -> 'a) -> ('a -> 'a) -> 'a -> 'b list -> 'a
    val fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val rev_map : ('a -> 'b) -> 'a list -> 'b list
    val join_gen : 'a -> 'a list -> 'a list
    val do_withenv :
      (('a -> 'b) -> 'c -> 'd) ->
      ('e -> 'a -> 'b * 'e) -> 'e -> 'c -> 'd * 'e
    val map_withenv : ('a -> 'b -> 'c * 'a) -> 'a -> 'b list -> 'c list * 'a
    val collect_accu : ('a -> 'b list) -> 'b list -> 'a list -> 'b list
    val collect : ('a -> 'b list) -> 'a list -> 'b list
    val remove : 'a -> 'a list -> 'a list
    val exclude : ('a -> bool) -> 'a list -> 'a list
    val uniq : 'a list -> 'a list
    val has_no_duplicate : 'a list -> bool
    val is_set_as_list : 'a list -> bool
    val get_duplicates : 'a list -> 'a list
    val doublon : 'a list -> bool
    val reverse : 'a list -> 'a list
    val rev : 'a list -> 'a list
    val rotate : 'a list -> 'a list
    val map_flatten : ('a -> 'b list) -> 'a list -> 'b list
    val map2 : ('a -> 'b) -> 'a list -> 'b list
    val map3 : ('a -> 'b) -> 'a list -> 'b list
    val maximum : 'a list -> 'a
    val minimum : 'a list -> 'a
    val min_with : ('a -> 'b) -> 'a list -> 'a
    val all_assoc : 'a -> ('a * 'b) list -> 'b list
    val or_list : bool list -> bool
    val and_list : bool list -> bool
    val sum_float : float list -> float
    val sum_int : int list -> int
    val return_when : ('a -> 'b option) -> 'a list -> 'b
    val grep_with_previous : ('a -> 'a -> bool) -> 'a list -> 'a list
    val iter_with_previous : ('a -> 'a -> 'b) -> 'a list -> unit
    val get_pair : 'a list -> ('a * 'a) list
    val permutation : 'a list -> 'a list list
    val remove_elem_pos : int -> 'a list -> 'a list
    val insert_elem_pos : 'a * int -> 'a list -> 'a list
    val uncons_permut : 'a list -> (('a * int) * 'a list) list
    val uncons_permut_lazy : 'a list -> (('a * int) * 'a list Lazy.t) list
    val pack_sorted : ('a -> 'a -> bool) -> 'a list -> 'a list list
    val keep_best : ('a * 'a -> 'a option) -> 'a list -> 'a list
    val sorted_keep_best : ('a -> 'a -> 'a option) -> 'a list -> 'a list
    val cartesian_product : 'a list -> 'b list -> ('a * 'b) list
    val equal_list : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val surEnsemble : 'a list -> 'a list list -> 'a list list
    val realCombinaison : 'a list -> 'a list list
    val combinaison : 'a list -> ('a * 'a) list
    val insere : 'a -> 'a list list -> 'a list list
    val insereListeContenant : 'a list -> 'a -> 'a list list -> 'a list list
    val fusionneListeContenant : 'a * 'a -> 'a list list -> 'a list list
    val array_find_index : (int -> bool) -> 'a array -> int
    type 'a matrix = 'a array array
    val make_matrix_init :
      nrow:int -> ncolumn:int -> (int -> int -> 'a) -> 'a matrix
    val nb_rows_matrix : 'a matrix -> int
    val nb_columns_matrix : 'a matrix -> int
    val rows_of_matrix : 'a matrix -> 'a list list
    val columns_of_matrix : 'a matrix -> 'a list list
    type 'a set = 'a list
    val empty_set : 'a set
    val insert_set : 'a -> 'a set -> 'a set
    val single_set : 'a -> 'a set
    val set : 'a list -> 'a set
    val exists_set : ('a -> bool) -> 'a set -> bool
    val forall_set : ('a -> bool) -> 'a set -> bool
    val filter_set : ('a -> bool) -> 'a set -> 'a set
    val fold_set : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a
    val map_set : ('a -> 'b) -> 'a set -> 'b set
    val member_set : 'a -> 'a set -> bool
    val find_set : ('a -> bool) -> 'a list -> 'a
    val sort_set : ('a -> 'a -> int) -> 'a list -> 'a list
    val iter_set : ('a -> unit) -> 'a list -> unit
    val top_set : 'a set -> 'a
    val inter_set : 'a set -> 'a set -> 'a set
    val union_set : 'a set -> 'a set -> 'a set
    val minus_set : 'a set -> 'a set -> 'a set
    val union_all : 'a set list -> 'a set
    val inter_all : 'a set list -> 'a set
    val card_set : 'a set -> int
    val include_set : 'a set -> 'a set -> bool
    val equal_set : 'a set -> 'a set -> bool
    val include_set_strict : 'a set -> 'a set -> bool
    val ( $*$ ) : 'a set -> 'a set -> 'a set
    val ( $+$ ) : 'a set -> 'a set -> 'a set
    val ( $-$ ) : 'a set -> 'a set -> 'a set
    val ( $?$ ) : 'a -> 'a set -> bool
    val ( $<$ ) : 'a set -> 'a set -> bool
    val ( $<=$ ) : 'a set -> 'a set -> bool
    val ( $=$ ) : 'a set -> 'a set -> bool
    val ( $@$ ) : 'a list -> 'a list -> 'a list
    val nub : 'a list -> 'a list
    type ('a, 'b) assoc = ('a * 'b) list
    val assoc_to_function : ('a, 'b) assoc -> 'a -> 'b
    val empty_assoc : ('a, 'b) assoc
    val fold_assoc : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val insert_assoc : 'a -> 'a list -> 'a list
    val map_assoc : ('a -> 'b) -> 'a list -> 'b list
    val filter_assoc : ('a -> bool) -> 'a list -> 'a list
    val assoc : 'a -> ('a * 'b) list -> 'b
    val keys : ('a * 'b) list -> 'a list
    val lookup : 'a -> ('a * 'b) list -> 'b
    val del_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
    val assoc_reverse : ('a * 'b) list -> ('b * 'a) list
    val assoc_map : ('a * 'b) list -> ('a * 'b) list -> ('a * 'a) list
    val lookup_list : 'a -> ('a, 'b) assoc list -> 'b
    val lookup_list2 : 'a -> ('a, 'b) assoc list -> 'b * int
    val assoc_option : 'a -> ('a, 'b) assoc -> 'b option
    val assoc_with_err_msg : 'a -> ('a, 'b) assoc -> 'b
    val sort_by_key_lowfirst : (int, 'a) assoc -> (int * 'a) list
    val sort_by_key_highfirst : (int, 'a) assoc -> (int * 'a) list
    module IntMap : Map.S with type key = int
    module IntIntMap : Map.S with type key = int * int
    val hfind_default : 'a -> (unit -> 'b) -> ('a, 'b) Hashtbl.t -> 'b
    val hfind_option : 'a -> ('a, 'b) Hashtbl.t -> 'b option
    val hupdate_default :
      'a -> ('b -> 'b) -> (unit -> 'b) -> ('a, 'b) Hashtbl.t -> unit
    val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
    val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
    val hashadd : ('a, 'b list ref) Hashtbl.t -> 'a -> 'b -> unit
    val hkeys : ('a, 'b) Hashtbl.t -> 'a list
    type 'a hashset = ('a, bool) Hashtbl.t
    val hashset_to_list : 'a hashset -> 'a list
    type 'a stack = 'a list
    val empty_stack : 'a stack
    val push : 'a -> 'a stack -> 'a stack
    val top : 'a stack -> 'a
    val pop : 'a stack -> 'a stack
    val top_option : 'a stack -> 'a option
    val push2 : 'a -> 'a stack ref -> unit
    val pop2 : 'a stack ref -> 'a
    type 'a undo_stack = 'a list * 'a list
    val empty_undo_stack : 'a undo_stack
    val push_undo : 'a -> 'a undo_stack -> 'a undo_stack
    val top_undo : 'a undo_stack -> 'a
    val pop_undo : 'a undo_stack -> 'a undo_stack
    val undo_pop : 'a undo_stack -> 'a undo_stack
    val top_undo_option : 'a undo_stack -> 'a option
    type 'a bintree =
      'a Common.bintree =
        Leaf of 'a
      | Branch of ('a bintree * 'a bintree)
    type 'a tree = 'a Common.tree = Tree of 'a * 'a tree list
    val tree_iter : ('a -> unit) -> 'a tree -> unit
    type 'a treeref = 'a Common.treeref = NodeRef of 'a * 'a treeref list ref
    val treeref_node_iter :
      ('a * 'a treeref list ref -> unit) -> 'a treeref -> unit
    val treeref_node_iter_with_parents :
      ('a * 'a treeref list ref -> 'a list -> unit) -> 'a treeref -> unit
    type ('a, 'b) treeref2 =
      ('a, 'b) Common.treeref2 =
        NodeRef2 of 'a * ('a, 'b) treeref2 list ref
      | LeafRef2 of 'b
    val treeref_node_iter2 :
      ('a * ('a, 'b) treeref2 list ref -> unit) -> ('a, 'b) treeref2 -> unit
    type 'a graph = 'a set * ('a * 'a) set
    val add_node : 'a -> 'a graph -> 'a graph
    val del_node : 'a -> 'a graph -> 'a graph
    val add_arc : 'a * 'a -> 'a graph -> 'a graph
    val del_arc : 'a * 'a -> 'a graph -> 'a graph
    val successors : 'a -> 'a graph -> 'a set
    val predecessors : 'a -> 'a graph -> 'a set
    val nodes : 'a graph -> 'a set
    val fold_upward : ('a -> 'b -> 'a) -> 'b set -> 'a -> 'b graph -> 'a
    val empty_graph : 'a list * 'b list
    val map : ('a -> 'b) -> 'a list -> 'b list
    val tail_map : ('a -> 'b) -> 'a list -> 'b list
    val filter : ('a -> bool) -> 'a list -> 'a list
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val member : 'a -> 'a list -> bool
    val iter : ('a -> unit) -> 'a list -> unit
    val find : ('a -> bool) -> 'a list -> 'a
    val exists : ('a -> bool) -> 'a list -> bool
    val forall : ('a -> bool) -> 'a list -> bool
    val sort : ('a -> 'a -> int) -> 'a list -> 'a list
    val length : 'a list -> int
    val head : 'a list -> 'a
    val tail : 'a list -> 'a list
    val is_singleton : 'a list -> bool
    type vector = float * float * float
    type point = vector
    type color = vector
    val dotproduct : vector * vector -> float
    val vector_length : vector -> float
    val minus_point : point * point -> vector
    val distance : point * point -> float
    val normalise : vector -> vector
    val mult_coeff : vector -> float -> vector
    val add_vector : vector -> vector -> vector
    val mult_vector : vector -> vector -> vector
    val sum_vector : vector list -> vector
    type pixel = int * int * int
    val write_ppm : int -> int -> pixel list -> filename -> unit
    type diff = Common.diff = Match | BnotinA | AnotinB
    val diff :
      (int -> int -> diff -> unit) -> string list * string list -> unit
    val diff2 : (int -> int -> diff -> unit) -> string * string -> unit
    type parse_info =
      Common.parse_info = {
      str : string;
      charpos : int;
      line : int;
      column : int;
      file : filename;
    }
    val fake_parse_info : parse_info
    val string_of_parse_info : parse_info -> string
    val string_of_parse_info_bis : parse_info -> string
    val full_charpos_to_pos : filename -> (int * int) array
    val info_from_charpos : int -> filename -> int * int * string
    val error_message : filename -> string * int -> string
    val error_message_short : filename -> string * int -> string
    val error_messagebis : filename -> string * int -> int -> string
    type ('a, 'b) scoped_env = ('a, 'b) assoc list
    val lookup_env : 'a -> ('a, 'b) scoped_env -> 'b
    val new_scope : ('a, 'b) scoped_env ref -> unit
    val del_scope : ('a, 'b) scoped_env ref -> unit
    val add_in_scope : ('a, 'b) scoped_env ref -> 'a * 'b -> unit
    type ('a, 'b) scoped_h_env =
      ('a, 'b) Common.scoped_h_env = {
      scoped_h : ('a, 'b) Hashtbl.t;
      scoped_list : ('a, 'b) assoc list;
    }
    val empty_scoped_h_env : unit -> ('a, 'b) scoped_h_env
    val clone_scoped_h_env : ('a, 'b) scoped_h_env -> ('a, 'b) scoped_h_env
    val lookup_h_env : 'a -> ('a, 'b) scoped_h_env -> 'b
    val new_scope_h : ('a, 'b) scoped_h_env ref -> unit
    val del_scope_h : ('a, 'b) scoped_h_env ref -> unit
    val clean_scope_h : ('a, 'b) scoped_h_env ref -> unit
    val add_in_scope_h : ('a, 'b) scoped_h_env ref -> 'a * 'b -> unit
    val _execute_and_show_progress_func :
      (int -> ((unit -> unit) -> unit) -> unit) ref
    val execute_and_show_progress : int -> ((unit -> unit) -> unit) -> unit
    val generic_print : 'a -> string -> string
    class ['a] olist :
      'a list ->
      object
        val xs : 'a list
        method fold : ('b -> 'a -> 'b) -> 'b -> 'b
        method view : 'a list
      end
    module StringSet : Set.S with type elt = string
  end
module Ast_cocci :
  sig
    type added_string =
      Ast_cocci.added_string =
        Noindent of string
      | Indent of string
      | Space of string
    type info =
      Ast_cocci.info = {
      line : int;
      column : int;
      strbef : (added_string * int * int) list;
      straft : (added_string * int * int) list;
      whitespace : string;
    }
    type line = int
    type meta_name = string * string
    type script_position = string * line
    type 'a wrap =
      'a Ast_cocci.wrap = {
      node : 'a;
      node_line : line;
      free_vars : meta_name list;
      minus_free_vars : meta_name list;
      minus_nc_free_vars : meta_name list;
      fresh_vars : (meta_name * seed) list;
      inherited : meta_name list;
      positive_inherited_positions : meta_name list;
      constraints : (meta_name * constraints) list;
      saved_witness : meta_name list;
      bef_aft : dots_bef_aft;
      pos_info : meta_name mcode option;
      true_if_test_exp : bool;
      safe_for_multi_decls : Ast_cocci.safety;
      iso_info : (string * anything) list;
    }
    and befaft =
      Ast_cocci.befaft =
        BEFORE of Ast_cocci.anything list list * count
      | AFTER of Ast_cocci.anything list list * count
      | BEFOREAFTER of Ast_cocci.anything list list * Ast_cocci.anything list list * count
      | NOTHING
    and replacement =
      Ast_cocci.replacement =
        REPLACEMENT of Ast_cocci.anything list list * count
      | NOREPLACEMENT
    and 'a mcode = 'a * info * mcodekind * meta_pos list
    and adj = Ast_cocci.adj = { counter : int; mutable ender : bool; }
    and adjacency = Ast_cocci.adjacency = ALLMINUS | ADJ of adj
    and mcodekind =
      Ast_cocci.mcodekind =
        MINUS of pos * int list * adjacency * replacement
      | CONTEXT of pos * befaft
      | PLUS of count
    and count = Ast_cocci.count = ONE | MANY
    and fixpos = Ast_cocci.fixpos = Real of int | Virt of int * int
    and pos =
      Ast_cocci.pos =
        NoPos
      | DontCarePos
      | FixPos of (fixpos * fixpos)
    and dots_bef_aft =
      Ast_cocci.dots_bef_aft =
        NoDots
      | AddingBetweenDots of statement * int
      | DroppingBetweenDots of statement * int
    and inherited = bool
    and keep_binding = Ast_cocci.keep_binding = Unitary | Nonunitary | Saved
    and multi = bool
    and end_info =
        meta_name list * (meta_name * seed) list * meta_name list * mcodekind
    and safety = Safe | Unsafe | NoStorage
    and arity = Ast_cocci.arity = UNIQUE | OPT | MULTI | NONE
    and metavar =
      Ast_cocci.metavar =
        MetaMetaDecl of arity * meta_name
      | MetaIdDecl of arity * meta_name
      | MetaFreshIdDecl of meta_name * seed
      | MetaTypeDecl of arity * meta_name
      | MetaInitDecl of arity * meta_name
      | MetaInitListDecl of arity * meta_name * list_len
      | MetaListlenDecl of meta_name
      | MetaParamDecl of arity * meta_name
      | MetaParamListDecl of arity * meta_name * list_len
      | MetaBinaryOperatorDecl of arity * meta_name
      | MetaAssignmentOperatorDecl of arity * meta_name
      | MetaPragmaInfoDecl of arity * meta_name
      | MetaConstDecl of arity * meta_name * fullType list option
      | MetaErrDecl of arity * meta_name
      | MetaExpDecl of arity * meta_name * fullType list option *
          list_len option
      | MetaIdExpDecl of arity * meta_name * fullType list option
      | MetaLocalIdExpDecl of arity * meta_name * fullType list option
      | MetaGlobalIdExpDecl of arity * meta_name * fullType list option
      | MetaExpListDecl of arity * meta_name * list_len
      | MetaDeclDecl of arity * meta_name
      | MetaFieldDecl of arity * meta_name
      | MetaFieldListDecl of arity * meta_name * list_len
      | MetaStmDecl of arity * meta_name
      | MetaStmListDecl of arity * meta_name * list_len
      | MetaDParamListDecl of arity * meta_name * list_len
      | MetaFuncDecl of arity * meta_name
      | MetaLocalFuncDecl of arity * meta_name
      | MetaPosDecl of arity * meta_name
      | MetaComDecl of arity * meta_name
      | MetaFmtDecl of arity * meta_name
      | MetaAttributeDecl of arity * meta_name
      | MetaFragListDecl of arity * meta_name * list_len
      | MetaAnalysisDecl of string * meta_name
      | MetaDeclarerDecl of arity * meta_name
      | MetaIteratorDecl of arity * meta_name
      | MetaScriptDecl of metavar option ref * meta_name
    and list_len =
      Ast_cocci.list_len =
        AnyLen
      | MetaLen of meta_name * constraints
      | CstLen of int
    and seed =
      Ast_cocci.seed =
        NoVal
      | StringSeed of string
      | ListSeed of seed_elem list
      | ScriptSeed of script_constraint
    and seed_elem =
      Ast_cocci.seed_elem =
        SeedString of string
      | SeedId of meta_name
    and 'a dots = 'a list wrap
    and base_ident =
      Ast_cocci.base_ident =
        Id of string mcode
      | MetaId of meta_name mcode * constraints * keep_binding * inherited
      | MetaFunc of meta_name mcode * constraints * keep_binding * inherited
      | MetaLocalFunc of meta_name mcode * constraints * keep_binding *
          inherited
      | AsIdent of ident * ident
      | DisjId of ident list
      | ConjId of ident list
      | OptIdent of ident
    and ident = base_ident wrap
    and base_expression =
      Ast_cocci.base_expression =
        Ident of ident
      | Constant of constant mcode
      | StringConstant of
	  string mcode * string_fragment dots * string mcode * Ast_cocci.isWchar
      | FunCall of expression * string mcode * expression dots * string mcode
      | Assignment of expression * assignOp * expression * bool
      | Sequence of expression * string mcode * expression
      | CondExpr of expression * string mcode * expression option *
          string mcode * expression
      | Postfix of expression * fixOp mcode
      | Infix of expression * fixOp mcode
      | Unary of expression * unaryOp mcode
      | Binary of expression * binaryOp * expression
      | Nested of expression * binaryOp * expression
      | ArrayAccess of expression * string mcode * expression dots * string mcode
      | RecordAccess of expression * string mcode * ident
      | RecordPtAccess of expression * string mcode * ident
      | QualifiedAccess of fullType option * string mcode * ident
      | Cast of string mcode * fullType * string mcode * expression
      | SizeOfExpr of string mcode * expression
      | SizeOfType of string mcode * string mcode * fullType * string mcode
      | Delete of string mcode * expression
      | DeleteArr of string mcode * string mcode * string mcode * expression
      | New of string mcode * arguments option * string mcode option * fullType * string mcode option *
	    arguments option
      | TemplateInst of expression (* name *) * string mcode (* < *) *
            expression dots * string mcode (* > *)
      | TupleExpr of initialiser
      | TypeExp of fullType
      | Paren of string mcode * expression * string mcode
      | Constructor of string mcode * fullType * string mcode * initialiser
      | MetaErr of meta_name mcode * constraints * keep_binding * inherited
      | MetaExpr of meta_name mcode * constraints * keep_binding *
          fullType list option * form * inherited * listlen option
      | MetaExprList of meta_name mcode * listlen * constraints *
          keep_binding * inherited
      | AsExpr of expression * expression
      | AsSExpr of expression * rule_elem
      | EComma of string mcode
      | DisjExpr of expression list
      | ConjExpr of expression list
      | NestExpr of string mcode * expression dots * string mcode *
          expression option * multi
      | Edots of string mcode * expression option
      | OptExp of expression
    and arguments = string mcode (* ( *) * expression dots * string mcode (* ) *)
    and 'a generic_constraints =
      'a Ast_cocci.generic_constraints =
        CstrFalse
      | CstrTrue
      | CstrExit
      | CstrAnd of 'a generic_constraints list
      | CstrOr of 'a generic_constraints list
      | CstrNot of 'a generic_constraints
      | CstrConstant of constant_constraint
      | CstrOperator of operator_constraint
      | CstrMeta_name of meta_name
      | CstrRegexp of string * Regexp.regexp
      | CstrScript of bool * script_constraint
      | CstrExpr of 'a
      | CstrSub of meta_name list
      | CstrType of fullType
    and constant_constraint =
      Ast_cocci.constant_constraint =
        CstrInt of int_constraint
      | CstrString of string
    and int_constraint =
      Ast_cocci.int_constraint =
        CstrIntEq of string
      | CstrIntLeq of int
      | CstrIntGeq of int
    and operator_constraint =
      Ast_cocci.operator_constraint =
        CstrAssignOp of assignOp
      | CstrBinaryOp of binaryOp
    and constraints = expression generic_constraints
    and script_constraint =
        string * string * (meta_name * metavar) list * script_position *
        string
    and form = Ast_cocci.form = ANY | ID | LocalID | GlobalID | CONST
    and expression = base_expression wrap
    and listlen =
      Ast_cocci.listlen =
        MetaListLen of meta_name mcode * constraints * keep_binding *
          inherited
      | CstListLen of int
      | AnyListLen
    and base_string_fragment =
      Ast_cocci.base_string_fragment =
        ConstantFragment of string mcode
      | FormatFragment of string mcode * string_format
      | Strdots of string mcode
      | MetaFormatList of string mcode * meta_name mcode * listlen *
          constraints * keep_binding * inherited
    and string_fragment = base_string_fragment wrap
    and base_string_format =
      Ast_cocci.base_string_format =
        ConstantFormat of string mcode
      | MetaFormat of meta_name mcode * constraints * keep_binding *
          inherited
    and string_format = base_string_format wrap
    and unaryOp =
      Ast_cocci.unaryOp =
        GetRef
      | GetRefLabel
      | DeRef
      | UnPlus
      | UnMinus
      | Tilde of string
      | Not of string
    and base_assignOp =
      Ast_cocci.base_assignOp =
        SimpleAssign of simpleAssignOp mcode
      | OpAssign of arithOp mcode
      | MetaAssign of meta_name mcode * constraints * keep_binding *
          inherited
    and simpleAssignOp = string
    and assignOp = base_assignOp wrap
    and fixOp = Ast_cocci.fixOp = Dec | Inc
    and base_binaryOp =
      Ast_cocci.base_binaryOp =
        Arith of arithOp mcode
      | Logical of logicalOp mcode
      | MetaBinary of meta_name mcode * constraints * keep_binding *
          inherited
    and binaryOp = base_binaryOp wrap
    and arithOp =
      Ast_cocci.arithOp =
        Plus
      | Minus
      | Mul
      | Div
      | Mod
      | DecLeft
      | DecRight
      | And of string
      | Or of string
      | Xor of string
      | Min
      | Max
    and logicalOp =
      Ast_cocci.logicalOp =
        Inf
      | Sup
      | InfEq
      | SupEq
      | Eq
      | NotEq of string
      | AndLog of string
      | OrLog of string
    and constant =
      Ast_cocci.constant =
        String of string * Ast_cocci.isWchar
      | Char of string * Ast_cocci.isWchar
      | Int of string
      | Float of string
      | DecimalConst of (string * string * string)
    and isWchar = Ast_cocci.isWchar = IsWchar | IsUchar | Isuchar | Isu8char | IsChar
    and base_fullType =
      Ast_cocci.base_fullType =
        Type of bool * cvattr list * typeC * cvattr list
      | AsType of fullType * fullType
      | DisjType of fullType list
      | ConjType of fullType list
      | OptType of fullType
    and base_typeC =
      Ast_cocci.base_typeC =
        BaseType of baseType * string mcode list
      | SignedT of sign mcode * typeC option
      | Pointer of fullType * unaryOp mcode
      | ParenType of string mcode (* ( *) * fullType * string mcode (* ) *)
      | FunctionType of fullType *
          string mcode (* ( *) * parameter_list * string mcode (* ) *)
      | Array of fullType * string mcode * expression option * string mcode
      | Decimal of string mcode * string mcode * expression *
          string mcode option * expression option * string mcode
      | EnumName of string mcode * structUnion mcode option * ident option
      | EnumDef of fullType * enum_base option * string mcode * enum_decl dots * string mcode
      | StructUnionName of structUnion mcode * ident option
      | StructUnionDef of fullType * string mcode * annotated_field dots *
          string mcode
      | TypeName        of string mcode (* typename - C++ *) * ident (* name *)
      | TypeOfExpr of string mcode * string mcode * expression * string mcode
      | TypeOfType of string mcode * string mcode * fullType * string mcode
      | NamedType of string mcode
      | QualifiedType of fullType option * string mcode * ident
      | AutoType of string mcode (* auto *) (* c++ >= 11 *)
      | TemplateType of fullType (* name *) * string mcode (* < *) *
            expression dots * string mcode (* > *)
      | MetaType of meta_name mcode * constraints * keep_binding * inherited
    and fullType = base_fullType wrap
    and typeC = base_typeC wrap
    and baseType =
      Ast_cocci.baseType =
        VoidType
      | CharType
      | ShortType
      | ShortIntType
      | IntType
      | DoubleType
      | LongDoubleType
      | FloatType
      | LongDoubleComplexType
      | DoubleComplexType
      | FloatComplexType
      | LongType
      | LongIntType
      | LongLongType
      | LongLongIntType
      | SizeType
      | SSizeType
      | PtrDiffType
      | BoolType
      | Unknown
    and structUnion = Ast_cocci.structUnion = Struct | Union | Class
    and enum_base = string mcode (* : *) * fullType
    and sign = Ast_cocci.sign = Signed | Unsigned
    and const_vol = Ast_cocci.const_vol = Const | Volatile
    and base_declaration =
      Ast_cocci.base_declaration =
        Init of alignas option * storage mcode option * fullType * ident *
            attr list * string mcode * initialiser * string mcode option
      | UnInit of alignas option * storage mcode option * fullType * ident *
            attr list * string mcode
      | FunProto of fninfo list * ident * string mcode * parameter_list *
          (string mcode * string mcode) option * string mcode * string mcode
      | TyDecl of fullType * string mcode
      | MacroDecl of storage mcode option * attr list * ident * string mcode *
          expression dots * string mcode * attr list * string mcode
      | MacroDeclInit of storage mcode option * attr list * ident * string mcode *
          expression dots * string mcode * attr list * string mcode * initialiser *
          string mcode
      | Typedef of string mcode * fullType * typeC * string mcode
      | DisjDecl of declaration list
      | ConjDecl of declaration list
      | MetaDecl of meta_name mcode * constraints * keep_binding * inherited
      | AsDecl of declaration * declaration
      | OptDecl of declaration
    and declaration = base_declaration wrap
    and alignas =
      Ast_cocci.alignas =
      Align of string mcode (* alignas *) * string mcode (* ( *)
					* expression * string mcode (* ) *)
    and base_annotated_decl =
      Ast_cocci.base_annotated_decl =
        DElem of mcodekind * bool * declaration
    and annotated_decl = base_annotated_decl wrap
    and base_field =
      Ast_cocci.base_field =
        Field of fullType * ident option * bitfield option * attr list * string mcode
      | MacroDeclField of ident * string mcode *
          expression dots * string mcode * attr list * string mcode
      | CppField of directive
      | MetaField of meta_name mcode * constraints * keep_binding * inherited
      | MetaFieldList of meta_name mcode * listlen * constraints *
          keep_binding * inherited
      | AccSpec of string mcode * string mcode
    and bitfield = string mcode * expression
    and field = base_field wrap
    and base_annotated_field =
      Ast_cocci.base_annotated_field =
        FElem of mcodekind * bool * field
      | Fdots of string mcode * field option
      | DisjField of annotated_field list
      | ConjField of annotated_field list
      | OptField  of annotated_field
    and annotated_field = base_annotated_field wrap
    and base_enum_decl =
      Ast_cocci.base_enum_decl =
        Enum of ident * (string mcode * expression) option
      | EnumComma of string mcode
      | EnumDots of string mcode * enum_decl option
    and enum_decl = base_enum_decl wrap
    and base_initialiser =
      Ast_cocci.base_initialiser =
        MetaInit of meta_name mcode * constraints * keep_binding * inherited
      | MetaInitList of meta_name mcode * listlen * constraints *
          keep_binding * inherited
      | AsInit of initialiser * initialiser
      | InitExpr of expression
      | ArInitList of string mcode * initialiser dots * string mcode
      | StrInitList of bool * string mcode * initialiser list *
          string mcode * initialiser list
      | InitGccExt of designator list * string mcode * initialiser
      | InitGccName of ident * string mcode * initialiser
      | IComma of string mcode
      | Idots of string mcode * initialiser option
      | OptIni of initialiser
    and designator =
      Ast_cocci.designator =
        DesignatorField of string mcode * ident
      | DesignatorIndex of string mcode * expression * string mcode
      | DesignatorRange of string mcode * expression * string mcode *
          expression * string mcode
    and initialiser = base_initialiser wrap
    and base_parameterTypeDef =
      Ast_cocci.base_parameterTypeDef =
        Param of fullType * ident option * attr list
      | MetaParam of meta_name mcode * constraints * keep_binding * inherited
      | MetaParamList of meta_name mcode * listlen * constraints *
          keep_binding * inherited
      | AsParam of parameterTypeDef * expression
      | PComma of string mcode
      | Pdots of string mcode
      | OptParam of parameterTypeDef
    and parameterTypeDef = base_parameterTypeDef wrap
    and parameter_list = parameterTypeDef dots
    and base_define_param =
      Ast_cocci.base_define_param =
        DParam of ident
      | DParamEll of ident * string mcode (* ... *)
      | MetaDParamList of meta_name mcode * listlen * constraints *
          keep_binding * inherited
      | DPComma of string mcode
      | DPdots of string mcode
      | OptDParam of define_param
    and define_param = base_define_param wrap
    and base_define_parameters =
      Ast_cocci.base_define_parameters =
        NoParams
      | DParams of string mcode * define_param dots * string mcode
    and define_parameters = base_define_parameters wrap
    and meta_collect = Ast_cocci.meta_collect = PER | ALL
    and meta_pos =
      Ast_cocci.meta_pos =
        MetaPos of meta_name mcode * constraints * meta_collect *
          keep_binding * inherited
      | MetaCom of meta_name mcode * constraints * keep_binding * inherited
    and storage = Ast_cocci.storage = Static | Auto | Register | Extern
    and base_rule_elem =
      Ast_cocci.base_rule_elem =
        FunHeader of mcodekind * bool * fninfo list * ident * string mcode *
          parameter_list * (string mcode * string mcode) option *
          string mcode * attr list
      | Decl of annotated_decl
      | SeqStart of string mcode
      | SeqEnd of string mcode
      | ExprStatement of expression option * string mcode
      | IfHeader of string mcode * string mcode * expression * string mcode
      | Else of string mcode
      | WhileHeader of string mcode * string mcode * whileinfo *
          string mcode
      | DoHeader of string mcode
      | WhileTail of string mcode * string mcode * expression *
          string mcode * string mcode
      | ForHeader of string mcode * string mcode * forinfo * string mcode
      | IteratorHeader of ident * string mcode * expression dots *
          string mcode
      | SwitchHeader of string mcode * string mcode * expression *
          string mcode
      | ScopedGuardHeader of string mcode * string mcode * expression dots * string mcode
      | Break of string mcode * string mcode
      | Continue of string mcode * string mcode
      | Label of ident * string mcode
      | Goto of string mcode * ident * string mcode
      | Return of string mcode * string mcode
      | ReturnExpr of string mcode * expression * string mcode
      | Exec of string mcode * string mcode * exec_code dots * string mcode
      | MetaRuleElem of meta_name mcode * constraints * keep_binding *
          inherited
      | MetaStmt of meta_name mcode * constraints * keep_binding *
          metaStmtInfo * inherited
      | MetaStmtList of meta_name mcode * listlen * constraints *
          keep_binding * inherited
      | Exp of expression
      | TopExp of expression
      | Ty of fullType
      | TopId of ident
      | TopInit of initialiser
      | CppTop of directive
      | Undef of string mcode * ident
      | DefineHeader of string mcode * ident * define_parameters
      | TemplateDefinitionHeader
                     of string mcode (* template *) * string mcode (* < *) *
			 template_parameter_list * string mcode (* > *)
      | Case of string mcode * expression * string mcode
      | Default of string mcode * string mcode
      | AsRe of rule_elem * rule_elem
      | DisjRuleElem of rule_elem list
    and base_templateParameterTypeDef =
      Ast_cocci.base_templateParameterTypeDef =
        TypenameOrClassParam of string mcode (* typename|class *) * ident (* name *) * (string mcode (* = *) * fullType) option
      | VarNameParam of fullType * ident (* name *) * (string mcode (* = *) * initialiser) option
      | TPComma of string mcode
      | TPDots of string mcode (* ... *)
      (* Note: TemplateParam not supported yet. *)
    and templateParameterTypeDef = base_templateParameterTypeDef wrap
    and template_parameter_list = templateParameterTypeDef dots

    and base_pragmainfo =
      Ast_cocci.base_pragmainfo =
        PragmaString of string mcode
      | PragmaDots of string mcode
      | MetaPragmaInfo of meta_name mcode * constraints * keep_binding *
          inherited
    and pragmainfo = base_pragmainfo wrap
    and forinfo =
      Ast_cocci.forinfo =
	ForExp of expression option * string mcode (*;*) *
	    expression option * string mcode (*;*) *
            expression option
      | ForDecl of annotated_decl * expression option * string mcode (*;*) *
            expression option
      | ForRange of annotated_decl * initialiser
    and whileinfo =
      Ast_cocci.whileinfo =
	WhileExp  of expression
      | WhileDecl of annotated_decl
    and fninfo =
      Ast_cocci.fninfo =
        FStorage of storage mcode
      | FType of fullType
      | FInline of string mcode
    and cvattr =
      Ast_cocci.cvattr =
	CV of const_vol mcode
      | Attr of attr
    and base_attr =
      Ast_cocci.base_attr =
        Attribute of Ast_cocci.attr_arg
      | GccAttribute of string mcode * string mcode * string mcode *
                        expression dots * string mcode * string mcode
      | CxxAttribute of string mcode * expression dots * string mcode * string mcode
      | CxxAttributeUsing of string mcode (* [[ *) * string mcode (* using *) *
                        ident * string mcode (* : *) *
                        expression dots * string mcode (* ] *) * string mcode (* ] *)
    and attr = base_attr wrap
    and base_attr_arg =
      Ast_cocci.base_attr_arg =
        MacroAttr of string mcode
      | MacroAttrArgs of string mcode * string mcode * expression dots * string mcode
      | MetaAttr of meta_name mcode * constraints * keep_binding *
          inherited
    and attr_arg = base_attr_arg wrap
    and metaStmtInfo =
      Ast_cocci.metaStmtInfo =
        NotSequencible
      | SequencibleAfterDots of dots_whencode list
      | Sequencible
    and rule_elem = base_rule_elem wrap
    and base_statement =
      Ast_cocci.base_statement =
        Seq of rule_elem * statement dots * rule_elem
      | IfThen of rule_elem * statement * end_info
      | IfThenElse of rule_elem * statement * rule_elem * statement *
          end_info
      | While of rule_elem * statement * end_info
      | Do of rule_elem * statement * rule_elem
      | For of rule_elem * statement * end_info
      | Iterator of rule_elem * statement * end_info
      | Switch of rule_elem * rule_elem * statement dots * case_line list *
          rule_elem
      | ScopedGuard of rule_elem * statement * end_info
      | Atomic of rule_elem
      | Disj of statement dots list
      | Conj of statement dots list
      | Nest of string mcode * statement dots * string mcode *
          (statement dots, statement) whencode list * multi *
          dots_whencode list * dots_whencode list
      | FunDecl of rule_elem * rule_elem * statement dots * rule_elem *
          end_info
      | TemplateDefinition of rule_elem * statement
      | Define of rule_elem * statement dots
      | AsStmt of statement * statement
      | Dots of string mcode * (statement dots, statement) whencode list *
          dots_whencode list * dots_whencode list
      | OptStm of statement
    and base_directive =
	Ast_cocci.base_directive =
	Include       of string mcode (*#include*) * inc_file mcode (*file*)
      | MetaInclude   of string mcode (* #include *) * expression (* file *)
      | Pragma        of string mcode (* #pragma *) * ident * pragmainfo
      | UsingNamespace of string mcode (*using*) * string mcode (*namespace*) *
	    ident (*name*) * string mcode (*;*)
      | UsingTypename of string mcode (*using*) * ident (*name*) *
	    string mcode (*=*) * string mcode option (*typename*) *
	    fullType (*full_type*) * string mcode (*;*)
      | UsingMember of string mcode (*using*) * ident (*name*) *
	    string mcode (*;*)

    and directive = base_directive wrap
    and ('a, 'b) whencode =
      ('a, 'b) Ast_cocci.whencode =
        WhenNot of 'a
      | WhenAlways of 'b
      | WhenModifier of when_modifier
      | WhenNotTrue of rule_elem
      | WhenNotFalse of rule_elem
    and when_modifier =
      Ast_cocci.when_modifier =
        WhenAny
      | WhenStrict
      | WhenForall
      | WhenExists
    and dots_whencode =
      Ast_cocci.dots_whencode =
        WParen of rule_elem * meta_name
      | Other of statement
      | Other_dots of statement dots
    and statement = base_statement wrap
    and base_case_line =
      Ast_cocci.base_case_line =
        CaseLine of rule_elem * statement dots
      | OptCase of case_line
    and case_line = base_case_line wrap
    and base_exec_code =
      Ast_cocci.base_exec_code =
        ExecEval of string mcode * expression
      | ExecToken of string mcode
      | ExecDots of string mcode
    and exec_code = base_exec_code wrap
    and inc_file =
      Ast_cocci.inc_file =
        Local of inc_elem list
      | NonLocal of inc_elem list
      | AnyInc
    and inc_elem = Ast_cocci.inc_elem = IncPath of string | IncDots
    and base_top_level =
      Ast_cocci.base_top_level =
        NONDECL of statement
      | CODE of statement dots
      | FILEINFO of string mcode * string mcode
      | ERRORWORDS of expression list
    and top_level = base_top_level wrap
    and parser_kind = Ast_cocci.parser_kind = ExpP | IdP | TyP | AnyP
    and rulename =
      Ast_cocci.rulename =
        CocciRulename of string option * dependency * string list *
          string list * exists * parser_kind
      | GeneratedRulename of string option * dependency * string list *
          string list * exists * parser_kind
      | ScriptRulename of string option * string * dependency
      | InitialScriptRulename of string option * string * dependency
      | FinalScriptRulename of string option * string * dependency
    and ruletype = Ast_cocci.ruletype = Normal | Generated
    and rule =
      Ast_cocci.rule =
        CocciRule of string * (dependency * string list * exists) *
          top_level list * bool list * ruletype
      | ScriptRule of string * string * dependency *
          (script_meta_name * meta_name * metavar * mvinit) list *
          meta_name list * script_position * string
      | InitialScriptRule of string * string * dependency *
          (script_meta_name * meta_name * metavar * mvinit) list *
          script_position * string
      | FinalScriptRule of string * string * dependency *
          (script_meta_name * meta_name * metavar * mvinit) list *
          script_position * string
    and script_meta_name = string option * string option
    and mvinit =
      Ast_cocci.mvinit =
        NoMVInit
      | MVInitString of string
      | MVInitPosList
    and dep =
      Ast_cocci.dep =
        Dep of string
      | AntiDep of string
      | EverDep of string
      | NeverDep of string
      | AndDep of dep * dep
      | OrDep of dep * dep
      | FileIn of string
      | NotFileIn of string
    and dependency =
      Ast_cocci.dependency =
        NoDep
      | FailDep
      | ExistsDep of dep
      | ForallDep of dep
    and rule_with_metavars = metavar list * rule
    and anything =
      Ast_cocci.anything =
        FullTypeTag of fullType
      | BaseTypeTag of baseType
      | StructUnionTag of structUnion
      | SignTag of sign
      | IdentTag of ident
      | ExpressionTag of expression
      | ConstantTag of constant
      | UnaryOpTag of unaryOp
      | AssignOpTag of assignOp
      | SimpleAssignOpTag of simpleAssignOp
      | OpAssignOpTag of arithOp
      | FixOpTag of fixOp
      | BinaryOpTag of binaryOp
      | ArithOpTag of arithOp
      | LogicalOpTag of logicalOp
      | DeclarationTag of declaration
      | FieldTag of field
      | EnumDeclTag of enum_decl
      | InitTag of initialiser
      | StorageTag of storage
      | IncFileTag of inc_file
      | Rule_elemTag of rule_elem
      | StatementTag of statement
      | ForInfoTag of forinfo
      | CaseLineTag of case_line
      | StringFragmentTag of string_fragment
      | AttributeTag of attr
      | AttrArgTag of attr_arg
      | ConstVolTag of const_vol
      | Token of string * info option
      | Directive of added_string list
      | Code of top_level
      | ExprDotsTag of expression dots
      | ParamDotsTag of parameterTypeDef dots
      | TemplateParamDotsTag of templateParameterTypeDef dots
      | StmtDotsTag of statement dots
      | AnnDeclDotsTag of annotated_decl dots
      | AnnFieldDotsTag of annotated_field dots
      | EnumDeclDotsTag of enum_decl dots
      | DefParDotsTag of define_param dots
      | TypeCTag of typeC
      | ParamTag of parameterTypeDef
      | TemplateParamTag    of templateParameterTypeDef
      | SgrepStartTag of string
      | SgrepEndTag of string
    and exists = Ast_cocci.exists = Exists | Forall | Undetermined
    val mkToken : string -> anything
    val lub_count : count -> count -> count
    val rewrap : 'a wrap -> 'b -> 'b wrap
    val rewrap_mcode : 'a mcode -> 'b -> 'b mcode
    val unwrap : 'a wrap -> 'a
    val unwrap_mcode : 'a mcode -> 'a
    val get_mcodekind : 'a mcode -> mcodekind
    val get_line : 'a wrap -> line
    val get_mcode_line : 'a mcode -> line
    val get_fvs : 'a wrap -> meta_name list
    val get_wcfvs : ('a wrap, 'b wrap) whencode list -> meta_name list
    val set_fvs : meta_name list -> 'a wrap -> 'a wrap
    val get_mfvs : 'a wrap -> meta_name list
    val get_minus_nc_fvs : 'a wrap -> meta_name list
    val get_fresh : 'a wrap -> (meta_name * seed) list
    val get_inherited : 'a wrap -> meta_name list
    val get_inherited_pos : 'a wrap -> meta_name list
    val get_constraints : 'a wrap -> (meta_name * constraints) list
    val add_constraint : 'a wrap -> meta_name * constraints -> 'a wrap
    val get_saved : 'a wrap -> meta_name list
    val get_dots_bef_aft : statement -> dots_bef_aft
    val set_dots_bef_aft : dots_bef_aft -> statement -> statement
    val get_pos : 'a wrap -> meta_name mcode option
    val set_pos : 'a wrap -> meta_name mcode option -> 'a wrap
    val get_test_exp : 'a wrap -> bool
    val set_test_exp : expression -> expression
    val get_safe_decl : 'a wrap -> Ast_cocci.safety
    val get_isos : 'a wrap -> (string * anything) list
    val set_isos : 'a wrap -> (string * anything) list -> 'a wrap
    val get_pos_var : 'a mcode -> meta_pos list
    val set_pos_var : meta_pos list -> 'a mcode -> 'a mcode
    val drop_pos : 'a mcode -> 'a mcode
    val get_meta_name : metavar -> meta_name
    val no_info : info
    val make_meta_rule_elem :
      string ->
      mcodekind ->
      constraints ->
      meta_name list * (meta_name * seed) list * meta_name list -> rule_elem
    val make_term : 'a -> 'a wrap
    val make_inherited_term :
      'a -> meta_name list -> meta_name list -> 'a wrap
    val make_mcode : 'a -> 'a mcode
    val equal_pos : fixpos -> fixpos -> bool
    val string_of_arithOp : arithOp -> string
    val string_of_logicalOp : logicalOp -> string
    val string_of_assignOp : assignOp -> string
    val string_of_binaryOp : binaryOp -> string
    val string_of_sign : sign -> string
    val string_of_baseType : baseType -> string
    val string_of_const_vol : const_vol -> string
    val string_of_structUnion : structUnion -> string
    val string_of_fullType : fullType -> string
    val typeC_of_fullType_opt : fullType -> typeC option
    val ident_of_expression_opt : expression -> ident option
    type 'a transformer =
      'a Ast_cocci.transformer = {
      baseType : (baseType -> string mcode list -> 'a) option;
      decimal :
        (string mcode ->
         string mcode ->
         expression ->
         string mcode option -> expression option -> string mcode -> 'a)
        option;
      enumName : (string mcode -> structUnion mcode option -> ident option -> 'a) option;
      structUnionName : (structUnion mcode -> ident option -> 'a) option;
      typeName : (string mcode -> ident -> 'a) option;
      namedType : (string mcode -> 'a) option;
      metaType :
        (meta_name mcode -> constraints -> keep_binding -> inherited -> 'a)
        option;
    }
    val empty_transformer : 'a transformer
    val fullType_map : base_typeC transformer -> fullType -> fullType
    val fullType_fold : ('a -> 'a) transformer -> fullType -> 'a -> 'a
    val fullType_iter : unit transformer -> fullType -> unit
    val meta_names_of_fullType : fullType -> meta_name list
    val string_of_expression : expression -> string option
    type ('a, 'b) cstr_transformer =
      ('a, 'b) Ast_cocci.cstr_transformer = {
      cstr_constant : (constant_constraint -> 'b) option;
      cstr_operator : (operator_constraint -> 'b) option;
      cstr_meta_name : (meta_name -> 'b) option;
      cstr_regexp : (string -> Regexp.regexp -> 'b) option;
      cstr_script : (bool * script_constraint -> 'b) option;
      cstr_expr : ('a -> 'b) option;
      cstr_sub : (meta_name list -> 'b) option;
      cstr_type : (fullType -> 'b) option;
    }
    val empty_cstr_transformer : ('a, 'b) cstr_transformer
    val cstr_fold_sign :
      ('a, 'b -> 'b) cstr_transformer ->
      ('a, 'b -> 'b) cstr_transformer -> 'a generic_constraints -> 'b -> 'b
    val cstr_fold :
      ('a, 'b -> 'b) cstr_transformer -> 'a generic_constraints -> 'b -> 'b
    val cstr_iter :
      ('a, unit) cstr_transformer -> 'a generic_constraints -> unit
    val cstr_map :
      ('a, 'b generic_constraints) cstr_transformer ->
      'a generic_constraints -> 'b generic_constraints
    val cstr_meta_names : 'a generic_constraints -> meta_name list
    val cstr_pos_meta_names : 'a generic_constraints -> meta_name list
    val prepare_merge_variables :
      ('a ->
       ('b * (script_meta_name * meta_name * metavar * mvinit) list) option) ->
      'a list -> ('b * (int * string array)) list * string array
  end
module Ast0_cocci :
  sig
    type arity = Ast0_cocci.arity = OPT | NONE
    type token_info =
      Ast0_cocci.token_info = {
      tline_start : int;
      tline_end : int;
      left_offset : int;
      right_offset : int;
    }
    val default_token_info : token_info
    type mcodekind =
      Ast0_cocci.mcodekind =
        MINUS of (Ast_cocci.replacement * token_info) ref
      | PLUS of Ast_cocci.count
      | CONTEXT of
          (Ast_cocci.befaft * token_info * token_info) ref
      | MIXED of
          (Ast_cocci.befaft * token_info * token_info) ref
    type position_info =
      Ast0_cocci.position_info = {
      line_start : int;
      line_end : int;
      logical_start : int;
      logical_end : int;
      column : int;
      offset : int;
    }
    type info =
      Ast0_cocci.info = {
      pos_info : position_info;
      whitespace : string;
      attachable_start : bool;
      attachable_end : bool;
      mcode_start : mcodekind list;
      mcode_end : mcodekind list;
      strings_before : (Ast_cocci.added_string * position_info) list;
      strings_after : (Ast_cocci.added_string * position_info) list;
      isSymbolIdent : bool;
    }
    type fake_mcode = info * mcodekind * Ast_cocci.adj
    type 'a mcode =
        'a * arity * info * mcodekind * anything list ref * Ast_cocci.adj
    and 'a wrap =
      'a Ast0_cocci.wrap = {
      node : 'a;
      info : info;
      index : int ref;
      mcodekind : mcodekind ref;
      exp_ty : typeC option ref;
      bef_aft : dots_bef_aft;
      true_if_arg : bool;
      true_if_test : bool;
      true_if_test_exp : bool;
      iso_info : (string * anything) list;
    }
    and dots_bef_aft =
      Ast0_cocci.dots_bef_aft =
        NoDots
      | AddingBetweenDots of statement
      | DroppingBetweenDots of statement
    and pure = Ast0_cocci.pure = Impure | Pure | Context | PureContext
    and 'a dots = 'a list wrap
    and base_ident =
      Ast0_cocci.base_ident =
        Id of string mcode
      | MetaId of Ast_cocci.meta_name mcode * constraints * Ast_cocci.seed *
          pure
      | MetaFunc of Ast_cocci.meta_name mcode * constraints * pure
      | MetaLocalFunc of Ast_cocci.meta_name mcode * constraints * pure
      | AsIdent of ident * ident
      | DisjId of string mcode * ident list * string mcode list *
          string mcode
      | ConjId of string mcode * ident list * string mcode list *
          string mcode
      | OptIdent of ident
    and ident = base_ident wrap
    and base_expression =
      Ast0_cocci.base_expression =
        Ident of ident
      | Constant of Ast_cocci.constant mcode
      | StringConstant of
	  string mcode * string_fragment dots * string mcode * Ast_cocci.isWchar
      | FunCall of expression * string mcode * expression dots * string mcode
      | Assignment of expression * assignOp * expression * bool
      | Sequence of expression * string mcode * expression
      | CondExpr of expression * string mcode * expression option *
          string mcode * expression
      | Postfix of expression * Ast_cocci.fixOp mcode
      | Infix of expression * Ast_cocci.fixOp mcode
      | Unary of expression * Ast_cocci.unaryOp mcode
      | Binary of expression * binaryOp * expression
      | Nested of expression * binaryOp * expression
      | Paren of string mcode * expression * string mcode
      | ArrayAccess of expression * string mcode * expression dots * string mcode
      | RecordAccess of expression * string mcode * ident
      | RecordPtAccess of expression * string mcode * ident
      | QualifiedAccess of typeC option * string mcode * ident
      | Cast of string mcode * typeC * string mcode * expression
      | SizeOfExpr of string mcode * expression
      | SizeOfType of string mcode * string mcode * typeC * string mcode
      | Delete of string mcode * expression
      | DeleteArr of string mcode * string mcode * string mcode * expression
      | New of string mcode * arguments option * string mcode option * typeC * string mcode option *
	    arguments option
      | TemplateInst of expression (* name *) * string mcode (* < *) *
            expression dots * string mcode (* > *)
      | TupleExpr of initialiser
      | TypeExp of typeC
      | Constructor of string mcode * typeC * string mcode * initialiser
      | MetaErr of Ast_cocci.meta_name mcode * constraints * pure
      | MetaExpr of Ast_cocci.meta_name mcode * constraints *
          typeC list option * Ast_cocci.form * pure * listlen option
      | MetaExprList of Ast_cocci.meta_name mcode * listlen * constraints *
          pure
      | AsExpr of expression * expression
      | AsSExpr of expression * statement
      | EComma of string mcode
      | DisjExpr of string mcode * expression list * string mcode list *
          string mcode
      | ConjExpr of string mcode * expression list * string mcode list *
          string mcode
      | NestExpr of string mcode * expression dots * string mcode *
          (string mcode * string mcode * expression) option * Ast_cocci.multi
      | Edots of string mcode *
          (string mcode * string mcode * expression) option
      | OptExp of expression
    and expression = base_expression wrap
    and arguments = string mcode (* ( *) * expression dots * string mcode (* ) *)
    and constraints = expression Ast_cocci.generic_constraints
    and listlen =
      Ast0_cocci.listlen =
        MetaListLen of Ast_cocci.meta_name mcode * constraints
      | CstListLen of int
      | AnyListLen
    and base_string_fragment =
      Ast0_cocci.base_string_fragment =
        ConstantFragment of string mcode
      | FormatFragment of string mcode * string_format
      | Strdots of string mcode
      | MetaFormatList of string mcode * Ast_cocci.meta_name mcode *
          constraints * listlen
    and string_fragment = base_string_fragment wrap
    and base_string_format =
      Ast0_cocci.base_string_format =
        ConstantFormat of string mcode
      | MetaFormat of Ast_cocci.meta_name mcode * constraints
    and string_format = base_string_format wrap
    and base_assignOp =
      Ast0_cocci.base_assignOp =
        SimpleAssign of simpleAssignOp mcode
      | OpAssign of Ast_cocci.arithOp mcode
      | MetaAssign of Ast_cocci.meta_name mcode * constraints * pure
    and simpleAssignOp = string
    and assignOp = base_assignOp wrap
    and base_binaryOp =
      Ast0_cocci.base_binaryOp =
        Arith of Ast_cocci.arithOp mcode
      | Logical of Ast_cocci.logicalOp mcode
      | MetaBinary of Ast_cocci.meta_name mcode * constraints * pure
    and binaryOp = base_binaryOp wrap
    and base_typeC =
      Ast0_cocci.base_typeC =
        ConstVol of cvattr list * typeC * cvattr list
      | BaseType of Ast_cocci.baseType * string mcode list
      | Signed of Ast_cocci.sign mcode * typeC option
      | Pointer of typeC * Ast_cocci.unaryOp mcode
      | ParenType of string mcode * typeC * string mcode
      | FunctionType of typeC *
          string mcode * parameter_list * string mcode
      | Array of typeC * string mcode * expression option * string mcode
      | Decimal of string mcode * string mcode * expression *
          string mcode option * expression option * string mcode
      | EnumName of string mcode * Ast_cocci.structUnion mcode option * ident option
      | EnumDef of typeC * enum_base option * string mcode * enum_decl dots * string mcode
      | StructUnionName of Ast_cocci.structUnion mcode * ident option
      | StructUnionDef of typeC * string mcode * field dots * string mcode
      | TypeName        of string mcode (* typename - C++ *) * ident (* name *)
      | TypeOfExpr of string mcode * string mcode * expression * string mcode
      | TypeOfType of string mcode * string mcode * typeC * string mcode
      | NamedType of string mcode
      | QualifiedType of typeC option * string mcode * ident
      | AutoType of string mcode (* auto *) (* c++ >= 11 *)
      | TemplateType of typeC (* name *) * string mcode (* < *) *
            expression dots * string mcode (* > *)
      | MetaType of Ast_cocci.meta_name mcode * constraints * pure
      | AsType of typeC * typeC
      | DisjType of string mcode * typeC list * string mcode list *
          string mcode
      | ConjType of string mcode * typeC list * string mcode list *
          string mcode
      | OptType of typeC
    and typeC = base_typeC wrap
    and enum_base = string mcode (* : *) * typeC
    and base_declaration =
      Ast0_cocci.base_declaration =
        MetaDecl of Ast_cocci.meta_name mcode * constraints * pure
      | AsDecl of declaration * declaration
      | Init of alignas option * Ast_cocci.storage mcode option * typeC * ident *
          attr list * string mcode * initialiser * string mcode option
      | UnInit of alignas option * Ast_cocci.storage mcode option * typeC * ident *
          attr list * string mcode
      | FunProto of fninfo list * ident * string mcode * parameter_list *
          (string mcode * string mcode) option * string mcode * string mcode
      | TyDecl of typeC * string mcode
      | MacroDecl of Ast_cocci.storage mcode option * attr list * ident * string mcode *
          expression dots * string mcode * attr list * string mcode
      | MacroDeclInit of Ast_cocci.storage mcode option * attr list * ident *
          string mcode * expression dots * string mcode * attr list * string mcode *
          initialiser * string mcode
      | Typedef of string mcode * typeC * typeC * string mcode
      | DisjDecl of string mcode * declaration list * string mcode list *
          string mcode
      | ConjDecl of string mcode * declaration list * string mcode list *
          string mcode
      | OptDecl of declaration
    and declaration = base_declaration wrap
    and alignas =
      Ast0_cocci.alignas =
      Align of string mcode (* alignas *) * string mcode (* ( *)
					* expression * string mcode (* ) *)
    and base_field =
      Ast0_cocci.base_field =
        MetaField of Ast_cocci.meta_name mcode * constraints * pure
      | MetaFieldList of Ast_cocci.meta_name mcode * listlen * constraints *
          pure
      | Field of typeC * ident option * bitfield option * attr list * string mcode
      | MacroDeclField of ident * string mcode *
          expression dots * string mcode * attr list * string mcode
      | CppField    of directive
      | DisjField of string mcode * field list * string mcode list *
          string mcode
      | ConjField of string mcode * field list * string mcode list *
          string mcode
      | AccSpec of string mcode * string mcode
      | Fdots of string mcode * (string mcode * string mcode * field) option
      | OptField of field
    and bitfield = string mcode * expression
    and field = base_field wrap
    and base_enum_decl =
      Ast0_cocci.base_enum_decl =
        Enum of ident * (string mcode * expression) option
      | EnumComma of string mcode
      | EnumDots of string mcode *
                    (string mcode * string mcode * enum_decl) option
    and enum_decl = base_enum_decl wrap
    and base_initialiser =
      Ast0_cocci.base_initialiser =
        MetaInit of Ast_cocci.meta_name mcode * constraints * pure
      | MetaInitList of Ast_cocci.meta_name mcode * listlen * constraints *
          pure
      | AsInit of initialiser * initialiser
      | InitExpr of expression
      | InitList of string mcode * initialiser_list * string mcode * bool
      | InitGccExt of designator list * string mcode * initialiser
      | InitGccName of ident * string mcode * initialiser
      | IComma of string mcode
      | Idots of string mcode *
          (string mcode * string mcode * initialiser) option
      | OptIni of initialiser
    and designator =
      Ast0_cocci.designator =
        DesignatorField of string mcode * ident
      | DesignatorIndex of string mcode * expression * string mcode
      | DesignatorRange of string mcode * expression * string mcode *
          expression * string mcode
    and initialiser = base_initialiser wrap
    and initialiser_list = initialiser dots
    and base_parameterTypeDef =
      Ast0_cocci.base_parameterTypeDef =
        Param of typeC * ident option * attr list
      | MetaParam of Ast_cocci.meta_name mcode * constraints * pure
      | MetaParamList of Ast_cocci.meta_name mcode * listlen * constraints *
          pure
      | AsParam of parameterTypeDef * expression
      | PComma of string mcode
      | Pdots of string mcode
      | OptParam of parameterTypeDef
    and parameterTypeDef = base_parameterTypeDef wrap
    and parameter_list = parameterTypeDef dots
    and base_define_param =
      Ast0_cocci.base_define_param =
        DParam of ident
      | DParamEll of ident * string mcode (* ... *)
      | MetaDParamList of Ast_cocci.meta_name mcode * listlen * constraints *
          pure
      | DPComma of string mcode
      | DPdots of string mcode
      | OptDParam of define_param
    and define_param = base_define_param wrap
    and base_define_parameters =
      Ast0_cocci.base_define_parameters =
        NoParams
      | DParams of string mcode * define_param dots * string mcode
    and define_parameters = base_define_parameters wrap
    and base_statement =
      Ast0_cocci.base_statement =
        Decl of (info * mcodekind) * declaration
      | Seq of string mcode * statement dots * string mcode
      | ExprStatement of expression option * string mcode
      | IfThen of string mcode * string mcode * expression * string mcode *
          statement * fake_mcode
      | IfThenElse of string mcode * string mcode * expression *
          string mcode * statement * string mcode * statement * fake_mcode
      | While of string mcode * string mcode * whileinfo * string mcode *
          statement * fake_mcode
      | Do of string mcode * statement * string mcode * string mcode *
          expression * string mcode * string mcode
      | For of string mcode (* for *) * string mcode (* ( *) * forinfo *
	    string mcode (* ) *) * statement *
	    fake_mcode (* after info *)
      | Iterator of ident * string mcode * expression dots * string mcode *
          statement * fake_mcode
      | Switch of string mcode * string mcode * expression * string mcode *
          string mcode * statement dots * case_line dots * string mcode
      | ScopedGuard of string mcode (* scoped_guard *) * string mcode (* ( *) *
          expression dots * string mcode (* ) *) * statement *
          fake_mcode (* after info *)
      | Break of string mcode * string mcode
      | Continue of string mcode * string mcode
      | Label of ident * string mcode
      | Goto of string mcode * ident * string mcode
      | Return of string mcode * string mcode
      | ReturnExpr of string mcode * expression * string mcode
      | Exec of string mcode * string mcode * exec_code dots * string mcode
      | MetaStmt of Ast_cocci.meta_name mcode * constraints * pure
      | MetaStmtList of Ast_cocci.meta_name mcode * listlen * constraints *
          pure
      | AsStmt of statement * statement
      | Exp of expression
      | TopExp of expression
      | Ty of typeC
      | TopId of ident
      | TopInit of initialiser
      | CppTop of directive
      | Disj of string mcode * statement dots list * string mcode list *
          string mcode
      | Conj of string mcode * statement dots list * string mcode list *
          string mcode
      | Nest of string mcode * statement dots * string mcode *
          (statement dots, statement) whencode list * Ast_cocci.multi
      | Dots of string mcode * (statement dots, statement) whencode list
      | FunDecl of (info * mcodekind) * fninfo list * ident * string mcode *
          parameter_list * (string mcode * string mcode) option *
          string mcode * attr list * string mcode * statement dots *
          string mcode * (info * mcodekind)
      | TemplateDefinition of
            string mcode (* template *) * string mcode (* < *) *
            template_parameter_list * string mcode (* > *) *
            statement
      | Undef of string mcode * ident
      | Define of string mcode * ident * define_parameters * statement dots
      | OptStm of statement
    and base_templateParameterTypeDef =
      Ast0_cocci.base_templateParameterTypeDef =
        TypenameOrClassParam of string mcode (* typename|class *) * ident (* name *) * (string mcode (* = *) * typeC) option
      | VarNameParam of typeC * ident (* name *) * (string mcode (* = *) * initialiser) option
      | TPComma of string mcode
      | TPDots of string mcode (* ... *)
      (* Note: TemplateParam not supported yet. *)
    and templateParameterTypeDef = base_templateParameterTypeDef wrap
    and template_parameter_list = templateParameterTypeDef dots

    and base_directive =
	Ast0_cocci.base_directive =
      | Include of string mcode (* #include *) * Ast_cocci.inc_file mcode (* file *)
      | MetaInclude of string mcode (* #include *) * expression (* file *)
      | Pragma of string mcode (* #pragma *) * ident * pragmainfo
      | UsingNamespace of string mcode (*using*) * string mcode (*namespace*) *
	    ident (*name*) * string mcode (*;*)
      | UsingTypename of string mcode (*using*) * ident (*name*) *
	    string mcode (*=*) * string mcode option (*typename*) *
	    typeC (*full_type*) * string mcode (*;*)
      | UsingMember of string mcode (*using*) * ident (*name*) * string mcode (*;*)

    and directive = base_directive wrap

    and base_pragmainfo =
      Ast0_cocci.base_pragmainfo =
        PragmaString of string mcode
      | PragmaDots of string mcode
      | MetaPragmaInfo of Ast_cocci.meta_name mcode * constraints * pure
    and pragmainfo = base_pragmainfo wrap
    and base_forinfo =
      Ast0_cocci.base_forinfo =
	ForExp of expression option * string mcode (*;*) *
	    expression option * string mcode (*;*) *
            expression option
      | ForDecl of (info * mcodekind) (* before the decl *) * declaration *
	    expression option * string mcode (*;*) *
            expression option
      | ForRange of (info * mcodekind) (* before the decl *) * declaration *
	    initialiser
    and forinfo = base_forinfo wrap
    and whileinfo =
      Ast0_cocci.whileinfo =
	WhileExp  of expression
      | WhileDecl of (info * mcodekind) (* before the decl *) * declaration
    and fninfo =
      Ast0_cocci.fninfo =
        FStorage of Ast_cocci.storage mcode
      | FType of typeC
      | FInline of string mcode
    and cvattr =
      Ast0_cocci.cvattr =
	CV of Ast_cocci.const_vol mcode
      | Attr of attr
    and base_attr =
      Ast0_cocci.base_attr =
        Attribute of Ast0_cocci.attr_arg
      | GccAttribute of string mcode * string mcode * string mcode *
                        expression dots * string mcode * string mcode
      | CxxAttribute of string mcode * expression dots * string mcode * string mcode
      | CxxAttributeUsing of string mcode (* [[ *) * string mcode (* using *) *
                        ident * string mcode (* : *) *
                        expression dots * string mcode (* ] *) * string mcode (* ] *)
    and attr = base_attr wrap
    and base_attr_arg =
      Ast0_cocci.base_attr_arg =
        MacroAttr of string mcode
      | MacroAttrArgs of string mcode * string mcode * expression dots * string mcode
      | MetaAttr of Ast_cocci.meta_name mcode * constraints * pure
    and attr_arg = base_attr_arg wrap
    and ('a, 'b) whencode =
      ('a, 'b) Ast0_cocci.whencode =
        WhenNot of string mcode * string mcode * 'a
      | WhenAlways of string mcode * string mcode * 'b
      | WhenModifier of string mcode * Ast_cocci.when_modifier
      | WhenNotTrue of string mcode * string mcode * expression
      | WhenNotFalse of string mcode * string mcode * expression
    and statement = base_statement wrap
    and base_case_line =
      Ast0_cocci.base_case_line =
        Default of string mcode * string mcode * statement dots
      | Case of string mcode * expression * string mcode * statement dots
      | DisjCase of string mcode * case_line list * string mcode list *
          string mcode
      | OptCase of case_line
    and case_line = base_case_line wrap
    and base_exec_code =
      Ast0_cocci.base_exec_code =
        ExecEval of string mcode * expression
      | ExecToken of string mcode
      | ExecDots of string mcode
    and exec_code = base_exec_code wrap
    and meta_pos =
      Ast0_cocci.meta_pos =
        MetaPos of Ast_cocci.meta_name mcode * constraints *
          Ast_cocci.meta_collect
      | MetaCom of Ast_cocci.meta_name mcode * constraints
    and base_top_level =
      Ast0_cocci.base_top_level =
        NONDECL of statement
      | TOPCODE of statement dots
      | CODE of statement dots
      | FILEINFO of string mcode * string mcode
      | ERRORWORDS of expression list
      | OTHER of statement
    and top_level = base_top_level wrap
    and rule = top_level list
    and parsed_rule =
      Ast0_cocci.parsed_rule =
        CocciRule of
          (rule * Ast_cocci.metavar list *
           (string list * string list * Ast_cocci.dependency * string *
            Ast_cocci.exists)) *
          (rule * Ast_cocci.metavar list) * Ast_cocci.metavar list *
          Ast_cocci.ruletype
      | ScriptRule of string * string * Ast_cocci.dependency *
          (Ast_cocci.script_meta_name * Ast_cocci.meta_name *
           Ast_cocci.metavar * Ast_cocci.mvinit)
          list * Ast_cocci.meta_name list * Ast_cocci.script_position *
          string
      | InitialScriptRule of string * string * Ast_cocci.dependency *
          (Ast_cocci.script_meta_name * Ast_cocci.meta_name *
           Ast_cocci.metavar * Ast_cocci.mvinit)
          list * Ast_cocci.script_position * string
      | FinalScriptRule of string * string * Ast_cocci.dependency *
          (Ast_cocci.script_meta_name * Ast_cocci.meta_name *
           Ast_cocci.metavar * Ast_cocci.mvinit)
          list * Ast_cocci.script_position * string
    and dep =
      Ast0_cocci.dep =
        Dep of string
      | AntiDep of dep
      | EverDep of string
      | NeverDep of string
      | AndDep of dep * dep
      | OrDep of dep * dep
      | FileIn of string
    and dependency =
      Ast0_cocci.dependency =
        NoDep
      | FailDep
      | ExistsDep of dep
      | ForallDep of dep
    and anything =
      Ast0_cocci.anything =
        DotsExprTag of expression dots
      | DotsInitTag of initialiser dots
      | DotsParamTag of parameterTypeDef dots
      | DotsTemplateParamTag of templateParameterTypeDef dots
      | DotsStmtTag of statement dots
      | DotsDeclTag of declaration dots
      | DotsFieldTag of field dots
      | DotsEnumDeclTag of enum_decl dots
      | DotsCaseTag of case_line dots
      | DotsDefParamTag of define_param dots
      | IdentTag of ident
      | ExprTag of expression
      | AssignOpTag of assignOp
      | BinaryOpTag of binaryOp
      | ArgExprTag of expression
      | TestExprTag of expression
      | TypeCTag of typeC
      | ParamTag of parameterTypeDef
      | TemplateParamTag of templateParameterTypeDef
      | InitTag of initialiser
      | DeclTag of declaration
      | FieldTag of field
      | EnumDeclTag of enum_decl
      | StmtTag of statement
      | ForInfoTag of forinfo
      | CaseLineTag of case_line
      | StringFragmentTag of string_fragment
      | AttributeTag of attr
      | AttrArgTag of attr_arg
      | TopTag of top_level
      | IsoWhenTag of Ast_cocci.when_modifier
      | IsoWhenTTag of expression
      | IsoWhenFTag of expression
      | MetaPosTag of meta_pos
      | HiddenVarTag of anything list
      | WhenTag of string mcode * string mcode option * anything
    val dotsExpr : expression dots -> anything
    val dotsInit : initialiser dots -> anything
    val dotsParam : parameterTypeDef dots -> anything
    val dotsStmt : statement dots -> anything
    val dotsDecl : declaration dots -> anything
    val dotsField : field dots -> anything
    val dotsEnumDecl : enum_decl dots -> anything
    val dotsCase : case_line dots -> anything
    val dotsDefParam : define_param dots -> anything
    val ident : ident -> anything
    val expr : expression -> anything
    val assignOp : assignOp -> anything
    val binaryOp : binaryOp -> anything
    val typeC : typeC -> anything
    val param : parameterTypeDef -> anything
    val ini : initialiser -> anything
    val decl : declaration -> anything
    val field : field -> anything
    val stmt : statement -> anything
    val forinfo : forinfo -> anything
    val case_line : case_line -> anything
    val string_fragment : string_fragment -> anything
    val top : top_level -> anything
    val default_info : unit -> info
    val context_befaft : unit -> mcodekind
    val wrap : 'a -> 'a wrap
    val context_wrap : 'a -> 'a wrap
    val unwrap : 'a wrap -> 'a
    val unwrap_mcode : 'a mcode -> 'a
    val rewrap : 'a wrap -> 'b -> 'b wrap
    val rewrap_mcode : 'a mcode -> 'b -> 'b mcode
    val copywrap : 'a wrap -> 'b -> 'b wrap
    val get_pos : 'a mcode -> anything list
    val get_pos_ref : 'a mcode -> anything list ref
    val set_pos : anything list -> 'a mcode -> 'a mcode
    val get_info : 'a wrap -> info
    val set_info : 'a wrap -> info -> 'a wrap
    val get_index : 'a wrap -> int
    val set_index : 'a wrap -> int -> unit
    val get_line : 'a wrap -> int
    val get_mcode_line : 'a mcode -> int
    val get_line_end : 'a wrap -> int
    val get_mcodekind : 'a wrap -> mcodekind
    val get_mcode_mcodekind : 'a mcode -> mcodekind
    val get_mcodekind_ref : 'a wrap -> mcodekind ref
    val set_mcodekind : 'a wrap -> mcodekind -> unit
    val set_type : 'a wrap -> typeC option -> unit
    val get_type : 'a wrap -> typeC option
    val set_dots_bef_aft : statement -> dots_bef_aft -> statement
    val get_dots_bef_aft : 'a wrap -> dots_bef_aft
    val set_arg_exp : expression -> expression
    val get_arg_exp : expression -> bool
    val set_test_pos : expression -> expression
    val get_test_pos : 'a wrap -> bool
    val set_test_exp : expression -> expression
    val clear_test_exp : expression -> expression
    val get_test_exp : 'a wrap -> bool
    val set_iso : 'a wrap -> (string * anything) list -> 'a wrap
    val get_iso : 'a wrap -> (string * anything) list
    val fresh_index : unit -> int
    val set_mcode_data : 'a -> 'a mcode -> 'a mcode
    val make_mcode : 'a -> 'a mcode
    val make_mcode_info : 'a -> info -> 'a mcode
    val make_minus_mcode : 'a -> 'a mcode
    val get_rule_name : parsed_rule -> string
    val meta_pos_name : anything -> Ast_cocci.meta_name mcode
    val meta_names_of_typeC : typeC -> Ast_cocci.meta_name list
    val meta_pos_constraint_names : anything -> Ast_cocci.meta_name list
    val lub_pure : pure -> pure -> pure
    val rule_name : string ref
    val string_of_assignOp : assignOp -> string
    val string_of_binaryOp : binaryOp -> string
    val is_unknown_type : typeC -> bool
  end
module Dumper : sig val dump : 'a -> string end
type pos = {
  current_element : string;
  current_element_line : int;
  current_element_col : int;
  current_element_line_end : int;
  current_element_col_end : int;
  file : string;
  line : int;
  col : int;
  line_end : int;
  col_end : int;
}
type param_type =
    Pos of pos list
  | Com of (string list * string list * string list) list
  | AstCom of (Token_c.comment_like_token list *
		 Token_c.comment_like_token list *
		 Token_c.comment_like_token list) list
  | AssignOp of Ast_c.assignOp
  | BinaryOp of Ast_c.binaryOp
  | PragmaInfo of Ast_c.info
  | Str of string
  | Type of Ast_c.fullType
  | Init of Ast_c.initialiser
  | InitList of Ast_c.initialiser Ast_c.wrap2 list
  | Int of int
  | Param of Ast_c.parameterType
  | ParamList of Ast_c.parameterType Ast_c.wrap2 list
  | TemplateParam of Ast_c.templateParameterType
  | TemplateParamList of Ast_c.templateParameterType Ast_c.wrap2 list
  | DParamList of string Ast_c.wrap Ast_c.wrap2 list
  | Expr of Ast_c.expression
  | ExprList of Ast_c.argument Ast_c.wrap2 list
  | Decl of Ast_c.declaration
  | Field of Ast_c.field
  | FieldList of Ast_c.field list
  | FragList of Ast_c.string_fragment list
  | Fmt of Ast_c.string_format
  | Attribute of Ast_c.attribute
  | AttrArg of Ast_c.attr_arg
  | Stmt of Ast_c.statement
  | StmtList of Ast_c.statement_sequencable list
val fcts :
  (string, param_type list -> Ast_c.metavar_binding_kind ref list -> unit)
  Hashtbl.t
val bool_fcts : (string, param_type list -> bool) Hashtbl.t
val string_fcts : (string, param_type list -> string) Hashtbl.t
val variables_to_merge : (unit -> string array) ref
val merged_variables : string list array option ref
val no_format : string -> bool
val cstatement_of_string : string -> string -> Ast_c.statement
val cexpression_of_string : string -> string -> Ast_c.expression
val parse_failure : string -> string -> (string -> 'a) -> 'a
val make_ident : string -> Ast_c.metavar_binding_kind
val make_expr : string -> Ast_c.metavar_binding_kind
val make_expr_with_env : string -> string -> Ast_c.metavar_binding_kind
val make_stmt : string -> Ast_c.metavar_binding_kind
val make_stmt_with_env : string -> string -> Ast_c.metavar_binding_kind
val make_type : string -> Ast_c.metavar_binding_kind
val make_pragmainfo : string -> Ast_c.metavar_binding_kind
val make_listlen : int -> Ast_c.metavar_binding_kind
val make_full_position :
  Common.filename ->
  string ->
  int ->
  int -> int -> int -> int -> int -> int -> int -> Ast_c.metavar_binding_kind
val make_position :
  Common.filename ->
  string -> int -> int -> int -> int -> Ast_c.metavar_binding_kind
val inc_match : bool ref
val include_match : bool -> unit
val exited : bool ref
val exit : unit -> unit
val dir : unit -> string
val files : unit -> string list
val cocci_version : unit -> string
val build_link : pos -> string -> string -> string
val print_todo : ?color:string -> ?msg:string -> pos -> unit
val print_link : ?color:string -> ?msg:string -> pos -> unit
val print_safe_todo : ?color:string -> ?msg:string -> pos -> unit
val print_safe_link : ?color:string -> ?msg:string -> pos -> unit
val print_main : ?color:string -> string -> pos list -> unit
val print_sec : ?color:string -> string -> pos list -> unit
val print_secs : ?color:string -> string -> pos list -> unit
val basename_pos : pos -> pos
module Ana :
  sig
    type result = Externalanalysis.result
    type bound = Externalanalysis.bound
    val show_bound : Externalanalysis.bound -> string
    val show_result : Externalanalysis.result -> string
    val load_results : string -> unit
    val find : pos -> Externalanalysis.result list
    val inter :
      Externalanalysis.result ->
      Externalanalysis.result -> Externalanalysis.result option
    val satisfy : (Externalanalysis.result list -> bool) -> pos -> bool
    val satisfy1 : (Externalanalysis.result -> bool) -> pos -> bool
    val has_any : pos -> bool
    val for_all : (Externalanalysis.result -> bool) -> pos -> bool
    val for_all1 : (Externalanalysis.result -> bool) -> pos -> bool
    val exists : (Externalanalysis.result -> bool) -> pos -> bool
    val single_int : int64 -> Externalanalysis.result -> bool
    val contains_int : int64 -> Externalanalysis.result -> bool
    val has_only_nul : pos -> bool
    val has_also_nul : pos -> bool
    val has_also_int : int64 -> pos -> bool
  end
