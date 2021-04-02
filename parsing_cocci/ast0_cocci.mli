(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* --------------------------------------------------------------------- *)
(* Modified code *)

type arity = OPT | NONE

type token_info =
    { tline_start : int; tline_end : int;
      left_offset : int; right_offset : int }
val default_token_info : token_info

type mcodekind =
    MINUS       of (Ast_cocci.anything Ast_cocci.replacement * token_info) ref
  | PLUS        of Ast_cocci.count
  | CONTEXT     of (Ast_cocci.anything Ast_cocci.befaft *
		      token_info * token_info) ref
  | MIXED       of (Ast_cocci.anything Ast_cocci.befaft *
		      token_info * token_info) ref

type position_info = { line_start : int; line_end : int;
		       logical_start : int; logical_end : int;
		       column : int; offset : int; }

type info = { pos_info : position_info;  whitespace : string;
	      attachable_start : bool; attachable_end : bool;
	      mcode_start : mcodekind list; mcode_end : mcodekind list;
	      (* the following are only for + code *)
	      strings_before : (Ast_cocci.added_string * position_info) list;
	      strings_after : (Ast_cocci.added_string * position_info) list;
	      isSymbolIdent : bool; (* is the token a symbol identifier or not *) }

type adjacency = int

type fake_mcode = info * mcodekind * adjacency

type 'a mcode =
    'a * arity * info * mcodekind * anything list ref (* pos, - only *) *
      adjacency (* adjacency_index *)

and 'a wrap =
    { node : 'a;
      info : info;
      index : int ref;
      mcodekind : mcodekind ref;
      exp_ty : typeC option ref; (* only for expressions *)
      bef_aft : dots_bef_aft; (* only for statements *)
      true_if_arg : bool; (* true if "arg_exp", only for exprs *)
      true_if_test : bool; (* true if "test position", only for exprs *)
      true_if_test_exp : bool;(* true if "test_exp from iso", only for exprs *)
      (*nonempty if this represents the use of an iso*)
      iso_info : (string*anything) list }

and dots_bef_aft =
    NoDots | AddingBetweenDots of statement | DroppingBetweenDots of statement

(* for iso metavariables, true if they can only match nonmodified, unitary
   metavariables
   for SP metavariables, true if the metavariable is unitary (valid up to
   isomorphism phase only) *)
and pure = Impure | Pure | Context | PureContext (* pure and only context *)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

and 'a dots = 'a list wrap

(* --------------------------------------------------------------------- *)
(* Identifier *)

and base_ident =
    Id            of string mcode
  | MetaId        of Ast_cocci.meta_name mcode * constraints *
		     Ast_cocci.seed * pure
  | MetaFunc      of Ast_cocci.meta_name mcode * constraints * pure
  | MetaLocalFunc of Ast_cocci.meta_name mcode * constraints * pure
  | AsIdent       of ident * ident (* as ident, always metavar *)
  | DisjId        of string mcode * ident list *
                     string mcode list (* the |s *) * string mcode
  | ConjId        of string mcode * ident list *
                     string mcode list (* the &s *) * string mcode
  | OptIdent      of ident

and ident = base_ident wrap

(* --------------------------------------------------------------------- *)
(* Expression *)

and base_expression =
    Ident          of ident
  | Constant       of Ast_cocci.constant mcode
  | StringConstant of string mcode (* quote *) * string_fragment dots *
		      string mcode (* quote *) * Ast_cocci.isWchar
  | FunCall        of expression * string mcode (* ( *) *
                      expression dots * string mcode (* ) *)
  | Assignment     of expression * assignOp * expression *
	              bool (* true if it can match an initialization *)
  | Sequence       of expression * string mcode (* , *) * expression
  | CondExpr       of expression * string mcode (* ? *) * expression option *
	              string mcode (* : *) * expression
  | Postfix        of expression * Ast_cocci.fixOp mcode
  | Infix          of expression * Ast_cocci.fixOp mcode
  | Unary          of expression * Ast_cocci.unaryOp mcode
  | Binary         of expression * binaryOp * expression
  | Nested         of expression * binaryOp * expression
  | Paren          of string mcode (* ( *) * expression *
                      string mcode (* ) *)
  | ArrayAccess    of expression * string mcode (* [ *) * expression *
	              string mcode (* ] *)
  | RecordAccess   of expression * string mcode (* . *) * ident
  | RecordPtAccess of expression * string mcode (* -> *) * ident
  | Cast           of string mcode (* ( *) * typeC * attr list *
                      string mcode (* ) *) * expression
  | SizeOfExpr     of string mcode (* sizeof *) * expression
  | SizeOfType     of string mcode (* sizeof *) * string mcode (* ( *) *
                      typeC * string mcode (* ) *)
  | TypeExp        of typeC
  | Constructor    of string mcode (* ( *) * typeC * string mcode (* ) *) *
 	              initialiser
  | MetaErr        of Ast_cocci.meta_name mcode * constraints * pure
  | MetaExpr       of Ast_cocci.meta_name mcode * constraints *
	typeC list option * Ast_cocci.form * pure *
	listlen option (* bitfield *)
  | MetaExprList   of Ast_cocci.meta_name mcode (* only in arglists *) *
	              listlen * constraints * pure
  | AsExpr         of expression * expression (* as expr, always metavar *)
  | AsSExpr        of expression * statement (* as expr, always metavar *)
  | EComma         of string mcode (* only in arglists *)
  | DisjExpr       of string mcode * expression list * string mcode list *
	              string mcode
  | ConjExpr       of string mcode * expression list * string mcode list *
	              string mcode
  | NestExpr       of string mcode * expression dots * string mcode *
                      (string mcode * string mcode * expression) option
                      (* whencode *) * Ast_cocci.multi
  | Edots          of string mcode (* ... *) * (string mcode * string mcode *
                      expression) option (* whencode *)
  | OptExp         of expression

and expression = base_expression wrap

and constraints = expression Ast_cocci.generic_constraints

and listlen =
    MetaListLen of Ast_cocci.meta_name mcode * constraints
  | CstListLen of int
  | AnyListLen

and base_string_fragment =
    ConstantFragment of string mcode
  | FormatFragment of string mcode (*%*) * string_format (* format *)
  | Strdots of string mcode
  | MetaFormatList of string mcode (*%*) * Ast_cocci.meta_name mcode *
	constraints * listlen

and string_fragment = base_string_fragment wrap

and base_string_format =
    ConstantFormat of string mcode
  | MetaFormat of Ast_cocci.meta_name mcode * constraints

and string_format = base_string_format wrap

(* --------------------------------------------------------------------- *)
(* First class operators *)
and base_assignOp =
    SimpleAssign of simpleAssignOp mcode
  | OpAssign of Ast_cocci.arithOp mcode
  | MetaAssign of
      Ast_cocci.meta_name mcode * constraints * pure
and simpleAssignOp = string
and assignOp = base_assignOp wrap

and base_binaryOp =
    Arith of Ast_cocci.arithOp mcode
  | Logical of Ast_cocci.logicalOp mcode
  | MetaBinary of
      Ast_cocci.meta_name mcode * constraints * pure
and binaryOp = base_binaryOp wrap

(* --------------------------------------------------------------------- *)
(* Types *)

and base_typeC =
    ConstVol        of Ast_cocci.const_vol mcode * typeC
  | BaseType        of Ast_cocci.baseType * string mcode list
  | Signed          of Ast_cocci.sign mcode * typeC option
  | Pointer         of typeC * string mcode (* * *)
  | ParenType       of string mcode (* ( *) * typeC * string mcode (* ) *)
  | FunctionType    of typeC *
                  string mcode (* ( *) * parameter_list * string mcode (* ) *)
  | Array           of typeC * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | Decimal         of string mcode (* decimal *) * string mcode (* ( *) *
	               expression *
	               string mcode option (* , *) * expression option *
	               string mcode (* ) *) (* IBM C only *)
  | EnumName        of string mcode (*enum*) * ident option (* name *)
  | EnumDef  of typeC (* either StructUnionName or metavar *) *
	string mcode (* { *) * enum_decl dots * string mcode (* } *)
  | StructUnionName of Ast_cocci.structUnion mcode * ident option (* name *)
  | StructUnionDef  of typeC (* either StructUnionName or metavar *) *
	string mcode (* { *) * field dots * string mcode (* } *)
  | TypeOfExpr      of string mcode (* typeof *) * string mcode (* ( *) *
                       expression * string mcode (* ) *)
  | TypeOfType      of string mcode (* typeof *) * string mcode (* ( *) *
                       typeC * string mcode (* ) *)
  | TypeName        of string mcode
  | AutoType        of string mcode (* auto *) (* c++ >= 11 *)
  | MetaType        of Ast_cocci.meta_name mcode * constraints
	* pure
  | AsType          of typeC * typeC (* as type, always metavar *)
  | DisjType        of string mcode * typeC list * (* only after iso *)
                       string mcode list (* the |s *)  * string mcode
  | ConjType        of string mcode * typeC list * (* only after iso *)
                       string mcode list (* the |s *)  * string mcode
  | OptType         of typeC

and typeC = base_typeC wrap

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and base_declaration =
    MetaDecl   of Ast_cocci.meta_name mcode * constraints * pure (* variables *)
  | AsDecl        of declaration * declaration
  | Init       of Ast_cocci.storage mcode option * typeC * ident * attr list *
	string mcode (*=*) * initialiser * string mcode (*;*)
  | UnInit     of Ast_cocci.storage mcode option * typeC * ident * attr list *
	string mcode (* ; *)
  | FunProto of
	fninfo list * ident (* name *) *
	string mcode (* ( *) * parameter_list *
        (string mcode (* , *) * string mcode (* ...... *) ) option *
	string mcode (* ) *) * string mcode (* ; *)
  | TyDecl of typeC * attr list * string mcode (* ; *)
  | MacroDecl of Ast_cocci.storage mcode option *
	ident (* name *) * string mcode (* ( *) *
        expression dots * string mcode (* ) *) *
        attr list * string mcode (* ; *)
  | MacroDeclInit of Ast_cocci.storage mcode option *
	ident (* name *) * string mcode (* ( *) *
        expression dots * string mcode (* ) *) * string mcode (*=*) *
        initialiser * string mcode (* ; *)
  | Typedef of string mcode (* typedef *) * typeC * typeC * string mcode (*;*)
  | DisjDecl   of string mcode * declaration list * string mcode list *
	          string mcode
  | ConjDecl   of string mcode * declaration list * string mcode list *
	          string mcode
  | OptDecl    of declaration

and declaration = base_declaration wrap

(* --------------------------------------------------------------------- *)
(* Field declaration *)

and base_field =
  | MetaField  of Ast_cocci.meta_name mcode * constraints *
	pure (* structure fields *)
  | MetaFieldList of Ast_cocci.meta_name mcode * listlen * constraints * pure
  | Field     of typeC * ident option * bitfield option * string mcode (* ; *)
  | DisjField   of string mcode * field list * string mcode list *
	          string mcode
  | ConjField   of string mcode * field list * string mcode list *
	          string mcode
  | Fdots      of string mcode (* ... *) * (string mcode * string mcode *
                  field) option (* whencode *)
  | OptField    of field

and bitfield = string mcode (* : *) * expression

and field = base_field wrap

and base_enum_decl =
    Enum of ident * (string mcode (* = *) * expression) option
  | EnumComma of string mcode (* , *)
  | EnumDots of string mcode (* ... *) * (string mcode * string mcode *
                enum_decl) option (* whencode *)

and enum_decl = base_enum_decl wrap

(* --------------------------------------------------------------------- *)
(* Initializers *)

and base_initialiser =
    MetaInit of Ast_cocci.meta_name mcode * constraints * pure
  | MetaInitList of Ast_cocci.meta_name mcode * listlen * constraints * pure
  | AsInit of initialiser * initialiser (* as init, always metavar *)
  | InitExpr of expression
  | InitList of string mcode (*{*) * initialiser_list * string mcode (*}*) *
	bool (* true if ordered, false if unordered *)
  | InitGccExt of
      designator list (* name *) * string mcode (*=*) *
	initialiser (* gccext: *)
  | InitGccName of ident (* name *) * string mcode (*:*) *
	initialiser
  | IComma of string mcode
  | Idots  of string mcode (* ... *) *
              (string mcode * string mcode * initialiser) option (* whencode *)
  | OptIni    of initialiser

and designator =
    DesignatorField of string mcode (* . *) * ident
  | DesignatorIndex of string mcode (* [ *) * expression * string mcode (* ] *)
  | DesignatorRange of
      string mcode (* [ *) * expression * string mcode (* ... *) *
      expression * string mcode (* ] *)

and initialiser = base_initialiser wrap

and initialiser_list = initialiser dots

(* --------------------------------------------------------------------- *)
(* Parameter *)

and base_parameterTypeDef =
    VoidParam     of typeC * attr list
  | Param         of typeC * ident option * attr list
  | MetaParam     of Ast_cocci.meta_name mcode * constraints * pure
  | MetaParamList of Ast_cocci.meta_name mcode * listlen * constraints * pure
  | AsParam       of parameterTypeDef * expression (* expr, always metavar *)
  | PComma        of string mcode
  | Pdots         of string mcode (* ... *)
  | OptParam      of parameterTypeDef

and parameterTypeDef = base_parameterTypeDef wrap

and parameter_list = parameterTypeDef dots

(* --------------------------------------------------------------------- *)
(* #define Parameters *)

and base_define_param =
    DParam        of ident
  | MetaDParamList of Ast_cocci.meta_name mcode * listlen * constraints * pure
  | DPComma       of string mcode
  | DPdots        of string mcode (* ... *)
  | OptDParam     of define_param

and define_param = base_define_param wrap

and base_define_parameters =
    NoParams
  | DParams      of string mcode(*( *) * define_param dots * string mcode(* )*)

and define_parameters = base_define_parameters wrap

(* --------------------------------------------------------------------- *)
(* Statement*)

and base_statement =
    Decl          of (info * mcodekind) (* before the decl *) * declaration
  | Seq           of string mcode (* { *) * statement dots *
 	             string mcode (* } *)
  | ExprStatement of expression option * string mcode (*;*)
  | IfThen        of string mcode (* if *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
	             statement * fake_mcode (* after info *)
  | IfThenElse    of string mcode (* if *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
	             statement * string mcode (* else *) * statement *
	             fake_mcode (* after info *)
  | While         of string mcode (* while *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
	             statement * fake_mcode (* after info *)
  | Do            of string mcode (* do *) * statement *
                     string mcode (* while *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) *
                     string mcode (* ; *)
  | For           of string mcode (* for *) * string mcode (* ( *) * forinfo *
	             expression option * string mcode (*;*) *
                     expression option * string mcode (* ) *) * statement *
	             fake_mcode (* after info *)
  | Iterator      of ident (* name *) * string mcode (* ( *) *
	             expression dots * string mcode (* ) *) *
	             statement * fake_mcode (* after info *)
  | Switch        of string mcode (* switch *) * string mcode (* ( *) *
	             expression * string mcode (* ) *) * string mcode (* { *) *
	             statement (*decl*) dots *
	             case_line dots * string mcode (* } *)
  | Break         of string mcode (* break *) * string mcode (* ; *)
  | Continue      of string mcode (* continue *) * string mcode (* ; *)
  | Label         of ident * string mcode (* : *)
  | Goto          of string mcode (* goto *) * ident * string mcode (* ; *)
  | Return        of string mcode (* return *) * string mcode (* ; *)
  | ReturnExpr    of string mcode (* return *) * expression *
	             string mcode (* ; *)
  | Exec          of string mcode (* EXEC *) * string mcode (* language *) *
	             exec_code dots * string mcode (* ; *)
  | MetaStmt      of Ast_cocci.meta_name mcode * constraints * pure
  | MetaStmtList  of Ast_cocci.meta_name mcode (*only in statement lists*) *
	             listlen * constraints * pure
  | AsStmt        of statement * statement (* as statement, always metavar *)
  | Exp           of expression  (* only in dotted statement lists *)
  | TopExp        of expression (* for macros body *)
  | Ty            of typeC (* only at top level *)
  | TopId         of ident (* only at top level *)
  | TopInit       of initialiser (* only at top level *)
  | Disj          of string mcode * statement dots list * string mcode list *
	             string mcode
  | Conj          of string mcode * statement dots list * string mcode list *
	             string mcode
  | Nest          of string mcode * statement dots * string mcode *
	             (statement dots,statement) whencode list * Ast_cocci.multi
  | Dots          of string mcode (* ... *) *
                     (statement dots,statement) whencode list
  | FunDecl of (info * mcodekind) (* before the function decl *) *
	fninfo list * ident (* name *) *
	string mcode (* ( *) * parameter_list *
	(string mcode (* , *) * string mcode (* ...... *) ) option *
	string mcode (* ) *) *
	string mcode (* { *) * statement dots *
	string mcode (* } *) *
	(info * mcodekind) (* after the function decl *)
  | Include of string mcode (* #include *) * Ast_cocci.inc_file mcode(* file *)
  | MetaInclude of string mcode (* #include *) * expression (* file *)
  | Undef of string mcode (* #define *) * ident (* name *)
  | Define of string mcode (* #define *) * ident (* name *) *
	define_parameters (*params*) * statement dots
  | Pragma of string mcode (* #pragma *) * ident * pragmainfo
  | OptStm   of statement

and base_pragmainfo =
    PragmaString of string mcode
  | PragmaDots of string mcode

and pragmainfo = base_pragmainfo wrap

and base_forinfo =
    ForExp of expression option * string mcode (*;*)
  | ForDecl of (info * mcodekind) (* before the decl *) * declaration

and forinfo = base_forinfo wrap

and fninfo =
    FStorage of Ast_cocci.storage mcode
  | FType of typeC
  | FInline of string mcode
  | FAttr of attr

and base_attr =
    Attribute of attr_arg
  | GccAttribute of string mcode (* __attribute__ *) *
                    string mcode (* ( *) * string mcode (* ( *) *
                    attr_arg * string mcode (* ) *) * string mcode (* ) *)

and attr = base_attr wrap

and base_attr_arg =
    AttrName of string mcode
  | MetaAttr of Ast_cocci.meta_name mcode * constraints * pure

and attr_arg = base_attr_arg wrap

and ('a,'b) whencode =
    WhenNot of string mcode (* when *) * string mcode (* != *) * 'a
  | WhenAlways of string mcode (* when *) * string mcode (* = *) * 'b
  | WhenModifier of string mcode (* when *) * Ast_cocci.when_modifier
  | WhenNotTrue of string mcode (* when *) * string mcode (* != *) * expression
  | WhenNotFalse of string mcode (* when *) * string mcode (* != *) *
    expression

and statement = base_statement wrap

and base_case_line =
    Default of string mcode (* default *) * string mcode (*:*) * statement dots
  | Case of string mcode (* case *) * expression * string mcode (*:*) *
	statement dots
  | DisjCase of string mcode * case_line list *
	string mcode list (* the |s *) * string mcode
  | OptCase of case_line

and case_line = base_case_line wrap

and base_exec_code =
    ExecEval of string mcode (* : *) * expression
  | ExecToken of string mcode
  | ExecDots of string mcode (* ... *)

and exec_code = base_exec_code wrap

(* --------------------------------------------------------------------- *)
(* Positions *)

and meta_pos =
    MetaPos of Ast_cocci.meta_name mcode * constraints *
	Ast_cocci.meta_collect
  | MetaCom of Ast_cocci.meta_name mcode * constraints

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and base_top_level =
    NONDECL of statement (* cannot match all of a top-level declaration *)
  | TOPCODE of statement dots
  | CODE of statement dots
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | ERRORWORDS of expression list
  | OTHER of statement (* temporary, disappears after top_level.ml *)

and top_level = base_top_level wrap
and rule = top_level list

and parsed_rule =
    CocciRule of
      (rule * Ast_cocci.metavar list *
	 (string list * string list * Ast_cocci.dependency * string *
	    Ast_cocci.exists)) *
	(rule * Ast_cocci.metavar list) *
	Ast_cocci.metavar list (* inhertited metavars *) * Ast_cocci.ruletype
  | ScriptRule of string (* name *) *
      string * Ast_cocci.dependency *
	(Ast_cocci.script_meta_name *
	   Ast_cocci.meta_name * Ast_cocci.metavar * Ast_cocci.mvinit)
	    list (*inherited vars*) *
	Ast_cocci.meta_name list (*script vars*) * Ast_cocci.script_position *
	string
  | InitialScriptRule of string (* name *) * string * Ast_cocci.dependency *
	(Ast_cocci.script_meta_name *
	   Ast_cocci.meta_name * Ast_cocci.metavar * Ast_cocci.mvinit)
	     list (*virtual vars*) * Ast_cocci.script_position *
	string
  | FinalScriptRule of string (* name *) * string * Ast_cocci.dependency *
	(Ast_cocci.script_meta_name *
	   Ast_cocci.meta_name * Ast_cocci.metavar * Ast_cocci.mvinit)
	     list (*virtual vars*) * Ast_cocci.script_position *
	string

(* --------------------------------------------------------------------- *)

and dep =
    Dep of string (* rule applies for the current binding *)
  | AntiDep of dep (* rule doesn't apply for the current binding *)
  | EverDep of string (* rule applies for some binding *)
  | NeverDep of string (* rule never applies for any binding *)
  | AndDep of dep * dep
  | OrDep of dep * dep
  | FileIn of string

and dependency =
    NoDep | FailDep | ExistsDep of dep | ForallDep of dep

(* --------------------------------------------------------------------- *)

and anything =
    DotsExprTag of expression dots
  | DotsInitTag of initialiser dots
  | DotsParamTag of parameterTypeDef dots
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
  | ArgExprTag of expression  (* for isos *)
  | TestExprTag of expression (* for isos *)
  | TypeCTag of typeC
  | ParamTag of parameterTypeDef
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
  | IsoWhenTag of Ast_cocci.when_modifier (*only for when code, in iso phase*)
  | IsoWhenTTag of expression(*only for when code, in iso phase*)
  | IsoWhenFTag of expression(*only for when code, in iso phase*)
  | MetaPosTag of meta_pos
  | HiddenVarTag of anything list (* in iso_compile/pattern only *)
  | WhenTag of string mcode (* when *) * string mcode option *
    anything (* iso pattern *)

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
val enum_decl : enum_decl -> anything
val stmt : statement -> anything
val forinfo : forinfo -> anything
val case_line : case_line -> anything
val string_fragment : string_fragment -> anything
val attr : attr -> anything
val attr_arg : attr_arg -> anything
val top : top_level -> anything

(* --------------------------------------------------------------------- *)
(* Avoid cluttering the parser.  Calculated in compute_lines.ml. *)

val default_info : unit -> info
val default_befaft : unit -> mcodekind
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
val get_mcode_logline : 'a mcode -> int
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
val set_iso : 'a wrap -> (string*anything) list -> 'a wrap
val get_iso : 'a wrap -> (string*anything) list
val fresh_index : unit -> int
val set_mcode_data : 'a -> 'a mcode -> 'a mcode
val make_mcode : 'a -> 'a mcode
val make_mcode_info : 'a -> info -> 'a mcode
val make_minus_mcode : 'a -> 'a mcode
val get_rule_name : parsed_rule -> string

val meta_pos_name : anything -> Ast_cocci.meta_name mcode
val meta_names_of_typeC : typeC -> Ast_cocci.meta_name list
(**
 * [meta_names_of_typeC ty] enumerates all the meta names that occur in [ty].
 *)
val meta_pos_constraint_names : anything -> Ast_cocci.meta_name list

val lub_pure : pure -> pure -> pure

(* --------------------------------------------------------------------- *)

val rule_name : string ref (* for the convenience of the parser *)

val string_of_assignOp : assignOp -> string
val string_of_binaryOp : binaryOp -> string

val is_unknown_type : typeC -> bool
(**
 * [is_unknown_type ty] returns true iff [ty] is an unknown type
 * (i.e., of the form [BaseType (Ast_cocci.Unknown, _)]).
 *)
