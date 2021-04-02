(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)
(* Modified code *)

type arity = OPT | NONE

type token_info =
    { tline_start : int; tline_end : int;
      left_offset : int; right_offset : int; }
let default_token_info =
  { tline_start = -1; tline_end = -1; left_offset = -1; right_offset = -1 }

(* MIXED is like CONTEXT, since sometimes MIXED things have to revert to
CONTEXT - see insert_plus.ml *)

type mcodekind =
    MINUS       of (Ast.anything Ast.replacement * token_info) ref
  | PLUS        of Ast.count
  | CONTEXT     of (Ast.anything Ast.befaft * token_info * token_info) ref
  | MIXED       of (Ast.anything Ast.befaft * token_info * token_info) ref

type position_info = { line_start : int; line_end : int;
		       logical_start : int; logical_end : int;
		       column : int; offset : int; }

type info = { pos_info : position_info;
              (* preceding whitespace*) whitespace : string;
	      attachable_start : bool; attachable_end : bool;
	      mcode_start : mcodekind list; mcode_end : mcodekind list;
	      (* the following are only for + code *)
	      strings_before : (Ast.added_string * position_info) list;
	      strings_after : (Ast.added_string * position_info) list;
	      isSymbolIdent : bool; (* is the token a symbol identifier or not *) }

(* adjacency index is incremented when we skip over dots or nest delimiters
it is used in deciding how much to remove, when two adjacent code tokens are
removed. *)
type adjacency = int

type fake_mcode = info * mcodekind * adjacency

type 'a mcode =
    'a * arity * info * mcodekind * anything list ref (* pos, - only *) *
      adjacency (* adjacency_index *)
(* int ref is an index *)
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

(* for iso metavariables, true if they can only match nonmodified terms with
   all metavariables unitary
   for SP metavariables, true if the metavariable is unitary (valid up to
   isomorphism phase only)
   In SP, the only options are impure and context
*)
and pure = Impure | Pure | Context | PureContext (* pure and only context *)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

and 'a dots = 'a list wrap

(* --------------------------------------------------------------------- *)
(* Identifier *)

and base_ident =
    Id            of string mcode
  | MetaId        of Ast.meta_name mcode * constraints * Ast.seed * pure
  | MetaFunc      of Ast.meta_name mcode * constraints * pure
  | MetaLocalFunc of Ast.meta_name mcode * constraints * pure
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
  | Constant       of Ast.constant mcode
  | StringConstant of string mcode (* quote *) * string_fragment dots *
		      string mcode (* quote *) * Ast.isWchar
  | FunCall        of expression * string mcode (* ( *) *
                      expression dots * string mcode (* ) *)
  | Assignment     of expression * assignOp * expression *
	              bool (* true if it can match an initialization *)
  | Sequence       of expression * string mcode (* , *) * expression
  | CondExpr       of expression * string mcode (* ? *) * expression option *
	              string mcode (* : *) * expression
  | Postfix        of expression * Ast.fixOp mcode
  | Infix          of expression * Ast.fixOp mcode
  | Unary          of expression * Ast.unaryOp mcode
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
  | TypeExp        of typeC (* type name used as an expression, only in args *)
  | Constructor    of string mcode (* ( *) * typeC * string mcode (* ) *) *
	              initialiser
  | MetaErr        of Ast.meta_name mcode * constraints * pure
  | MetaExpr       of Ast.meta_name mcode * constraints *
	typeC list option * Ast.form * pure *
	listlen option (* bitfield *)
  | MetaExprList   of Ast.meta_name mcode (* only in arg lists *) *
	              listlen * constraints * pure
  | AsExpr         of expression * expression (* as expr, always metavar *)
  | AsSExpr        of expression * statement (* as expr, always metavar *)
  | EComma         of string mcode (* only in arg lists *)
  | DisjExpr       of string mcode * expression list *
	              string mcode list (* the |s *) * string mcode
  | ConjExpr       of string mcode * expression list *
	              string mcode list (* the &s *) * string mcode
  (* not clear why NestExpr allows dots on the expression, nor why it has a
     whencode field *)
  | NestExpr       of string mcode * expression dots * string mcode *
	              (string mcode * string mcode * expression) option
	              (* whencode *) * Ast.multi
  | Edots          of string mcode (* ... *) * (string mcode * string mcode *
                      expression) option (* whencode *)
  | OptExp         of expression

and expression = base_expression wrap

and constraints = expression Ast.generic_constraints

and listlen =
    MetaListLen of Ast.meta_name mcode * constraints
  | CstListLen of int
  | AnyListLen

and base_string_fragment =
    ConstantFragment of string mcode
  | FormatFragment of string mcode (*%*) * string_format (* format *)
  | Strdots of string mcode
  | MetaFormatList of string mcode (*%*) * Ast.meta_name mcode *
	constraints * listlen

and string_fragment = base_string_fragment wrap

and base_string_format =
    ConstantFormat of string mcode
  | MetaFormat of Ast.meta_name mcode * constraints

and string_format = base_string_format wrap

(* --------------------------------------------------------------------- *)
(* First class operators *)
and base_assignOp =
    SimpleAssign of simpleAssignOp mcode
  | OpAssign of Ast_cocci.arithOp mcode
  | MetaAssign of Ast_cocci.meta_name mcode * constraints * pure
and simpleAssignOp = string
and assignOp = base_assignOp wrap

and base_binaryOp =
    Arith of Ast_cocci.arithOp mcode
  | Logical of Ast_cocci.logicalOp mcode
  | MetaBinary of Ast_cocci.meta_name mcode * constraints * pure
and binaryOp = base_binaryOp wrap

(* --------------------------------------------------------------------- *)
(* Types *)

and base_typeC =
    ConstVol        of Ast.const_vol mcode * typeC
  | BaseType        of Ast.baseType * string mcode list
  | Signed          of Ast.sign mcode * typeC option
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
  | StructUnionName of Ast.structUnion mcode * ident option (* name *)
  | StructUnionDef  of typeC (* either StructUnionName or metavar *) *
	string mcode (* { *) * field dots * string mcode (* } *)
  | TypeOfExpr      of string mcode (* sizeof *) * string mcode (* ( *) *
                       expression * string mcode (* ) *)
  | TypeOfType      of string mcode (* sizeof *) * string mcode (* ( *) *
                       typeC * string mcode (* ) *)
  | TypeName        of string mcode
  | AutoType        of string mcode (* auto *) (* c++ >= 11 *)
  | MetaType        of Ast.meta_name mcode * constraints * pure
  | AsType          of typeC * typeC (* as type, always metavar *)
  | DisjType        of string mcode * typeC list *
                       string mcode list (* the |s *)  * string mcode
  | ConjType        of string mcode * typeC list *
                       string mcode list (* the |s *)  * string mcode
  | OptType         of typeC

and typeC = base_typeC wrap

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and base_declaration =
    MetaDecl of Ast.meta_name mcode * constraints * pure (* variables *)
    (* the following are kept separate from MetaDecls because ultimately
       they don't match the same thing at all.  Consider whether there
       should be a separate type for fields, as in the C AST *)
  | AsDecl        of declaration * declaration
  | Init of Ast.storage mcode option * typeC * ident * attr list *
	string mcode (*=*) * initialiser * string mcode (*;*)
  | UnInit of Ast.storage mcode option * typeC * ident * attr list *
	string mcode (* ; *)
  | FunProto of
	fninfo list * ident (* name *) *
	string mcode (* ( *) * parameter_list *
        (string mcode (* , *) * string mcode (* ...... *) ) option *
	string mcode (* ) *) *
	string mcode (* ; *)
  | TyDecl of typeC * attr list * string mcode (* ; *)
  | MacroDecl of Ast.storage mcode option *
	ident (* name *) * string mcode (* ( *) *
        expression dots * string mcode (* ) *) *
        attr list * string mcode (* ; *)
  | MacroDeclInit of Ast.storage mcode option *
	ident (* name *) * string mcode (* ( *) *
        expression dots * string mcode (* ) *) * string mcode (*=*) *
	initialiser * string mcode (* ; *)
  | Typedef of string mcode (* typedef *) * typeC * typeC * string mcode (*;*)
  | DisjDecl   of string mcode * declaration list *
	          string mcode list (* the |s *)  * string mcode
  | ConjDecl   of string mcode * declaration list *
	          string mcode list (* the &s *)  * string mcode
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
    MetaInit of Ast.meta_name mcode * constraints * pure
  | MetaInitList of Ast.meta_name mcode * listlen * constraints * pure
  | AsInit of initialiser * initialiser (* as init, always metavar *)
  | InitExpr of expression
  | InitList of string mcode (*{*) * initialiser_list * string mcode (*}*) *
	(* true if ordered, as for array, false if unordered, as for struct *)
	bool
  | InitGccExt of
      designator list (* name *) * string mcode (*=*) *
	initialiser (* gccext: *)
  | InitGccName of ident (* name *) * string mcode (*:*) *
	initialiser
  | IComma of string mcode (* , *)
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
  | MetaParam     of Ast.meta_name mcode * constraints * pure
  | MetaParamList of Ast.meta_name mcode * listlen * constraints * pure
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
  | MetaDParamList of Ast.meta_name mcode * listlen * constraints * pure
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
    (*Decl and FunDecl don't need adjacency.  Delete all comments in any case*)
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
  | MetaStmt      of Ast.meta_name mcode * constraints * pure
  | MetaStmtList  of Ast.meta_name mcode(*only in statement lists*) * listlen *
	             constraints * pure
  | AsStmt        of statement * statement (* as statement, always metavar *)
  | Exp           of expression  (* only in dotted statement lists *)
  | TopExp        of expression (* for macros body *)
  | Ty            of typeC (* only at top level *)
  | TopId         of ident (* only at top level *)
  | TopInit       of initialiser (* only at top level *)
  | Disj          of string mcode * statement dots list *
	             string mcode list (* the |s *)  * string mcode
  | Conj          of string mcode * statement dots list *
	             string mcode list (* the &s *)  * string mcode
  | Nest          of string mcode * statement dots * string mcode *
	             (statement dots,statement) whencode list * Ast.multi
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
  | Include of string mcode (* #include *) * Ast.inc_file mcode (* file *)
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
    FStorage of Ast.storage mcode
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
  | MetaAttr of Ast.meta_name mcode * constraints * pure

and attr_arg = base_attr_arg wrap

and ('a,'b) whencode =
    WhenNot of string mcode (* when *) * string mcode (* != *) * 'a
  | WhenAlways of string mcode (* when *) * string mcode (* = *) * 'b
  | WhenModifier of string mcode (* when *) * Ast.when_modifier
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
    MetaPos of Ast.meta_name mcode * constraints *
	Ast.meta_collect
  | MetaCom of Ast.meta_name mcode * constraints

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and base_top_level =
    NONDECL of statement
  | TOPCODE of statement dots
  | CODE of statement dots
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | ERRORWORDS of expression list
  | OTHER of statement (* temporary, disappears after top_level.ml *)

and top_level = base_top_level wrap
and rule = top_level list

and parsed_rule =
    CocciRule of
      (rule (*minus*) * Ast.metavar list (*minus metavars*) *
	(string list (*isos*) * string list (*drop_isos*) *
         Ast.dependency (*dependencies*) * string (*rulename*) * Ast.exists)) *
      (rule (*plus*) * Ast.metavar list (*plus metavars*)) *
	Ast.metavar list (* inhertited metavars *) * Ast.ruletype
  | ScriptRule of string (* name *) * string * Ast.dependency *
	(Ast.script_meta_name * Ast.meta_name * Ast.metavar * Ast.mvinit)
	  list *
	Ast.meta_name list (*script vars*) * Ast.script_position *
	string
  | InitialScriptRule of string (* name *) * string * Ast.dependency *
	(Ast.script_meta_name * Ast.meta_name * Ast.metavar * Ast.mvinit)
	  list * Ast.script_position *
	string
  | FinalScriptRule of string (* name *) * string * Ast.dependency *
	(Ast.script_meta_name * Ast.meta_name * Ast.metavar * Ast.mvinit)
	  list * Ast.script_position *
	string (* no script vars possible here *)

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
  | IsoWhenTag of Ast.when_modifier
  | IsoWhenTTag of expression
  | IsoWhenFTag of expression
  | MetaPosTag of meta_pos
  | HiddenVarTag of anything list (* in iso_compile/pattern only *)
  | WhenTag of string mcode (* when *) * string mcode option
      (* !=, =, or none if whenmodifier*) * anything (* iso pattern *)

let dotsExpr x = DotsExprTag x
let dotsParam x = DotsParamTag x
let dotsInit x = DotsInitTag x
let dotsStmt x = DotsStmtTag x
let dotsDecl x = DotsDeclTag x
let dotsField x = DotsFieldTag x
let dotsEnumDecl x = DotsEnumDeclTag x
let dotsCase x = DotsCaseTag x
let dotsDefParam x = DotsDefParamTag x
let ident x = IdentTag x
let expr x = ExprTag x
let assignOp x = AssignOpTag x
let binaryOp x = BinaryOpTag x
let typeC x = TypeCTag x
let param x = ParamTag x
let ini x = InitTag x
let decl x = DeclTag x
let field x = FieldTag x
let stmt x = StmtTag x
let forinfo x = ForInfoTag x
let case_line x = CaseLineTag x
let string_fragment x = StringFragmentTag x
let attr x = AttributeTag x
let attr_arg x = AttrArgTag x
let top x = TopTag x
let enum_decl x = EnumDeclTag x

(* --------------------------------------------------------------------- *)
(* Avoid cluttering the parser.  Calculated in compute_lines.ml. *)

let pos_info =
  { line_start = -1; line_end = -1;
    logical_start = -1; logical_end = -1;
    column = -1; offset = -1; }

let default_info _ = (* why is this a function? *)
  { pos_info = pos_info; whitespace = "";
    attachable_start = true; attachable_end = true;
    mcode_start = []; mcode_end = [];
    strings_before = []; strings_after = []; isSymbolIdent = false; }

let default_befaft _ =
  MIXED(ref (Ast.NOTHING,default_token_info,default_token_info))
let context_befaft _ =
  CONTEXT(ref (Ast.NOTHING,default_token_info,default_token_info))
let minus_befaft _ = MINUS(ref (Ast.NOREPLACEMENT,default_token_info))

let wrap x =
  { node = x;
    info = default_info();
    index = ref (-1);
    mcodekind = ref (default_befaft());
    exp_ty = ref None;
    bef_aft = NoDots;
    true_if_arg = false;
    true_if_test = false;
    true_if_test_exp = false;
    iso_info = [] }
let context_wrap x =
  { node = x;
    info = default_info();
    index = ref (-1);
    mcodekind = ref (context_befaft());
    exp_ty = ref None;
    bef_aft = NoDots;
    true_if_arg = false;
    true_if_test = false;
    true_if_test_exp = false;
    iso_info = [] }
let unwrap x = x.node
let unwrap_mcode (x,_,_,_,_,_) = x
let rewrap model x = { model with node = x }
let rewrap_mcode (_,arity,info,mcodekind,pos,adj) x =
  (x,arity,info,mcodekind,pos,adj)
let copywrap model x =
  { model with node = x; index = ref !(model.index);
    mcodekind = ref !(model.mcodekind); exp_ty = ref !(model.exp_ty)}
let get_pos (_,_,_,_,x,_) = !x
let get_pos_ref (_,_,_,_,x,_) = x
let set_pos pos (m,arity,info,mcodekind,_,adj) =
  (m,arity,info,mcodekind,ref pos,adj)
let get_info x      = x.info
let set_info x info = {x with info = info}
let get_line x      = x.info.pos_info.line_start
let get_line_end x  = x.info.pos_info.line_end
let get_index x     = !(x.index)
let set_index x i   = x.index := i
let get_mcodekind x = !(x.mcodekind)
let get_mcode_mcodekind (_,_,_,mcodekind,_,_) = mcodekind
let get_mcode_line (_,_,info,_,_,_) = info.pos_info.line_start
let get_mcode_logline (_,_,info,_,_,_) = info.pos_info.logical_start
let get_mcodekind_ref x = x.mcodekind
let set_mcodekind x mk  = x.mcodekind := mk
let set_type x t        = x.exp_ty := t
let get_type x          = !(x.exp_ty)
let get_dots_bef_aft x  = x.bef_aft
let set_dots_bef_aft x dots_bef_aft = {x with bef_aft = dots_bef_aft}
let get_arg_exp x       = x.true_if_arg
let set_arg_exp x       = {x with true_if_arg = true}
let get_test_pos x      = x.true_if_test
let set_test_pos x      = {x with true_if_test = true}
let get_test_exp x      = x.true_if_test_exp
let set_test_exp x      = {x with true_if_test_exp = true}
let clear_test_exp x      = {x with true_if_test_exp = false}
let get_iso x           = x.iso_info
let set_iso x i = if !Flag.track_iso_usage then {x with iso_info = i} else x
let set_mcode_data data (_,ar,info,mc,pos,adj) = (data,ar,info,mc,pos,adj)
let get_rule_name = function
  | CocciRule ((_,_,(_,_,_,nm,_)),_,_,_) | InitialScriptRule (nm,_,_,_,_,_)
  | FinalScriptRule (nm,_,_,_,_,_) | ScriptRule (nm,_,_,_,_,_,_) -> nm

(* --------------------------------------------------------------------- *)

let rec meta_pos_name = function
    HiddenVarTag(vars) ->
	(* totally fake, just drop the rest, only for isos *)
      meta_pos_name (List.hd vars)
  | MetaPosTag(MetaPos(name,_constraints,_)) -> name
  | MetaPosTag(MetaCom(name,_constraints)) -> name
  | IdentTag(i) ->
      (match unwrap i with
	MetaId(name,_constraints,_seed,_pure) -> name
      | _ -> failwith "bad metavariable")
  | ExprTag(e) ->
      (match unwrap e with
	MetaExpr(name,_constraints,_ty,_form,_pure,_bitfield) -> name
      | MetaExprList(name,_len,_constraints,_pure) -> name
      | _ -> failwith "bad metavariable")
  | TypeCTag(t) ->
      (match unwrap t with
	MetaType(name,_cstr,_pure) -> name
      | _ -> failwith "bad metavariable")
  | DeclTag(d) ->
      (match unwrap d with
	MetaDecl(name,_constraints,_pure) -> name
      | _ -> failwith "bad metavariable")
  | InitTag(i) ->
      (match unwrap i with
	MetaInit(name,_constraints,_pure) -> name
      | _ -> failwith "bad metavariable")
  | StmtTag(s) ->
      (match unwrap s with
	MetaStmt(name,_constraints,_pure) -> name
      | _ -> failwith "bad metavariable")
  | _ -> failwith "bad metavariable"

let rec meta_names_of_ident ident =
  match unwrap ident with
    Id _ -> []
  | MetaId (tyname, _, _, _)
  | MetaFunc (tyname, _, _)
  | MetaLocalFunc (tyname, _, _) -> [unwrap_mcode tyname]
  | AsIdent (ident0, ident1) ->
      meta_names_of_ident ident0 @ meta_names_of_ident ident1
  | DisjId (_, l, _, _) | ConjId (_, l, _, _) ->
      List.flatten (List.map meta_names_of_ident l)
  | OptIdent ident' -> meta_names_of_ident ident'

let meta_names_of_expression expression =
  match unwrap expression with
    Ident ident -> meta_names_of_ident ident
  | _ -> []

let rec meta_names_of_typeC ty =
  match unwrap ty with
    ConstVol (_, ty)
  | Signed (_, Some ty)
  | Pointer (ty, _)
  | Array (ty, _, _, _) -> meta_names_of_typeC ty
  | EnumName (_, Some ident)
  | StructUnionName(_, Some ident) -> meta_names_of_ident ident
  | MetaType (tyname, _, _) -> [unwrap_mcode tyname]
  | Decimal (_, _, e1, _, e2, _) ->
      let mn1 = meta_names_of_expression e1 in
      let mn2 = Common.default [] meta_names_of_expression e2 in
      mn1 @ mn2
  | _ty -> []

let meta_pos_constraint_names = function
    ExprTag(e) ->
      (match unwrap e with
	MetaExpr(_name,_constraints,ty,_form,_pure,_bitfield) ->
	  (match ty with
	    Some tylist ->
              List.fold_left (fun prev cur -> meta_names_of_typeC cur @ prev)
		[] tylist
	  | None -> [])
      |	_ -> [])
  | _ -> []

(* --------------------------------------------------------------------- *)

(* unique indices, for mcode and tree nodes *)
let index_counter = ref 0
let fresh_index _ = let cur = !index_counter in index_counter := cur + 1; cur

(* --------------------------------------------------------------------- *)
(* this function is a rather minimal attempt.  the problem is that information
has been lost.  but since it is only used for metavariable types in the isos,
perhaps it doesn't matter *)
let make_mcode x = (x,NONE,default_info(),context_befaft(),ref [],-1)
let make_mcode_info x info = (x,NONE,info,context_befaft(),ref [],-1)
and make_minus_mcode x =
  (x,NONE,default_info(),minus_befaft(),ref [],-1)

(* --------------------------------------------------------------------- *)

let lub_pure x y =
  match (x,y) with
    (Impure,_) | (_,Impure) -> Impure
  | (Pure,Context) | (Context,Pure) -> Impure
  | (Pure,_) | (_,Pure) -> Pure
  | (_,Context) | (Context,_) -> Context
  | _ -> PureContext

(* --------------------------------------------------------------------- *)

let rule_name = ref "" (* for the convenience of the parser *)

let string_of_binaryOp op = match (unwrap op) with
  | Arith arithOp -> Ast.string_of_arithOp (unwrap_mcode arithOp)
  | Logical logicalOp -> Ast.string_of_logicalOp (unwrap_mcode logicalOp)
  | MetaBinary _ -> "MetaBinary"

let string_of_assignOp op = match (unwrap op) with
  | SimpleAssign _ -> "="
  | OpAssign op' ->
    let op'' = rewrap op (Arith op') in
    let s = string_of_binaryOp op'' in
    s ^ "="
  | MetaAssign _ -> "MetaAssign"

let is_unknown_type ty =
  match unwrap ty with
    BaseType (Ast.Unknown, _) -> true
  | _ -> false
