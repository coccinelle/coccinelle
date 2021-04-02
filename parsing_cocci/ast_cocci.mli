(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* --------------------------------------------------------------------- *)
(* Modified code *)

type added_string = Noindent of string | Indent of string | Space of string

type info = { line : int; column : int;
	      strbef : (added_string * int (* line *) * int (* col *)) list;
	      straft : (added_string * int (* line *) * int (* col *)) list;
              whitespace : string }
type line = int
type meta_name = string * string
type script_position = string (* filename *) * line (* line *)
type 'a wrap =
    {node : 'a;
      node_line : line;
      free_vars : meta_name list; (*free vars*)
      minus_free_vars : meta_name list; (*minus free vars*)
      minus_nc_free_vars : meta_name list; (*minus free vars, excluding cstrs*)
      fresh_vars : (meta_name * seed) list; (*fresh vars*)
      inherited : meta_name list; (*inherited vars*)
      positive_inherited_positions : meta_name list;
      constraints : (meta_name * constraints) list;
      saved_witness : meta_name list; (*witness vars*)
      bef_aft : dots_bef_aft;
      pos_info : meta_name mcode option; (* pos info, try not to duplicate *)
      true_if_test_exp : bool;(* true if "test_exp from iso", only for exprs *)
      (* the following is only for declarations *)
      safe_for_multi_decls : safety;
      (* isos relevant to the term; ultimately only used for rule_elems *)
      iso_info : (string*anything) list }

and 'a befaft =
    BEFORE      of 'a list list * count
  | AFTER       of 'a list list * count
  | BEFOREAFTER of 'a list list * 'a list list * count
  | NOTHING

and 'a replacement = REPLACEMENT of 'a list list * count | NOREPLACEMENT

and 'a mcode = 'a * info * mcodekind * meta_pos list (* pos variables *)
 (* pos is an offset indicating where in the C code the mcodekind has an
 effect *)
and adjacency = ALLMINUS | ADJ of int
 and mcodekind =
    MINUS       of pos * int list * adjacency * anything replacement
  | CONTEXT     of pos * anything befaft
  | PLUS        of count
 and count = ONE (* + *) | MANY (* ++ *)
 and fixpos =
    Real of int (* charpos *) | Virt of int * int (* charpos + offset *)
 and pos = NoPos | DontCarePos | FixPos of (fixpos * fixpos)

and dots_bef_aft =
    NoDots
  | AddingBetweenDots of statement * int (*index of let var*)
  | DroppingBetweenDots of statement * int (*index of let var*)

and inherited = bool (* true if inherited *)
and keep_binding = Unitary (* need no info *)
  | Nonunitary (* need an env entry *) | Saved (* need a witness *)
and multi = bool (*true if a nest is one or more, false if it is zero or more*)

and end_info =
    meta_name list (*free vars*) * (meta_name * seed) list (*fresh*) *
      meta_name list (*inherited vars*) * mcodekind

and safety = Safe | Unsafe | NoStorage (* the result of safe_for_multi_decls *)

(* --------------------------------------------------------------------- *)
(* Metavariables *)

and arity = UNIQUE | OPT | MULTI | NONE

and metavar =
    MetaMetaDecl of arity * meta_name (* name *)
  | MetaIdDecl of arity * meta_name (* name *)
  | MetaFreshIdDecl of meta_name (* name *) * seed (* seed *)
  | MetaTypeDecl of arity * meta_name (* name *)
  | MetaInitDecl of arity * meta_name (* name *)
  | MetaInitListDecl of arity * meta_name (* name *) * list_len (*len*)
  | MetaListlenDecl of meta_name (* name *)
  | MetaParamDecl of arity * meta_name (* name *)
  | MetaParamListDecl of arity * meta_name (*name*) * list_len (*len*)
  | MetaBinaryOperatorDecl of arity * meta_name
  | MetaAssignmentOperatorDecl of arity * meta_name
  | MetaConstDecl of
      arity * meta_name (* name *) * fullType list option
  | MetaErrDecl of arity * meta_name (* name *)
  | MetaExpDecl of
      arity * meta_name (* name *) * fullType list option *
	list_len option (* bitfield *)
  | MetaIdExpDecl of
      arity * meta_name (* name *) * fullType list option
  | MetaLocalIdExpDecl of
      arity * meta_name (* name *) * fullType list option
  | MetaGlobalIdExpDecl of
      arity * meta_name (* name *) * fullType list option
  | MetaExpListDecl of arity * meta_name (*name*) * list_len (*len*)
  | MetaDeclDecl of arity * meta_name (* name *)
  | MetaFieldDecl of arity * meta_name (* name *)
  | MetaFieldListDecl of arity * meta_name (* name *) * list_len (*len*)
  | MetaStmDecl of arity * meta_name (* name *)
  | MetaStmListDecl of arity * meta_name (* name *) * list_len (*len*)
  | MetaDParamListDecl of arity * meta_name (* name *) * list_len (*len*)
  | MetaFuncDecl of arity * meta_name (* name *)
  | MetaLocalFuncDecl of arity * meta_name (* name *)
  | MetaPosDecl of arity * meta_name (* name *)
  | MetaComDecl of arity * meta_name (* name *)
  | MetaFmtDecl of arity * meta_name (* name *)
  | MetaAttributeDecl of arity * meta_name (* name *)
  | MetaFragListDecl of arity * meta_name (* name *) * list_len (*len*)
  | MetaAnalysisDecl of string * meta_name (* name *)
  | MetaDeclarerDecl of arity * meta_name (* name *)
  | MetaIteratorDecl of arity * meta_name (* name *)
  | MetaScriptDecl of metavar option ref * meta_name (* name *)

and list_len = AnyLen | MetaLen of meta_name * constraints | CstLen of int

and seed = NoVal | StringSeed of string | ListSeed of seed_elem list | ScriptSeed of seed_script
and seed_elem = SeedString of string | SeedId of meta_name
and seed_script = (* similar to script_constraint but kept separated to allow more flexibility *)
      string (* name of generated function *) *
	string (* language *) *
	(meta_name * metavar) list (* params *) *
	script_position *
	string (* code *)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

and 'a dots = 'a list wrap

(* --------------------------------------------------------------------- *)
(* Identifier *)

and base_ident =
    Id            of string mcode
  | MetaId        of
      meta_name mcode * constraints * keep_binding * inherited
  | MetaFunc      of
      meta_name mcode * constraints * keep_binding * inherited
  | MetaLocalFunc of
      meta_name mcode * constraints * keep_binding * inherited
  | AsIdent       of ident * ident (* as ident, always metavar *)

  | DisjId        of ident list
  | ConjId        of ident list
  | OptIdent      of ident

and ident = base_ident wrap

(* --------------------------------------------------------------------- *)
(* Expression *)

and base_expression =
    Ident          of ident
  | Constant       of constant mcode
  | StringConstant of string mcode (* quote *) * string_fragment dots *
		      string mcode (* quote *) * isWchar
  | FunCall        of expression * string mcode (* ( *) *
                      expression dots * string mcode (* ) *)
  | Assignment     of expression * assignOp * expression * bool
  | Sequence       of expression * string mcode (* , *) * expression
  | CondExpr       of expression * string mcode (* ? *) * expression option *
	              string mcode (* : *) * expression
  | Postfix        of expression * fixOp mcode
  | Infix          of expression * fixOp mcode
  | Unary          of expression * unaryOp mcode
  | Binary         of expression * binaryOp * expression
  | Nested         of expression * binaryOp * expression
  | ArrayAccess    of expression * string mcode (* [ *) * expression *
	              string mcode (* ] *)
  | RecordAccess   of expression * string mcode (* . *) * ident
  | RecordPtAccess of expression * string mcode (* -> *) * ident
  | Cast           of string mcode (* ( *) * fullType * attr list *
                      string mcode (* ) *) * expression

  | SizeOfExpr     of string mcode (* sizeof *) * expression
  | SizeOfType     of string mcode (* sizeof *) * string mcode (* ( *) *
                      fullType * string mcode (* ) *)
  | TypeExp        of fullType

  | Paren          of string mcode (* ( *) * expression *
                      string mcode (* ) *)

  | Constructor    of string mcode (* ( *) * fullType * string mcode (* ) *) *
	              initialiser
  | MetaErr        of meta_name mcode * constraints * keep_binding *
	              inherited
  | MetaExpr       of meta_name mcode * constraints * keep_binding *
	fullType list option * form * inherited *
	listlen option (* bitfield *)
  | MetaExprList   of meta_name mcode * listlen * constraints *
	              keep_binding * inherited (* only in arg lists *)
  | AsExpr         of expression * expression (* as expr, always metavar *)
  | AsSExpr        of expression * rule_elem (* as expr, always metavar *)

  | EComma         of string mcode (* only in arg lists *)

  | DisjExpr       of expression list
  | ConjExpr       of expression list
  | NestExpr       of string mcode (* <.../<+... *) *
	              expression dots *
	              string mcode (* ...>/...+> *) *
                      expression option * multi

  (* can appear in arg lists, and also inside Nest, as in:
   if(< ... X ... Y ...>)
   In the following, the expression option is the WHEN  *)
  | Edots          of string mcode (* ... *) * expression option

  | OptExp         of expression

and 'expression generic_constraints =
    CstrFalse
  | CstrTrue
  | CstrAnd of 'expression generic_constraints list
  | CstrOr of 'expression generic_constraints list
  | CstrNot of 'expression generic_constraints
  | CstrConstant of constant_constraint
  | CstrOperator of operator_constraint
  | CstrMeta_name of meta_name
  | CstrRegexp of string * Regexp.regexp
  | CstrScript of bool (*true if immediately evaluable*) * script_constraint
  | CstrExpr of 'expression
  | CstrSub of meta_name list
  | CstrType of fullType

and constant_constraint =
    CstrInt of int_constraint
  | CstrString of string

and int_constraint =
    CstrIntEq of string
  | CstrIntLeq of int
  | CstrIntGeq of int

and operator_constraint =
    CstrAssignOp of assignOp
  | CstrBinaryOp of binaryOp

and constraints = expression generic_constraints

and script_constraint =
      string (* name of generated function *) *
	string (* language *) *
	(meta_name * metavar) list (* params *) *
	script_position *
	string (* code *)

and form = ANY | ID | LocalID| GlobalID | CONST (* form for MetaExp *)

and expression = base_expression wrap

and listlen =
    MetaListLen of meta_name mcode * constraints * keep_binding * inherited
  | CstListLen of int
  | AnyListLen

and base_string_fragment =
    ConstantFragment of string mcode
  | FormatFragment of string mcode (*%*) * string_format (* format *)
  | Strdots of string mcode
  | MetaFormatList of string mcode (*%*) * meta_name mcode * listlen *
	constraints * keep_binding * inherited

and string_fragment = base_string_fragment wrap

and base_string_format =
    ConstantFormat of string mcode
  | MetaFormat of
      meta_name mcode * constraints * keep_binding * inherited

and string_format = base_string_format wrap

and  unaryOp = GetRef | GetRefLabel | DeRef | UnPlus |  UnMinus | Tilde | Not
and  base_assignOp =
    SimpleAssign of simpleAssignOp mcode
  | OpAssign of arithOp mcode
  | MetaAssign of
      meta_name mcode * constraints * keep_binding * inherited
and simpleAssignOp = string
and assignOp = base_assignOp wrap
and  fixOp = Dec | Inc

and  base_binaryOp =
    Arith of arithOp mcode
  | Logical of logicalOp mcode
  | MetaBinary of
      meta_name mcode * constraints * keep_binding * inherited
and binaryOp = base_binaryOp wrap
and  arithOp =
    Plus | Minus | Mul | Div | Mod | DecLeft | DecRight | And | Or | Xor | Min | Max
and  logicalOp = Inf | Sup | InfEq | SupEq | Eq | NotEq | AndLog | OrLog

and constant =
    String of string * isWchar
  | Char   of string * isWchar
  | Int    of string
  | Float  of string
  | DecimalConst of (string * string * string)
and isWchar = IsWchar | IsUchar | Isuchar | Isu8char | IsChar

(* --------------------------------------------------------------------- *)
(* Types *)

and base_fullType =
    Type            of bool (* true if all minus *) *
                       const_vol mcode option * typeC
  | AsType          of fullType * fullType (* as type, always metavar *)
  | DisjType        of fullType list
  | ConjType        of fullType list
  | OptType         of fullType

and base_typeC =
    BaseType        of baseType * string mcode list (* Yoann style *)
  | SignedT         of sign mcode * typeC option
  | Pointer         of fullType * string mcode (* * *)
  | ParenType       of string mcode (* ( *) * fullType * string mcode (* ) *)
  | FunctionType    of fullType *
                  string mcode (* ( *) * parameter_list * string mcode (* ) *)
  | Array           of fullType * string mcode (* [ *) *
	               expression option * string mcode (* ] *)
  | Decimal         of string mcode (* decimal *) * string mcode (* ( *) *
	               expression *
	               string mcode option (* , *) * expression option *
	               string mcode (* ) *) (* IBM C only *)
  | EnumName        of string mcode (*enum*) * ident option (* name *)
  | EnumDef  of fullType (* either EnumName or metavar *) *
	string mcode (* { *) * enum_decl dots * string mcode (* } *)
  | StructUnionName of structUnion mcode * ident option (* name *)
  | StructUnionDef  of fullType (* either StructUnionName or metavar *) *
	string mcode (* { *) * annotated_field dots * string mcode (* } *)
  | TypeOfExpr      of string mcode (* typeof *) * string mcode (* ( *) *
                       expression * string mcode (* ) *)
  | TypeOfType      of string mcode (* typeof *) * string mcode (* ( *) *
                       fullType * string mcode (* ) *)
  | TypeName        of string mcode
  | AutoType        of string mcode (* auto *) (* c++ >= 11 *)

  | MetaType        of meta_name mcode * constraints * keep_binding *
	inherited

and fullType = base_fullType wrap
and typeC = base_typeC wrap

and baseType = VoidType | CharType | ShortType | ShortIntType | IntType
| DoubleType | LongDoubleType | FloatType
| LongDoubleComplexType | DoubleComplexType | FloatComplexType
| LongType | LongIntType | LongLongType | LongLongIntType
| SizeType | SSizeType | PtrDiffType
| BoolType | Unknown

and structUnion = Struct | Union

and sign = Signed | Unsigned

and const_vol = Const | Volatile

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and base_declaration =
    Init of storage mcode option * fullType * ident * attr list *
	string mcode (*=*) * initialiser * string mcode (*;*)
  | UnInit of storage mcode option * fullType * ident * attr list *
	string mcode (* ; *)
  | FunProto of
	fninfo list * ident (* name *) *
	string mcode (* ( *) * parameter_list *
	(string mcode (* , *) * string mcode (* ...... *) ) option *
	string mcode (* ) *) * string mcode (* ; *)
  | TyDecl of fullType * attr list * string mcode (* ; *)
  | MacroDecl of storage mcode option *
	ident (* name *) * string mcode (* ( *) *
        expression dots * string mcode (* ) *) *
        attr list * string mcode (* ; *)
  | MacroDeclInit of storage mcode option *
	ident (* name *) * string mcode (* ( *) *
        expression dots * string mcode (* ) *) * string mcode (*=*) *
        initialiser * string mcode (* ; *)
  | Typedef of string mcode (*typedef*) * fullType * typeC * string mcode (*;*)
  | DisjDecl   of declaration list
  | ConjDecl of declaration list
  | MetaDecl of meta_name mcode * constraints * keep_binding * inherited
  | AsDecl        of declaration * declaration

  | OptDecl    of declaration

and declaration = base_declaration wrap

and base_annotated_decl =
    DElem of mcodekind (* before the decl *) * bool (* true if all minus *) *
      declaration

and annotated_decl = base_annotated_decl wrap

(* --------------------------------------------------------------------- *)
(* Field declaration *)

and base_field =
    Field of fullType * ident option * bitfield option * string mcode (* ; *)
  | MetaField of meta_name mcode * constraints * keep_binding * inherited
  | MetaFieldList of meta_name mcode * listlen * constraints * keep_binding *
	inherited

and bitfield = string mcode (* : *) * expression

and field = base_field wrap

and base_annotated_field =
    FElem of mcodekind (* before the decl *) * bool (* true if all minus *) *
      field
  | Fdots     of string mcode (* ... *) * field option (* whencode *)
  | DisjField of annotated_field list
  | ConjField of annotated_field list
  | OptField  of annotated_field

and annotated_field = base_annotated_field wrap

and base_enum_decl =
    Enum of ident * (string mcode (* = *) * expression) option
  | EnumComma of string mcode (* , *)
  | EnumDots of string mcode (* ... *) * enum_decl option (* whencode *)

and enum_decl = base_enum_decl wrap


(* --------------------------------------------------------------------- *)
(* Initializers *)

and base_initialiser =
    MetaInit of meta_name mcode * constraints * keep_binding * inherited
  | MetaInitList of meta_name mcode * listlen * constraints * keep_binding
	* inherited
  | AsInit of initialiser * initialiser (* as init, always metavar *)
  | InitExpr of expression
  | ArInitList of string mcode (*{*) * initialiser dots * string mcode (*}*)
  | StrInitList of bool (* true if all are - *) *
        string mcode (*{*) * initialiser list * string mcode (*}*) *
	initialiser list (* whencode: elements that shouldn't appear in init *)
  | InitGccExt of
      designator list (* name *) * string mcode (*=*) *
	initialiser (* gccext: *)
  | InitGccName of ident (* name *) * string mcode (*:*) *
	initialiser
  | IComma of string mcode (* , *)
  | Idots  of string mcode (* ... *) * initialiser option (* whencode *)
  | OptIni    of initialiser

and designator =
    DesignatorField of string mcode (* . *) * ident
  | DesignatorIndex of string mcode (* [ *) * expression * string mcode (* ] *)
  | DesignatorRange of
      string mcode (* [ *) * expression * string mcode (* ... *) *
      expression * string mcode (* ] *)

and initialiser = base_initialiser wrap

(* --------------------------------------------------------------------- *)
(* Parameter *)

and base_parameterTypeDef =
    VoidParam     of fullType * attr list
  | Param         of fullType * ident option * attr list

  | MetaParam     of meta_name mcode * constraints * keep_binding * inherited
  | MetaParamList of meta_name mcode * listlen * constraints * keep_binding *
	inherited

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
  | MetaDParamList of meta_name mcode * listlen * constraints * keep_binding *
	inherited
  | DPComma       of string mcode
  | DPdots        of string mcode (* ... *)
  | OptDParam     of define_param

and define_param = base_define_param wrap

and base_define_parameters =
    NoParams
  | DParams      of string mcode(*( *) * define_param dots * string mcode(* )*)

and define_parameters = base_define_parameters wrap

(* --------------------------------------------------------------------- *)
(* positions *)

(* PER = keep bindings separate, ANY = collect them *)
and meta_collect = PER | ALL

and meta_pos =
    MetaPos of meta_name mcode * constraints *
	meta_collect * keep_binding * inherited
  | MetaCom of meta_name mcode * constraints * keep_binding * inherited

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
                     (string mcode (* , *) * string mcode (* ...... *) ) option *
                     string mcode (* ) *)
  | Decl          of annotated_decl

  | SeqStart      of string mcode (* { *)
  | SeqEnd        of string mcode (* } *)

  | ExprStatement of expression option * string mcode (*;*)
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
                     forinfo *
	             expression option * string mcode (*;*) *
                     expression option * string mcode (* ) *)
  | IteratorHeader of ident (* name *) * string mcode (* ( *) *
	             expression dots * string mcode (* ) *)
  | SwitchHeader  of string mcode (* switch *) * string mcode (* ( *) *
	             expression * string mcode (* ) *)
  | Break         of string mcode (* break *) * string mcode (* ; *)
  | Continue      of string mcode (* continue *) * string mcode (* ; *)
  | Label         of ident * string mcode (* : *)
  | Goto          of string mcode (* goto *) * ident * string mcode (* ; *)
  | Return        of string mcode (* return *) * string mcode (* ; *)
  | ReturnExpr    of string mcode (* return *) * expression *
	             string mcode (* ; *)
  | Exec          of string mcode (* EXEC *) * string mcode (* language *) *
	             exec_code dots * string mcode (* ; *)

  | MetaRuleElem  of meta_name mcode * constraints * keep_binding * inherited
  | MetaStmt      of meta_name mcode * constraints * keep_binding *
	metaStmtInfo * inherited
  | MetaStmtList  of meta_name mcode * listlen * constraints * keep_binding *
	inherited

  | Exp           of expression
  | TopExp        of expression (* for macros body *)
  | Ty            of fullType (* only at top level *)
  | TopId         of ident (* only at top level *)
  | TopInit       of initialiser (* only at top level *)
  | Include       of string mcode (*#include*) * inc_file mcode (*file *)
  | MetaInclude   of string mcode (* #include *) * expression (* file *)
  | Undef         of string mcode (* #define *) * ident (* name *)
  | DefineHeader  of string mcode (* #define *) * ident (* name *) *
	             define_parameters (*params*)
  | Pragma        of string mcode (* #pragma *) * ident * pragmainfo
  | Case          of string mcode (* case *) * expression * string mcode (*:*)
  | Default       of string mcode (* default *) * string mcode (*:*)
  | AsRe          of rule_elem * rule_elem (* always { and MetaStmtList *)
  | DisjRuleElem  of rule_elem list

and base_pragmainfo =
    PragmaString of string mcode
  | PragmaDots of string mcode

and pragmainfo = base_pragmainfo wrap

and forinfo =
    ForExp of expression option * string mcode (*;*)
  | ForDecl of annotated_decl

and fninfo =
    FStorage of storage mcode
  | FType of fullType
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
  | MetaAttr of meta_name mcode * constraints * keep_binding * inherited

and attr_arg = base_attr_arg wrap

and metaStmtInfo =
    NotSequencible | SequencibleAfterDots of dots_whencode list | Sequencible

and rule_elem = base_rule_elem wrap

and base_statement =
    Seq           of rule_elem (* { *) *
	             statement dots * rule_elem (* } *)
  | IfThen        of rule_elem (* header *) * statement * end_info
  | IfThenElse    of rule_elem (* header *) * statement *
	             rule_elem (* else *) * statement * end_info
  | While         of rule_elem (* header *) * statement * end_info
  | Do            of rule_elem (* do *) * statement * rule_elem (* tail *)
  | For           of rule_elem (* header *) * statement * end_info
  | Iterator      of rule_elem (* header *) * statement * end_info (*enditer*)
  | Switch        of rule_elem (* header *) * rule_elem (* { *) *
	             statement (*decl*) dots * case_line list * rule_elem(*}*)
  | Atomic        of rule_elem
  | Disj          of statement dots list
  | Conj          of statement dots list
  | Nest          of string mcode (* <.../<+... *) * statement dots *
	             string mcode (* ...>/...+> *) *
	             (statement dots,statement) whencode list * multi *
	             dots_whencode list * dots_whencode list
  | FunDecl       of rule_elem (* header *) * rule_elem (* { *) *
     	             statement dots * rule_elem (* } *) * end_info (*exit*)
  | Define        of rule_elem (* header *) * statement dots
  | AsStmt        of statement * statement (* as statement, always metavar *)
  | Dots          of string mcode (* ... *) *
	             (statement dots,statement) whencode list *
	             dots_whencode list * dots_whencode list
  | OptStm        of statement

and ('a,'b) whencode =
    WhenNot of 'a
  | WhenAlways of 'b
  | WhenModifier of when_modifier
  | WhenNotTrue of rule_elem
  | WhenNotFalse of rule_elem

and when_modifier =
    WhenAny
  | WhenStrict
  | WhenForall
  | WhenExists

and dots_whencode =
    WParen of rule_elem * meta_name (*pren_var*)
  | Other of statement
  | Other_dots of statement dots

and statement = base_statement wrap

and base_case_line =
    CaseLine of rule_elem (* case/default header *) * statement dots
  | OptCase of case_line

and case_line = base_case_line wrap

and base_exec_code =
    ExecEval of string mcode (* : *) * expression
  | ExecToken of string mcode
  | ExecDots of string mcode (* ... *)

and exec_code = base_exec_code wrap

and inc_file =
    Local of inc_elem list
  | NonLocal of inc_elem list
  | AnyInc

and inc_elem =
    IncPath of string
  | IncDots

and base_top_level =
    NONDECL of statement (* cannot match all of a top-level declaration *)
  | CODE of statement dots
  | FILEINFO of string mcode (* old file *) * string mcode (* new file *)
  | ERRORWORDS of expression list

and top_level = base_top_level wrap

and parser_kind = ExpP | IdP | TyP | AnyP

and rulename =
    CocciRulename of string option * dependency * string list * string list *
	exists * parser_kind
  | GeneratedRulename of string option * dependency *
	string list * string list * exists * parser_kind
  | ScriptRulename of string option (* name *) * string (* language *) *
	dependency
  | InitialScriptRulename of string option (* name *) * string (* language *) *
	dependency
  | FinalScriptRulename of string option (* name *) * string (* language *) *
	dependency

and ruletype = Normal | Generated

and rule =
    CocciRule of string (* name *) *
	(dependency * string list (* dropped isos *) * exists) *
	top_level list * bool list (* true if generates an exp *) * ruletype
  | ScriptRule of string (* name *) *
      string * dependency *
	(script_meta_name * meta_name * metavar * mvinit) list *
	meta_name list (*script vars*) * script_position * string
  | InitialScriptRule of  string (* name *) * string * dependency *
	(script_meta_name * meta_name * metavar * mvinit)
	  list (*virtual vars*) * script_position *
	string
  | FinalScriptRule of  string (* name *) * string * dependency *
	(script_meta_name * meta_name * metavar * mvinit)
	  list (*virtual vars*) * script_position *
	string

and script_meta_name = string option (*string*) * string option (*ast*)

and mvinit =
    NoMVInit
  | MVInitString of string
  | MVInitPosList
and dep =
    Dep of string (* rule applies for the current binding *)
  | AntiDep of string (* rule doesn't apply for the current binding *)
  | EverDep of string (* rule applies for some binding *)
  | NeverDep of string (* rule never applies for any binding *)
  | AndDep of dep * dep
  | OrDep of dep * dep
  | FileIn of string
  | NotFileIn of string

and dependency =
    NoDep | FailDep | ExistsDep of dep | ForallDep of dep

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
  | SimpleAssignOpTag   of simpleAssignOp
  | OpAssignOpTag       of arithOp
  | FixOpTag            of fixOp
  | BinaryOpTag         of binaryOp
  | ArithOpTag          of arithOp
  | LogicalOpTag        of logicalOp
  | DeclarationTag      of declaration
  | FieldTag            of field
  | EnumDeclTag         of enum_decl
  | InitTag             of initialiser
  | StorageTag          of storage
  | IncFileTag          of inc_file
  | Rule_elemTag        of rule_elem
  | StatementTag        of statement
  | ForInfoTag          of forinfo
  | CaseLineTag         of case_line
  | StringFragmentTag   of string_fragment
  | AttributeTag        of attr
  | AttrArgTag          of attr_arg
  | ConstVolTag         of const_vol
  | Token               of string * info option
  | Directive           of added_string list
  | Code                of top_level
  | ExprDotsTag         of expression dots
  | ParamDotsTag        of parameterTypeDef dots
  | StmtDotsTag         of statement dots
  | AnnDeclDotsTag      of annotated_decl dots
  | AnnFieldDotsTag     of annotated_field dots
  | EnumDeclDotsTag     of enum_decl dots
  | DefParDotsTag       of define_param dots
  | TypeCTag            of typeC
  | ParamTag            of parameterTypeDef
  | SgrepStartTag       of string
  | SgrepEndTag         of string

(* --------------------------------------------------------------------- *)

and exists = Exists | Forall | Undetermined

(* --------------------------------------------------------------------- *)

val mkToken : string -> anything

val lub_count : count -> count -> count

(* --------------------------------------------------------------------- *)

val rewrap : 'a wrap -> 'b -> 'b wrap
val rewrap_mcode : 'a mcode -> 'b -> 'b mcode
val unwrap : 'a wrap -> 'a
val unwrap_mcode : 'a mcode -> 'a
val get_mcodekind : 'a mcode -> mcodekind
val get_line : 'a wrap -> line
val get_mcode_line : 'a mcode -> line
val get_mcode_col : 'a mcode -> int
val get_fvs : 'a wrap -> meta_name list
val get_wcfvs : ('a wrap,'b wrap) whencode list -> meta_name list
val set_fvs : meta_name list -> 'a wrap -> 'a wrap
val get_mfvs : 'a wrap -> meta_name list
val set_mfvs : meta_name list -> 'a wrap -> 'a wrap
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
val get_safe_decl : 'a wrap -> safety
val get_isos : 'a wrap -> (string*anything) list
val set_isos : 'a wrap -> (string*anything) list -> 'a wrap
val get_pos_var : 'a mcode -> meta_pos list
val set_pos_var : meta_pos list -> 'a mcode -> 'a mcode
val drop_pos : 'a mcode -> 'a mcode

val get_meta_name : metavar -> meta_name

val tag2c : anything -> string

val no_info : info

val make_meta_rule_elem :
    string -> mcodekind -> constraints ->
      (meta_name list * (meta_name * seed) list * meta_name list) ->
      rule_elem

val make_meta_decl :
    string -> mcodekind -> constraints ->
      (meta_name list * (meta_name * seed) list * meta_name list) ->
      declaration

val make_term : 'a -> 'a wrap
val make_inherited_term : 'a -> meta_name list (* inherited vars *) ->
  meta_name list (* definitely inherited positions *) -> 'a wrap
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
val string_of_typeC : typeC -> string
val string_of_fullType : fullType -> string

val typeC_of_fullType_opt : fullType -> typeC option
(**
 * [typeC_of_fullType_opt ty] returns [Some ty'] iff [ty] is of the form
 * [Type (_, false, ty')].
 *)

val ident_of_expression_opt : expression -> ident option
(**
 * [ident_of_expression_opt e] returns [Some id] iff [e] is of the form
 * [Ident e] *)

val string_of_meta_name : meta_name -> string

type 'a transformer = {
    baseType: (baseType -> string mcode list -> 'a) option;
    decimal: (string mcode -> string mcode -> expression ->
      string mcode option -> expression option -> string mcode -> 'a) option;
    enumName: (string mcode -> ident option -> 'a) option;
    structUnionName: (structUnion mcode -> ident option -> 'a) option;
    typeName: (string mcode -> 'a) option;
    metaType: (meta_name mcode -> constraints -> keep_binding ->
      inherited -> 'a) option
  }
(** ['a transformer] is an auxiliary record type for the iterators fullType_map,
 * fullType_fold and fullType_iter.
 * Each field corresponds to a constructor for a leaf in the AST of types.
 *)

val empty_transformer: 'a transformer
(** The transformer with no function defined. *)


val fullType_map: base_typeC transformer -> fullType -> fullType
(**
 * [fullType_map tr ty] returns [ty] where every subparts that match one of the
 * kinds, which a function has been given for in [tr], has been substituted by
 * the result of the corresponding function.
 * Other subparts, for the kind of which no function has been provided, are
 * preserved as is.
 *)

val fullType_fold: ('a -> 'a) transformer -> fullType -> 'a -> 'a
(**
  * [fullType_fold tr ty v] iterates the given functions to every subparts of
  * [ty] that match one of the kinds, which a function has been given for in
  * [tr], computing the iterated result value from the initial seed v.
  *)

val fullType_iter: unit transformer -> fullType -> unit
(**
  * [fullType_iter tr ty] applies the given functions to every subparts of [ty]
  * that match one of the kinds, which a function has been given for in [tr].
  *)

val meta_names_of_fullType: fullType -> meta_name list
(**
 * [meta_names_of_fullType ty] enumerates all the meta names that occur in [ty].
 *)

val string_of_expression: expression -> string option
(** [string_of_expression e] returns [Some id] if [e] is of the form
    [Ident (Id id)], [None] otherwise.
    Unquoted identifiers in constraints are represented as such expressions,
    and associated strings are extracted when needed. *)

type ('expression, 'a) cstr_transformer = {
    cstr_constant: (constant_constraint -> 'a) option;
    cstr_operator: (operator_constraint -> 'a) option;
    cstr_meta_name: (meta_name -> 'a) option;
    cstr_regexp: (string -> Regexp.regexp -> 'a) option;
    cstr_script: (bool*script_constraint -> 'a) option;
    cstr_expr: ('expression -> 'a) option;
    cstr_sub: (meta_name list -> 'a) option;
    cstr_type: (fullType -> 'a) option;
  }

val empty_cstr_transformer: ('expression, 'a) cstr_transformer

val cstr_fold_sign:
    ('expression, 'a -> 'a) cstr_transformer ->
      ('expression, 'a -> 'a) cstr_transformer ->
      'expression generic_constraints -> 'a -> 'a

val cstr_fold: ('expression, 'a -> 'a) cstr_transformer ->
  'expression generic_constraints -> 'a -> 'a

val cstr_iter: ('expression, unit) cstr_transformer ->
  'expression generic_constraints -> unit

val cstr_map:
    ('expression, 'expression' generic_constraints) cstr_transformer ->
      'expression generic_constraints ->
	'expression' generic_constraints
(* Untransformed expressions are discarded! *)

val cstr_push_not:
    'expression generic_constraints -> 'expression generic_constraints

val cstr_meta_names: 'expression generic_constraints -> meta_name list

val cstr_pos_meta_names: 'expression generic_constraints -> meta_name list

val filter_merge_variables:
    (script_meta_name * meta_name * metavar * mvinit) list ->
      (string * string) list

val prepare_merge_variables:
    ('a -> ('b * (script_meta_name * meta_name * metavar * mvinit) list) option)
    -> 'a list -> ('b * (int * string array)) list * string array
