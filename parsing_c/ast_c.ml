(* Yoann Padioleau
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2002, 2006, 2007, 2008, 2009 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* The AST C related types *)
(*****************************************************************************)
(*
 * Some stuff are tagged semantic: which means that they are computed
 * after parsing.
 *
 * This means that some elements in this AST are present only if
 * some annotation/transformation has been done on the original AST returned
 * by the parser. Cf type_annotater, comment_annotater, cpp_ast_c, etc.
 *)


(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

(* To allow some transformations over the AST, we keep as much information
 * as possible in the AST such as the tokens content and their locations.
 * Those info are called 'info' (how original) and can be tagged.
 * For instance one tag may say that the unparser should remove this token.
 *
 * Update: Now I use a ref! in those 'info' so take care.
 * That means that modifications of the info of tokens can have
 * an effect on the info stored in the ast (which is sometimes
 * convenient, cf unparse_c.ml or comment_annotater_c.ml)
 *
 * convention: I often use 'ii' for the name of a list of info.
 *
 * Sometimes we want to add something at the beginning or at the end
 * of a construct. For 'function' and 'decl' we want to add something
 * to their left and for 'if' 'while' et 'for' and so on at their right.
 * We want some kinds of "virtual placeholders" that represent the start or
 * end of a construct. We use fakeInfo for that purpose.
 * To identify those cases I have added a fakestart/fakeend comment.
 *
 * cocci: Each token will be decorated in the future by the mcodekind
 * of cocci. It is the job of the pretty printer to look at this
 * information and decide to print or not the token (and also the
 * pending '+' associated sometimes with the token).
 *
 * The first time that we parse the original C file, the mcodekind is
 * empty, or more precisely all is tagged as a CONTEXT with NOTHING
 * associated. This is what I call a "clean" expr/statement/....
 *
 * Each token will also be decorated in the future with an environment,
 * because the pending '+' may contain metavariables that refer to some
 * C code.
 *
 *)

(* for unparser: *)

type posl = int * int (* line-col, for MetaPosValList, for position variables *)
 (* with sexp *)

(* the virtual position is set in Parsing_hacks.insert_virtual_positions *)
type virtual_position = Common.parse_info * int (* character offset *)
 (* with sexp *)

type parse_info =
  (* Present both in ast and list of tokens *)
  | OriginTok of Common.parse_info
  (* Present only in ast and generated after parsing. Used mainly
   * by Julia, to add stuff at virtual places, beginning of func or decl *)
  | FakeTok of string * virtual_position
  (* Present both in ast and list of tokens.  *)
  | ExpandedTok of Common.parse_info * virtual_position

  (* Present neither in ast nor in list of tokens
   * but only in the '+' of the mcode of some tokens. Those kind of tokens
   * are used to be able to use '=' to compare big ast portions.
   *)
  | AbstractLineTok of Common.parse_info (* local to the abstracted thing *)
 (* with sexp *)

(* for tokens that are shared in the C ast, and thus require care
when transforming *)
type danger = DangerStart | DangerEnd | Danger | NoDanger

type info = {
  pinfo : parse_info;

  (* this cocci_tag can be changed, which is how we can express some program
   * transformations by tagging the tokens involved in this transformation.
   *)
  cocci_tag: (Ast_cocci.mcodekind * metavars_binding list) option ref;
  (* set in comment_annotater_c.ml *)
  comments_tag: comments_around ref;

  (* annotations on the token (mutable) *)
  mutable annots_tag: Token_annot.annots;

  danger: danger ref;

  (* todo? token_info : sometimes useful to know what token it was *)
  }
and il = info list

(* wrap2 is like wrap, except that I use it often for separator such
 * as ','. In that case the info is associated to the argument that
 * follows, so in 'a,b' I will have in the list [(a,[]); (b,[','])].
 *
 * wrap3 is like wrap, except that I use it in case sometimes it
 * will be empty because the info will be included in a nested
 * entity (e.g. for Ident in expr because it's inlined in the name)
 * so user should never assume List.length wrap3 > 0.
 *)
and 'a wrap  = 'a * il
and 'a wrap2 = 'a * il
and 'a wrap3 = 'a * il (* * evotype*)

(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)

(* was called 'ident' before, but 'name' is I think better
 * as concatenated strings can be used not only for identifiers and for
 * declarators, but also for fields, for labels, etc.
 *
 * Note: because now the info is embedded in the name, the info for
 * expression like Ident, or types like Typename, are not anymore
 * stored in the expression or type. Hence if you assume this,
 * which was true before, you are now wrong. So never write code like
 * let (unwrape,_), ii = e  and use 'ii' believing it contains
 * the local ii to e. If you want to do that, use the appropriate
 * wrapper get_local_ii_of_expr_inlining_ii_of_name.
 *)
and name =
   | RegularName of string wrap
   | CppConcatenatedName of (string wrap) wrap2 (* the ## separators *) list
   (* normally only used inside list of things, as in parameters or arguments
    * in which case, cf cpp-manual, it has a special meaning *)
   | CppVariadicName of string wrap (* ## s *)
   | CppIdentBuilder of string wrap (* s ( ) *) *
                       ((string wrap) wrap2 list) (* arguments *)


(* ------------------------------------------------------------------------- *)
(* C Type *)
(* ------------------------------------------------------------------------- *)
(* Could have more precise type in fullType, in expression, etc, but
 * it requires to do too much things in parsing such as checking no
 * conflicting structname, computing value, etc. Better to separate
 * concern. So I put '=>' to mean what we would really like. In fact
 * what we really like is defining another fullType, expression, etc
 * from scratch, because many stuff are just sugar.
 *
 * invariant: Array and FunctionType have also typeQualifier but they
 * don't have sense. I put this to factorise some code. If you look in
 * the grammar, you see that we can never specify const for the array
 * itself (but we can do it for pointer) or function, we always
 * have in the action rule of the grammar a { (nQ, FunctionType ...) }.
 *
 *
 * Because of ExprStatement, we can have more 'new scope' events, but
 * rare I think. For instance with 'array of constExpression' there can
 * have an exprStatement and a new (local) struct defined. Same for
 * Constructor.
 *
 *)


and fullType = typeQualifier * typeC
 and typeC = typeCbis wrap (* todo reput wrap3 *)

  and typeCbis =
    NoType (* for c++ only, and for K&R C *)
  | BaseType        of baseType

  | Pointer         of fullType
  | Array           of constExpression option * fullType
  | Decimal         of constExpression * constExpression option
  | FunctionType    of functionType

  | Enum            of string option * enumType
  | StructUnion     of structUnion * string option * structType (* new scope *)

  | EnumName        of string
  | StructUnionName of structUnion * string

  | TypeName   of name * fullType option (* semantic: filled later *)

  | FieldType of fullType * name * constExpression option

  | ParenType of fullType (* for unparser: *)

  (* gccext: TypeOfType below may seems useless; Why declare a
   *     __typeof__(int) x; ?
   * When used with macros, it allows to fix a problem of C which
   * is that type declaration can be spread around the ident. Indeed it
   * may be difficult to have a macro such as
   *    '#define macro(type, ident) type ident;'
   * because when you want to do a
   *     macro(char[256], x),
   * then it will generate invalid code, but with a
   *       '#define macro(type, ident) __typeof(type) ident;'
   * it will work.
   *)
  | TypeOfExpr of expression
  | TypeOfType of fullType
  | AutoType (* c++ >= 11 *)

  (* cppext: IfdefType TODO *)

(* -------------------------------------- *)
     and  baseType = Void
                   | IntType   of intType
		   | FloatType of floatType
		   | SizeType
		   | SSizeType
		   | PtrDiffType

	  (* stdC: type section
           * add  a | SizeT ?
           * note: char and signed char are semantically different!!
           *)
          and intType   = CChar (* obsolete? | CWchar  *)
	                | Si of signed

           and signed = sign * base
            and base = CChar2 | CShort | CInt | CLong | CLongLong (* gccext: *)
            and sign = Signed | UnSigned

          and floatType = CFloat | CDouble | CLongDouble |
	                  CFloatComplex | CDoubleComplex | CLongDoubleComplex


     (* -------------------------------------- *)
     and structUnion = Struct | Union
     and structType  = field list
         and field =
           | DeclarationField of field_declaration
           (* gccext: *)
           | EmptyField of info

            (* cppext: *)
           | MacroDeclField of (string * argument wrap2 list)
                               wrap (* optional ';'*)

            (* cppext: *)
           | CppDirectiveStruct of cpp_directive
           | IfdefStruct of ifdef_directive (* * field list list *)


        (* before unparser, I didn't have a FieldDeclList but just a Field. *)
         and field_declaration  =
           | FieldDeclList of fieldkind wrap2 list (* , *) wrap (*; fakestart*)

          (* At first I thought that a bitfield could be only Signed/Unsigned.
           * But it seems that gcc allow char i:4. C rule must say that you
           * can cast into int so enum too, ...
           *)
           and fieldkind =
             | Simple   of name option * fullType
             | BitField of name option * fullType *
                 info (* : *) * constExpression
              (* fullType => BitFieldInt | BitFieldUnsigned *)


     (* -------------------------------------- *)
     and enumType = oneEnumType wrap2 (* , *) list
                   (* => string * int list *)

     and oneEnumType = name * (info (* = *) * constExpression) option

     (* -------------------------------------- *)
     (* return * (params * has "...") *)
     and functionType = fullType * (parameterType wrap2 list * bool wrap)
        and parameterType =
        { p_namei: name option;
          p_register: bool wrap;
          p_type: fullType;
          p_attr: attribute list;
        }
        (* => (bool (register) * fullType) list * bool *)


and typeQualifier = typeQualifierbis wrap
and typeQualifierbis = {const: bool; volatile: bool}

(* gccext: cppext: *)
and attribute = attributebis wrap
  and attributebis =
    | Attribute of attr_arg

and attr_arg = attr_arg_bis wrap
  and attr_arg_bis =
    | AttrName of string

(* ------------------------------------------------------------------------- *)
(* C expression *)
(* ------------------------------------------------------------------------- *)
and expression = (expressionbis * exp_info ref (* semantic: *)) wrap3
 and exp_info = exp_type option * test
  and exp_type = fullType (* Type_c.completed_and_simplified *) * local
    and local = LocalVar of parse_info | StaticLocalVar of parse_info
              | NotLocalVar (* cocci: *)
  and test = Test | NotTest (* cocci: *)

 and expressionbis =

  (* Ident can be a enumeration constant, a simple variable, a name of a func.
   * With cppext, Ident can also be the name of a macro. Sparse says
   * "an identifier with a meaning is a symbol" *)
  | Ident          of name (* todo? more semantic info such as LocalFunc *)

  | Constant       of constant
  | StringConstant of string_fragment list * string (*src string*) * isWchar
  | FunCall        of expression * argument wrap2 (* , *) list
  (* gccext: x ? /* empty */ : y <=> x ? x : y;  hence the 'option' below *)
  | CondExpr       of expression * expression option * expression

  (* should be considered as statements, bad C langage *)
  | Sequence       of expression * expression
  | Assignment     of expression * assignOp * expression


  | Postfix        of expression * fixOp
  | Infix          of expression * fixOp

  | Unary          of expression * unaryOp
  | Binary         of expression * binaryOp * expression

  | ArrayAccess    of expression * expression

  (* field ident access *)
  | RecordAccess   of expression * name
  | RecordPtAccess of expression * name
  (* redundant normally, could replace it by DeRef RecordAcces *)

  | SizeOfExpr     of expression
  | SizeOfType     of fullType
  | Cast           of fullType * attribute list * expression

  (* gccext: *)
  | StatementExpr of compound wrap (* ( )     new scope *)
  | Constructor  of fullType * initialiser

  (* for unparser: *)
  | ParenExpr of expression

  (* for C++: *)
  | New of (argument wrap2 (* , *) list) option * argument
  | Delete of bool (* true if [] *) * expression

  (* CPP [defined] operator, e.g. #if defined(A) *)
  | Defined of name

  (* cppext: IfdefExpr TODO *)

  (* cppext: normally just expression *)
  and argument = (expression, weird_argument) Common.either
   and weird_argument =
       | ArgType of parameterType
       | ArgAction of action_macro
      and action_macro =
        (* todo: ArgStatement of statement, possibly have ghost token *)
         | ActMisc of il


  (* I put string for Int and Float because int would not be enough because
   * OCaml int are 31 bits. So simpler to do string. Same reason to have
   * string instead of int list for the String case.
   *
   * note: -2 is not a constant, it is the unary operator '-'
   * applied to constant 2. So the string must represent a positive
   * integer only. *)

  and constant =
    | String of (string * isWchar)
    | MultiString of string list (* can contain MacroString, todo: more info *)
    | Char   of (string * isWchar) (* normally it is equivalent to Int *)
    | Int    of (string * intType)
    | Float  of (string * floatType)
    | DecimalConst of (string * string * string)

    and isWchar = IsWchar | IsUchar | Isuchar | Isu8char | IsChar


  and unaryOp  = GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not
                 | GetRefLabel (* gccext: GetRefLabel, via &&label notation *)
  and assignOpbis = SimpleAssign | OpAssign of arithOp
  and assignOp = assignOpbis wrap
  and fixOp    = Dec | Inc

  and binaryOpbis = Arith of arithOp | Logical of logicalOp
  and binaryOp = binaryOpbis wrap

       and arithOp   =
         | Plus | Minus | Mul | Div | Mod
         | DecLeft | DecRight
         | And | Or | Xor | Max | Min

       and logicalOp =
         | Inf | Sup | InfEq | SupEq
         | Eq | NotEq
         | AndLog | OrLog

 and constExpression = expression (* => int *)

and string_fragment = string_fragment_bis wrap

and string_fragment_bis =
  ConstantFragment of string
| FormatFragment of string_format (* format *)

and string_format = string_format_bis wrap

and string_format_bis =
  ConstantFormat of string

(* ------------------------------------------------------------------------- *)
(* C statement *)
(* ------------------------------------------------------------------------- *)
(* note: that assignment is not a statement but an expression;
 * wonderful C langage.
 *
 * note: I use 'and' for type definition cos gccext allow statement as
 * expression, so need mutual recursive type definition.
 *
 *)

and statement = statementbis wrap3
 and statementbis =
  | Labeled       of labeled
  | Compound      of compound   (* new scope *)
  | ExprStatement of exprStatement
  | Selection     of selection (* have fakeend *)
  | Iteration     of iteration (* have fakeend *)
  | Jump          of jump

  (* simplify cocci: only at the beginning of a compound normally *)
  | Decl  of declaration

  (* gccext: *)
  | Asm of asmbody
  | NestedFunc of definition

  (* cppext: *)
  | MacroStmt
  | Exec of exec_code list
  | IfdefStmt1 of ifdef_directive list * statement list

  and labeled = Label   of name * statement
              | Case    of expression * statement
              | CaseRange of expression * expression * statement (* gccext: *)
              |	Default of statement

  (* cppext:
   * old: compound = (declaration list * statement list)
   * old: (declaration, statement) either list
   * Simplify cocci to just have statement list, by integrating Decl in stmt.
   *
   * update: now introduce also the _sequencable to allow ifdef in the middle.
   * Indeed, I now allow ifdefs in the ast but they must be only between
   * "sequencable" elements. They can be put in a type only if this type
   * is used in a list, like at the toplevel, used in a 'toplevel list',
   * or inside a compound, used in a 'statement list'. I must not allow
   * ifdef anywhere. For instance I can not make ifdef a statement
   * cos some instruction like If accept only one statement and the
   * ifdef directive must not take the place of a legitimate instruction.
   * We had a similar phenomena in SmPL where we have the notion
   * of statement and sequencable statement too. Once you have
   * such a type of sequencable thing, then s/xx list/xx_sequencable list/
   * and introduce the ifdef.
   *
   * update: those ifdefs are either passed, or present in the AST but in
   * a flat form. To structure those flat ifdefs you have to run
   * a transformation that will put in a tree the statements inside
   * ifdefs branches. Cf cpp_ast_c.ml. This is for instance the difference
   * between a IfdefStmt (flat) and IfdefStmt2 (tree structured).
   *
   *)
  and compound = statement_sequencable list

  (* cppext: easier to put at statement_list level than statement level *)
  and statement_sequencable =
    | StmtElem of statement

    (* cppext: *)
    | CppDirectiveStmt of cpp_directive
    | IfdefStmt of ifdef_directive

    (* this will be build in cpp_ast_c from the previous flat IfdefStmt *)
    | IfdefStmt2 of ifdef_directive list * (statement_sequencable list) list

  and exprStatement = expression option

  and declOrExpr = ForDecl of declaration | ForExp of expression option wrap

 (* for Switch, need check that all elements in the compound start
  * with a case:, otherwise unreachable code.
  *)
  and selection     =
   | If     of expression * statement * statement
   | Switch of expression * statement
   (* #ifdef A if e S1 else #endif S2 *)
   | Ifdef_Ite of expression * statement * statement
   (* #ifdef A if e S1 else #else S2 #endif S3 *)
   | Ifdef_Ite2 of expression * statement * statement * statement


  and iteration     =
    | While   of expression * statement
    | DoWhile of statement * expression
    | For     of declOrExpr * exprStatement wrap * exprStatement wrap *
                 statement
    (* cppext: *)
    | MacroIteration of string * argument wrap2 list * statement

  and jump  = Goto of name
            | Continue | Break
            | Return   | ReturnExpr of expression
            | GotoComputed of expression (* gccext: goto *exp ';' *)


  (* gccext: *)
  and asmbody = il (* string list *) * colon wrap (* : *) list
      and colon = Colon of colon_option wrap2 list
      and colon_option = colon_option_bis wrap
          and colon_option_bis = ColonMisc | ColonExpr of expression

  and exec_code_bis =
    ExecEval of expression
  | ExecToken

  and exec_code = exec_code_bis wrap

(* ------------------------------------------------------------------------- *)
(* Declaration *)
(* ------------------------------------------------------------------------- *)
(* (string * ...) option cos can have empty declaration or struct tag
 * declaration.
 *
 * Before I had a Typedef constructor, but why make this special case and not
 * have StructDef, EnumDef, ... so that 'struct t {...} v' will generate 2
 * declarations ? So I try to generalise and not have Typedef either. This
 * requires more work in parsing. Better to separate concern.
 *
 * Before the need for unparser, I didn't have a DeclList but just a Decl.
 *
 * I am not sure what it means to declare a prototype inline, but gcc
 * accepts it.
 *)

and declaration =
  | DeclList of onedecl wrap2 (* , *) list wrap (* ; fakestart sto *)
  (* cppext: *)
    (* bool is true if there is a ; at the end *)
  | MacroDecl of
      (storagebis * string * argument wrap2 list * attribute list * bool)
        wrap (* fakestart *)
  | MacroDeclInit of
      (storagebis * string * argument wrap2 list * initialiser)
	wrap (* fakestart *)

     and onedecl =
       { v_namei: (name * v_init) option;
         v_type: fullType;
         (* semantic: set in type annotated and used in cocci_vs_c
          * when we transform some initialisation into affectation
          *)
         v_type_bis: fullType (* Type_c.completed_and_simplified *) option ref;
         v_storage: storage;
         v_local: local_decl; (* cocci: *)
         v_attr: attribute list; (* gccext: *)
         v_endattr: attribute list; (* gccext: *)
       }
     and v_init =
       NoInit | ValInit of info * initialiser
     | ConstrInit of argument wrap2 (* , *) list wrap
     and storage       = storagebis * bool (* gccext: inline or not *)
     and storagebis    = NoSto | StoTypedef | Sto of storageClass
     and storageClass  = Auto  | Static | Register | Extern

     and local_decl = LocalDecl | NotLocalDecl

     (* fullType is the type used if the type should be converted to
	an assignment.  It can be adjusted in the type annotation
	phase when typedef information is available *)
     and initialiser = initialiserbis wrap
       and initialiserbis =
          | InitExpr of expression
          | InitList of initialiser wrap2 (* , *) list
          (* gccext: *)
          | InitDesignators of designator list * initialiser
          | InitFieldOld  of string * initialiser
          | InitIndexOld  of expression * initialiser

       (* ex: [2].y = x,  or .y[2]  or .y.x. They can be nested *)
       and designator = designatorbis wrap
        and designatorbis =
            | DesignatorField of string
            | DesignatorIndex of expression
            | DesignatorRange of expression * expression

(* ------------------------------------------------------------------------- *)
(* Function definition *)
(* ------------------------------------------------------------------------- *)
(* Normally we should define another type functionType2 because there
 * are more restrictions on what can define a function than a pointer
 * function. For instance a function declaration can omit the name of the
 * parameter whereas a function definition can not. But, in some cases such
 * as 'f(void) {', there is no name too, so I simplified and reused the
 * same functionType type for both declaration and function definition.
 *
 * Also old style C does not have type in the parameter, so again simpler
 * to abuse the functionType and allow missing type.
 *)
and definition = definitionbis wrap (* ( ) { } fakestart sto *)
  and definitionbis =
  { f_name: name;
    f_type: functionType; (* less? a functionType2 ? *)
    f_storage: storage;
    f_body: compound;
    f_attr: attribute list; (* gccext: *)
    f_old_c_style: declaration list option;
  }
  (* cppext: IfdefFunHeader TODO *)

(* ------------------------------------------------------------------------- *)
(* cppext: cpp directives, #ifdef, #define and #include body *)
(* ------------------------------------------------------------------------- *)
and cpp_directive =
  | Define of define
  | Include of includ
  | Pragma of (name * string wrap list) wrap
  | OtherDirective of il
(*| Ifdef ? no, ifdefs are handled differently, cf ifdef_directive below *)

and define = string wrap (* #define s eol *) * (define_kind * define_val)
   and define_kind =
   | DefineVar
   | DefineFunc   of ((string wrap) wrap2 list) wrap (* () *)
   | Undef
   and define_val =
     (* most common case; e.g. to define int constant *)
     | DefineExpr of expression

     | DefineStmt of statement
     | DefineType of fullType
     | DefineDoWhileZero of (statement * expression) wrap (* do { } while(0) *)

     | DefineFunction of definition
     | DefineInit of initialiser (* in practice only { } with possible ',' *)

     | DefineMulti of statement list

     | DefineText of string wrap
     | DefineEmpty

     | DefineTodo

and includ =
  { i_include: inc_file wrap; (* #include s *)
    (* cocci: computed in cocci.ml, in update_include_rel_pos *)
    i_rel_pos: include_rel_pos option ref; (* rel to other "" or for <> *)
    i_overall_rel_pos: include_rel_pos option ref; (* rel to all *)
    (* cocci: cf -test incl *)
    i_is_in_ifdef: bool;
    (* cf cpp_ast_c.ml. set to None at parsing time. *)
    i_content: (Common.filename (* full path *) * program) option;
  }
and inc_file =
  | Local    of inc_elem list
  | NonLocal of inc_elem list
  | Weird of string (* ex: #include SYSTEM_H *)
and inc_elem = string

 (* cocci: to tag the first of #include <xx/> and last of #include <yy/>
  *
  * The first_of and last_of store the list of prefixes that was
  * introduced by the include. On #include <a/b/x>, if the include was
  * the first in the file, it would give in first_of the following
  * prefixes a/b/c; a/b/; a/ ; <empty>
  *
  * This is set after parsing, in cocci.ml, in update_rel_pos.
  *)
and include_rel_pos = {
   first_of : string list list;
   last_of :  string list list;
 }

(* todo? to specialize if someone need more info *)
and ifdef_directive = (* or and 'a ifdefed = 'a list wrap *)
  | IfdefDirective of (ifdefkind * matching_tag) wrap
  and ifdefkind =
    | Ifdef of ifdef_guard
    | IfdefElseif of ifdef_guard
    | IfdefElse
    | IfdefEndif
    (** Guards for #if conditionals
     *
     * Having #if guards in the AST is useful for cpp-aware analyses, or to
     * later add support for matching against #ifS.
     *
     * General #if guards are stored as a string in a [Gif_str] constructor.
     * A traversal of the syntax tree with a parsing function would transform
     * these into the parsed [Gif] form.
     *
     * NOTE that there is no actually guaranteee that a traversal will
     * eliminate all [Gif_str] constructors, since the parsing function may
     * fail.
     *
     * See [Parsing #if guards] to know why this design choice.
     *
     * @author Iago Abal
     *)
  and ifdef_guard =
      Gifdef  of macro_symbol (* #ifdef *)
    | Gifndef of macro_symbol (* #ifndef *)
    | Gif_str of Lexing.position * string (* #if <string to be parsed> *)
    | Gif     of expression   (* #if *)
    | Gnone   (* ignored #if condition: TIfdefBool,
                 * TIfdefMisc, and TIfdefVersion
                             *)
  and macro_symbol = string
  (* set in Parsing_hacks.set_ifdef_parenthize_info. It internally use
   * a global so it means if you parse the same file twice you may get
   * different id. I try now to avoid this pb by resetting it each
   * time I parse a file.
   *)
  and matching_tag =
    IfdefTag of (int (* tag *) * int (* total with this tag *))


(* Note [Parsing #if guards]
 *
 * What I wanted to do:
 * The lexer should tokenize the #if guard, we use the TDefEOL-trick to
 * mark the end of the guard, and finally we add a rule to the ocamlyacc
 * parser.
 *
 * Problem:
 * The [lookahead] pass in [Parsing_hacks] assumes that an #if header is
 * a single token. The above solution breaks that assumption. So, when an
 * #if appears in some weird position, [lookahead] will comment out the #if
 * token, but not any subsequent token of the guard. It seems doable to
 * modify [lookahead] to handle this new situation, but this seems a complex
 * and delicate function to touch, and (who knows) we may break something
 * else. Also note that [TCommentCpp] only takes one [info], so we would
 * need to combine all the [info]s of an #if into one.
 *
 * What I finally did:
 * The safest way I found is to save the #if guard in the [TIfdef] token as
 * a (yet-to-be-parsed) string. This is enough to reason about, or match
 * against, simple and most-common #ifdef and #ifndef conditionals. (Rough
 * estimate: 80% of #if conditionals in Linux are #if[n]def.) But the
 * AST for #if guards can still be easily obtained by traversing the syntax
 * tree with a parsing function.
 *
 * @author Iago Abal
 *)


(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
(* ------------------------------------------------------------------------- *)
and toplevel =
  | Declaration of declaration
  | Definition of definition

  (* cppext: *)
  | CppTop of cpp_directive
  | IfdefTop of ifdef_directive (* * toplevel list *)

  (* cppext: *)
  | MacroTop of string * argument wrap2 list * il

  | EmptyDef of il      (* gccext: allow redundant ';' *)
  | NotParsedCorrectly of il

  | FinalDef of info (* EOF *)

  (* c++ *)
  | Namespace of toplevel list * il

(* ------------------------------------------------------------------------- *)
and program = toplevel list

(*****************************************************************************)
(* Cocci Bindings *)
(*****************************************************************************)
(* Was previously in pattern.ml, but because of the transformer,
 * we need to decorate each token with some cocci code AND the environment
 * for this cocci code.
 *)
and metavars_binding = (Ast_cocci.meta_name, metavar_binding_kind) assoc
  and metavar_binding_kind =
  | MetaIdVal        of string
  | MetaFuncVal      of string
  | MetaLocalFuncVal of string

  | MetaExprVal      of expression (* a "clean expr" *) *
	                (*subterm constraints, currently exprs*)
	                Ast_cocci.meta_name list * stripped
  | MetaExprListVal  of argument wrap2 list
  | MetaParamVal     of parameterType
  | MetaParamListVal of parameterType wrap2 list

  | MetaTypeVal      of fullType
  | MetaInitVal      of initialiser
  | MetaInitListVal  of initialiser wrap2 list
  | MetaDeclVal      of declaration * declaration
  | MetaFieldVal     of field
  | MetaFieldListVal of field list
  | MetaStmtVal      of statement * statement * stripped
  | MetaStmtListVal  of statement_sequencable list * stripped
  | MetaDParamListVal of (string wrap) wrap2 list
  | MetaFmtVal       of string_format
  | MetaAttrArgVal   of attr_arg
  | MetaFragListVal  of string_fragment list
  | MetaAssignOpVal  of assignOp
  | MetaBinaryOpVal  of binaryOp
  (* Could also be in Lib_engine.metavars_binding2 with the ParenVal,
   * because don't need to have the value for a position in the env of
   * a '+'. But ParenVal or LabelVal are used only by CTL, they are not
   * variables accessible via SmPL whereas the position can be one day
   * so I think it's better to put MetaPosVal here *)
  | MetaPosVal       of (Ast_cocci.fixpos * Ast_cocci.fixpos) (* max, min *)
  | MetaPosValList   of (* current elem, min, max *)
      (Common.filename * string (*element*) * (posl * posl) option *
	 posl * posl) list
  | MetaComValList   of (Token_c.comment_like_token list *
			   Token_c.comment_like_token list *
			   Token_c.comment_like_token list) list
  | MetaListlenVal   of int
  | MetaNoVal

(* WITHOUT_TYPES is only for code generated by a script *)
and stripped = WITH_TYPES | WITHOUT_TYPES

(*****************************************************************************)
(* C comments *)
(*****************************************************************************)

(* convention: I often use "m" for comments as I can not use "c"
 * (already use for c stuff) and "com" is too long.
 *)

(* this type will be associated to each token.
 *)
and comments_around = {
  mbefore: Token_c.comment_like_token list;
  mafter:  Token_c.comment_like_token list;

  (* less: could remove ? do something simpler than CComment for
   * coccinelle, cf above. *)
  mbefore2: comment_and_relative_pos list;
  mafter2:  comment_and_relative_pos list;
  }
  and comment_and_relative_pos = {

   minfo: Common.parse_info;
   (* the int represent the number of lines of difference between the
    * current token and the comment. When on same line, this number is 0.
    * When previous line, -1. In some way the after/before in previous
    * record is useless because the sign of the integer can helps
    * do the difference too, but I keep it that way.
    *)
   mpos: int;
   (* todo?
    *  cppbetween: bool; touse? if false positive
    *  is_alone_in_line: bool; (*for labels, to avoid false positive*)
   *)
  }

and comment = Common.parse_info
and com = comment list ref

 (* with sexp *)


(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
let nullQualif = ({const=false; volatile= false}, [])
let nQ = nullQualif

let defaultInt = (BaseType (IntType (Si (Signed, CInt))))

let noType () = ref (None,NotTest)
let noInstr = (ExprStatement (None), [])
let noTypedefDef () = None

let emptyMetavarsBinding =
  ([]: metavars_binding)

let emptyAnnotCocci =
  (Ast_cocci.CONTEXT (Ast_cocci.NoPos,Ast_cocci.NOTHING),
  ([] : metavars_binding list))

let emptyAnnot =
  (None: (Ast_cocci.mcodekind * metavars_binding list) option)

(* compatibility mode *)
let mcode_and_env_of_cocciref aref =
  match !aref with
  | Some x -> x
  | None -> emptyAnnotCocci


let emptyComments= {
  mbefore = [];
  mafter = [];
  mbefore2 = [];
  mafter2 = [];
}


(* for include, some meta information needed by cocci *)
let noRelPos () =
  ref (None: include_rel_pos option)
let noInIfdef () =
  ref false


(* When want add some info in ast that does not correspond to
 * an existing C element.
 * old: or when don't want 'synchronize' on it in unparse_c.ml
 * (now have other mark for the matter).
 *)
let no_virt_pos = ({str="";charpos=0;line=0;column=0;file=""},-1)

let fakeInfo pi  =
  { pinfo = FakeTok ("",no_virt_pos);
    cocci_tag = ref emptyAnnot;
    annots_tag = Token_annot.empty;
    comments_tag = ref emptyComments;
    danger = ref NoDanger;
  }

let noii = []
let noattr = []
let noi_content = (None: ((Common.filename * program) option))

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap = fst

let unwrap2 = fst

let unwrap_expr ((unwrap_e, typ), iie) = unwrap_e
let rewrap_expr ((_old_unwrap_e, typ), iie)  newe = ((newe, typ), iie)

let unwrap_typeC (qu, (typeC, ii)) = typeC
let rewrap_typeC (qu, (typeC, ii)) newtypeC  = (qu, (newtypeC, ii))

let unwrap_typeCbis (typeC, ii) = typeC

let unwrap_st (unwrap_st, ii) = unwrap_st

(* ------------------------------------------------------------------------- *)
let mk_e unwrap_e ii = (unwrap_e, noType()), ii
let mk_e_bis unwrap_e ty ii = (unwrap_e, ty), ii

let mk_ty typeC ii = nQ, (typeC, ii)
let mk_tybis typeC ii = (typeC, ii)

let mk_st unwrap_st ii = (unwrap_st, ii)

(* ------------------------------------------------------------------------- *)
let get_ii_typeC_take_care (typeC, ii) = ii
let get_ii_st_take_care (st, ii) = ii
let get_ii_expr_take_care (e, ii) = ii

let get_st_and_ii (st, ii) = st, ii
let get_ty_and_ii (qu, (typeC, ii)) = qu, (typeC, ii)
let get_e_and_ii  (e, ii) = e, ii


(* ------------------------------------------------------------------------- *)
let get_type_expr ((unwrap_e, typ), iie) = !typ
let set_type_expr ((unwrap_e, oldtyp), iie) newtyp =
  oldtyp := newtyp
  (* old: (unwrap_e, newtyp), iie *)

let get_onlytype_expr ((unwrap_e, typ), iie) =
  match !typ with
  | Some (ft,_local), _test -> Some ft
  | None, _ -> None

let get_onlylocal_expr ((unwrap_e, typ), iie) =
  match !typ with
  | Some (ft,local), _test -> Some local
  | None, _ -> None

(* ------------------------------------------------------------------------- *)
let rewrap_str s ii =
  {ii with pinfo =
    (match ii.pinfo with
      OriginTok pi -> OriginTok { pi with Common.str = s;}
    | ExpandedTok (pi,vpi) -> ExpandedTok ({ pi with Common.str = s;},vpi)
    | FakeTok (_,vpi) -> FakeTok (s,vpi)
    | AbstractLineTok pi -> OriginTok { pi with Common.str = s;})}

let rewrap_charpos charpos ii =
  {ii with pinfo =
    (match ii.pinfo with
      OriginTok pi -> OriginTok { pi with Common.charpos = charpos;}
    | ExpandedTok (pi,vpi) ->
	ExpandedTok ({ pi with Common.charpos = charpos;},vpi)
    | FakeTok (s,vpi) -> FakeTok (s,vpi)
    | AbstractLineTok pi -> OriginTok { pi with Common.charpos = charpos;})}

let rewrap_col col ii =
  {ii with pinfo =
    (match ii.pinfo with
      OriginTok pi -> OriginTok { pi with Common.column = col;}
    | ExpandedTok (pi,vpi) -> ExpandedTok ({ pi with Common.column = col;},vpi)
    | FakeTok (s,vpi) -> FakeTok (s,vpi)
    | AbstractLineTok pi -> OriginTok { pi with Common.column = col;})}

let rewrap_pinfo pi ii =
  {ii with pinfo = pi}



(* info about the current location *)
let get_pi = function
    OriginTok pi -> pi
  | ExpandedTok (_,(pi,_)) -> pi
  | FakeTok (_,(pi,_)) -> pi
  | AbstractLineTok pi -> pi

(* original info *)
let get_opi = function
    OriginTok pi -> pi
  | ExpandedTok (pi,_) -> pi (* diff with get_pi *)
  | FakeTok (_,_) -> failwith "no position information"
  | AbstractLineTok pi -> pi

let str_of_info ii =
  match ii.pinfo with
    OriginTok pi -> pi.Common.str
  | ExpandedTok (pi,_) -> pi.Common.str
  | FakeTok (s,_) -> s
  | AbstractLineTok pi -> pi.Common.str

let get_info f ii =
  match ii.pinfo with
    OriginTok pi -> f pi
  | ExpandedTok (_,(pi,_)) -> f pi
  | FakeTok (_,(pi,_)) -> f pi
  | AbstractLineTok pi -> f pi

let get_orig_info f ii =
  match ii.pinfo with
    OriginTok pi -> f pi
  | ExpandedTok (pi,_) -> f pi (* diff with get_info *)
  | FakeTok (_,(pi,_)) -> f pi
  | AbstractLineTok pi -> f pi

let make_expanded ii =
  {ii with pinfo = ExpandedTok (get_opi ii.pinfo,no_virt_pos)}

let pos_of_info   ii = get_info      (function x -> x.Common.charpos) ii
let opos_of_info  ii = get_orig_info (function x -> x.Common.charpos) ii
let line_of_info  ii = get_orig_info (function x -> x.Common.line)    ii
let col_of_info   ii = get_orig_info (function x -> x.Common.column)  ii
let file_of_info  ii = get_orig_info (function x -> x.Common.file)    ii
let mcode_of_info ii = fst (mcode_and_env_of_cocciref ii.cocci_tag)
let pinfo_of_info ii = ii.pinfo
let parse_info_of_info ii = get_pi ii.pinfo

let strloc_of_info ii =
  spf "%s:%d" (file_of_info ii) (line_of_info ii)

let is_fake ii =
  match ii.pinfo with
    FakeTok (_,_) -> true
  | _ -> false

let is_origintok ii =
  match ii.pinfo with
  | OriginTok pi -> true
  | _ -> false

(* ------------------------------------------------------------------------- *)
type posrv = Real of Common.parse_info | Virt of virtual_position

let compare_pos ii1 ii2 =
  let get_pos = function
      OriginTok pi -> Real pi
    | FakeTok (s,vpi) -> Virt vpi
    | ExpandedTok (pi,vpi) -> Virt vpi
    | AbstractLineTok pi -> Real pi in (* used for printing *)
  let pos1 = get_pos (pinfo_of_info ii1) in
  let pos2 = get_pos (pinfo_of_info ii2) in
  match (pos1,pos2) with
    (Real p1, Real p2) ->
      compare p1.Common.charpos p2.Common.charpos
  | (Virt (p1,offset), Real p2) ->
      if p1.Common.charpos = p2.Common.charpos
      then if offset < 0 then -1 else 1
      else compare p1.Common.charpos p2.Common.charpos
  | (Real p1, Virt (p2,offset)) ->
      if p1.Common.charpos = p2.Common.charpos
      then if offset < 0 then 1 else -1
      else compare p1.Common.charpos p2.Common.charpos
  | (Virt (p1,o1), Virt (p2,o2)) ->
      let poi1 = p1.Common.charpos in
      let poi2 = p2.Common.charpos in
      match compare poi1 poi2 with
	-1 -> -1
      |	0 -> compare o1 o2
      |	x -> x

let equal_posl (l1,c1) (l2,c2) =
  (l1 = l2) && (c1 = c2)

let compare_posl (l1,c1) (l2,c2) =
  match l2 - l1 with
    0 -> c2 - c1
  | r -> r

let info_to_fixpos ii =
  match pinfo_of_info ii with
    OriginTok pi -> Ast_cocci.Real pi.Common.charpos
  | ExpandedTok (_,(pi,offset)) ->
      Ast_cocci.Virt (pi.Common.charpos,offset)
  | FakeTok (_,(pi,offset)) ->
      Ast_cocci.Virt (pi.Common.charpos,offset)
  | AbstractLineTok pi -> failwith "unexpected abstract"

(* cocci: *)
let is_test (e : expression) =
  let (_,info), _ = e in
  let (_,test) = !info in
  test = Test

(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* When we have extended the C Ast to add some info to the tokens,
 * such as its line number in the file, we can not use anymore the
 * ocaml '=' to compare Ast elements. To overcome this problem, to be
 * able to use again '=', we just have to get rid of all those extra
 * information, to "abstract those line" (al) information.
 *
 * Julia then modifies it a little to have a tokenindex, so the original
 * true al_info is in fact real_al_info.
 *)

let al_info tokenindex x =
  { pinfo =
    (AbstractLineTok
       {charpos = tokenindex;
	 line = tokenindex;
	 column = tokenindex;
	 file = "";
	 str = str_of_info x});
    cocci_tag = ref emptyAnnot;
    annots_tag = Token_annot.empty;
    comments_tag = ref emptyComments;
    danger = ref NoDanger;
  }

let semi_al_info x =
  { x with
    cocci_tag = ref emptyAnnot;
    comments_tag = ref emptyComments;
    danger = ref NoDanger;
  }

let magic_real_number = -10

let real_al_info x =
  { pinfo =
    (AbstractLineTok
       {charpos = magic_real_number;
	 line = magic_real_number;
	 column = magic_real_number;
	 file = "";
	 str = str_of_info x});
    cocci_tag = ref emptyAnnot;
    annots_tag = Token_annot.empty;
    comments_tag = ref emptyComments;
    danger = ref NoDanger;
  }

let al_comments keep_comments x =
  let keep_cpp l =
    if keep_comments
    then l
    else
      (* This is a hack.  If we keep something, we need to keep the newlines
	 involved.  But keeping this
	 information may mean that metavariables don't match what they are
	 supposed to, if this information is taken into account.  Actually,
	 we only want this information for + code.  If we just put l here,
	 then lots of tests fail, so it seems that metavariables are being
	 matched again, and not just used for + *)
      let rec loop = function
	  [] -> []
	| ((Token_c.TCommentNewline,_) as x)::
	  (((Token_c.TCommentCpp _,_)::_) as xs) -> x :: loop xs
	| ((Token_c.TCommentCpp _,_) as x)::
	  ((Token_c.TCommentNewline,_) as y)::xs -> x :: y :: loop xs
	| ((Token_c.TCommentCpp _,_) as x)::xs -> x :: loop xs
	| x::xs -> loop xs in
      loop l in
  let al_com (x,i) =
    (x,{i with Common.charpos = magic_real_number;
	 Common.line = magic_real_number;
	 Common.file = "";
	 Common.column = magic_real_number}) in
  {mbefore = []; (* duplicates mafter of the previous token *)
   mafter = List.map al_com (keep_cpp x.mafter);
   mbefore2=[];
   mafter2=[];
  }

let al_info_cpp tokenindex x =
  { pinfo =
    (AbstractLineTok
       {charpos = tokenindex;
	 line = tokenindex;
	 column = tokenindex;
	 file = "";
	 str = str_of_info x});
    cocci_tag = ref emptyAnnot;
    annots_tag = Token_annot.empty;
    comments_tag = ref (al_comments false !(x.comments_tag));
    danger = ref NoDanger;
  }

let semi_al_info_cpp x =
  { x with
    cocci_tag = ref emptyAnnot;
    annots_tag = Token_annot.empty;
    comments_tag = ref (al_comments false !(x.comments_tag));
    danger = ref NoDanger;
  }

let real_al_info_cpp keep_comments x =
  { pinfo =
    (AbstractLineTok
       {charpos = magic_real_number;
	 line = magic_real_number;
	 column = magic_real_number;
	 file = "";
	 str = str_of_info x});
    cocci_tag = ref emptyAnnot;
    annots_tag = Token_annot.empty;
    comments_tag =  ref (al_comments keep_comments !(x.comments_tag));
    danger = ref NoDanger;
  }


(*****************************************************************************)
(* Views *)
(*****************************************************************************)

(* Transform a list of arguments (or parameters) where the commas are
 * represented via the wrap2 and associated with an element, with
 * a list where the comma are on their own. f(1,2,2) was
 * [(1,[]); (2,[,]); (2,[,])] and become [1;',';2;',';2].
 *
 * Used in cocci_vs_c.ml, to have a more direct correspondence between
 * the ast_cocci of julia and ast_c.
 *)
let rec (split_comma: 'a wrap2 list -> ('a, il) either list) =
  function
  | [] -> []
  | (e, ii)::xs ->
      if ii=[]
      then (Left e)::split_comma xs
      else Right ii::Left e::split_comma xs

let rec (unsplit_comma: ('a, il) either list -> 'a wrap2 list) =
  function
  | [] -> []
  | Right ii::Left e::xs ->
      (e, ii)::unsplit_comma xs
  | Left e::xs ->
      let empty_ii = [] in
      (e, empty_ii)::unsplit_comma xs
  | Right ii::_ ->
      raise (Impossible 59)

let (split_nocomma: 'a list -> ('a, il) either list) = function l ->
  List.map (function x -> Left x) l

let (unsplit_nocomma: ('a, il) either list -> 'a list) = function l ->
  l +>
  List.map
    (function Left x -> x | Right x -> failwith "unsplit: not possible")

(*****************************************************************************)
(* Helpers, could also be put in lib_parsing_c.ml instead *)
(*****************************************************************************)

(* should maybe be in pretty_print_c ? *)

let s_of_inc_file inc_file =
  match inc_file with
  | Local xs -> xs +> String.concat "/"
  | NonLocal xs -> xs +> String.concat "/"
  | Weird s -> s

let s_of_inc_file_bis inc_file =
  match inc_file with
  | Local xs -> "\"" ^ xs +> String.concat "/" ^ "\""
  | NonLocal xs -> "<" ^ xs +> String.concat "/" ^ ">"
  | Weird s -> s

let fieldname_of_fieldkind fieldkind =
  match fieldkind with
  | Simple (sopt, ft) -> sopt
  | BitField (sopt, ft, info, expr) -> sopt


let s_of_attr attr =
  attr
  +> List.map (function (Attribute a, ii) -> unwrap a)
  +> List.map (function AttrName s -> s)
  +> String.concat ","


(* ------------------------------------------------------------------------- *)
let str_of_name ident =
  match ident with
  | RegularName (s,ii) -> s
  | CppConcatenatedName xs ->
      xs +> List.map (fun (x,iiop) -> unwrap x) +> String.concat "##"
  | CppVariadicName (s, ii) -> "##" ^ s
  | CppIdentBuilder ((s,iis), xs) ->
      s ^ "(" ^
        (xs +> List.map (fun ((x,iix), iicomma) -> x) +> String.concat ",") ^
        ")"

let get_s_and_ii_of_name name =
  match name with
  | RegularName (s, iis) -> s, iis
  | CppIdentBuilder ((s, iis), xs) -> s, iis
  | CppVariadicName (s,iis)  ->
      let (iop, iis) = Common.tuple_of_list2 iis in
      s, [iis]
  | CppConcatenatedName xs ->
      (match xs with
      | [] -> raise (Impossible 60)
      | ((s,iis),noiiop)::xs ->
          s, iis
      )

let get_s_and_info_of_name name =
  let (s,ii) = get_s_and_ii_of_name name in
  s, List.hd ii

let info_of_name name =
  let (s,ii) = get_s_and_ii_of_name name in
  List.hd ii

let ii_of_name name =
  let (s,ii) = get_s_and_ii_of_name name in
  ii

let get_local_ii_of_expr_inlining_ii_of_name e =
  let (ebis,_),ii = e in
  match ebis, ii with
  | Ident name, noii ->
      assert (noii = []);
      ii_of_name name
  | RecordAccess   (e, name), ii ->
      ii @ ii_of_name name
  | RecordPtAccess (e, name), ii ->
      ii @ ii_of_name name
  | _, ii -> ii


let get_local_ii_of_tybis_inlining_ii_of_name ty =
  match ty with
  | TypeName (name, _typ), [] -> ii_of_name name
  | _, ii -> ii

(* the following is used to obtain the argument to LocalVar *)
let info_of_type ft =
  let (qu, ty) = ft in
  (* bugfix: because of string->name, the ii can be deeper *)
  let ii = get_local_ii_of_tybis_inlining_ii_of_name ty in
  match ii with
  | ii::_ -> Some ii.pinfo
  | [] -> None

(* only Label and Goto have name *)
let get_local_ii_of_st_inlining_ii_of_name st =
  match st with
  | Labeled (Label (name, st)), ii -> ii_of_name name @ ii
  | Jump (Goto name), ii ->
      let (i1, i3) = Common.tuple_of_list2 ii in
      [i1] @ ii_of_name name @ [i3]
  | _, ii -> ii



(* ------------------------------------------------------------------------- *)
let name_of_parameter param =
  param.p_namei +> Common.map_option (str_of_name)


(* ------------------------------------------------------------------------- *)
(* Annotations on tokens *)
(* ------------------------------------------------------------------------- *)

(* to put a given annotation on a token *)
let put_annot_info info key value =
  info.annots_tag <- Token_annot.put_annot key value info.annots_tag

(* to check if an annotation has such a token *)
let get_annot_info info key =
  Token_annot.get_annot info.annots_tag key

let get_comments_before info = (!(info.comments_tag)).mbefore
let get_comments_after info = (!(info.comments_tag)).mafter

let string_of_toplevel = function
  | Declaration _ -> "declaration"
  | Definition _ -> "definition"
  | CppTop _ -> "CppTop"
  | IfdefTop _ -> "IfdefTop"
  | MacroTop _ -> "MacroTop"
  | EmptyDef _ -> "EmptyDef"
  | NotParsedCorrectly _ -> "NotParsedCorrectly"
  | FinalDef _ -> "FinalDef"
  | Namespace _ -> "Namespace"

let string_of_inc_file = function
  | Local lst -> "local://" ^ (String.concat "/" lst)
  | NonLocal lst -> "nonlocal://" ^ (String.concat "/" lst)
  | Weird s -> "weird://" ^ s
