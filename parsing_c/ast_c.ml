(* Copyright (C) 2002, 2006, 2007, 2008 Yoann Padioleau
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

(* To allow some transformations over the AST, we keep as much information 
 * as possible in the AST such as the tokens content and their locations. 
 * Those info are called 'info' (how original) and can be tagged.
 * For instance one tag may say that the unparser should remove this token.
 * 
 * Update: Now I use a ref! in those 'info' so take care.
 * 
 * Sometimes we want to add someting at the beginning or at the end 
 * of a construct. For 'function' and 'decl' we want to add something
 * to their left and for 'if' 'while' et 'for' and so on at their right.
 * We want some kinds of "virtual placeholders" that represent the start or
 * end of a construct. We use fakeInfo for that purpose.
 * To identify those cases I have added a fakestart/fakeend comment.
 * 
 * convention: I often use 'ii' for the name of a list of info. 
 * 
 * update: I now allow ifdefs in the ast but there must be only between
 * "sequencable" elements. They can be put in a type only if this type
 * is used only in a list, like at toplevel, used in 'toplevel list', 
 * or inside compound, used in 'statement list'. I must not allow 
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
 * Some stuff are tagged semantic: which means that they are computed
 * after parsing. 
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
 * 
 * All of this means that some elements in this AST are present only if 
 * some annotation/transformation has been done on the original AST returned
 * by the parser. Cf type_annotater, comment_annotater, cpp_ast_c, etc.
 *)

(* forunparser: *)

type posl = int * int (* lin-col, for MetaPosValList, for position variables *)

(* the virtual position is set in Parsing_hacks.insert_virtual_positions *)
type virtual_position = Common.parse_info * int (* character offset *)

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

type info = { 
  pinfo : parse_info;
  (* this tag can be changed, which is how we can express some progra
   * transformations by tagging the tokens involved in this transformation. 
   *)
  cocci_tag: (Ast_cocci.mcodekind * metavars_binding) ref;
  (* set in comment_annotater.ml *)
  comments_tag: comments_around ref;
  (* todo? token_info : sometimes useful to know what token it was *)
  }
and il = info list

(* wrap2 is like wrap, except that I use it often for separator such
 * as ','. In that case the info is associated to the argument that
 * follows, so in 'a,b' I will have in the list [(a,[]); (b,[','])]. *)
and 'a wrap  = 'a * il
and 'a wrap2 = 'a * il

(* ------------------------------------------------------------------------- *)
(* C Type *)
(* ------------------------------------------------------------------------- *)
(* Could have more precise type in fullType, in expression, etc, but
 * it requires to do too much things in parsing such as checking no
 * conflicting structname, computing value, etc. Better to separate
 * concern, so I put '=>' to mean what we would really like. In fact
 * what we really like is defining another fullType, expression, etc
 * from scratch, because many stuff are just sugar.
 * 
 * invariant: Array and FunctionType have also typeQualifier but they
 * dont have sense. I put this to factorise some code. If you look in
 * grammar, you see that we can never specify const for the array
 * himself (but we can do it for pointer).
 * 
 * 
 * Because of ExprStatement, we can have more 'new scope' events, but
 * rare I think. For instance with 'array of constExpression' there can
 * have an exprStatement and a new (local) struct defined. Same for
 * Constructor.
 * 
 * Some stuff are tagged semantic: which means that they are computed
 * after parsing. 
*)


and fullType = typeQualifier * typeC
and  typeC = typeCbis wrap

and typeCbis =
  | BaseType        of baseType

  | Pointer         of fullType
  | Array           of constExpression option * fullType
  | FunctionType    of functionType

  | Enum            of string option * enumType    
  | StructUnion     of structUnion * string option * structType (* new scope *)

  | EnumName        of string
  | StructUnionName of structUnion * string 

  | TypeName   of string * fullType option (* semantic: filled later *)
 
  | ParenType of fullType (* forunparser: *)

  (* gccext: TypeOfType may seems useless; why declare a 
   *     __typeof__(int) x; ? 
   * But when used with macro, it allows to fix a problem of C which
   * is that type declaration can be spread around the ident. Indeed it
   * may be difficult to have a macro such as 
   *    '#define macro(type, ident) type ident;' 
   * because when you want to do a 
   *     macro(char[256], x), 
   * then it will generate invalid code, but with a 
   *       '#define macro(type, ident) __typeof(type) ident;' 
   * it will work. *)
  | TypeOfExpr of expression  
  | TypeOfType of fullType    

  (* cppext: IfdefType TODO *)
      
(* -------------------------------------- *)    
     and  baseType = Void 
                   | IntType   of intType 
		   | FloatType of floatType

	  (* stdC: type section 
           * add  a | SizeT ?
           * note: char and signed char are semantically different!! 
           *)
          and intType   = CChar (* obsolete? | CWchar  *)
	                | Si of signed

           and signed = sign * base
            and base = CChar2 | CShort | CInt | CLong | CLongLong (* gccext: *)
            and sign = Signed | UnSigned

          and floatType = CFloat | CDouble | CLongDouble


     (* -------------------------------------- *)    
     and structUnion = Struct | Union
     and structType  = field list 
        and field = fieldbis wrap 
         and fieldbis = 
           | DeclarationField of field_declaration
           | EmptyField (* gccext: *)
            (* cppext: *)
           | MacroStructDeclTodo

            (* cppext: *)
           | CppDirectiveStruct of cpp_directive
           | IfdefStruct of ifdef_directive (* * field list list *)


        (* before unparser, I didn't have a FieldDeclList but just a Field. *)
         and field_declaration  = 
           | FieldDeclList of fieldkind wrap2 list (* , *) wrap  (* ; *)

          (* At first I thought that a bitfield could be only Signed/Unsigned.
           * But it seems that gcc allow char i:4. C rule must say that you
           * can cast into int so enum too, ... 
           *)
           and fieldkind = fieldkindbis wrap (* s : *)
            and fieldkindbis = 
                | Simple   of string option * fullType
                | BitField of string option * fullType * constExpression
                 (* fullType => BitFieldInt | BitFieldUnsigned *) 


     (* -------------------------------------- *)    
     and enumType = (string * constExpression option) wrap (* s = *) 
                    wrap2 (* , *) list 
                   (* => string * int list *)


     (* -------------------------------------- *)    
     (* return * (params * has "...") *)
     and functionType = fullType * (parameterType wrap2 list * bool wrap)
        and parameterType = (bool * string option * fullType) wrap (* reg s *)
              (* => (bool (register) * fullType) list * bool *)


and typeQualifier = typeQualifierbis wrap 
and typeQualifierbis = {const: bool; volatile: bool}

(* gccext: cppext: *)
and attribute = attributebis wrap
  and attributebis =
    | Attribute of string 

(* ------------------------------------------------------------------------- *)
(* C expression *)
(* ------------------------------------------------------------------------- *)
and expression = (expressionbis * exp_info ref (* semantic: *)) wrap
 and exp_info = exp_type option * test
  and exp_type = fullType * local
    and local = LocalVar of parse_info | NotLocalVar (* cocci: *)
  and test = Test | NotTest (* cocci: *)

 and expressionbis = 

  (* Ident can be a enumeration constant, a simple variable, a name of a func.
   * With cppext, Ident can also be the name of a macro. Sparse says
   * "an identifier with a meaning is a symbol".
   *)
  | Ident          of string  (* todo? more semantic info such as LocalFunc *)
  | Constant       of constant                                  
  | FunCall        of expression * argument wrap2 (* , *) list
  (* gccext: x ? /* empty */ : y <=> x ? x : y; *)
  | CondExpr       of expression * expression option * expression

  (* should be considered as statements, bad C langage *)
  | Sequence       of expression * expression                   
  | Assignment     of expression * assignOp * expression        

  | Postfix        of expression * fixOp                        
  | Infix          of expression * fixOp                        
  | Unary          of expression * unaryOp                      
  | Binary         of expression * binaryOp * expression        

  | ArrayAccess    of expression * expression                   
  | RecordAccess   of expression * string 
  | RecordPtAccess of expression * string 
  (* redundant normally, could replace it by DeRef RecordAcces *)

  | SizeOfExpr     of expression                                
  | SizeOfType     of fullType                                  
  | Cast           of fullType * expression                     

  (* gccext: *)        
  | StatementExpr of compound wrap (* ( )     new scope *) 
  | Constructor  of fullType * initialiser wrap2 (* , *) list 

  (* forunparser: *)
  | ParenExpr of expression 

  (* cppext: IfdefExpr TODO *)

  (* cppext: normmally just expression *)
  and argument = (expression, wierd_argument) either
   and wierd_argument = 
       | ArgType of parameterType
       | ArgAction of action_macro
      and action_macro = 
        (* todo: ArgStatement of statement, possibly have ghost token *)
         | ActMisc of il 


  (* I put string for Int and Float because int would not be enough because
   * OCaml int are 31 bits. So simpler to do string. Same reason to have
   * string instead of int list for the String case.
   * 
   * note: that -2 is not a constant, it is the unary operator '-'
   * applied to constant 2. So the string must represent a positive
   * integer only. *)

  and constant = 
    | String of (string * isWchar) 
    | MultiString  (* can contain MacroString, todo: more info *)
    | Char   of (string * isWchar) (* normally it is equivalent to Int *)
    | Int    of (string  (* * intType*)) 
    | Float  of (string * floatType)

    and isWchar = IsWchar | IsChar

  
  and unaryOp  = GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not 
                 | GetRefLabel (* gccext: GetRefLabel, via &&label notation *)
  and assignOp = SimpleAssign | OpAssign of arithOp
  and fixOp    = Dec | Inc

  and binaryOp = Arith of arithOp | Logical of logicalOp

       and arithOp   = 
         | Plus | Minus | Mul | Div | Mod
         | DecLeft | DecRight 
         | And | Or | Xor

       and logicalOp = 
         | Inf | Sup | InfEq | SupEq 
         | Eq | NotEq 
         | AndLog | OrLog

 and constExpression = expression (* => int *)



(* ------------------------------------------------------------------------- *)
(* C statement *)
(* ------------------------------------------------------------------------- *)
(* note: that assignement is not a statement but an expression;
 * wonderful C langage.
 * 
 * note: I use 'and' for type definition cos gccext allow statement as
 * expression, so need mutual recursive type definition. *)

and statement = statementbis wrap 
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
  


  and labeled = Label   of string * statement
              | Case    of expression * statement 
              | CaseRange of expression * expression * statement (* gccext: *)
	      |	Default of statement

  (* cppext: 
   * old: compound = (declaration list * statement list) 
   * old: (declaration, statement) either list 
   * Simplify cocci to just have statement list, by integrating Decl in stmt.
   * 
   * update: now introduce also the _sequencable to allow ifdef in the middle.
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

 (* for Switch, need check that all elements in the compound start 
  * with a case:, otherwise unreachable code.
  *)
  and selection     = 
   | If     of expression * statement * statement
   | Switch of expression * statement 


  and iteration     = 
    | While   of expression * statement
    | DoWhile of statement * expression
    | For     of exprStatement wrap * exprStatement wrap * exprStatement wrap *
                 statement
    (* cppext: *)
    | MacroIteration of string * argument wrap2 list * statement

  and jump  = Goto of string
            | Continue | Break 
            | Return   | ReturnExpr of expression
            | GotoComputed of expression (* gccext: goto *exp ';' *)


  (* gccext: *)
  and asmbody = il (* string list *) * colon wrap (* : *) list
      and colon = Colon of colon_option wrap2 list
      and colon_option = colon_option_bis wrap
          and colon_option_bis = ColonMisc | ColonExpr of expression


(* ------------------------------------------------------------------------- *)
(* Declaration *)
(* ------------------------------------------------------------------------- *)
(* (string * ...) option cos can have empty declaration or struct tag 
 * declaration.
 *   
 * Before I had Typedef constructor, but why make this special case and not 
 * have StructDef, EnumDef, ... so that 'struct t {...} v' will generate 2 
 * declarations ? So I try to generalise and not have Typedef either. This
 * requires more work in parsing. Better to separate concern.
 * 
 * Before the need for unparser, I didn't have a DeclList but just a Decl.
 *
 * I am not sure what it means to declare a prototype inline, but gcc
 * accepts it. 
 *)

and local_decl = LocalDecl | NotLocalDecl

and declaration = 
  | DeclList of onedecl wrap2 (* , *) list wrap (* ; fakestart sto *)
  (* cppext: *)
  | MacroDecl of (string * argument wrap2 list) wrap

     and onedecl = 
       { v_namei: (string * initialiser option) wrap (* s = *) option;
         v_type: fullType;
         v_storage: storage;
         v_local: local_decl; (* cocci: *)
         v_attr: attribute list; (* gccext: *)
       }
     and storage       = storagebis * bool (* gccext: inline or not *)
     and storagebis    = NoSto | StoTypedef | Sto of storageClass
     and storageClass  = Auto  | Static | Register | Extern

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
 * parameter wheras a function definition can not. But, in some cases such
 * as 'f(void) {', there is no name too, so I simplified and reused the 
 * same functionType type for both declaration and function definition.
 *)
and definition = definitionbis wrap (* s ( ) { } fakestart sto *)
  and definitionbis = 
  { f_name: string;
    f_type: functionType;
    f_storage: storage;
    f_body: compound;
    f_attr: attribute list; (* gccext: *)
  }
  (* cppext: IfdefFunHeader TODO *)

(* ------------------------------------------------------------------------- *)
(* cppext: cpp directives, #ifdef, #define and #include body *)
(* ------------------------------------------------------------------------- *)
and cpp_directive =
  | Include of includ 
  | Define of define 
  | Undef of string wrap
  | PragmaAndCo of il 

(* to specialize if someone need more info *)
and ifdef_directive = (* or and 'a ifdefed = 'a list wrap *)
  | IfdefDirective of (ifdefkind * matching_tag) wrap
  and ifdefkind = 
    | Ifdef (* todo? of string ? of formula_cpp *)
    | IfdefElseif (* same *)
    | IfdefElse (* same *)
    | IfdefEndif 
  (* set in Parsing_hacks.set_ifdef_parenthize_info. It internally use 
   * a global so it means if you parse same file twice you may get
   * different id. I try now to avoid this pb by resetting it each 
   * time I parse a file.
   *)
  and matching_tag = 
    IfdefTag of (int (* tag *) * int (* total with this tag *))

and define = string wrap * define_body   (* #define s *)
 and define_body = define_kind * define_val
   and define_kind =
   | DefineVar
   | DefineFunc   of ((string wrap) wrap2 list) wrap (* () *)
   and define_val = 
     | DefineExpr of expression
     | DefineStmt of statement
     | DefineType of fullType
     | DefineDoWhileZero of (statement * expression) wrap (* do { } while(0) *)
     | DefineFunction of definition
     | DefineInit of initialiser (* in practice only { } with possible ',' *)
     (* TODO DefineMulti of define_val list *)

     | DefineText of string wrap
     | DefineEmpty

     | DefineTodo



and includ = 
  { i_include: inc_file wrap; (* #include s *)
    (* cocci: computed in ? *)
    i_rel_pos: include_rel_pos option ref;
    (* cocci: cf -test incl *)
    i_is_in_ifdef: bool; 
    (* cf cpp_ast_c.ml. set to None at parsing time. *)
    i_content: (Common.filename (* full path *) * program) option;
  }
 and inc_file = 
  | Local    of inc_elem list
  | NonLocal of inc_elem list
  | Wierd of string (* ex: #include SYSTEM_H *)
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

  | MetaExprVal      of expression (* a "clean expr" *)
  | MetaExprListVal  of argument wrap2 list
  | MetaParamVal     of parameterType
  | MetaParamListVal of parameterType wrap2 list

  | MetaTypeVal      of fullType
  | MetaStmtVal      of statement

  (* Could also be in Lib_engine.metavars_binding2 with the ParenVal,
   * because don't need to have the value for a position in the env of
   * a '+'. But ParenVal or LabelVal are used only by CTL, they are not
   * variables accessible via SmPL whereas the position can be one day
   * so I think it's better to put MetaPosVal here *)
  | MetaPosVal       of (Ast_cocci.fixpos * Ast_cocci.fixpos) (* max, min *)
  | MetaPosValList   of (Common.filename * posl * posl) list (* min, max *)
  | MetaListlenVal   of int


(*****************************************************************************)
(* C comments *)
(*****************************************************************************)

(* convention: I often use "m" for comments as I can not use "c" 
 * (already use for c stuff) and "com" is too long.
 *)

(* this type will be associated to each token *)
and comments_around = {
  mbefore: comment_and_relative_pos list;
  mafter:  comment_and_relative_pos list;
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


(*****************************************************************************)
(* Cpp constructs put it comments in lexer or parsing_hack *)
(*****************************************************************************)

(* This type is not in the Ast but is associated with the TCommentCpp token.
 * I put this enum here because parser_c.mly need it. I could have put
 * it also in lexer_parser.
 *)
type cppcommentkind = 
  | CppDirective 
  | CppAttr 
  | CppMacro 
  | CppPassingNormal (* ifdef 0, cplusplus, etc *) 
  | CppPassingCosWouldGetError (* expr passsing *)




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

let emptyAnnot = 
  (Ast_cocci.CONTEXT (Ast_cocci.NoPos,Ast_cocci.NOTHING),
  emptyMetavarsBinding)

let emptyComments= {
  mbefore = [];
  mafter = [];
}


(* for include, some meta information needed by cocci *)
let noRelPos () = 
  ref (None: include_rel_pos option)
let noInIfdef () = 
  ref false


(* When want add some info in ast that does not correspond to 
 * an existing C element.
 * old: or when don't want 'synchronize' on it in unparse_c.ml
 * (now have other mark for tha matter).
 *)
let no_virt_pos = ({str="";charpos=0;line=0;column=0;file=""},-1)

let fakeInfo pi  = 
  { pinfo = FakeTok ("",no_virt_pos);
    cocci_tag = ref emptyAnnot;
    comments_tag = ref emptyComments;
  }

let noii = []
let noattr = []
let noi_content = (None: ((Common.filename * program) option))

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap = fst


let unwrap_expr ((unwrap_e, typ), iie) = unwrap_e
let rewrap_expr ((_old_unwrap_e, typ), iie)  newe = ((newe, typ), iie)

let get_type_expr ((unwrap_e, typ), iie) = !typ
let set_type_expr ((unwrap_e, oldtyp), iie) newtyp =
  oldtyp := newtyp
  (* old: (unwrap_e, newtyp), iie *)


let unwrap_typeC (qu, (typeC, ii)) = typeC
let rewrap_typeC (qu, (typeC, ii)) newtypeC  = (qu, (newtypeC, ii))


let rewrap_str s ii =  
  {ii with pinfo =
    (match ii.pinfo with
      OriginTok pi -> OriginTok { pi with Common.str = s;}
    | ExpandedTok (pi,vpi) -> ExpandedTok ({ pi with Common.str = s;},vpi)
    | FakeTok (_,vpi) -> FakeTok (s,vpi)
    | AbstractLineTok pi -> OriginTok { pi with Common.str = s;})}

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
  | ExpandedTok (pi,_) -> pi
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
  | ExpandedTok (pi,_) -> f pi
  | FakeTok (_,(pi,_)) -> f pi
  | AbstractLineTok pi -> f pi

let make_expanded ii =
  {ii with pinfo = ExpandedTok (get_opi ii.pinfo,no_virt_pos)}

let pos_of_info   ii = get_info      (function x -> x.Common.charpos) ii
let opos_of_info  ii = get_orig_info (function x -> x.Common.charpos) ii
let line_of_info  ii = get_orig_info (function x -> x.Common.line)    ii
let col_of_info   ii = get_orig_info (function x -> x.Common.column)  ii
let file_of_info  ii = get_orig_info (function x -> x.Common.file)    ii
let mcode_of_info ii = fst (!(ii.cocci_tag))
let pinfo_of_info ii = ii.pinfo
let parse_info_of_info ii = get_pi ii.pinfo

let is_fake ii =
  match ii.pinfo with
    FakeTok (_,_) -> true
  | _ -> false

let is_origintok ii = 
  match ii.pinfo with
  | OriginTok pi -> true
  | _ -> false

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
    (Real p1, Real p2) -> compare p1.Common.charpos p2.Common.charpos
  | (Virt (p1,_), Real p2) ->
      if (compare p1.Common.charpos p2.Common.charpos) = (-1) then (-1) else 1
  | (Real p1, Virt (p2,_)) ->
      if (compare p1.Common.charpos p2.Common.charpos) = 1 then 1 else (-1)
  | (Virt (p1,o1), Virt (p2,o2)) ->
      let poi1 = p1.Common.charpos in
      let poi2 = p2.Common.charpos in
      match compare poi1 poi2 with
	-1 -> -1
      |	0 -> compare o1 o2
      |	x -> x

let equal_posl (l1,c1) (l2,c2) = 
  (l1 =|= l2) && (c1 =|= c2)

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
  let (_,info) = unwrap e in
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
    comments_tag = ref emptyComments;
  }

let semi_al_info x = 
  { x with
    cocci_tag = ref emptyAnnot;
    comments_tag = ref emptyComments;
  }

(*****************************************************************************)
(* Views *)
(*****************************************************************************)

(* Transform a list of arguments (or parameters) where the commas are
 * represented via the wrap2 and associated with an element, with
 * a list where the comma are on their own. f(1,2,2) was
 * [(1,[]); (2,[,]); (2,[,])] and become [1;',';2;',';2].
 * 
 * Used in cocci_vs_c.ml, to have a more direct correspondance between
 * the ast_cocci of julia and ast_c.
 *)
let rec (split_comma: 'a wrap2 list -> ('a, il) either list) = 
  function
  | [] -> []
  | (e, ii)::xs -> 
      if null ii 
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
      raise Impossible




let split_register_param = fun (hasreg, idb, ii_b_s) -> 
  match hasreg, idb,  ii_b_s with
  | false, Some s, [i1] -> Left (s, [], i1)
  | true, Some s, [i1;i2] -> Left (s, [i1], i2)
  | _, None, ii -> Right ii
  | _ -> raise Impossible



(*****************************************************************************)
(* Helpers, could also be put in lib_parsing_c.ml instead *)
(*****************************************************************************)

let rec stmt_elems_of_sequencable xs = 
  xs +> Common.map (fun x -> 
    match x with
    | StmtElem e -> [e]
    | CppDirectiveStmt _
    | IfdefStmt _ 
        -> 
        pr2 ("stmt_elems_of_sequencable: filter a directive");
        []
    | IfdefStmt2 (_ifdef, xxs) -> 
        pr2 ("stmt_elems_of_sequencable: IfdefStm2 TODO?");
        xxs +> List.map (fun xs -> 
          let xs' = stmt_elems_of_sequencable xs in
          xs'
        ) +> List.flatten
  ) +> List.flatten
        
  


let s_of_inc_file inc_file = 
  match inc_file with
  | Local xs -> xs +> Common.join "/"
  | NonLocal xs -> xs +> Common.join "/"
  | Wierd s -> s

let s_of_inc_file_bis inc_file = 
  match inc_file with
  | Local xs -> "\"" ^ xs +> Common.join "/" ^ "\""
  | NonLocal xs -> "<" ^ xs +> Common.join "/" ^ ">"
  | Wierd s -> s

let fieldname_of_fieldkind fieldkind = 
  match unwrap fieldkind with
  | Simple (sopt, ft) -> sopt
  | BitField (sopt, ft, expr) -> sopt

