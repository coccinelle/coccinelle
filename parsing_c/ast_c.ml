open Common open Commonop

(*****************************************************************************)
(* The AST C related types *)
(*****************************************************************************)

(* Cocci: Each token will be decorated in the futur by the mcodekind
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
 * Update: Now I use a ref! so take care.
 * 
 * Sometimes we want to add someting at the beginning or at the end 
 * of a construct. For 'function' and 'decl' we want add something
 * to their left and for 'if' 'while' et 'for' and so on at their right.
 * We want some kinds of "virtual placeholders" that represent the start or
 * end of a construct. We use fakeInfo for that purpose.
 * To identify those cases I have added a fakestart/fakeend comment.
 * 
 * Concerning the mark, FakeTok are present only in ast and generated 
 * after parsing, OriginTok are both in ast and list of tokens,
 * ExpandedTok are both in ast and list of tokens, AbstractLineTok
 * are neither in ast nor in list of tokens but only in the '+'
 * of the mcode of some tokens.
 * 
 * convention: I often use 'ii' for the name of a list of info. 
 * 
 *)

(* forunparser: *)

type pos = int
type mark_token = OriginTok | FakeTok | ExpandedTok | AbstractLineTok

type info = { 
  pinfo : Common.parse_info;
  mark : mark_token;
  cocci_tag: (Ast_cocci.mcodekind * metavars_binding) ref;
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
 * after parsing. *)


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

  | TypeName   of string
 
  | ParenType of fullType (* forunparser: *)

  (* gccext: TypeOfType may seems useless, why declare a __typeof__(int)
   * x; ? But when used with macro, it allows to fix a problem of C which
   * is that type declaration can be spread around the ident. Indeed it
   * may be difficult to have a macro such as '#define macro(type,
   * ident) type ident;' because when you want to do a macro(char[256],
   * x), then it will generate invalid code, but with a '#define
   * macro(type, ident) __typeof(type) ident;' it will work. *)
  | TypeOfExpr of expression  
  | TypeOfType of fullType    
      
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
     and structType  = (field wrap) list  (* ; *)

        (* before unparser, I didn't have a FieldDeclList but just a Field. *)
         and field  =  FieldDeclList of fieldkind wrap2 list (* , *)

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


(* ------------------------------------------------------------------------- *)
(* C expression *)
(* ------------------------------------------------------------------------- *)
and expression = (expressionbis * fullType list ref (* semantic: *)) wrap
and expressionbis = 

  (* Ident can be a enumeration constant, a simple variable, a name of a func.
   * With cppext, Ident can also be the name of a macro.
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

  (* cppext: normmally just expression *)
  and argument = (expression, wierd_argument) either
   and wierd_argument = 
       | ArgType of parameterType
       | ArgAction of action_macro
      and action_macro = 
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
    | MultiString 
    | Char   of (string * isWchar) (* normally it is equivalent to Int *)
    | Int    of (string  (* * intType*)) 
    | Float  of (string * floatType)

    and isWchar = IsWchar | IsChar

  (* gccext: GetRefLabel, via &&label notation *)
  and unaryOp  = GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not | GetRefLabel
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
   *)
  and compound = statement list 

  and exprStatement = expression option

 (* for Switch, need check that all elements in the compound start 
  * with a case:, otherwise unreachable code.
  *)
  and selection     = 
   | If     of expression * statement * statement
   | Switch of expression * statement 
   | Ifdef of statement list * statement list    (* cppext: *)

  and iteration     = 
    | While   of expression * statement
    | DoWhile of statement * expression
    | For     of exprStatement wrap * exprStatement wrap * exprStatement wrap *
                 statement

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
 * declarations ? So I try to generalise and not have not Typedef too. This
 * requires more work in parsing. Better to separate concern.
 * 
 * Before the need for unparser, I didn't have a DeclList but just a Decl.
 *
 * I am not sure what it means to declare a prototype inline, but gcc
 * accepts it. 
 *)

and declaration = 
  | DeclList of (((string * initialiser option) wrap (* s = *) option) * 
                 fullType * storage)
                wrap2 (* , *) list wrap (* ; fakestart sto *)
  (* cppext: *)
  | MacroDecl of (string * argument wrap2 list) wrap

     and storage       = storagebis * bool (* inline or not, gccext: *)
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
and definition = (string * functionType * storage * compound) 
                 wrap (* s ( ) { } fakestart sto *)

(* ------------------------------------------------------------------------- *)
(* #define and #include body *)
(* ------------------------------------------------------------------------- *)
(* cppext *) 
and define = define_kind * define_val
   and define_kind =
   | DefineVar
   | DefineFunc   of ((string wrap) wrap2 list) wrap
   and define_val = 
     | DefineExpr of expression
     | DefineStmt of statement
     | DefineType of fullType
     | DefineDoWhileZero of statement wrap (* do { } while(0) *)
     | DefineFunction of definition
     | DefineText of string wrap
     | DefineEmpty

and inc_file = 
  | Local    of inc_elem list
  | NonLocal of inc_elem list
  | Wierd of string (* ex: #include SYSTEM_H *)
  and inc_elem = string

(* Cocci: to tag the first of #include <xx/> and last of #include <yy/>
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
  | Include of inc_file wrap * include_rel_pos option ref (* #include s *)
  | Define of string wrap * define   (* #define s *)
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
  | MetaTypeVal      of fullType
  | MetaStmtVal      of statement
  | MetaParamVal     of parameterType
  | MetaParamListVal of parameterType wrap2 list
  | MetaConstVal     of (constant, string) either wrap
  (* Could also be in Lib_engine.metavars_binding2 with the ParenVal,
   * because don't need to have the value for a position in the env of
   * a '+'. But ParenVal or LabelVal are used only by CTL, they are not
   * variables accessible via SmPL whereas the position can be one day
   * so I think it's better to put MetaPosVal here *)
  | MetaPosVal       of pos * pos


(*****************************************************************************)
(* Cpp comments *)
(*****************************************************************************)
(* This type is not in the Ast but is associated with the TCommentCpp token.
 * I put this enum here because parser_c.mly need it. I could have put
 * it also in lexer_parser.
*)

type cppcommentkind = 
  CppDirective | CppAttr | CppOther

(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
let nullQualif = ({const=false; volatile= false}, [])
let nQ = nullQualif 

let defaultInt = (BaseType (IntType (Si (Signed, CInt))))
let noType () = ref [] (* old: None *)
let noInstr = (ExprStatement (None), [])

let emptyMetavarsBinding = ([]: metavars_binding)
let emptyAnnot = (Ast_cocci.CONTEXT(Ast_cocci.NoPos,Ast_cocci.NOTHING),
                 emptyMetavarsBinding)


let noRelPos () = ref (None: include_rel_pos option)


(* When want add some info in ast that does not correspond to 
 * an existing C element.
 * old: or when don't want 'synchronize' on it in unparse_c.ml
 * (now have other mark for tha matter).
 *)
let fakeInfo ()  = 
  { pinfo = Common.fake_parse_info;
    cocci_tag = ref emptyAnnot;
    mark = FakeTok;
  }


(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap = fst


let unwrap_expr ((unwrap_e, typ), iie) = unwrap_e
let rewrap_expr ((_old_unwrap_e, typ), iie)  newe = ((newe, typ), iie)

let get_types_expr ((unwrap_e, typ), iie) = !typ
let set_types_expr ((unwrap_e, oldtyps), iie) newtyps =
  oldtyps := newtyps
  (* old: (unwrap_e, newtyp), iie *)


let unwrap_typeC (qu, (typeC, ii)) = typeC
let rewrap_typeC (qu, (typeC, ii)) newtypeC  = (qu, (newtypeC, ii))


let rewrap_str s ii =  
  {ii with pinfo = { ii.pinfo with Common.str = s;}}
let rewrap_mark mark ii =  
  {ii with mark = mark}

let pos_of_info  ii = ii.pinfo.Common.charpos
let line_of_info  ii = ii.pinfo.Common.line
let str_of_info  ii = ii.pinfo.Common.str
let file_of_info ii = ii.pinfo.Common.file
let mark_of_info ii = ii.mark
let mcode_of_info ii  = fst (!(ii.cocci_tag))

(* todo: use virtual pos ? *)
let compare_pos i1 i2 = 
  compare i1.pinfo.charpos i2.pinfo.charpos
let equal_pos p1 p2 = 
  p1 =|= p2

(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* When we have extended the C Ast to add some info to the tokens,
 * such as its line number in the file, we can not use anymore the
 * ocaml '=' to compare Ast elements. To overcome this problem, to be
 * able to use again '=', we just have to get rid of all those extra
 * information, to "abstract those line" information.
 *)

let al_info x = 
  let _Magic_info_number = -10 in
  { pinfo = 
      { charpos = _Magic_info_number; 
        str = x.pinfo.str;
        line = -1; column = -1; file = "";
      };
    cocci_tag = ref emptyAnnot;
    mark = AbstractLineTok;
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

