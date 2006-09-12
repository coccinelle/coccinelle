open Common open Commonop

(*****************************************************************************)
(* 
 * Could have more precise type in fullType, in expression, etc,
 * but it requires to do too much things in parsing (checking no conflicting 
 * structname, computing value, ...).
 * Better to separate concern, so I put '=>' to mean what we would really like
 * (in fact what we really like is a defining another fullType from scratch,
 * because many stuff are just sugar).
 * 
 * inv: 
 *  Array and FunctionType have also typeQualifier but they dont have sense.
 *  I put this to factorise some code. If you look in grammar, you see that 
 *  we can never specify const for the array himself (but we can do it for 
 *  pointer).
 *
 *
 * Because of ExprStatement, can have more 'new scope', but rare I think.
 * For instance array of constExpression => possibly an exprStatement and 
 * a new (local) struct defined. Same for Constructor.
 *
 * Some stuff are tagged semantic: which means that they are computed after
 * parsing.
 *)

(*****************************************************************************)
(* 
 * Cocci: Each token will be decorated in the futur by the mcodekind of cocci.
 * It is the job of the pretty printer to look at this information and decide
 * to print or not the token (and also the pending '+' associated sometimes 
 * with the token).
 *
 * The first time that we parse the original C file, the mcodekind is empty, 
 * or more precisely all is tagged as a CONTEXT with NOTHING associated.
 * This is what I call a "clean" expr/statement/....
 * 
 * Each token will also be decorated in the futur with an environment, because
 * the pending '+' may contain metavariables that refer to some C code.
 * 
 * convention: I often use 'ii' for the name of a list of info.
 *)

(* forunparser: *)
type info = Common.parse_info *  (Ast_cocci.mcodekind * metavars_binding) 
and il = info list

and 'a wrap  = 'a * il   

(* wrap2 is like wrap, except that I use it often for separator such as ','.
 * In that case the info is associated to the argument that follows, so in
 * 'a,b'  I will have in the list [(a,[]); (b,[','])].
 *)
and 'a wrap2 = 'a * il

(*****************************************************************************)
and fullType = typeQualifier * typeC
and  typeC = typeCbis wrap

and typeCbis =
  | BaseType        of baseType

  | Pointer         of fullType
  | Array           of constExpression option * fullType
  | FunctionType    of functionType

  | Enum            of string option * enumType    
  | StructUnion     of string option * structType (* new scope *)

  | EnumName        of string
  | StructUnionName of string * structUnion

  | TypeName   of string
 
  | ParenType of fullType (* forunparser: *)
 
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
     and structType  = structUnion * field wrap list  (* ; *)

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
and expression = (expressionbis * fullType option (* semantic: *)) wrap
and expressionbis = 

  (* Ident can be a enumeration constant, a simple variable, a name of a func.
   * todo? put more semantic info on this ident, such as: is it a local func?
   *)
  | Ident          of string   
  | Constant       of constant                                  

  (* cppext: normmally just   expression * expression list. *)
  | FunCall of expression * 
              (expression, fullType * storage wrap) either wrap2 (* , *) list

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
  | StatementExpr of compound wrap (* ( ) *) 
  | Constructor 

  (* forunparser: *)
  | ParenExpr of expression 

  (* cppext: *)
  | MacroCall of (expression, fullType * storage wrap, action_macro) either3
                 wrap2 list
  | MacroCall2 of (expression, statement list) either
 
  and action_macro = 
    | ActJump of (jump * il) 
    | ActMisc of il 
    | ActTodo
    | ActExpr of expression 
    | ActExpr2 of (expression * info * action_macro)


  (* I put string for Int and Float cos int would not be enough cos Caml Int 
   * are 31 bits. So simpler to do string.
   * Same reason to have string instead of int list, for String.
   *
   * note: that -2 is not a constant, it is the unary operator - applied to 
   * constant 2. So the string must represent a positive integer only.
   *)
  and constant = 
  | String of (string * isWchar) 
  | Char   of (string * isWchar) (* normally it is equivalent to Int *)
  | Int    of (string  (* * intType*)) 
  | Float  of (string * floatType)


  and isWchar = IsWchar | IsChar

  and  unaryOp  = GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not
  and  assignOp = SimpleAssign | OpAssign of arithOp
  and  fixOp    = Dec | Inc

  and  binaryOp = Arith of arithOp | Logical of logicalOp

    and  arithOp   = 
      | Plus | Minus | Mul | Div | Mod
      | DecLeft | DecRight 
      | And | Or | Xor

    and  logicalOp = 
      | Inf | Sup | InfEq | SupEq 
      | Eq | NotEq 
      | AndLog | OrLog

 and constExpression = expression (* => int *)


(* ------------------------------------------------------------------------- *)
(* note: that assignement is not a statement but an expression; 
 * wonderful C langage.
 * note: I use  'and' for type definition cos gccext allow statement as 
 * expression, so need mutual recursive type definition.
 *)
and statement = statementbis wrap 
and statementbis = 
  | Labeled       of labeled
  | Compound      of compound   (* new scope *)
  | ExprStatement of exprStatement
  | Selection     of selection
  | Iteration     of iteration
  | Jump          of jump

  | Decl  of declaration (* simplify cocci: only start of compound normally *)
  | Asm  (* gccext: *)



  and labeled = Label   of string * statement
              | Case    of expression * statement 
              | CaseRange of expression * expression * statement (* gccext: *)
	      |	Default of statement

  (* cppext: 
   * old: compound = (declaration list * statement list) 
   * old: (declaration, statement) either list 
   * simplify cocci to just have statement list, by integrating Decl in stmt.
   *)
  and compound = statement list 

  and exprStatement = expression option

  and selection     = 
   | If     of expression * statement * statement        
   | Switch of expression * statement 
   (* need check that all elements in the compound start with a case: 
      (otherwise unreachable code)  *)


  and iteration     = 
    | While   of expression * statement
    | DoWhile of statement * expression
    | For     of exprStatement wrap * exprStatement wrap * exprStatement wrap *
                 statement

  and jump  = Goto of string
            | Continue | Break 
            | Return   | ReturnExpr of expression



(* ------------------------------------------------------------------------- *)
(* 
 * (string * ...) option cos can have empty declaration or struct tag 
 * declaration.
 *   
 * Before I had Typedef constructor, but why make this special case and not 
 * have StructDef, EnumDef, ... so that struc t {...} v will generate 2 
 * declaration ? So I try to generalise and not have not Typedef too. This
 * requires more work in parsing. Better to separate concern.
 * 
 * Before the need for unparser, I didn't have a DeclList but just a Decl.
 *)

and declaration = 
   DeclList of (((string * initialiser option) wrap (* s = *) option) * 
                 fullType * storage)
                wrap2 (* , *) list wrap (* ; sto *)
     and storage       = NoSto | StoTypedef | Sto of storageClass
     and storageClass  = Auto  | Static | Register | Extern

     and initialiser = initialiserbis wrap
       and initialiserbis = 
          | InitExpr of expression 
          | InitList of initialiser wrap2 (* , *) list 
          | InitGcc of string * initialiser  (* gccext: *)
          | InitGccIndex of expression * initialiser
          | InitGccRange of expression * expression * initialiser

and definition = (string * functionType * storage * compound) 
                 wrap (* s ( ) { } sto *)

 

and program = programElement list
     and programElement = 
          | Declaration of declaration
          | Definition of definition
          | EmptyDef of il      (* gccext: allow redundant ';' *)
          | SpecialDeclMacro of (* cppext: *)
             string * 
             (expression, fullType * storage wrap) either wrap2 list *
             il 
          | CPPInclude of il
          | CPPDefine of il
          | NotParsedCorrectly of il
          | FinalDef of info



(*****************************************************************************)
(* Was previously in pattern.ml, but because of the transformer, 
 * we need to decorate each token with some cocci code AND the environment 
 * for this cocci code.
 *)
and metavars_binding = (string, metavar_binding_kind) assoc
  and metavar_binding_kind = 
  | MetaIdVal        of string
  | MetaFuncVal      of string
  | MetaLocalFuncVal of string
  | MetaExprVal      of expression (* a "clean expr" *)
  | MetaExprListVal  of expression list
  | MetaTypeVal      of fullType
  | MetaStmtVal      of statement
  | MetaParamVal     of parameterType wrap
  | MetaParamListVal of (parameterType wrap) list

(*****************************************************************************)
type info_item = (filename * (pos_file * pos_file) * string * il)

type program2 = programElement2 list
     and programElement2 = programElement * info_item


(*****************************************************************************)
let nullQualif = ({const=false; volatile= false}, [])
let nQ = nullQualif 

let defaultInt = (BaseType (IntType (Si (Signed, CInt))))
let noType = None
let noInstr = (ExprStatement (None), [])

let emptyMetavarsBinding = ([]: metavars_binding)
let emptyAnnot = (Ast_cocci.CONTEXT(Ast_cocci.NOTHING),emptyMetavarsBinding)

(*****************************************************************************)
(* abstract line *)

let _Magic_info_number = -10

let al_info x = 
  { charpos = _Magic_info_number; 
    str = (fst x).str 
  }, 
  emptyAnnot
let is_al_info x = x.charpos = _Magic_info_number

