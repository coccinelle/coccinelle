open Common open Commonop

(*******************************************************************************)
(* 
  could have more precise type in fullType, in expression, etc
    but it requires to do too much things in parsing (checking no conflicting structname, computing value, ...)
    better to separate concern, so I put '=>' to mean what we would really like, in fact
    what we really like is a fullType2, after all many stuff are just sugar.
  
  inv: Array and FunctionType have also typeQualifier but they dont have any sense.
   I put this to factorise some code. If you see the grammar, you see that we can never specify const
   for the array himself (but we can do it for pointer).


 because of ExprStatement,  can have more 'new scope',  but rare I think
  for instance array of constExpression => possibly an exprstatement and a new (local) struct defined
 same for Constructor
*)

(*******************************************************************************)
(* 
   Each token will be decorated in the futur by the mcode of cocci.
    It is the job of the pretty printer to look at this information to print or not
    the token (and also the pending '+' associated sometimes with the token).
    The first time that we parse the original C file, the mcode is empty, or more precisely
    all is tagged as a CONTEXT with NOTHING associated.
    This is what I call a "clean expr/statement/...".

   Each token will also be decorated in the futur with an environment, because the pending
    '+' may contain metavariables that refer to some C code.

   convention: I often use 'ii' for the name of a list of info.
*)

type info = Common.parse_info *  (Ast_cocci.mcodekind * metavars_binding)   (* forunparser: *)
and il = info list

and 'a wrap = 'a * il

(*******************************************************************************)
and fullType = typeQualifier * typeC
and  typeC = typeCbis wrap (* forunparser: *)

and typeCbis =
  | BaseType        of baseType

  | Pointer         of fullType
  | Array           of (constExpression option) * fullType
  | FunctionType    of functionDeclType

  | Enum            of (string option) * enumType    
  | StructUnion     of (string option) * structType (* new scope *)

  | StructUnionName of tagged_string * structUnion
  | EnumName        of tagged_string

  | TypeName   of string
 
  | ParenType of fullType (* forunparser: *)

    (* for reason related to compatible/composite type, 2 struct with same name are different if declared
       in different namespace, the tag is the corresponding depth in the nested namespace scope *)
     and tagged_string = string * int  
     

     and  baseType = Void 
                   | IntType   of intType 
		   | FloatType of floatType

	  (* stdC: type section *)
          and intType   = CChar | CWchar (* | SizeT, char and signed char are semantically different!! *)
	                | Si of signed

              and signed = sign * base
	      and base = CChar2 | CShort | CInt | CLong | (* gccext: *) CLongLong
	      and sign = Signed | UnSigned

          and floatType = CFloat | CDouble | CLongDouble


     and structType  = structUnion * (field * il) list   (* infoptvirg *)
     and structUnion = Struct | Union
         (* Before unparser, I didn't have a FieldDeclList but just a Field *)
         and field  =  FieldDeclList of (fieldkind * il) list
          (* at first i thought that a bitfield could be only Signed or Unsigned, but seems that
              gcc allow char i:4, C rule must say that you can cast into int => enum too 
          *)
           and fieldkind = 
                    | Simple   of (string option) * fullType * il
                    | BitField of (string option) * (typeQualifier * bitfieldType (* <=> fullType *)) * constExpression  * il

	     and bitfieldType = typeC (* => BitFieldInt | BitFieldUnsigned *)


     and enumType =  (((string * info) * ((constExpression * info) option)) * il)  list
                     (* => string * int list (assign value now) *)



     and functionDeclType = fullType * functionDeclType2 
            (* 1st bool = hasregister, 2nd = has "..." *)
         and functionDeclType2 = Classic of ((parameterTypeDecl * il) list * bool   * il)
           and parameterTypeDecl = (bool * (string option) * fullType               * (il * il))
          (* => (bool * fullType) list * bool *)


and typeQualifier = typeQualifierbis wrap (* forunparser: *)
and typeQualifierbis = {const: bool; volatile: bool}





(* ------------------------------------------------------------------------------ *)

and expression = expressionbis * fullType option (* computed after parsing *) * il (* forunparser: *)
and expressionbis = 
  | Ident          of string    (* can be a enumeration constant, or a simple variable (or name of a func) *)
  | Constant       of constant                                  

  (* cppext: normmally just   expression * expression list *)
  (* the il correspond to the ',' and it is associated to the argument that follows *)
  | FunCall        of expression * (((expression, (fullType * (storage * il))) either) * il) list 

  | CondExpr       of expression * expression * expression       (*   x ? : y --> x ? x : y;  if the then part is NoExpr, then we must return the same value as in the condition, cf gccext manual *)

  (* should be considered as statements, bad C langage *)
  | Sequence       of expression * expression                   
  | Assignment     of expression * assignOp * expression        

  | Postfix        of expression * fixOp                        
  | Infix          of expression * fixOp                        
  | Unary          of expression * unaryOp                      
  | Binary         of expression * binaryOp * expression        

  | ArrayAccess    of expression * expression                   
  | RecordAccess   of expression * string                       
  | RecordPtAccess of expression * string                       (* redundant normally, could replace it by DeRef RecordAcces *)

  | SizeOfExpr     of expression                                
  | SizeOfType     of fullType                                  
  | Cast           of fullType * expression                     

  (* should be considered as statements, bad C langage *)
  | StatementExpr of (compound * il) (* gccext: *)        
  | Constructor (* gccext: TODO, we will certainly have to then do a special visitor for initializer *)
  | NoExpr      (* gccext: allowed in CondExpr, but in fact it has a special meaning *)


  | ParenExpr of expression (* forunparser: *)


  | MacroCall of(((expression, (fullType * (storage * il)), action_macro) either3) * il) list (* cppext: *)
  | MacroCall2 of (expression, (statement list)) either                                       (* cppext: *)
          and action_macro = ActJump of (jump * il) | ActMisc of il | ActTodo
                           | ActExpr of expression | ActExpr2 of (expression * info * action_macro)


     (*  I put string for Int and Float cos int would not be enough cos caml Int are 31 bits, so simpler to do string *)
     (*  same reason to have string instead of int list, for String *)
     and constant = String of (string * isWchar) 
                  | Char   of (string * isWchar) (* normally it is equivalent to Int *)
		  | Int    of (string            (* * intType*)) (* -2 is not a constant, it is the unary operator - apply to constant 2, => string must represent a positive integer only *)
		  | Float  of (string * floatType)

         and isWchar = IsWchar | IsChar

     and  unaryOp  = GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not
     and  assignOp = SimpleAssign | OpAssign of arithOp
     and  fixOp    = Dec | Inc

     and  binaryOp = Arith of arithOp | Logical of logicalOp
          and  arithOp   = Plus | Minus | Mul | Div | Mod |        DecLeft | DecRight |       And | Or | Xor
          and  logicalOp = Inf | Sup | InfEq | SupEq |       Eq | NotEq |           AndLog | OrLog

 and constExpression = expression (* => int *)



(* ------------------------------------------------------------------------------ *)

(* note: that assignement is not a statement but an expression; wonderful C langage *)
(* note: I use  'and' for type definition cos gccext allow statement as expression, so need mutual recursive type definition *)
and statement = statementbis wrap (* forunparser: *)
and statementbis = 
  | Labeled       of labeled
  | Compound      of compound   (* new scope *)
  | ExprStatement of exprStatement
  | Selection     of selection
  | Iteration     of iteration
  | Jump          of jump
  | Asm (* gccext: *)


  and labeled = Label   of string * statement
              | Case    of expression * statement 
              | CaseRange of expression * expression * statement (* gccext: *)
	      |	Default of statement

  (* old: compound = (declaration list * statement list) *)
  and compound = ((declaration, statement) either) list (* cppext: (or because of cpp) *)

  and exprStatement = (expression option)

  and selection     = If     of expression * statement * statement        
                    | Switch of expression * statement (* need check that all elements in the compound start with a case: (otherwise unreachable code)  *)


  and iteration     = While   of expression * statement
		    | DoWhile of statement * expression
		    | For     of (exprStatement * il) * (exprStatement * il) * (exprStatement * il) * statement

  and jump  = Goto of string
            | Continue | Break 
            | Return   | ReturnExpr of expression





(* ------------------------------------------------------------------------------ *)

(* (string * ...) option cos can have empty declaration or struct tag declaration 
     
   before I had Typedef constructor, but why make this special case and not have StructDef, EnumDef, ...
    so that struc t {...} v will generate 2 declaration ? So I try to generalise and not have
    not Typedef too, this require more work in parsing. Better to separate concern.
   
   Before the need for unparser, I didn't have a DeclList but just a Decl.
*)
and declaration = DeclList of ((((string * ((initialiser * info) option) * (info)) option) * fullType * storage) * il) list * (il * info) (* ilsto, infoptvirg *)
     and storage       = NoSto | StoTypedef | Sto of storageClass
     and storageClass  = Auto  | Static | Register | Extern

     and initialiser = initialiserbis wrap
       and initialiserbis = InitExpr of expression | InitList of (initialiser * il) list 
                          | InitGcc of string * initialiser  (* gccext: *)
                          | InitGccIndex of expression * initialiser
                          | InitGccRange of expression * expression * initialiser


and definition = (string * functionDefType * storage * compound * 
                    (info * il * il)) (* info for string, then for storage, and then for compound *)
         and functionDefType = fullType * ((parameterTypeDef * il) list) * bool (* has... *) * 
                    (il * il) (* info for dots, and then info for '(' and ')' for parameters *)
             and parameterTypeDef = (bool (* hasregister *) * string * fullType * 
                    (il * info))


and program = programElement list
     and programElement = Declaration of declaration
                        | Definition of definition
                        | EmptyDef of il  (* gccext: allow redundant ; *)
                        | SpecialDeclMacro of string * (((expression, (fullType * (storage * il))) either) * il) list   *  (il) (* cppext: *)

                        | NotParsedCorrectly of il
                        | FinalDef of info


         (* before I had functionDefType = functionDeclType, and I said that 
            "in old C rule, you can just say name, and then state
             type, but normally after function check, we must transform it so that 
             we dont have this anymore"
             but i prefer have a clean abstractSyntax type, prototype and definition 
             are different, they dont
             have the same constraint, so I do more job in parsing for this case only
	 *)



(*******************************************************************************)
(* Was previously in pattern.ml, but because of the transformer, 
   we need to decorate each token with some cocci code AND the environment 
   for this cocci code.
 *)
and metavars_binding = (string, metavar_binding_kind) assoc
  and metavar_binding_kind = 
  | MetaId        of string
  | MetaFunc      of string
  | MetaLocalFunc of string
  | MetaExpr      of expression (* a "clean expr" *)
  | MetaExprList  of expression list
  | MetaType      of fullType
  | MetaStmt      of statement
  | MetaParam     of (parameterTypeDef * il)
  | MetaParamList of (parameterTypeDef * il) list

(*******************************************************************************)

type info_item = (filename * (pos_file *  pos_file) * string * il)

type program2 = programElement2 list
     and programElement2 = programElement * info_item



(*******************************************************************************)
let nullQualif = ({const=false; volatile= false}, [])
let nQ = nullQualif 

let defaultInt = (BaseType (IntType (Si (Signed, CInt))))

let iiTodovide = []

let noType = None

let emptyMetavarsBinding = ([]: metavars_binding)

let dumbAnnot = (Ast_cocci.CONTEXT(Ast_cocci.NOTHING),emptyMetavarsBinding)

let noInstr = (ExprStatement (None), [])

(*******************************************************************************)

(* al for  Abstract Line information *)

let _Magic_info_number = -10
let is_al_info x = x.charpos = _Magic_info_number

let al_info x = { charpos = _Magic_info_number; str = (fst x).str }, dumbAnnot

let rec (al_expr: expression -> expression) = fun (exp, typ, ii) -> 
  let ii' = List.map (fun i -> al_info i) ii in

  (match exp with
  | Ident (s) -> Ident (s)
  | Constant (c) -> Constant (c)
  | FunCall  (e, es)         -> 
      let e' = al_expr e in
      let es' = 
        List.map (fun (exp_or_decl, ii) -> 
          let ii' = List.map (fun i -> al_info i) ii in
          (match exp_or_decl with
          | Left e -> Left (al_expr e)
          | Right _ -> raise Todo
          ), ii'
                 ) es in
      FunCall (e', es')


  | RecordPtAccess (e, field) -> 
      let e' = al_expr e in
      RecordPtAccess (e', field)

  | _ -> raise Todo
  ), typ, ii'

and (al_statement: statement -> statement) = fun (stat, ii) -> 
  raise Todo

and (al_type: fullType -> fullType) = fun (stat, ii) -> 
  raise Todo




let ex1 = 
  (FunCall
     ((Ident "f",
       None,
       [({str = "f"; charpos = 26},
         (Ast_cocci.CONTEXT (Ast_cocci.NOTHING),
          []))]),
      [(Left
          (Constant (Int "1"),
           None,
           [({str = "1"; charpos = 28},
             (Ast_cocci.CONTEXT (Ast_cocci.NOTHING),
              []))]),
        [])]),
   None,
   [({str = "("; charpos = 27},
     (Ast_cocci.CONTEXT (Ast_cocci.NOTHING),
      []));
     ({str = ")"; charpos = 29},
      (Ast_cocci.CONTEXT (Ast_cocci.NOTHING),
       []))])

let ex1final = 
  (FunCall
     ((Ident "f",
       None,
       [({str = "f"; charpos = -10},
         (Ast_cocci.CONTEXT (Ast_cocci.NOTHING), []))]),
      [(Left
	  (Constant (Int "1"),
           None,
           [({str = "1"; charpos = -10},
             (Ast_cocci.CONTEXT (Ast_cocci.NOTHING), []))]),
        [])]),
   None,
   [({str = "("; charpos = -10},
     (Ast_cocci.CONTEXT (Ast_cocci.NOTHING), []));
     ({str = ")"; charpos = -10},
      (Ast_cocci.CONTEXT (Ast_cocci.NOTHING), []))])
    
let _ = assert (al_expr ex1 = ex1final)
