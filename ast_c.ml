open Common open Commonop
(*******************************************************************************)
(* 
  could have more precise type in fullType, in expression, etc
    but it requires to do too much things in parsing (checking no conflicting structname, computing value, ...)
    better to separate concern, so i put '=>' to mean what we would really like, in fact
    what we really like is a fullType2, after all many stuff are just sugar
  
  inv: Array and FunctionType have also typeQualifier but they dont have any sense.
   I put this to factorise some code, if you see the grammar, you see that we can never specify const
   for the array himself (but we can do it for pointer)
*)

(*
 because of ExprStatement,  can have more 'new scope',  but rare I think
  for instance array of constExpression => possibly an exprstatement and a new (local) struct defined
 same for Constructor
*)
(*******************************************************************************)

type info = Common.parse_info  (* forunparser: *)
type il = info list

let iitodovide = []

(*******************************************************************************)
type fullType = typeQualifier * typeC
and  typeC = typeCbis * il (* forunparser: *)

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


and typeQualifier = typeQualifierbis * il (* forunparser: *)
and typeQualifierbis = {const: bool; volatile: bool}







and expression = expressionbis * il (* forunparser: *)
and expressionbis = 
  | Constant       of constant                                  
  | FunCall        of expression * (((expression, (fullType * (storage * il))) either) * il) list (* cppext: normmally just   expression * expression list *)
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
                  | Ident  of (string)             (* can be a enumeration constant, or a simple variable (or name of a func) *)
         and isWchar = IsWchar | IsChar

     and  unaryOp  = GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not
     and  assignOp = SimpleAssign | OpAssign of arithOp
     and  fixOp    = Dec | Inc

     and  binaryOp = Arith of arithOp | Logical of logicalOp
          and  arithOp   = Plus | Minus | Mul | Div | Mod |        DecLeft | DecRight |       And | Or | Xor
          and  logicalOp = Inf | Sup | InfEq | SupEq |       Eq | NotEq |           AndLog | OrLog

 and constExpression = expression (* => int *)




(* note: that assignement is not a statement but an expression; wonderful C langage *)
(* note: I use  'and' for type definition cos gccext allow statement as expression, so need mutual recursive type definition *)
and statement = statementbis * il (* forunparser: *)
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

  and compound = (*  old: (declaration list * statement list) *)
                  ((declaration, statement) either) list (* cppext: (or because of cpp ...) *)

  and exprStatement = (expression option)

  and selection     = If     of expression * statement * statement        
                    | Switch of expression * statement

  and iteration     = While   of expression * statement
		    | DoWhile of statement * expression
		    | For     of (exprStatement * il) * (exprStatement * il) * (exprStatement * il) * statement

  and jump  = Goto of string
            | Continue | Break 
            | Return   | ReturnExpr of expression






(* (string * ...) option cos can have empty declaration or struct tag declaration 
   .  
   before I had Typedef constructor, but why make this special case and not have StructDef, EnumDef, ...
    so that struc t {...} v will generate 2 declaration ? So I try to generalise and not have
    not Typedef too, this require more work in parsing. Better to separate concern.
   .
   Before unparser, I didn't have a DeclList but just a Decl.
*)
and declaration = DeclList of ((((string * ((initialiser * info) option) * (info)) option) * fullType * storage) * il) list * (il * info) (* ilsto, infoptvirg *)
     and storage       = NoSto | StoTypedef | Sto of storageClass
     and storageClass  = Auto  | Static | Register | Extern

     and initialiser = initialiserbis * il
       and initialiserbis = InitExpr of expression | InitList of (initialiser * il) list 
                          | InitGcc of string * initialiser  (* gccext: *)
                          | InitGccIndex of expression * initialiser
                          | InitGccRange of expression * expression * initialiser


type definition = (string * functionDefType * storage * compound                               * (info * il * il))
         and functionDefType = fullType * ((parameterTypeDef * il) list) * bool (* has... *)   * (il * il)
             and parameterTypeDef = (bool (* hasregister *) * string * fullType                * (il * info))


type program = programElement list
     and programElement = Declaration of declaration
                        | Definition of definition
                        | EmptyDef of il  (* gccext: allow redundant ; *)
                        | SpecialDeclMacro of string * (((expression, (fullType * (storage * il))) either) * il) list   *  (il) (* cppext: *)

                        | NotParsedCorrectly of il
                        | FinalDef of info


         (* before i had functionDefType = functionDeclType, and i said that 
            "in old C rule, you can just say name, and then state
             type, but normally after function check, we must transform it so that 
             we dont have this anymore"
             but i prefer have a clean abstractSyntax type, prototype and definition 
             are different, they dont
             have the same constraint, so i do more job in parsing for this case only
	 *)



(*******************************************************************************)

type info_item = (filename * (pos_file *  pos_file) * string * il)

type program2 = programElement2 list
     and programElement2 = programElement * info_item



(*******************************************************************************)
let nullQualif = ({const=false; volatile= false}, [])
let nQ = nullQualif 

let default_int = (BaseType (IntType (Si (Signed, CInt))))

(*******************************************************************************)

