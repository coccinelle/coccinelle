type typeC = 
    ConstVol        of const_vol * typeC
  | BaseType        of baseType * sign option
  | Pointer         of typeC
  | Array           of typeC (* drop size info *)
  | StructUnionName of string * structUnion
  | TypeName        of string
  | MetaType        of string
  | Unknown (* for metavariables of type expression *^* *)

and tagged_string = string
     
and baseType = VoidType | CharType | ShortType | IntType | DoubleType
| FloatType | LongType | BoolType

and structUnion = Struct | Union

and sign = Signed | Unsigned

and const_vol = Const | Volatile

(* --------------------------------------------------------------------- *)
(* Printer *)
	
let rec typeC = function
    ConstVol(cv,ty) -> const_vol cv; typeC ty
  | BaseType(ty,None) -> baseType ty
  | BaseType(ty,Some sgn) -> sign sgn; baseType ty
  | Pointer(ty) -> typeC ty; print_string "*"
  | Array(ty) -> typeC ty; print_string "[] "
  | StructUnionName(name,kind) ->
      structUnion kind; print_string name; print_string " "
  | TypeName(name) -> print_string name; print_string " "
  | MetaType(name) -> print_string name; print_string " "
  | Unknown -> print_string "unknown "

and baseType = function
    VoidType -> print_string "void "
  | CharType -> print_string "char "
  | ShortType -> print_string "short "
  | IntType -> print_string "int "
  | DoubleType -> print_string "double "
  | FloatType -> print_string "float "
  | LongType -> print_string "long "
  | BoolType -> print_string "bool "

and structUnion = function
    Struct -> print_string "struct "
  | Union -> print_string "union "

and sign = function
    Signed -> print_string "signed "
  | Unsigned -> print_string "unsigned "

and const_vol = function
    Const -> print_string "const "
  | Volatile -> print_string "volatile "
