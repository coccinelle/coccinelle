type typeC = 
    ConstVol        of const_vol * typeC
  | BaseType        of baseType * sign option
  | Pointer         of typeC
  | Array           of typeC (* drop size info *)
  | StructUnionName of structUnion * string
  | TypeName        of string
  | MetaType        of string
  | Unknown (* for metavariables of type expression *^* *)

and tagged_string = string
     
and baseType = VoidType | CharType | ShortType | IntType | DoubleType
| FloatType | LongType | BoolType

and structUnion = Struct | Union

and sign = Signed | Unsigned

and const_vol = Const | Volatile

val typeC : typeC -> unit

val compatible : typeC -> typeC option -> bool
