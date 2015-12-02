(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

type inherited = bool (* true if inherited *)
type keep_binding = Unitary (* need no info *)
  | Nonunitary (* need an env entry *) | Saved (* need a witness *)

type meta_name = string * string (*Ast_cocci.meta_name*)

type typeC =
    ConstVol        of const_vol * typeC
  | BaseType        of baseType
  | SignedT         of sign * typeC option
  | Pointer         of typeC
  | FunctionPointer of typeC (* only return type *)
  | Array           of typeC (* drop size info *)
  | Decimal         of name * name
  | EnumName        of name
  | StructUnionName of structUnion * name
  | TypeName        of string
  | MetaType        of meta_name * keep_binding * inherited
  | Unknown (* for metavariables of type expression *^* *)

and name =
    NoName
  | Name of string
  | Num of string
  | MV of meta_name * keep_binding * inherited

and tagged_string = string

and baseType = VoidType | CharType | ShortType | ShortIntType | IntType
| DoubleType | LongDoubleType | FloatType
| LongType | LongIntType | LongLongType | LongLongIntType
| SizeType | SSizeType | PtrDiffType | BoolType

and structUnion = Struct | Union

and sign = Signed | Unsigned

and const_vol = Const | Volatile

val type2c : typeC -> string
val typeC : typeC -> unit

val compatible : typeC -> typeC option -> bool

val meta_names : typeC -> meta_name list
