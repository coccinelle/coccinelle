(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
* This file is part of Coccinelle.
* 
* Coccinelle is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, according to version 2 of the License.
* 
* Coccinelle is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
* 
* The authors reserve the right to distribute this or future versions of
* Coccinelle under other licenses.
*)


(* for metavariables in general, but here because needed for metatypes *)
type inherited = bool (* true if inherited *)
type keep_binding = Unitary (* need no info *)
  | Nonunitary (* need an env entry *) | Saved (* need a witness *)

type typeC = 
    ConstVol        of const_vol * typeC
  | BaseType        of baseType * sign option
  | Pointer         of typeC
  | FunctionPointer of typeC (* only return type *)
  | Array           of typeC (* drop size info *)
  | StructUnionName of structUnion * bool (* true if a metaId *) * string
  | TypeName        of string
  | MetaType        of (string * string) * keep_binding * inherited
  | Unknown (* for metavariables of type expression *^* *)

and tagged_string = string
     
and baseType = VoidType | CharType | ShortType | IntType | DoubleType
| FloatType | LongType | BoolType

and structUnion = Struct | Union

and sign = Signed | Unsigned

and const_vol = Const | Volatile

(* --------------------------------------------------------------------- *)
(* Printer *)
open Format 
	
let rec typeC = function
    ConstVol(cv,ty) -> const_vol cv; typeC ty
  | BaseType(ty,None) -> baseType ty
  | BaseType(ty,Some sgn) -> sign sgn; baseType ty
  | Pointer(ty) -> typeC ty; print_string "*"
  | FunctionPointer(ty) -> typeC ty; print_string "(*)(...)"
  | Array(ty) -> typeC ty; print_string "[] "
  | StructUnionName(kind,mv,name) ->
      structUnion kind; print_string name; print_string " "
  | TypeName(name) -> print_string name; print_string " "
  | MetaType((rule,name),keep,inherited) ->
      print_string "<"; print_string name; print_string ">"; print_string " ";
      (*
      let print_unitary = function
	  Unitary -> print_string "unitary"
	| Nonunitary -> print_string "nonunitary"
	| Saved -> print_string "saved" in
      print_string "/* ";
      print_string "keep:"; print_unitary keep;
      print_string " inherited:"; print_bool inherited;
      print_string " */"
      *)
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

(* t1 should be less informative than t1, eg t1 = Pointer(Unknown) and t2 =
Pointer(int) *)
(* only used in iso *)
(* needs to do something for MetaType *)
let compatible t1 = function
    None -> t1 = Unknown
  | Some t2 ->
      let rec loop = function
	  (Unknown,_) -> true
	| (ConstVol(cv1,ty1),ConstVol(cv2,ty2)) when cv1 = cv2 ->
	    loop(ty1,ty2)
	| (Pointer(ty1),Pointer(ty2)) -> loop(ty1,ty2)
	| (FunctionPointer(ty1),_) -> false (* not enough info *)
	| (_,FunctionPointer(ty2)) -> false (* not enough info *)
	| (Array(ty1),Array(ty2)) -> loop(ty1,ty2)
	| (_,_) -> t1=t2 in
      loop (t1,t2)

