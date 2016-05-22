(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast = Ast_cocci

(* ------------------------------------------------------------------------- *)
(* TOSTRING FUNCTIONS FOR AST_COCCI BASE TYPES *)

(* takes a list of 'a and concatenates it using fn ('a -> string),
 * delimitering with between (string)
 *)
let between_tostring between tostring_fn =
  let rec between_tostring' acc between fn = function
    | [] -> acc
    | [x] -> acc ^ (fn x)
    | x::xs -> between_tostring' (acc ^ (fn x) ^ between) between fn xs in
  between_tostring' "" between tostring_fn

let meta_tostring (r, x) = x

let constant_tostring = function
  | Ast.String(s) -> "\"" ^ s ^ "\""
  | Ast.Char(s) -> "'" ^ s ^ "'"
  | Ast.Int(s) -> s
  | Ast.Float(s) -> s
  | Ast.DecimalConst(s,_,_) -> s

let struct_union_tostring = Ast.string_of_structUnion

let sign_tostring = function
  | Ast.Signed -> "signed"
  | Ast.Unsigned -> "unsigned"

let const_vol_tostring = function
  | Ast.Const -> "const"
  | Ast.Volatile -> "volatile"

let storage_tostring = function
  | Ast.Static -> "static"
  | Ast.Auto -> "auto"
  | Ast.Register -> "register"
  | Ast.Extern -> "extern"

let inc_elem_tostring = function
  | Ast.IncPath s -> s
  | Ast.IncDots -> "..."

let inc_file_tostring = function
  | Ast.Local(elems) ->
      "\"" ^ (between_tostring "/" inc_elem_tostring elems) ^ "\""
  | Ast.NonLocal(elems) ->
      "<" ^ (between_tostring "/" inc_elem_tostring elems) ^ ">"
  | Ast.AnyInc -> "..."

let fix_tostring = function
  | Ast.Dec -> "--"
  | Ast.Inc -> "++"

let arith_tostring = Ast.string_of_arithOp

let logic_tostring = Ast.string_of_logicalOp

let unary_tostring = function
  | Ast.GetRef -> "&"
  | Ast.GetRefLabel -> "&&"
  | Ast.DeRef -> "*"
  | Ast.UnPlus -> "+"
  | Ast.UnMinus -> "-"
  | Ast.Tilde -> "~"
  | Ast.Not -> "!"

let binary_tostring = Ast.string_of_binaryOp

let assign_tostring = Ast.string_of_assignOp

let type_tostring = Ast.string_of_baseType

let whenmodifier_tostring = function
  | Ast.WhenAny -> "any"
  | Ast.WhenStrict -> "strict"
  | Ast.WhenForall -> "forall"
  | Ast.WhenExists -> "exists"
