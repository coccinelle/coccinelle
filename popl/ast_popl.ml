(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

type sequence =
    Seq of element * sequence
  | Empty
  | SExists of Ast_cocci.meta_name * sequence

and element =
    Term of Ast_cocci.rule_elem
  | Or of sequence * sequence
  | DInfo of dots * element list (* before *) * element list (* after *)
  | EExists of Ast_cocci.meta_name * element

and dots =
    Dots
  | Nest of sequence
  | When of dots * sequence
  | DExists of Ast_cocci.meta_name * dots
