(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val context_neg :
    Ast0_cocci.rule -> Ast0_cocci.rule ->
      (Ast0_cocci.top_level * Ast0_cocci.top_level) list

val minus_table :
    (int list, Ast0_cocci.anything * int Common.set list) Hashtbl.t
val plus_table :
    (int list, Ast0_cocci.anything * int Common.set list) Hashtbl.t
