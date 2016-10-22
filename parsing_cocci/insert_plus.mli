(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val insert_plus : Ast0_cocci.rule -> Ast0_cocci.rule -> bool -> unit
(* bool is true if no isos *)
