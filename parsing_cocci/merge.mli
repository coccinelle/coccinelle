(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val do_merge :
    Ast0_cocci.rule ->
      (Ast_cocci.anything * int * int * int * int) list list list ->
      unit (* updates Ast0_cocci.rule argument *)
