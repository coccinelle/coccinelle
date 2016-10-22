(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val compute_lines : bool -> Ast0_cocci.rule -> Ast0_cocci.rule

val compute_statement_dots_lines : bool ->
    Ast0_cocci.statement Ast0_cocci.dots ->
      Ast0_cocci.statement Ast0_cocci.dots

val compute_statement_lines :
    bool -> Ast0_cocci.statement -> Ast0_cocci.statement
