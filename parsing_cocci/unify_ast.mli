(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

type res = NO | MAYBE

val unify_statement_dots :
    Ast_cocci.statement Ast_cocci.dots ->
      Ast_cocci.statement Ast_cocci.dots -> res
