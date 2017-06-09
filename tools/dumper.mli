(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Dump an OCaml value into a printable string.
 * By Richard W.M. Jones (rich@annexia.org).
 * dumper.mli 1.1 2005/02/03 23:07:47 rich Exp
 *)

val dump : 'a -> string
