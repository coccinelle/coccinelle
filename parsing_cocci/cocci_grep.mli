(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val interpret :
    (Str.regexp * Str.regexp list) (*pattern*) -> string (*filename*) -> bool

val split : string list list -> string list list
