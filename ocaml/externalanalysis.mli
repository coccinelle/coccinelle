(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Int64Set : Set.S with type elt = int64

type bound = int64 option

type result =
    IntSet of Int64Set.t
  | IntBounds of bound * bound
  | Other of string

val show_bound : bound -> string
val show_result : result -> string

val load_external_results : string -> unit
val find_results : Common.filename -> Ast_c.posl -> Ast_c.posl -> result list

val intersect_results : result -> result -> result option

val satisfy :
  (result list -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val satisfy1 :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool

val has_any_result : Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val for_all :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val for_all1 :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val exists :
  (result -> bool) -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool

val single_int : int64 -> result -> bool
val contains_int : int64 -> result -> bool
val has_only_nul : Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val has_also_nul : Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
val has_also_int : int64 -> Common.filename -> Ast_c.posl -> Ast_c.posl -> bool
