(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

(* could be in pretty_print_c because dependent of ast_c but metavars
 * are in ast_c for "bad" reason, so better put the pretty_print
 * of metavars here
 *)

val pp_binding_kind : Ast_c.metavar_binding_kind -> unit
val pp_binding : Ast_c.metavars_binding -> unit

val pp_binding_kind2 : Lib_engine.metavar_binding_kind2 -> unit
val pp_binding2_ctlsubst :
    (Lib_engine.mvar, Lib_engine.metavar_binding_kind2)
    Ast_ctl.generic_substitution ->
    unit
val pp_predicate : Lib_engine.predicate -> unit

val pp_ctlcocci :
  bool (* show_plus *) -> bool (* inline_let *) -> Lib_engine.ctlcocci -> unit
