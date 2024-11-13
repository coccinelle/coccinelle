(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)


val pp_ctl:
 ('pred -> unit) * ('mvar -> unit) -> bool (* inline_let_def *) ->
 ('pred, 'mvar, 'info) Ast_ctl.generic_ctl -> unit
