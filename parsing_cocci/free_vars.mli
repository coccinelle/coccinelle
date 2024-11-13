(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

(* Used after things can only have one binding.  Positions can have many
bindings.  These are combined in ctlcocciintegration, ie after the CTL
generation. *)

val collect_all_refs : Ast_cocci.meta_name list Visitor_ast.combiner

val free_vars : Ast_cocci.rule_with_metavars list ->
  string list (* dropped rules *) ->
  (Ast_cocci.metavar list list) * (Ast_cocci.rule list) *
    (((Ast_cocci.meta_name list) list) list) (*fvs of the rule*) *
    (((Ast_cocci.meta_name list * Ast_cocci.meta_name list)
	list) list) (*negated position vars*) *
    ((((Ast_cocci.meta_name list) list) list) (*used after list*) *
       (((Ast_cocci.meta_name list) list) list) (*fresh used after list*) *
       (((Ast_cocci.meta_name list) list)list)(*fresh used after list seeds*))*
    (((Ast_cocci.meta_name list) list) list) (*positions list*)
