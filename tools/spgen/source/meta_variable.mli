(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

(* Extract all metavariables used in a rule. We can't really use the metavars
 * returned by the parser since a lot of them are missing.
 * This also means that metavariables declared in the header but unused in the
 * rule are not included.
 *)

(* ------------------------------------------------------------------------- *)
(* META_VARIABLE FUNCTIONS *)

type t

(* make a metavariable.
 *  - inherit_rule: if the metavariable is declared in a different rule than it
 *    appears in, set this to that rule's name. Default is "" (not inherited).
 *  - constraints: e.g "!=0" (default to "").
 *  - typ: type of the metavariable.
 *)
val make :
  ?inherit_rule:string ->
  ?constraints:string ->
  typ:string ->
  string (* metavariable name *) ->
  t

(* getters *)
val get_rule : t -> string
val get_name : t -> string
val tostring_mv : t -> string

(* adds inheritance of new_rule to the metavar.
 * if force is false (default), inheritance is only added if the metavar is not
 * already inherited. Otherwise, forcibly overwrite inheritance.
 *)
val inherit_rule : ?force:bool -> new_rule:string -> t -> t

(* prints the metavariable in the format used in rule headers to out_channel *)
val print : out_channel -> t -> unit

(* prints the metavariables in the format used in rule headers to out_channel.
 * if do_group, group by type.
 *)
val print_list : out_channel -> do_group:bool -> t list -> unit

(* Given the minus abstract syntax tree (Ast0) for a rule, extracts all
 * metavariables used in the rule.
 * Rulename used to determine whether the metavariables are inherited or not.
 *)
val extract : minus_rule:Ast0_cocci.rule -> rule_name:string -> t list
