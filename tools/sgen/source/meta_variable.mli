(* Extract all metavariables used in a rule. We can't really use the metavars
 * returned by the parser since a lot of them are missing.
 *)

(* ------------------------------------------------------------------------- *)
(* META_VARIABLE FUNCTIONS *)

type t

(* constructor, constraints are optional (default to "")
 * NOTE: rule_name is only for inherited rules, ie. akin to "rulename.mv". If
 * the metavariable is in local scope, set rule_name to "".
 *)
val make :
  ?constraints:string ->
  typ:string ->
  rule_name:string ->
  string (* metavariable name *) ->
  t

(* getters *)
val get_rule : t -> string
val get_name : t -> string

(* forces inheritance if the metavar is not already inherited *)
val inherit_rule : t -> new_rule:string -> t

(* prints the metavariable in the format used in rule headers to out_channel *)
val print : t -> out_channel -> unit

(* prints the metavariables in the format used in rule headers to out_channel.
 * if do_group, group by type
 *)
val print_list : t list -> out_channel -> do_group:bool -> unit

(* Given the minus abstract syntax tree (Ast0) for a rule, extracts all
 * metavariables used in the rule.
 * Rulename used to determine whether the metavariables are inherited or not.
 *)
val unparse : minus_rule:Ast0_cocci.rule -> rulename:string -> t list
