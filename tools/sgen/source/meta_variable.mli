(* Extract all metavariables used in a rule. We can't really use the metavars
 * returned by the parser since a lot of them are missing.
 *)

(* ------------------------------------------------------------------------- *)
(* META_VARIABLE FUNCTIONS *)

type t

(* Given the minus abstract syntax tree (Ast0) for a rule, extracts all
 * metavariables used in the rule.
 * Rulename used to determine whether the metavariables are inherited or not.
 *)
val unparse : minus:Ast0_cocci.rule -> rulename:string -> t list

(* constructor *)
val make_metavar :
  ?rulename:string ->
  ?constraints:string ->
  ?typ:string ->
  string (* metavariable name *) -> t

(* getters *)
val get_rule : t -> string
val get_name : t -> string

(* forces inheritance if the metavar is not already inherited *)
val inherit_rule : new_rule:string -> t list -> t list

(* prints the metavariables in the format used in rule headers to out_channel.
 * if do_group, group by type *)
val print : out_channel -> do_group:bool -> t list -> unit
