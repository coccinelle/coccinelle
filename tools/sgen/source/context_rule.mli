(* Generate context rule
 *
 * If context_mode (ie. the original rule is already * a context rule), just
 * add positions, but let the stars be.
 * If patch mode, add positions AND stars, disregard -/+.
 *)

(* ------------------------------------------------------------------------- *)
type t

(* generates a context rule from a */+/- rule.
 * Invariants: rule contains */+/-. new_name, if any, must be valid.
 *
 * Returns the generated context rule and list of added metapositions
 * (at least one, and inherited from the generated rule for convenience).
 *)
val generate :
  new_name:string option -> (* Some <name> if rule is nameless, else None*)
  disj_map:bool list Common.IntMap.t -> (* disjunction map!! *)
  rule:Ast0_cocci.parsed_rule -> (* the original rule, must be a */+/- rule *)
  context_mode:bool ->
  t * Meta_variable.t list

(* prints the generated context rule to the specified out_channel. *)
val print : out_channel -> t -> unit
