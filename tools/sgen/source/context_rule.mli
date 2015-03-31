(* Generate context rule
 *
 * If context_mode (ie. the original rule is already * a context rule), just
 * add positions, but let the stars be.
 *
 * If patch mode, add positions.
 *   - If rule has -, then put the *'s at the -'s place.
 *   - If rule only has +, put the *'s where the added positions are.
 *
 * ----------------------------------------------------------------------------
 * Example of generated context rule:
 * Input rule is a "+" rule represented as an AST0.
 *
 * @some_rule@
 * identifier i;
 * @@
 *
 * i = 2;
 * + call_me(i);
 * ...
 *
 * ----------------------------------------------------------------------------
 * Then the output will be a "*" rule similar to the one below.
 * It is not returned as AST0 because the contents of the rule may require a
 * splitting into several rules.
 *
 * @some_rule_context depends on !patch && (context || org || report)@
 * identifier i;
 * position p;
 * @@
 *
 * * i@p = 2;
 * ...
 *
 *)

(* ------------------------------------------------------------------------- *)
type t

(* generates a context rule from a */+/- rule.
 * Invariants: rule contains */+/-. new_name, if any, must be valid.
 *
 * new_name: Some <name> if input rule is nameless, else None
 * disj_map: disjunction map. TODO: description
 * rule: the input rule, must be a */+/- rule
 * context_mode: if true, input rule has *, else input rule has +/-
 *
 * Returns the generated context rule and list of added metapositions
 * (at least one, and inherited from the generated rule for convenience).
 *)
val generate :
  new_name:string option ->
  disj_map:bool list Common.IntMap.t ->
  rule:Ast0_cocci.parsed_rule ->
  context_mode:bool ->
  t * Meta_variable.t list

(* prints the generated context rule to the specified out_channel. *)
val print : t -> out_channel -> unit
