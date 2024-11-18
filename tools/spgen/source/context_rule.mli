(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

(* Generate context rule
 *
 * If context_mode (ie. the original rule is already * a context rule), just
 * add positions, but let the stars be.
 *
 * If patch mode, add positions.
 *  - If rule has -, then put the *'s at the -'s place.
 *  - If rule only has +, put the *'s where the added positions are.
 *
 * ----------------------------------------------------------------------------
 * Example of generated context rule (see more in rule_body.mli):
 * Input rule is a "+" rule represented as an AST0.
 *
 *      @some_rule@
 *      identifier i;
 *      @@
 *
 *      int i = 2;
 *      + call_me(i);
 *      ...
 *
 * Then the output will be a "*" rule similar to the one below.
 * It is not returned as AST0 because the contents of the rule may require a
 * splitting into several rules.
 *
 *      @some_rule_context depends on !patch && (context || org || report)@
 *      identifier i;
 *      position p;
 *      @@
 *
 *      * int i@p = 2;
 *      ...
 *)

(* ------------------------------------------------------------------------- *)
type t

(* generates a context rule from a */+/- rule.
 * Invariants:
 *  - rule contains */+/-.
 *  - new_name, if any, must be valid. no whitespace funny business
 *    (however, it can be <default rule name><number> at this point).
 *
 * Arguments:
 *  - context_mode: if true, input rule has *, else input rule has +/-.
 *  - disj_map: disjunction map, indicates */+/- slices in disjunctions.
 *  - new_name: new name if input rule is nameless, else same as name in rule.
 *  - rule_names: list of names of the other */+/- rules in the full script.
 *  - rule: the input rule, must be a */+/- rule.
 *
 * Returns:
 *  - the generated context rule
 *  - list of added metapositions (at least one, and inherited from the
 *    generated rule for convenience).
 *)
val generate :
  context_mode:bool ->
  disj_map:Detect_patch.t ->
  new_name:string ->
  rule_names:string list ->
  rule:Ast0_cocci.parsed_rule ->
  t * Meta_variable.t list

(* prints the generated context rule to the specified out_channel. *)
val print : out_channel -> t -> unit
