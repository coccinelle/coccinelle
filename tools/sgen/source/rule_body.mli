(* Generates context rule body with inserted positions and stars.
 *
 * If context_mode (ie. the original rule is already * a context rule), just
 * add positions, but let the stars be.
 * If patch mode, add positions structurally (see position_generator.ml) and
 * add stars where the minus transformation are OR where the positions were
 * generated.
 *
 * ----------------------------------------------------------------------------
 * Example: see context_rule.mli (basically does that, without the headers)
 *)

(* ------------------------------------------------------------------------- *)
(* RULE BODY FUNCTIONS *)

type t

(* Input:
 * * disjunction map, indicates */+/- slices in disjunctions
 * * context_mode, indicates whether the rule already has stars in it.
 * * the name of the rule
 * * the AST0 for the minus rule (we don't need the plus rule to generate *'s).
 *
 * Returns:
 * * List of added metapositions (as strings)
 * * The generated context rule, optional generated disjunction rule
 *)
val generate :
  disj_map:Detect_patch.t ->
  context_mode:bool ->
  rule_name:string ->
  minus_rule:Ast0_cocci.rule ->
  string list * (t * t option)

(* prints a context rule *)
val print : t -> out_channel -> unit
