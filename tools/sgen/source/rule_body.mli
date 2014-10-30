(* Generates context rule body with inserted positions and stars.
 *
 * If context_mode (ie. the original rule is already * a context rule), just
 * add positions, but let the stars be.
 * If patch mode, add positions structurally (see position_generator.ml) and
 * add stars where the minus transformation are OR where the positions were
 * generated.
 *
 *)

(* ------------------------------------------------------------------------- *)
(* RULE BODY FUNCTIONS *)

type t

(* Input:
 * * disj_map that maps disjunctions (represented by the line number they
 *   start on) to a list of bools indicating whether each case contains */+/-.
 *   Default is empty (ie. no disjunctions).
 * * context_mode, indicates whether the rule already has stars in it.
 * * the name of the rule and the AST0 for the rule.
 *
 * Returns:
 * * List of added metapositions (as strings)
 * * The generated context rule, optional generated disjunction rule
 *)
val generate :
  ?disj_map: bool list Common.IntMap.t ->
  ?context_mode: bool ->
  rule_name: string ->
  Ast0_cocci.rule ->
  string list * (t * t option)

(* prints a context rule *)
val print : out_channel -> t -> unit
