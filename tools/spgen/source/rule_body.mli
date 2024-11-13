(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

(* Generates context rule body with inserted positions and stars.
 *
 * If context_mode (ie. the original rule is already * a context rule), just
 * add positions, but let the stars be.
 * If patch mode, add positions structurally (see position_generator.ml) and
 * add stars where the minus transformation are OR where the positions were
 * generated.
 *
 * ----------------------------------------------------------------------------
 * Example:
 * Say we have two rule bodies like this (rule headers included for context):
 *
 *      @minus@ expression e; @@
 *
 *      if (e
 *      - != NULL
 *       ) {
 *      function1(e);
 *      ...
 *      e++;
 *      function2();
 *      - function3(e);
 *      + function4(e);
 *
 *      @plus@ expression e; @@
 *
 *      if (e
 *      + != NULL
 *       ) {
 *      function1(e);
 *      ...
 *      e++;
 *      function2();
 *      + function3(e);
 *
 * They are both patch rules, but one doesn't contain minuses.
 * In the first case, stars are added where the minuses were. Positions are
 * added according to the same heuristics as the other example.
 *
 *      @minus_context@ expression e; position p1,p2,p2; @@
 *
 *      if (e@p1
 *      * != NULL
 *       ) {
 *        function1@p2(e);
 *        ...
 *        e@p3++;
 *        function2();
 *      * function3(e);
 *      }
 *
 * In the second case, we don't know where the interesting parts are, so we add
 * positions on a best-guess basis (ie. the first in a sequence). The stars are
 * added on the same lines as the positions.
 *
 *      @plus_context@ expression e; position p1,p2,p3; @@
 *
 *      * if (e@p1)
 *      {
 *      * function1@p2(e);
 *        ...
 *      * e@p3++;
 *        function2();
 *      }
 *
 *)

(* ------------------------------------------------------------------------- *)
(* RULE BODY FUNCTIONS *)

type t

(* Arguments:
 *  - context_mode indicates whether the rule already has stars in it.
 *  - disjunction map, indicates */+/- slices in disjunctions.
 *  - AST0 for the minus rule (we don't need the plus rule to generate *'s).
 *
 * Returns:
 *  - List of added metapositions (as strings)
 *  - The generated context rule, optional generated disjunction rule
 *)
val generate :
  context_mode:bool ->
  disj_map:Detect_patch.t ->
  minus_rule:Ast0_cocci.rule ->
  string list * (t * t option)

(* prints a context rule *)
val print : out_channel -> t -> unit
