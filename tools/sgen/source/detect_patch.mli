(* Detects if a rule contains */+/- or not.
 * Does not distinguish between whether it has *, +, or -, just returns true
 * if it contains either.
 *
 * Also generates disjunction map, a map that maps the line number of the
 * beginning of the disjunction to an ordered list of bools, each indicating
 * whether their corresponding disjunction case contains */+/-.
 *
 * ----------------------------------------------------------------------------
 * Example of disjunction map:
 *
 *      @@ expression e; @@
 *
 *      (
 *       f(0)
 *      |
 *       f(1)
 *      |
 *      - f(e)
 *      + g(e)
 *      )
 *
 * This rule contains both +/- so it would return true on a call to detect.
 *
 * It also has one disjunction on <line number of opening parenthesis> so the
 * disjunction map would have one entry with
 * key = <line number>
 * value = [false;false;true;].
 *
 *)

(* ------------------------------------------------------------------------- *)
(* DETECTION FUNCTIONS *)

(* detects whether a rule contains */+/- *)
val detect :
  Ast0_cocci.parsed_rule ->
  bool * bool list Common.IntMap.t

(* returns true if the statement dots contain minus or plus code *)
val detect_statement_dots :
  Ast0_cocci.statement Ast0_cocci.dots ->
  bool * bool list Common.IntMap.t

(* returns only the rules that contained */+/- along with their disjunction
 * maps. Preserves order.
 *)
val get_patch_rules :
  Ast0_cocci.parsed_rule list ->
  Ast0_cocci.parsed_rule list * (bool list Common.IntMap.t) list
