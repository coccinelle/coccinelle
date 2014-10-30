(* Detects if a rule contains */+/- or not.
 * Does not distinguish between whether it has *, +, or -, just returns true
 * if it contains either.
 *
 * Also generates disjunction map, a map that maps the line number of the
 * beginning of the disjunction to an ordered list of bools, each indicating
 * whether their corresponding disjunction case contains */+/-.
 *
 * Example:
 *   (
 *    f(0)
 *   |
 *    f(1)
 *   |
 *   - f(e)
 *   + g(e)
 *   )
 * would have as key the line number of the opening parenthesis and as value
 * the list [false;false;true;].
 *
 *)

(* ------------------------------------------------------------------------- *)
(* DETECTION FUNCTIONS *)

(* detects whether a rule contains */+/- *)
val detect : Ast0_cocci.parsed_rule -> bool * bool list Common.IntMap.t

(* returns true if the statement dots contain minus or plus code *)
val detect_statement_dots : Ast0_cocci.statement Ast0_cocci.dots -> bool

(* returns only the rules that contained */+/- along with their disjunction
 * maps. Preserves order. *)
val get_patch_rules :
  Ast0_cocci.parsed_rule list ->
  Ast0_cocci.parsed_rule list * (bool list Common.IntMap.t) list
