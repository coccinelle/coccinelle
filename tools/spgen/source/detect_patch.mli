(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

(* Detects if a rule contains */+/- or not. Such a rule is here referred to as
 * a patch rule (includes also * rules).
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

type t

(* constructs t from AST0 rule, uses both minus and plus tree. *)
val make : Ast0_cocci.parsed_rule -> t

(* constructs t from statement dots.
 * NOTE: minus and plus slices are stored in separate AST0s, so the statement
 * dots here will only cover either the */- portion of the tree OR the
 * + portion of the tree. Ie. a call to is_patch will only return true if the
 * statement dots contain EITHER */- or +, depending on which AST0 it is from.
 *)
val make_statement_dots : Ast0_cocci.statement Ast0_cocci.dots -> t

(* returns true if the rule contains stars, pluses, or minuses. *)
val is_patch : t -> bool

(* get the patch map for the disjunction starting on given line number.
 * fails if no disjunction found on that line.
 *)
val get_disj_patch : int -> t -> bool list

(* returns only the rules that contained */+/- along with their disjunction
 * maps. Preserves order.
 *)
val filter_patch_rules :
  Ast0_cocci.parsed_rule list ->
  (Ast0_cocci.parsed_rule * t) list
