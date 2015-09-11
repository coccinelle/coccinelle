(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Encapsulates state variables to keep track of internal state during
 * context rule generation.
 *
 * Contains the generated rule, current line number, mode, positions.
 *)

(* ------------------------------------------------------------------------- *)
(* TYPE *)
type t

(* Constructor *)
val make: disj_map:Detect_patch.t -> t


(* ------------------------------------------------------------------------- *)
(* MODE AND LINE FUNCTIONS *)

(* Change the mode of the current line *)
val set_mode_star: arity:Ast0_cocci.arity -> t -> t
val set_mode_context: arity:Ast0_cocci.arity -> t -> t

(* Increase line number, set the mode of the new line to context. *)
val inc_line: t -> t

(* Increase line number, but only if current mode is star. *)
val inc_star: t -> t

(* Increase line number if rule line number exceeds current rule line. *)
val skip: rule_line:int -> t -> t


(* ------------------------------------------------------------------------- *)
(* ADDING VALUE FUNCTIONS *)

(* Add string value to the current line in the generated rule *)
val add: string -> t -> t

(* Add value, change the arity. *)
val add_with_arity: string -> Ast0_cocci.arity -> t -> t


(* ------------------------------------------------------------------------- *)
(* DISJUNCTION FUNCTIONS *)

(* start the disjunction rule *)
val init_disj_result : t -> t

(* get the disjunction map *)
val get_disj : int -> t -> bool list

(* set disjunction mode, in which everything added to the normal generated
 * rule is added to the disjunction rule as well. *)
val set_disj_mode : bool -> t -> t


(* ------------------------------------------------------------------------- *)
(* POSITION FUNCTIONS *)

(* freeze the position while executing the passed function *)
val do_freeze_pos: (t -> t) -> t -> t

(* Returns the numbered name of the new pos and the modified t *)
val add_position: t -> (string * t)


(* ------------------------------------------------------------------------- *)
(* NO GEN FUNCTIONS *)

(* flag for not generating positions *)
val set_no_gen : bool -> t -> t
val no_gen: t -> bool

(* enter whencode nest while executing the passed function *)
val do_whencode: (t -> t) -> t -> t


(* ------------------------------------------------------------------------- *)
(* GETTERS *)

val get_positions: t -> string list
val get_result: t ->
  string list (* main rule *) *
  string list option (* generated disj rule *)
