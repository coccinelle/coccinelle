(* Encapsulates state variables to keep track of internal state during
 * context rule generation.
 *
 * Contains the generated rule, current line number, mode, positions.
 *)

(* ------------------------------------------------------------------------- *)
(* TYPE *)
type snapshot

(* Constructor *)
val snap: disj_map:bool list Common.IntMap.t -> snapshot


(* ------------------------------------------------------------------------- *)
(* MODE AND LINE FUNCTIONS *)

(* Change the mode of the current line *)
val set_mode_star: arity:Ast0_cocci.arity -> snapshot -> snapshot
val set_mode_context: arity:Ast0_cocci.arity -> snapshot -> snapshot

(* Increase line number, set the mode of the new line to context. *)
val inc_line: snapshot -> snapshot

(* Increase line number, but only if current mode is star. *)
val inc_star: snapshot -> snapshot

(* Increase line number if rule line number exceeds current rule line. *)
val skip: rule_line:int -> snapshot -> snapshot


(* ------------------------------------------------------------------------- *)
(* ADDING VALUE FUNCTIONS *)

(* Add string value to the current line in the generated rule *)
val add: string -> snapshot -> snapshot

(* Add value, change the arity. *)
val add_with_arity: string -> Ast0_cocci.arity -> snapshot -> snapshot


(* ------------------------------------------------------------------------- *)
(* DISJUNCTION FUNCTIONS *)

(* start the disjunction rule *)
val init_disj_result : snapshot -> snapshot

(* get the disjunction map *)
val get_disj : int -> snapshot -> bool list

(* set disjunction mode, in which everything added to the normal generated
 * rule is added to the disjunction rule as well. *)
val set_disj_mode : bool -> snapshot -> snapshot


(* ------------------------------------------------------------------------- *)
(* POSITION FUNCTIONS *)

(* freeze the position while executing the passed function *)
val do_freeze_pos: (snapshot -> snapshot) -> snapshot -> snapshot

(* Returns the numbered name of the new pos and the modified snapshot *)
val add_position: snapshot -> (string * snapshot)


(* ------------------------------------------------------------------------- *)
(* NO GEN FUNCTIONS *)

(* flag for not generating positions *)
val set_no_gen : bool -> snapshot -> snapshot
val no_gen: snapshot -> bool

(* enter whencode nest while executing the passed function *)
val do_whencode: (snapshot -> snapshot) -> snapshot -> snapshot


(* ------------------------------------------------------------------------- *)
(* GETTERS *)

val get_positions: snapshot -> string list
val get_result: snapshot ->
  string list (* main rule *) *
  string list option (* generated disj rule *)
