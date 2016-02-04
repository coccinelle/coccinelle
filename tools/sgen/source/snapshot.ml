module Ast0 = Ast0_cocci
module StringSet = Set.Make (String)
module IntMap = Common.IntMap

(* ------------------------------------------------------------------------- *)

(* Types used to represent internal state during context rule generation.
 * We cannot pass the states from function to function in the visitor due to
 * the function signatures already having been decided for us.
 * Basically glorified global variables (but with limited access and scope, so
 * not quite!).
 *
 * The generated rule is represented by a (mode * string) IntMap. Mapping line
 * number to mode ( * or nothing) and the string contents.
 *
 * During execution, there are quite many cases dictated by the three internal
 * flags:
 *  - disj_mode (add to disj result)
 *  - no_gen_mode (do not generate positions) ALSO TRIGGERED BY WHENCODES!
 *  - freeze_pos (don't increment position counter).
 *
 * An overview of the logic:
 * State              | Add to disj result| Generate position| Incr pos counter
 * ---------------------------------------------------------------------------
 * normal             | NO                | YES              | YES
 * normal whencodes   | NO                | NO               | x
 * disj context       | NO                | NO               | x
 * disj patch         | YES               | YES              | NO
 * disj patch whencode| YES               | NO               | x
 * after special disj | YES               | YES              | YES
 *
 *)


(* ------------------------------------------------------------------------- *)
(* ARITY *)

(* Arity denotes whether a match is optional (OPT) or required (NONE).
 * Mostly the same as Ast0_cocci, but does not allow unique matches. *)

type arity = OPT | NONE

let tostring_arity = function
  | OPT -> "?"
  | NONE -> ""

(* converts Ast0.arity to local arity *)
let to_a = function
  | Ast0.OPT -> OPT
  | Ast0.NONE -> NONE
  | Ast0.UNIQUE -> failwith "Unique not supported."


(* ------------------------------------------------------------------------- *)
(* MODE *)

(* Mode denotes whether a line should be preprended with * or nothing. *)

type mode = Star of arity | Context of arity

let tostring_mode = function
  | Star a -> (tostring_arity a) ^ "* "
  | Context a -> (tostring_arity a) ^ ""

let is_star = function
  | Star _ -> true
  | Context _ -> false

let set_arity m a = match m, a with
  | Context _, Some a -> Context a
  | Star _, Some a -> Star a
  | m, None -> m


(* ------------------------------------------------------------------------- *)
(* STATE *)

(* Wrapper for state variables. *)
type t =
{
  result : (mode * string) IntMap.t; (* maps line number to content *)
  current_mode : mode; (* whether current line is in context or star mode *)
  current_line : int;  (* current line number (for hashtable indexing) *)
  rule_line : int;     (* current line number in the original rule *)
  whencode_nest : int; (* number of levels of whencode nests *)
  pos_counter : int;   (* number of added metapositions *)
  positions : StringSet.t; (* names of added metapositions *)
  disj_map : Detect_patch.t; (* maps line number to disj patch detect *)
  disj_result : (mode * string) IntMap.t option; (* generated disj rule *)
  disj_mode : bool;    (* flag for adding content to disj rule *)
  no_gen_mode : bool;  (* flag for not generating positions *)
  freeze_pos : bool * bool; (* flag for not incrementing the pos counter *)
                            (* the second part means a pos was added during *)
}

(* Constructor.
 * Note: disj_map stays invariant throughout the processing of the rule ...
 *)
let make ~disj_map =
{
  result = IntMap.empty;
  current_mode = Context NONE;
  current_line = 0;
  rule_line = 0;
  whencode_nest = 0;
  pos_counter = 0;
  positions = StringSet.empty;
  disj_result = None;
  disj_map;
  disj_mode = false;
  no_gen_mode = false;
  freeze_pos = false, false;
}

(* ------------------------------------------------------------------------- *)
(* STATE: MODE AND LINE FUNCTIONS *)

let set_mode m snp = { snp with current_mode = m }
let set_mode_star ~arity = set_mode (Star (to_a arity))
let set_mode_context ~arity = set_mode (Context (to_a arity))

let set_rule_line l snp = { snp with rule_line = l }
let inc_current_line snp = { snp with current_line = snp.current_line + 1 }

(* New lines default to Context, they have to be explicitly set to Star. *)
let inc_line snp = inc_current_line (set_mode_context Ast0.NONE snp)
let inc_star snp = if is_star snp.current_mode then inc_line snp else snp

(* if input number exceeds the current rule line number, increase the internal
 * line number.
 *)
let skip ~rule_line snp =
  let snp = if rule_line > snp.rule_line then inc_line snp else snp in
  set_rule_line rule_line snp 


(* ------------------------------------------------------------------------- *)
(* STATE: ADDING CONTENT TO MAP FUNCTIONS *)

(* Functions for modifying the generated rule (represented as map that maps
 * line number to mode and contents (string)).
 *
 * NOTE: can be changed to hashtbl if needed. Just also need to
 *  - make the record field mutable.
 *  - in get_result, need to sort by line number when extracting the lines.
 *)

(* add the value in v to the entry that has i as key *)
let add_map (v : string) (i : int) (m : mode) (r : (mode * string) IntMap.t) =
  if IntMap.mem i r then
    let (_, cur) = IntMap.find i r in
    IntMap.add i (m, cur ^ v) r
  else
    IntMap.add i (m, v) r

(* add the value in v to the current line entry, possibly changing arity. *)
let add_result (v : string) (a : arity option) (snp : t) =
  let (r, i, m) =
    (snp.result, snp.current_line, set_arity snp.current_mode a) in
  if snp.disj_mode then begin
    match snp.disj_result with
    | Some d ->
        {
          snp with
          result = add_map v i m r;
          disj_result = Some (add_map v i m d)
        }
    | None ->
        {
          snp with
          result = add_map v i m r
        }
  end else
    { snp with result = add_map v i m r }

(* add to current line *)
let add_with_arity value arity = add_result value (Some (to_a arity))
let add value = add_result value None


(* ------------------------------------------------------------------------- *)
(* STATE: POSITION FUNCTIONS *)

(* generate a position and add it to the internal list.
 * return the name of the new position and the modified snapshot.
 *)
let add_position snp =
  let pos_name = Globals.get_pos_name() in
  let newpos = pos_name ^ (string_of_int snp.pos_counter) in
  let newsnp =
    if fst snp.freeze_pos
    then
      {
        snp with
        positions = StringSet.add newpos snp.positions;
        freeze_pos = (true, true) (* set dirty bit, because adding position *)
      }
    else
      {
        snp with
        pos_counter = snp.pos_counter + 1;
        positions = StringSet.add newpos snp.positions;
      } in
  (newpos, newsnp)

(* set the freeze position flag to b.
 * if we're ending a freeze period AND one position was added during this time
 * (aka the dirty flag), then we need to increment the counter.
 * TODO: nested freezes in e.g. nested disjunctions are VERY error-prone.
 *)
let set_freeze_pos b snp =
  let (freez,dirty) = snp.freeze_pos in
  if freez && dirty && not(b) then
    {
       snp with
       freeze_pos = (b,false);
       pos_counter = snp.pos_counter + 1;
    }
  else
    { snp with freeze_pos = (b,false) }

(* do fn (t -> t) while position incrementing is frozen.
 * this is e.g. used in disjunctions where we want all cases to have the
 * same position.
 *)
let do_freeze_pos fn snp =
  let (current,_) = snp.freeze_pos in
  set_freeze_pos current (fn (set_freeze_pos true snp))


(* ------------------------------------------------------------------------- *)
(* STATE: DISJUNCTION FUNCTIONS *)

(* get the bool list for the disjunction starting at line l. *)
let get_disj l snp = Detect_patch.get_disj_patch l snp.disj_map

(* start generation of disjunction rule, copy the existing generated rule *)
let init_disj_result snp =
  match snp.disj_result with
  | Some s -> snp
  | None -> { snp with disj_result = Some snp.result }

let set_disj_mode b snp = { snp with disj_mode = b }


(* ------------------------------------------------------------------------- *)
(* STATE: WHENCODES FUNCTIONS *)

(* We never want to add stars or positions to whencodes, so we keep track of
 * whether we are currently inside a whencode (and they may be nested).
 *)

let inc_whencode snp = { snp with whencode_nest = snp.whencode_nest + 1 }
let dec_whencode snp = { snp with whencode_nest = snp.whencode_nest - 1 }
let in_whencode snp = snp.whencode_nest <> 0

let do_whencode fn snp = dec_whencode (fn (inc_whencode snp))


(* ------------------------------------------------------------------------- *)
(* STATE: NO GEN *)

(* if no_gen flag is set, unparse the AST0 as normal, don't insert positions
 * or forced newlines.
 *)
let set_no_gen b snp = { snp with no_gen_mode = b }

(* do not add positions or newlines if ... *)
let no_gen snp = in_whencode snp || snp.no_gen_mode


(* ------------------------------------------------------------------------- *)
(* STATE: GETTERS *)

let get_positions snp = StringSet.elements snp.positions

(* Returns the map as a string list, sorted by key and the same for
 * the disjunction result if any.
 *)
let get_result snp =
  let transform x no_mode =
    List.map (fun (_,(c,s)) -> if no_mode then s else (tostring_mode c) ^ s)
      (IntMap.bindings x) in
  match snp.disj_result with
  | Some d -> (transform snp.result true, Some (transform d false))
  | None -> (transform snp.result false, None)
