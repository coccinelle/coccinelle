(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(*external c_counter : unit -> int = "c_counter"*)
let timeout = 800
(* Optimize triples_conj by first extracting the intersection of the two sets,
which can certainly be in the intersection *)
let pTRIPLES_CONJ_OPT = ref true
(* For complement, make NegState for the negation of a single state *)
let pTRIPLES_COMPLEMENT_OPT = ref true
(* For complement, do something special for the case where the environment
and witnesses are empty *)
let pTRIPLES_COMPLEMENT_SIMPLE_OPT = ref true
(* "Double negate" the arguments of the path operators *)
let pDOUBLE_NEGATE_OPT = ref true
(* Only do pre_forall/pre_exists on new elements in fixpoint iteration *)
let pNEW_INFO_OPT = ref true
(* Filter the result of the label function to drop entries that aren't
compatible with any of the available environments *)
let pREQUIRED_ENV_OPT = ref true
(* Memoize the raw result of the label function *)
let pSATLABEL_MEMO_OPT = ref true
(* Filter results according to the required states *)
let pREQUIRED_STATES_OPT = ref true
(* Drop negative witnesses at Uncheck *)
let pUNCHECK_OPT = ref true
let pANY_NEG_OPT = ref true
let pLazyOpt = ref true

(* Nico: This stack is use for graphical traces *)
let graph_stack = ref ([] : string list)
let graph_hash = (Hashtbl.create 101)

(*
let pTRIPLES_CONJ_OPT = ref false
let pTRIPLES_COMPLEMENT_OPT = ref false
let pTRIPLES_COMPLEMENT_SIMPLE_OPT = ref false
let pDOUBLE_NEGATE_OPT = ref false
let pNEW_INFO_OPT = ref false
let pREQUIRED_ENV_OPT = ref false
let pSATLABEL_MEMO_OPT = ref false
let pREQUIRED_STATES_OPT = ref false
let pUNCHECK_OPT = ref false
let pANY_NEG_OPT = ref false
let pLazyOpt = ref false
*)


let step_count = ref 0
exception Steps
let inc_step _ =
  if not (!step_count = 0)
  then
    begin
      step_count := !step_count - 1;
      if !step_count = 0 then raise Steps
    end

let inc cell = cell := !cell + 1

let satEU_calls = ref 0
let satAW_calls = ref 0
let satAU_calls = ref 0
let satEF_calls = ref 0
let satAF_calls = ref 0
let satEG_calls = ref 0
let satAG_calls = ref 0

let triples = ref 0

let ctr = ref 0
let new_let _ =
  let c = !ctr in
  ctr := c + 1;
  Printf.sprintf "_fresh_r_%d" c

(* **********************************************************************
 *
 * Implementation of a Witness Tree model checking engine for CTL-FVex
 *
 *
 * **********************************************************************)

(* ********************************************************************** *)
(* Module: SUBST (substitutions: meta. vars and values)                   *)
(* ********************************************************************** *)

module type SUBST =
  sig
    type value
    type mvar
    val eq_mvar: mvar -> mvar -> bool
    val eq_val: value -> value -> bool
    val merge_val: value -> value -> value
    val print_mvar : mvar -> unit
    val print_value : value -> unit
  end
;;

(* ********************************************************************** *)
(* Module: GRAPH (control flow graphs / model)                            *)
(* ********************************************************************** *)

module type GRAPH =
  sig
    type node
    type cfg
    val predecessors:     cfg -> node -> node list
    val successors:       cfg -> node -> node list
    val extract_is_loop : cfg -> node -> bool
    val print_node :      node -> unit
    val size :            cfg -> int
    val print_graph :     cfg -> string option ->
      (node * string) list -> (node * string) list -> string -> unit
  end
;;

module OGRAPHEXT_GRAPH =
  struct
    module Key : Set.OrderedType with type t = int = struct
      type t = int
      let compare = compare
    end

    module KeySet : Set.S with type elt = Key.t = Set.Make (Key)

    module KeyMap : Map.S with type key = Key.t = Map.Make (Key)

    module Edge : Set.OrderedType with type t = unit = struct
      type t = unit
      let compare = compare
    end

    module KeyEdgePair : Set.OrderedType with type t = Key.t * Edge.t = struct
      type t = Key.t * Edge.t
      let compare = compare
    end

    module KeyEdgeSet : Set.S with type elt = KeyEdgePair.t =
      Set.Make (KeyEdgePair)

    module G = Ograph_extended.Make (Key) (KeySet) (KeyMap)
      (Edge) (KeyEdgePair) (KeyEdgeSet)

    type cfg = string G.ograph_mutable;;
    type node = Key.t

    let predecessors cfg n =
      List.map fst (KeyEdgeSet.elements (cfg#predecessors n));;

    let print_node i = Format.print_string (string_of_int i)
  end
;;

(* ********************************************************************** *)
(* Module: PREDICATE (predicates for CTL formulae)                        *)
(* ********************************************************************** *)

module type PREDICATE =
sig
  type t
  val print_predicate : t -> unit
end


(* ********************************************************************** *)

(* ---------------------------------------------------------------------- *)
(* Misc. useful generic functions                                         *)
(* ---------------------------------------------------------------------- *)

let get_graph_files () = !graph_stack
let get_graph_comp_files outfile = Hashtbl.find_all graph_hash outfile

let head = List.hd

let tail l =
  match l with
    [] -> []
  | (x::xs) -> xs
;;

let foldl = List.fold_left;;

let foldl1 f xs = foldl f (head xs) (tail xs)

type 'a esc = ESC of 'a | CONT of 'a

let foldr = List.fold_right;;

let concat = List.concat;;

let map = List.map;;

let filter = List.filter;;

let partition = List.partition;;

let concatmap f l = List.concat (List.map f l);;

let maybe f g opt =
  match opt with
    | None -> g
    | Some x -> f x
;;

let some_map f opts = map (maybe (fun x -> Some (f x)) None) opts

let rec some_tolist (opts : 'a option list) : 'a list =
  match opts with
    | []             -> []
    | (Some x)::rest -> x::(some_tolist rest)
    | _::rest        -> some_tolist rest
;;

let rec groupBy eq l =
    match l with
      [] -> []
    | (x::xs) ->
	let (xs1,xs2) = partition (fun x' -> eq x x') xs in
	(x::xs1)::(groupBy eq xs2)
;;

let group l = groupBy (=) l;;

let rec memBy eq x l =
  match l with
    [] -> false
  | (y::ys) -> if (eq x y) then true else (memBy eq x ys)
;;

let rec nubBy eq ls =
  match ls with
    [] -> []
  | (x::xs) when (memBy eq x xs) -> nubBy eq xs
  | (x::xs) -> x::(nubBy eq xs)
;;

let state_compare (s1,_,_) (s2,_,_) = compare s1 s2

let setifyBy eq xs = nubBy eq xs;;

let setify xs = Common.nub xs;;

let inner_setify xs = List.sort compare (Common.nub xs);;

let unionBy compare eq xs = function
    [] -> xs
  | ys ->
      let rec loop = function
	  [] -> ys
	| x::xs -> if memBy eq x ys then loop xs else x::(loop xs) in
      List.sort compare (loop xs)
;;

let union xs ys = unionBy state_compare (=) xs ys;;

let setdiff xs ys = filter (fun x -> not (List.mem x ys)) xs;;

let subseteqBy eq xs ys = List.for_all (fun x -> memBy eq x ys) xs;;

let subseteq xs ys = List.for_all (fun x -> List.mem x ys) xs;;
let supseteq xs ys = subseteq ys xs

let setequalBy eq xs ys = (subseteqBy eq xs ys) && (subseteqBy eq ys xs);;

let setequal xs ys = (subseteq xs ys) && (subseteq ys xs);;

(* Fix point calculation *)
let rec fix eq f x =
  let x' = f x in if (eq x' x) then x' else fix eq f x'
;;

(* Fix point calculation on set-valued functions *)
let setfix f x = (fix subseteq f x) (*if new is a subset of old, stop*)
let setgfix f x = (fix supseteq f x) (*if new is a supset of old, stop*)

let get_states l = Common.nub (List.map (function (s,_,_) -> s) l)

(* ********************************************************************** *)
(* Module: CTL_ENGINE                                                     *)
(* ********************************************************************** *)

module CTL_ENGINE =
  functor (SUB : SUBST) ->
    functor (G : GRAPH) ->
      functor (P : PREDICATE) ->
struct

module A = Ast_ctl

type substitution = (SUB.mvar, SUB.value) Ast_ctl.generic_substitution

type ('pred,'anno) witness =
    (G.node, substitution,
     ('pred, SUB.mvar, 'anno) Ast_ctl.generic_ctl list)
      Ast_ctl.generic_witnesstree

type ('pred,'anno) triples =
    (G.node * substitution * ('pred,'anno) witness list) list

(* ---------------------------------------------------------------------- *)
(* Pretty printing functions *)
(* ---------------------------------------------------------------------- *)

let (print_generic_substitution : substitution -> unit) = fun substxs ->
  let print_generic_subst = function
      A.Subst (mvar, v) ->
	SUB.print_mvar mvar; Format.print_string " --> "; SUB.print_value v
    | A.NegSubst (mvar, v) ->
	SUB.print_mvar mvar; Format.print_string " -/-> "; SUB.print_value v in
  Format.print_string "[";
  Common.print_between (fun () -> Format.print_string ";" )
    print_generic_subst substxs;
  Format.print_string "]"

let rec (print_generic_witness: ('pred, 'anno) witness -> unit) =
  function
  | A.Wit (state, subst, anno, childrens) ->
      Format.print_string "wit ";
      G.print_node state;
      print_generic_substitution subst;
      (match childrens with
	[] -> Format.print_string "{}"
      |	_ ->
	  Format.force_newline(); Format.print_string "   "; Format.open_box 0;
	  print_generic_witnesstree childrens; Format.close_box())
  | A.NegWit(wit) ->
      Format.print_string "!";
      print_generic_witness wit

and (print_generic_witnesstree: ('pred,'anno) witness list -> unit) =
  fun witnesstree ->
    Format.open_box 1;
    Format.print_string "{";
    Common.print_between
      (fun () -> Format.print_string ";"; Format.force_newline() )
      print_generic_witness witnesstree;
    Format.print_string "}";
    Format.close_box()

and print_generic_triple (node,subst,tree) =
  G.print_node node;
  print_generic_substitution subst;
  print_generic_witnesstree tree

and (print_generic_algo : ('pred,'anno) triples -> unit) = fun xs ->
  Format.print_string "<";
  Common.print_between
    (fun () -> Format.print_string ";"; Format.force_newline())
    print_generic_triple xs;
  Format.print_string ">"
;;

let print_state (str : string) (l : ('pred,'anno) triples) =
  Printf.printf "%s\n" str;
  List.iter (function x ->
    print_generic_triple x; Format.print_newline(); flush stdout)
    (List.sort compare l);
  Printf.printf "\n"

let print_required_states = function
    None -> Printf.printf "no required states\n"
  | Some states ->
      Printf.printf "required states: ";
      List.iter
	(function x ->
	  G.print_node x; Format.print_string " "; Format.print_flush())
	states;
      Printf.printf "\n"

let mkstates states = function
    None -> states
  | Some states -> states

let print_graph grp required_states res str = function
    A.Exists (keep,v,phi)     -> ()
  | phi ->
      if !Flag_ctl.graphical_trace && not !Flag_ctl.checking_reachability
      then
	 match phi with
	| A.Exists (keep,v,phi)     -> ()
	| _ ->
	    let label =
	      Printf.sprintf "%s%s"
		(String.escaped
		   (Common.format_to_string
		      (function _ ->
			Pretty_print_ctl.pp_ctl
			  (P.print_predicate, SUB.print_mvar)
			  false phi)))
		str in
	    let file = (match !Flag.currentfile with
	      None -> "graphical_trace"
	    | Some f -> f
		  ) in
	      (if not (List.mem file !graph_stack) then
		graph_stack := file :: !graph_stack);
	    let filename = Filename.temp_file (file^":") ".dot" in
	    Hashtbl.add graph_hash file filename;
	    G.print_graph grp
	      (if !Flag_ctl.gt_without_label then None else (Some label))
	      (match required_states with
		None -> []
	      | Some required_states ->
		  (List.map (function s -> (s,"blue")) required_states))
	      (List.map (function (s,_,_) -> (s,"\"#FF8080\"")) res)  filename

let print_graph_c grp required_states res ctr phi =
  let str = "iter: "^(string_of_int !ctr) in
  print_graph grp required_states res str phi

(* ---------------------------------------------------------------------- *)
(*                                                                        *)
(* ---------------------------------------------------------------------- *)


(* ************************* *)
(* Substitutions             *)
(* ************************* *)

let dom_sub sub =
  match sub with
  | A.Subst(x,_)    -> x
  | A.NegSubst(x,_) -> x
;;

let ran_sub sub =
  match sub with
  | A.Subst(_,x)    -> x
  | A.NegSubst(_,x) -> x
;;

let eq_subBy eqx eqv sub sub' =
  match (sub,sub') with
    | (A.Subst(x,v),A.Subst(x',v'))       -> (eqx x x') && (eqv v v')
    | (A.NegSubst(x,v),A.NegSubst(x',v')) -> (eqx x x') && (eqv v v')
    | _                               -> false
;;

(* NOTE: functor *)
let eq_sub sub sub' = eq_subBy SUB.eq_mvar SUB.eq_val sub sub'

let eq_subst th th' = setequalBy eq_sub th th';;

let merge_subBy eqx (===) (>+<) sub sub' =
  (* variable part is guaranteed to be the same *)
  match (sub,sub') with
    (A.Subst (x,v),A.Subst (x',v')) ->
      if (v === v')
      then Some [A.Subst(x, v >+< v')]
      else None
  | (A.NegSubst(x,v),A.Subst(x',v')) ->
      if (not (v === v'))
      then Some [A.Subst(x',v')]
      else None
  | (A.Subst(x,v),A.NegSubst(x',v')) ->
      if (not (v === v'))
      then Some [A.Subst(x,v)]
      else None
  | (A.NegSubst(x,v),A.NegSubst(x',v')) ->
      if (v === v')
      then
	let merged = v >+< v' in
	if merged = v && merged = v'
	then Some [A.NegSubst(x,v >+< v')]
	else
	  (* positions are compatible, but not identical. keep apart. *)
	  Some [A.NegSubst(x,v);A.NegSubst(x',v')]
      else Some [A.NegSubst(x,v);A.NegSubst(x',v')]
;;

(* NOTE: functor *)
(* How could we accomadate subterm constraints here??? *)
let merge_sub sub sub' =
  merge_subBy SUB.eq_mvar SUB.eq_val SUB.merge_val sub sub'

let clean_substBy eq cmp theta = List.sort cmp (nubBy eq theta);;

(* NOTE: we sort by using the generic "compare" on (meta-)variable
 *   names; we could also require a definition of compare for meta-variables
 *   or substitutions but that seems like overkill for sorting
 *)
let clean_subst theta =
  let res =
    clean_substBy eq_sub
      (fun s s' ->
	let res = compare (dom_sub s) (dom_sub s') in
	if res = 0
	then
	  match (s,s') with
	    (A.Subst(_,_),A.NegSubst(_,_)) -> -1
	  | (A.NegSubst(_,_),A.Subst(_,_)) -> 1
	  | _ -> compare (ran_sub s) (ran_sub s')
	else res)
      theta in
  let rec loop = function
      [] -> []
    | (A.Subst(x,v)::A.NegSubst(y,v')::rest) when SUB.eq_mvar x y ->
	loop (A.Subst(x,v)::rest)
    | x::xs -> x::(loop xs) in
  loop res

let top_subst = [];;			(* Always TRUE subst. *)

(* Split a theta in two parts: one with (only) "x" and one without *)
(* NOTE: functor *)
let split_subst theta x =
  partition (fun sub -> SUB.eq_mvar (dom_sub sub) x) theta;;

exception SUBST_MISMATCH
let conj_subst theta theta' =
  match (theta,theta') with
    | ([],_) -> Some theta'
    | (_,[]) -> Some theta
    | _ ->
	let rec classify = function
	    [] -> []
	  | [x] -> [(dom_sub x,[x])]
	  | x::xs ->
	      (match classify xs with
		((nm,y)::ys) as res ->
		  if dom_sub x = nm
		  then (nm,x::y)::ys
		  else (dom_sub x,[x])::res
	      |	_ -> failwith "not possible") in
	let merge_all theta theta' =
	  foldl
	    (function rest ->
	      function sub ->
		foldl
		  (function rest ->
		    function sub' ->
		      match (merge_sub sub sub') with
			Some subs -> subs @ rest
		      | _         -> raise SUBST_MISMATCH)
		  rest theta')
	    [] theta in
	let rec loop = function
	    ([],ctheta') ->
	      List.concat (List.map (function (_,ths) -> ths) ctheta')
	  | (ctheta,[]) ->
	      List.concat (List.map (function (_,ths) -> ths) ctheta)
	  | ((x,ths)::xs,(y,ths')::ys) ->
	      (match compare x y with
		0 -> (merge_all ths ths') @ loop (xs,ys)
	      |	-1 -> ths @ loop (xs,((y,ths')::ys))
	      |	1 -> ths' @ loop (((x,ths)::xs),ys)
	      |	_ -> failwith "not possible") in
	try Some (clean_subst(loop (classify theta, classify theta')))
	with SUBST_MISMATCH -> None
;;

(* theta' must be a subset of theta *)
let conj_subst_none theta theta' =
  match (theta,theta') with
    | (_,[]) -> Some theta
    | ([],_) -> None
    | _ ->
	let rec classify = function
	    [] -> []
	  | [x] -> [(dom_sub x,[x])]
	  | x::xs ->
	      (match classify xs with
		((nm,y)::ys) as res ->
		  if dom_sub x = nm
		  then (nm,x::y)::ys
		  else (dom_sub x,[x])::res
	      |	_ -> failwith "not possible") in
	let merge_all theta theta' =
	  foldl
	    (function rest ->
	      function sub ->
		foldl
		  (function rest ->
		    function sub' ->
		      match (merge_sub sub sub') with
			Some subs -> subs @ rest
		      | _         -> raise SUBST_MISMATCH)
		  rest theta')
	    [] theta in
	let rec loop = function
	    (ctheta,[]) ->
	      List.concat (List.map (function (_,ths) -> ths) ctheta)
	  | ([],ctheta') -> raise SUBST_MISMATCH
	  | ((x,ths)::xs,(y,ths')::ys) ->
	      (match compare x y with
		0 -> (merge_all ths ths') @ loop (xs,ys)
	      |	-1 -> ths @ loop (xs,((y,ths')::ys))
	      |	1 -> raise SUBST_MISMATCH
	      |	_ -> failwith "not possible") in
	try Some (clean_subst(loop (classify theta, classify theta')))
	with SUBST_MISMATCH -> None
;;

let negate_sub sub =
  match sub with
    | A.Subst(x,v)    -> A.NegSubst (x,v)
    | A.NegSubst(x,v) -> A.Subst(x,v)
;;

(* Turn a (big) theta into a list of (small) thetas *)
let negate_subst theta = (map (fun sub -> [negate_sub sub]) theta);;


(* ************************* *)
(* Witnesses                 *)
(* ************************* *)

(* Always TRUE witness *)
let top_wit = ([] : (('pred, 'anno) witness list));;

let eq_wit wit wit' = wit = wit';;

let union_wit wit wit' = (*List.sort compare (wit' @ wit) for popl*)
  let res = unionBy compare (=) wit wit' in
  let anynegwit = (* if any is neg, then all are *)
    List.exists (function A.NegWit _ -> true | A.Wit _ -> false) in
  if anynegwit res
  then List.filter (function A.NegWit _ -> true | A.Wit _ -> false) res
  else res

let negate_wit wit = A.NegWit wit (*
  match wit with
    | A.Wit(s,th,anno,ws)    -> A.NegWitWit(s,th,anno,ws)
    | A.NegWitWit(s,th,anno,ws) -> A.Wit(s,th,anno,ws)*)
;;

let negate_wits wits =
  List.sort compare (map (fun wit -> [negate_wit wit]) wits);;

let unwitify trips =
  let anynegwit = (* if any is neg, then all are *)
    List.exists (function A.NegWit _ -> true | A.Wit _ -> false) in
  setify
    (List.fold_left
       (function prev ->
	 function (s,th,wit) ->
	   if anynegwit wit then prev else (s,th,top_wit)::prev)
       [] trips)

(* ************************* *)
(* Triples                   *)
(* ************************* *)

(* Triples are equal when the constituents are equal *)
let eq_trip (s,th,wit) (s',th',wit') =
  (s = s') && (eq_wit wit wit') && (eq_subst th th');;

let triples_top states = map (fun s -> (s,top_subst,top_wit)) states;;

let normalize trips =
  List.map
    (function (st,th,wit) -> (st,List.sort compare th,List.sort compare wit))
    trips


(* conj opt doesn't work ((1,[],{{x=3}}) v (1,[],{{x=4}})) & (1,[],{{x=4}}) =
(1,[],{{x=3},{x=4}}), not (1,[],{{x=4}}) *)
let triples_conj trips trips' =
  let (trips,shared,trips') =
    if false && !pTRIPLES_CONJ_OPT (* see comment above *)
    then
      let (shared,trips) =
	List.partition (function t -> List.mem t trips') trips in
      let trips' =
	List.filter (function t -> not(List.mem t shared)) trips' in
      (trips,shared,trips')
    else (trips,[],trips') in
  foldl (* returns a set - setify inlined *)
    (function rest ->
      function (s1,th1,wit1) ->
	foldl
	  (function rest ->
	    function (s2,th2,wit2) ->
	      if (s1 = s2) then
		(match (conj_subst th1 th2) with
		  Some th ->
		    let t = (s1,th,union_wit wit1 wit2) in
		    if List.mem t rest then rest else t::rest
		| _       -> rest)
	      else rest)
	  rest trips')
    shared trips
;;

(* ignore the state in the right argument.  always pretend it is the same as
the left one *)
(* env on right has to be a subset of env on left *)
let triples_conj_none trips trips' =
  let (trips,shared,trips') =
    if false && !pTRIPLES_CONJ_OPT (* see comment above *)
    then
      let (shared,trips) =
	List.partition (function t -> List.mem t trips') trips in
      let trips' =
	List.filter (function t -> not(List.mem t shared)) trips' in
      (trips,shared,trips')
    else (trips,[],trips') in
  foldl (* returns a set - setify inlined *)
    (function rest ->
      function (s1,th1,wit1) ->
	foldl
	  (function rest ->
	    function (s2,th2,wit2) ->
	      match (conj_subst_none th1 th2) with
		Some th ->
		  let t = (s1,th,union_wit wit1 wit2) in
		  if List.mem t rest then rest else t::rest
	      | _       -> rest)
	  rest trips')
    shared trips
;;

exception AW

let triples_conj_AW trips trips' =
  let (trips,shared,trips') =
    if false && !pTRIPLES_CONJ_OPT
    then
      let (shared,trips) =
	List.partition (function t -> List.mem t trips') trips in
      let trips' =
	List.filter (function t -> not(List.mem t shared)) trips' in
      (trips,shared,trips')
    else (trips,[],trips') in
  foldl (* returns a set - setify inlined *)
    (function rest ->
      function (s1,th1,wit1) ->
	foldl
	  (function rest ->
	    function (s2,th2,wit2) ->
	      if (s1 = s2) then
		(match (conj_subst th1 th2) with
		  Some th ->
		    let t = (s1,th,union_wit wit1 wit2) in
		    if List.mem t rest then rest else t::rest
		| _ -> raise AW)
	      else rest)
	  rest trips')
    shared trips
;;

(* *************************** *)
(* NEGATION (NegState style)   *)
(* *************************** *)

(* Constructive negation at the state level *)
type ('a) state =
    PosState of 'a
  | NegState of 'a list
;;

let compatible_states = function
    (PosState s1, PosState s2) ->
      if s1 = s2 then Some (PosState s1) else None
  | (PosState s1, NegState s2) ->
      if List.mem s1 s2 then None else Some (PosState s1)
  | (NegState s1, PosState s2) ->
      if List.mem s2 s1 then None else Some (PosState s2)
  | (NegState s1, NegState s2) -> Some (NegState (s1 @ s2))
;;

(* Conjunction on triples with "special states" *)
let triples_state_conj trips trips' =
  let (trips,shared,trips') =
    if !pTRIPLES_CONJ_OPT
    then
      let (shared,trips) =
	List.partition (function t -> List.mem t trips') trips in
      let trips' =
	List.filter (function t -> not(List.mem t shared)) trips' in
      (trips,shared,trips')
    else (trips,[],trips') in
  foldl
    (function rest ->
      function (s1,th1,wit1) ->
	foldl
	  (function rest ->
	    function (s2,th2,wit2) ->
	      match compatible_states(s1,s2) with
		Some s ->
		  (match (conj_subst th1 th2) with
		    Some th ->
		      let t = (s,th,union_wit wit1 wit2) in
		      if List.mem t rest then rest else t::rest
		  | _ -> rest)
	      | _ -> rest)
	  rest trips')
    shared trips
;;

let triple_negate (s,th,wits) =
  let negstates = (NegState [s],top_subst,top_wit) in
  let negths = map (fun th -> (PosState s,th,top_wit)) (negate_subst th) in
  let negwits = map (fun nwit -> (PosState s,th,nwit)) (negate_wits wits) in
    negstates :: (negths @ negwits) (* all different *)

(* FIX ME: it is not necessary to do full conjunction *)
let triples_complement states (trips : ('pred, 'anno) triples) =
  if !pTRIPLES_COMPLEMENT_OPT
  then
    (let cleanup (s,th,wit) =
      match s with
	PosState s' -> [(s',th,wit)]
      | NegState ss ->
	  assert (th=top_subst);
	  assert (wit=top_wit);
	  map (fun st -> (st,top_subst,top_wit)) (setdiff states ss) in
    let (simple,complex) =
      if !pTRIPLES_COMPLEMENT_SIMPLE_OPT
      then
	let (simple,complex) =
	  List.partition (function (s,[],[]) -> true | _ -> false) trips in
	let simple =
	  [(NegState(List.map (function (s,_,_) -> s) simple),
	    top_subst,top_wit)] in
	(simple,complex)
      else ([(NegState [],top_subst,top_wit)],trips) in
    let rec compl trips =
      match trips with
	[] -> simple
      | (t::ts) -> triples_state_conj (triple_negate t) (compl ts) in
    let compld = (compl complex) in
    let compld = concatmap cleanup compld in
    compld)
  else
    let negstates (st,th,wits) =
      map (function st -> (st,top_subst,top_wit)) (setdiff states [st]) in
    let negths (st,th,wits) =
      map (function th -> (st,th,top_wit)) (negate_subst th) in
    let negwits (st,th,wits) =
      map (function nwit -> (st,th,nwit)) (negate_wits wits) in
    match trips with
      [] -> map (function st -> (st,top_subst,top_wit)) states
    | x::xs ->
	setify
	  (foldl
	     (function prev ->
	       function cur ->
		 triples_conj (negstates cur @ negths cur @ negwits cur) prev)
	     (negstates x @ negths x @ negwits x) xs)
;;

let triple_negate (s,th,wits) =
  let negths = map (fun th -> (s,th,top_wit)) (negate_subst th) in
  let negwits = map (fun nwit -> (s,th,nwit)) (negate_wits wits) in
  ([s], negths @ negwits) (* all different *)

let print_compl_state str (n,p) =
  Printf.printf "%s neg: " str;
  List.iter
    (function x -> G.print_node x; Format.print_flush(); Printf.printf " ")
    n;
  Printf.printf "\n";
  print_state "pos" p

let triples_complement states (trips : ('pred, 'anno) triples) =
  if trips = []
  then map (function st -> (st,top_subst,top_wit)) states
  else
    let cleanup (neg,pos) =
      let keep_pos =
	List.filter (function (s,_,_) -> List.mem s neg) pos in
      (map (fun st -> (st,top_subst,top_wit)) (setdiff states neg)) @
      keep_pos in
    let trips = List.sort state_compare trips in
    let all_negated = List.map triple_negate trips in
    let merge_one (neg1,pos1) (neg2,pos2) =
      let (pos1conj,pos1keep) =
	List.partition (function (s,_,_) -> List.mem s neg2) pos1 in
      let (pos2conj,pos2keep) =
	List.partition (function (s,_,_) -> List.mem s neg1) pos2 in
      (Common.union_set neg1 neg2,
       (triples_conj pos1conj pos2conj) @ pos1keep @ pos2keep) in
    let rec inner_loop = function
	x1::x2::rest -> (merge_one x1 x2) :: (inner_loop rest)
      | l -> l in
    let rec outer_loop = function
	[x] -> x
      | l -> outer_loop (inner_loop l) in
    cleanup (outer_loop all_negated)

(* ********************************** *)
(* END OF NEGATION (NegState style)   *)
(* ********************************** *)

(* now this is always true, so we could get rid of it *)
let something_dropped = ref true

let triples_union trips trips' =
  (*unionBy compare eq_trip trips trips';;*)
  (* returns -1 is t1 > t2, 1 if t2 >= t1, and 0 otherwise *)
(*
The following does not work.  Suppose we have ([x->3],{A}) and ([],{A,B}).
Then, the following says that since the first is a more restrictive
environment and has fewer witnesses, then it should be dropped. But having
fewer witnesses is not necessarily less informative than having more,
because fewer witnesses can mean the absence of the witness-causing thing.
So the fewer witnesses have to be kept around.
subseteq changed to = to make it hopefully work
*)
  if !pNEW_INFO_OPT
  then
    begin
      something_dropped := false;
      if trips = trips'
      then (something_dropped := true; trips)
      else
	let subsumes (s1,th1,wit1) (s2,th2,wit2) =
	  if s1 = s2
	  then
	    (match conj_subst th1 th2 with
	      Some conj ->
		if conj = th1
		then if (*subseteq*) wit1 = wit2 then 1 else 0
		else
		  if conj = th2
		  then if (*subseteq*) wit2 = wit1 then (-1) else 0
		  else 0
	    | None -> 0)
	  else 0 in
	let rec first_loop second = function
	    [] -> second
	  | x::xs -> first_loop (second_loop x second) xs
	and second_loop x = function
	    [] -> [x]
	  | (y::ys) as all ->
	      match subsumes x y with
		1 -> something_dropped := true; all
	      | (-1) -> second_loop x ys
	      | _ -> y::(second_loop x ys) in
	first_loop trips trips'
    end
  else unionBy compare eq_trip trips trips'


let triples_witness x unchecked not_keep trips =
  let anyneg = (* if any is neg, then all are *)
    List.exists (function A.NegSubst _ -> true | A.Subst _ -> false) in
  let anynegwit = (* if any is neg, then all are *)
    List.exists (function A.NegWit _ -> true | A.Wit _ -> false) in
  let allnegwit = (* if any is neg, then all are *)
    List.for_all (function A.NegWit _ -> true | A.Wit _ -> false) in
  let negtopos =
    List.map (function A.NegWit w -> w | A.Wit _ -> failwith "bad wit")in
  let res =
    List.fold_left
      (function prev ->
	function (s,th,wit) as t ->
	  let (th_x,newth) = split_subst th x in
	  match th_x with
	    [] ->
	      (* one consider whether if not not_keep is true, then we should
		 fail.  but it could be that the variable is a used_after and
		 then it is the later rule that should fail and not this one *)
	      if not not_keep && !Flag_ctl.verbose_ctl_engine
	      then
		(SUB.print_mvar x; Format.print_flush();
		 print_state ": empty witness from" [t]);
	      t::prev
	  | l when anyneg l && !pANY_NEG_OPT -> prev
	      (* see tests/nestseq for how neg bindings can come up even
		 without eg partial matches
              (* negated substitution only allowed with negwits.
		 just dropped *)
	      if anynegwit wit && allnegwit wit (* nonempty negwit list *)
	      then prev
	      else
		(print_generic_substitution l; Format.print_newline();
		failwith"unexpected negative binding with positive witnesses")*)
	  | _ ->
	      let new_triple =
		if unchecked || not_keep
		then (s,newth,wit)
		else
		  if anynegwit wit && allnegwit wit
		  then (s,newth,[A.NegWit(A.Wit(s,th_x,[],negtopos wit))])
		  else (s,newth,[A.Wit(s,th_x,[],wit)]) in
	      new_triple::prev)
      [] trips in
  if unchecked || !Flag_ctl.partial_match (* the only way to have a NegWit *)
  then setify res
  else List.rev res
;;


(* ---------------------------------------------------------------------- *)
(* SAT  - Model Checking Algorithm for CTL-FVex                           *)
(*                                                                        *)
(* TODO: Implement _all_ operators (directly)                             *)
(* ---------------------------------------------------------------------- *)


(* ************************************* *)
(* The SAT algorithm and special helpers *)
(* ************************************* *)

let pre_exist dir (grp,_,_) y reqst =
  let check s =
    match reqst with None -> true | Some reqst -> List.mem s reqst in
  let exp (s,th,wit) =
    let ss' = match dir with
                A.FORWARD  -> G.predecessors grp s
              | A.BACKWARD -> G.successors grp s
      in concatmap (fun s' -> if check s' then [(s',th,wit)] else []) ss'
    in setify (concatmap exp y)
;;

exception Empty

let pre_forall dir (grp,_,states) y all reqst =
  let check s =
    match reqst with
      None -> true | Some reqst -> List.mem s reqst in
  let pred =
    match dir with
      A.FORWARD -> G.predecessors | A.BACKWARD -> G.successors in
  let succ =
    match dir with
      A.FORWARD -> G.successors | A.BACKWARD -> G.predecessors in
  let neighbors =
    List.map
      (function p -> (p,succ grp p))
      (setify
	 (concatmap
	    (function (s,_,_) -> List.filter check (pred grp s)) y)) in
  (* would a hash table be more efficient? *)
  let all = List.sort state_compare all in
  let rec up_nodes child s = function
      [] -> []
    | (s1,th,wit)::xs ->
	(match compare s1 child with
	  -1 -> up_nodes child s xs
	| 0 -> (s,th,wit)::(up_nodes child s xs)
	| _ -> []) in
  let neighbor_triples =
    List.fold_left
      (function rest ->
	function (s,children) ->
	  try
	    (List.map
	       (function child ->
		 match up_nodes child s all with [] -> raise Empty | l -> l)
	       children) :: rest
	  with Empty -> rest)
      [] neighbors in
  match neighbor_triples with
    [] -> []
  | _ ->
      (*normalize*)
        (foldl1 (@) (List.map (foldl1 triples_conj) neighbor_triples))

(* drop_negwits will call setify *)
let satEX dir m s reqst = pre_exist dir m s reqst;;

let satAX dir m s reqst = pre_forall dir m s s reqst
;;

(* E[phi1 U phi2] == phi2 \/ (phi1 /\ EXE[phi1 U phi2]) *)
let satEU dir ((_,_,states) as m) s1 s2 reqst print_graph =
  (*Printf.printf "EU\n";
  let ctr = ref 0 in*)
  inc satEU_calls;
  if s1 = []
  then s2
  else
    (*let ctr = ref 0 in*)
    if !pNEW_INFO_OPT
    then
      let rec f y new_info =
	inc_step();
	match new_info with
	  [] -> y
	| new_info ->
	    (*ctr := !ctr + 1;
	    print_graph y ctr;*)
	    let first = triples_conj s1 (pre_exist dir m new_info reqst) in
	    let res = triples_union first y in
	    let new_info = setdiff res y in
	    (*Printf.printf "iter %d res %d new_info %d\n"
	    !ctr (List.length res) (List.length new_info);
	    print_state "res" res;
	    print_state "new_info" new_info;
	    flush stdout;*)
	    f res new_info in
      f s2 s2
    else
      let f y =
	inc_step();
	(*ctr := !ctr + 1;
	print_graph y ctr;*)
	let pre = pre_exist dir m y reqst in
	triples_union s2 (triples_conj s1 pre) in
      setfix f s2
;;

let satEU_forAW dir ((cfg,_,states) as m) s1 s2 reqst print_graph =
  if s1 = []
  then s2
  else
    if !pNEW_INFO_OPT
    then
      let rec f y new_info =
	if List.exists (G.extract_is_loop cfg) (get_states new_info)
	then raise AW
	else
	  match new_info with
	    [] -> y
	  | new_info ->
	      let first = triples_conj s1 (pre_exist dir m new_info reqst) in
	      let res = triples_union first y in
	      let new_info = setdiff res y in
	      f res new_info in
      f s2 s2
    else
      let f y =
	let pre = pre_exist dir m y reqst in
	triples_union s2 (triples_conj s1 pre) in
      setfix f s2
;;

(* EF phi == E[true U phi] *)
let satEF dir m s2 reqst =
  inc satEF_calls;
  (*let ctr = ref 0 in*)
  if !pNEW_INFO_OPT
  then
    let rec f y new_info =
      inc_step();
      match new_info with
	[] -> y
      | new_info ->
	  (*ctr := !ctr + 1;
	  print_state (Printf.sprintf "iteration %d\n" !ctr) y;*)
	  let first = pre_exist dir m new_info reqst in
	  let res = triples_union first y in
	  let new_info = setdiff res y in
	  (*Printf.printf "EF %s iter %d res %d new_info %d\n"
	    (if dir = A.BACKWARD then "reachable" else "real ef")
	    !ctr (List.length res) (List.length new_info);
	  print_state "new info" new_info;
	  flush stdout;*)
	  f res new_info in
    f s2 s2
  else
    let f y =
      inc_step();
      let pre = pre_exist dir m y reqst in
      triples_union s2 pre in
    setfix f s2


type ('pred,'anno) auok =
    AUok of ('pred,'anno) triples | AUfailed of ('pred,'anno) triples

(* A[phi1 U phi2] == phi2 \/ (phi1 /\ AXA[phi1 U phi2]) *)
let satAU dir ((cfg,_,states) as m) s1 s2 reqst print_graph =
  let strip s = List.map (function s -> (s,[],[])) (get_states s) in
  let ctr = ref 0 in
  inc satAU_calls;
  if s1 = []
  then AUok s2
  else
    (*let ctr = ref 0 in*)
    if !pNEW_INFO_OPT
    then
      let rec f y newinfo =
	inc_step();
	match newinfo with
	  [] -> AUok y
	| new_info ->
	    ctr := !ctr + 1;
	    print_graph y ctr;
	    let pre = pre_forall dir m new_info y reqst in
	    match triples_conj s1 pre with
	      [] -> AUok y
	    | first ->
		let res = triples_union first y in
		let new_info =
		  if not !something_dropped
		  then first
		  else setdiff res y in
		f res new_info in
	  try
	    (if !Flag_ctl.loop_in_src_code
	    then
	      let _ =
		satEU_forAW dir m (strip s1) (strip s2) reqst print_graph in
	      ());
	    f s2 s2
	  with AW -> AUfailed s2
    else
      if !Flag_ctl.loop_in_src_code
      then AUfailed s2
      else
	(*let setfix =
	   fix (function s1 -> function s2 ->
	   let s1 = List.map (function (s,th,w) -> (s,th,nub w)) s1 in
	   let s2 = List.map (function (s,th,w) -> (s,th,nub w)) s2 in
	   subseteq s1 s2) in for popl *)
	let f y =
	  inc_step();
	  ctr := !ctr + 1;
	  print_graph y ctr;
	  let pre = pre_forall dir m y y reqst in
	  triples_union s2 (triples_conj s1 pre) in
	AUok (setfix f s2)
;;


(* reqst could be the states of s1 *)
      (*
      let lstates = mkstates states reqst in
      let initial_removed =
	triples_complement lstates (triples_union s1 s2) in
      let initial_base = triples_conj s1 (triples_complement lstates s2) in
      let rec loop base removed =
	let new_removed =
	  triples_conj base (pre_exist dir m removed reqst) in
	let new_base =
	  triples_conj base (triples_complement lstates new_removed) in
	if supseteq new_base base
	then triples_union base s2
	else loop new_base new_removed in
      loop initial_base initial_removed *)

let satAW dir ((grp,_,states) as m) s1 s2 reqst =
  inc satAW_calls;
  if s1 = []
  then s2
  else
    (*
       This works extremely badly when the region is small and the end of the
       region is very ambiguous, eg free(x) ... x
       see free.c
    if !pNEW_INFO_OPT
    then
      let get_states l = setify(List.map (function (s,_,_) -> s) l) in
      let ostates = Common.union_set (get_states s1) (get_states s2) in
      let succ =
	(match dir with
	  A.FORWARD -> G.successors grp
	| A.BACKWARD -> G.predecessors grp) in
      let states =
	List.fold_left Common.union_set ostates (List.map succ ostates) in
      let negphi = triples_complement states s1 in
      let negpsi = triples_complement states s2 in
      triples_complement ostates
	(satEU dir m negpsi (triples_conj negphi negpsi) (Some ostates))
    else
       *)
      (*let ctr = ref 0 in*)
      let f y =
	inc_step();
	(*ctr := !ctr + 1;
	Printf.printf "iter %d y %d\n" !ctr (List.length y);
	print_state "y" y;
	flush stdout;*)
	let pre = pre_forall dir m y y reqst in
	(*print_state "pre" pre;*)
	let conj = triples_conj s1 pre in (* or triples_conj_AW *)
	triples_union s2 conj in
      let drop_wits = List.map (function (s,e,_) -> (s,e,[])) in
      (* drop wits on s1 represents that we don't want any witnesses from
	 the case that infinitely loops, only from the case that gets
	 out of the loop. s1 is like a guard. To see the problem, consider
	 an example where both s1 and s2 match some code after the loop.
	 we only want the witness from s2. *)
      setgfix f (triples_union (Common.nub(drop_wits s1)) s2)
;;

let satAF dir m s reqst =
  inc satAF_calls;
  if !pNEW_INFO_OPT
  then
    let rec f y newinfo =
      inc_step();
      match newinfo with
	[] -> y
      | new_info ->
	  let first = pre_forall dir m new_info y reqst in
	  let res = triples_union first y in
	  let new_info = setdiff res y in
	  f res new_info in
    f s s
  else
    let f y =
      inc_step();
      let pre = pre_forall dir m y y reqst in
      triples_union s pre in
    setfix f s

let satAG dir ((_,_,states) as m) s reqst =
  inc satAG_calls;
  let f y =
    inc_step();
    let pre = pre_forall dir m y y reqst in
    triples_conj y pre in
  setgfix f s

let satEG dir ((_,_,states) as m) s reqst =
  inc satEG_calls;
  let f y =
    inc_step();
    let pre = pre_exist dir m y reqst in
    triples_conj y pre in
  setgfix f s

(* **************************************************************** *)
(* Inner And - a way of dealing with multiple matches within a node *)
(* **************************************************************** *)
(* applied to the result of matching a node.  collect witnesses when the
states and environments are the same *)
(* not a good idea, poses problem for unparsing, because don't realize that
adjacent things come from different matches, leading to loss of newlines etc.
exple struct I { ... - int x; + int y; ...} *)

let inner_and trips = trips (*
  let rec loop = function
      [] -> ([],[])
    | (s,th,w)::trips ->
	let (cur,acc) = loop trips in
	(match cur with
	  (s',_,_)::_ when s = s' ->
	    let rec loop' = function
		[] -> [(s,th,w)]
	      |	((_,th',w') as t')::ts' ->
		  (match conj_subst th th' with
		    Some th'' -> (s,th'',union_wit w w')::ts'
		  | None -> t'::(loop' ts')) in
	    (loop' cur,acc)
	| _ -> ([(s,th,w)],cur@acc)) in
  let (cur,acc) =
    loop (List.sort state_compare trips) (* is this sort needed? *) in
  cur@acc *)

(* *************** *)
(* Partial matches *)
(* *************** *)

let filter_conj states unwanted partial_matches =
  let x =
    triples_conj (triples_complement states (unwitify unwanted))
      partial_matches in
  triples_conj (unwitify x) (triples_complement states x)

let strict_triples_conj strict states trips trips' =
  let res = triples_conj trips trips' in
  if !Flag_ctl.partial_match && strict = A.STRICT
  then
    let fail_left = filter_conj states trips trips' in
    let fail_right = filter_conj states trips' trips in
    let ors = triples_union fail_left fail_right in
    triples_union res ors
  else res

let strict_triples_conj_none strict states trips trips' =
  let res = triples_conj_none trips trips' in
  if !Flag_ctl.partial_match && strict = A.STRICT
  then
    let fail_left = filter_conj states trips trips' in
    let fail_right = filter_conj states trips' trips in
    let ors = triples_union fail_left fail_right in
    triples_union res ors
  else res

let left_strict_triples_conj strict states trips trips' =
  let res = triples_conj trips trips' in
  if !Flag_ctl.partial_match && strict = A.STRICT
  then
    let fail_left = filter_conj states trips trips' in
    triples_union res fail_left
  else res

let strict_A1 strict op failop dir ((_,_,states) as m) trips required_states =
  let res = op dir m trips required_states in
  if !Flag_ctl.partial_match && strict = A.STRICT
  then
    let states = mkstates states required_states in
    let fail = filter_conj states res (failop dir m trips required_states) in
    triples_union res fail
  else res

let strict_A2 strict op failop dir ((_,_,states) as m) trips trips'
    required_states =
  let res = op dir m trips trips' required_states in
  if !Flag_ctl.partial_match && strict = A.STRICT
  then
    let states = mkstates states required_states in
    let fail = filter_conj states res (failop dir m trips' required_states) in
    triples_union res fail
  else res

let strict_A2au strict op failop dir ((_,_,states) as m) trips trips'
    required_states print_graph =
  match op dir m trips trips' required_states print_graph with
    AUok res ->
      if !Flag_ctl.partial_match && strict = A.STRICT
      then
	let states = mkstates states required_states in
	let fail =
	  filter_conj states res (failop dir m trips' required_states) in
	AUok (triples_union res fail)
      else AUok res
  | AUfailed res -> AUfailed res

(* ********************* *)
(* Environment functions *)
(* ********************* *)

let drop_wits required_states s phi =
  match required_states with
    None -> s
  | Some states -> List.filter (function (s,_,_) -> List.mem s states) s


let print_required required =
  List.iter
    (function l ->
      Format.print_string "{";
      List.iter
	(function reqd ->
	  print_generic_substitution reqd; Format.print_newline())
	l;
      Format.print_string "}";
      Format.print_newline())
    required

exception Too_long

let extend_required trips required =
  if !Flag_ctl.partial_match
  then required
  else
      if !pREQUIRED_ENV_OPT
      then
    (* make it a set *)
	let envs =
	  List.fold_left
	    (function rest ->
	      function (_,t,_) -> if List.mem t rest then rest else t::rest)
	    [] trips in
	let envs = if List.mem [] envs then [] else envs in
	match (envs,required) with
	  ([],_) -> required
	| (envs,hd::tl) ->
	    (try
	      let hdln = List.length hd + 5 (* let it grow a little bit *) in
	      let (_,merged) =
		let add x (ln,y) =
		  if List.mem x y
		  then (ln,y)
		  else if ln + 1 > hdln then raise Too_long else (ln+1,x::y) in
		foldl
		  (function rest ->
		    function t ->
		      foldl
			(function rest ->
			  function r ->
			    match conj_subst t r with
			      None -> rest | Some th -> add th rest)
			rest hd)
		  (0,[]) envs in
	      merged :: tl
	    with Too_long -> envs :: required)
	| (envs,_) -> envs :: required
      else required

let drop_required v required =
  if !pREQUIRED_ENV_OPT
  then
    let res =
    inner_setify
      (List.map
	 (function l ->
	   inner_setify
	     (List.map (List.filter (function sub -> not(dom_sub sub = v))) l))
	 required) in
    (* check whether an entry has become useless *)
    List.filter (function l -> not (List.exists (function x -> x = []) l)) res
  else required

(* no idea how to write this function ... *)
let memo_label =
  (Hashtbl.create(101) : (P.t, (G.node * substitution) list) Hashtbl.t)

let satLabel label required p =
    let triples =
    if !pSATLABEL_MEMO_OPT
    then
      try
	let states_subs = Hashtbl.find memo_label p in
	List.map (function (st,th) -> (st,th,[])) states_subs
      with
	Not_found ->
	  let triples = setify(label p) in
	  Hashtbl.add memo_label p
	    (List.map (function (st,th,_) -> (st,th)) triples);
	  triples
    else setify(label p) in
    (* normalize first; conj_subst relies on sorting *)
    let ntriples = normalize triples in
    if !pREQUIRED_ENV_OPT
    then
      foldl
	(function rest ->
	  function ((s,th,_) as t) ->
	    if List.for_all
		(List.exists (function th' -> not(conj_subst th th' = None)))
		required
	    then t::rest
	    else rest)
	[] ntriples
    else ntriples

let get_required_states l =
  if !pREQUIRED_STATES_OPT && not !Flag_ctl.partial_match
  then
    Some(inner_setify (List.map (function (s,_,_) -> s) l))
  else None

let get_children_required_states dir (grp,_,_) required_states =
  if !pREQUIRED_STATES_OPT && not !Flag_ctl.partial_match
  then
    match required_states with
      None -> None
    | Some states ->
	let fn =
	  match dir with
	    A.FORWARD -> G.successors
	  | A.BACKWARD -> G.predecessors in
	Some (inner_setify (List.concat (List.map (fn grp) states)))
  else None

let reachable_table =
  (Hashtbl.create(50) : (G.node * A.direction, G.node list) Hashtbl.t)

(* like satEF, but specialized for get_reachable *)
let reachsatEF dir (grp,_,_) s2 =
  let dirop =
    match dir with A.FORWARD -> G.successors | A.BACKWARD -> G.predecessors in
  let union = unionBy compare (=) in
  let rec f y = function
      [] -> y
    | new_info ->
	let (pre_collected,new_info) =
	  List.partition (function Common.Left x -> true | _ -> false)
	    (List.map
	       (function x ->
		 try Common.Left (Hashtbl.find reachable_table (x,dir))
		 with Not_found -> Common.Right x)
	       new_info) in
	let y =
	  List.fold_left
	    (function rest ->
	      function Common.Left x -> union x rest
		| _ -> failwith "not possible")
	    y pre_collected in
	let new_info =
	  List.map
	    (function Common.Right x -> x | _ -> failwith "not possible")
	    new_info in
	let first = inner_setify (concatmap (dirop grp) new_info) in
	let new_info = setdiff first y in
	let res = new_info @ y in
	f res new_info in
  List.rev(f s2 s2) (* put root first *)

let get_reachable dir m required_states =
  match required_states with
    None -> None
  | Some states ->
      Some
	(List.fold_left
	   (function rest ->
	     function cur ->
	       if List.mem cur rest
	       then rest
	       else
		 Common.union_set
		   (try Hashtbl.find reachable_table (cur,dir)
		   with
		     Not_found ->
		       let states = reachsatEF dir m [cur] in
		       Hashtbl.add reachable_table (cur,dir) states;
		       states)
		   rest)
	   [] states)

let ctr = ref 0
let new_var _ =
  let c = !ctr in
  ctr := !ctr + 1;
  Printf.sprintf "_c%d" c

(* **************************** *)
(* End of environment functions *)
(* **************************** *)

type ('code,'value) cell = Frozen of 'code | Thawed of 'value

let rec satloop unchecked required required_states
    ((grp,label,states) as m) phi env =
  let rec loop unchecked required required_states phi =
    let res =
      match phi with
      A.False              -> []
    | A.True               -> triples_top states
    | A.Pred(p)            -> satLabel label required p
    | A.Uncheck(phi1) ->
	let unchecked = if !pUNCHECK_OPT then true else false in
	loop unchecked required required_states phi1
    | A.Not(phi)           ->
	let phires = loop unchecked required required_states phi in
	(*let phires =
	  List.map (function (s,th,w) -> (s,th,[])) phires in*)
	triples_complement (mkstates states required_states)
	  phires
    | A.Or(phi1,phi2)      ->
	triples_union
	  (loop unchecked required required_states phi1)
	  (loop unchecked required required_states phi2)
    | A.SeqOr(phi1,phi2)   ->
	let res1 = loop unchecked required required_states phi1 in
	let res2 = loop unchecked required required_states phi2 in
	let res1neg = unwitify res1 in
	let pm = !Flag_ctl.partial_match in
	(match (pm,res1,res2) with
	  (false,res1,[]) -> res1
	| (false,[],res2) -> res2
	| _ ->
	    triples_union res1
	      (triples_conj
		 (triples_complement (mkstates states required_states) res1neg)
		 res2))
    | A.And(strict,phi1,phi2)     ->
	(* phi1 is considered to be more likely to be [], because of the
	   definition of asttoctl.  Could use heuristics such as the size of
	   the term *)
	let pm = !Flag_ctl.partial_match in
	(match (pm,loop unchecked required required_states phi1) with
	  (false,[]) when !pLazyOpt -> []
	| (_,phi1res) ->
	    let new_required = extend_required phi1res required in
	    let new_required_states = get_required_states phi1res in
	    (match (pm,loop unchecked new_required new_required_states phi2)
	    with
	      (false,[]) when !pLazyOpt -> []
	    | (_,phi2res) ->
		strict_triples_conj strict
		  (mkstates states required_states)
		  phi1res phi2res))
    | A.AndAny(dir,strict,phi1,phi2)     ->
	(* phi2 can appear anywhere that is reachable *)
	let pm = !Flag_ctl.partial_match in
	(match (pm,loop unchecked required required_states phi1) with
	  (false,[]) -> []
	| (_,phi1res) ->
	    let new_required = extend_required phi1res required in
	    let new_required_states = get_required_states phi1res in
	    let new_required_states =
	      get_reachable dir m new_required_states in
	    (match (pm,loop unchecked new_required new_required_states phi2)
	    with
	      (false,[]) -> phi1res
	    | (_,phi2res) ->
		(match phi1res with
		  [] -> (* !Flag_ctl.partial_match must be true *)
		    if phi2res = []
		    then []
		    else
		      let s = mkstates states required_states in
		      List.fold_left
			(function a -> function b ->
			  strict_triples_conj strict s a [b])
			[List.hd phi2res] (List.tl phi2res)
		| [(state,_,_)] ->
		    let phi2res =
		      List.map (function (s,e,w) -> [(state,e,w)]) phi2res in
		    let s = mkstates states required_states in
		    List.fold_left
		      (function a -> function b ->
			strict_triples_conj strict s a b)
		      phi1res phi2res
		| _ ->
		    failwith
		      "only one result allowed for the left arg of AndAny")))
    | A.HackForStmt(dir,strict,phi1,phi2)     ->
	(* phi2 can appear anywhere that is reachable *)
	let pm = !Flag_ctl.partial_match in
	(match (pm,loop unchecked required required_states phi1) with
	  (false,[]) -> []
	| (_,phi1res) ->
	    let new_required = extend_required phi1res required in
	    let new_required_states = get_required_states phi1res in
	    let new_required_states =
	      get_reachable dir m new_required_states in
	    (match (pm,loop unchecked new_required new_required_states phi2)
	    with
	      (false,[]) -> phi1res
	    | (_,phi2res) ->
		    (* if there is more than one state, something about the
		       environment has to ensure that the right triples of
		       phi2 get associated with the triples of phi1.
		       the asttoctl2 has to ensure that that is the case.
		       these should thus be structural properties.
		       env of phi2 has to be a proper subset of env of phi1
		       to ensure all end up being consistent.  no new triples
		       should be generated.  strict_triples_conj_none takes
		       care of this.
		    *)
		    let s = mkstates states required_states in
		    List.fold_left
		      (function acc ->
			function (st,th,_) as phi2_elem ->
			  let inverse =
			    triples_complement [st] [(st,th,[])] in
			  strict_triples_conj_none strict s acc
			    (phi2_elem::inverse))
		      phi1res phi2res))
    | A.InnerAnd(phi) ->
	inner_and(loop unchecked required required_states phi)
    | A.EX(dir,phi)   ->
	let new_required_states =
	  get_children_required_states dir m required_states in
	satEX dir m (loop unchecked required new_required_states phi)
	  required_states
    | A.AX(dir,strict,phi) ->
	let new_required_states =
	  get_children_required_states dir m required_states in
	let res = loop unchecked required new_required_states phi in
	strict_A1 strict satAX satEX dir m res required_states
    | A.EF(dir,phi) ->
	let new_required_states = get_reachable dir m required_states in
	satEF dir m (loop unchecked required new_required_states phi)
	  new_required_states
    | A.AF(dir,strict,phi) ->
	if !Flag_ctl.loop_in_src_code
	then
	  loop unchecked required required_states
	    (A.AU(dir,strict,A.True,phi))
	else
	  let new_required_states = get_reachable dir m required_states in
	  let res = loop unchecked required new_required_states phi in
	  strict_A1 strict satAF satEF dir m res new_required_states
    | A.EG(dir,phi) ->
	let new_required_states = get_reachable dir m required_states in
	satEG dir m (loop unchecked required new_required_states phi)
	  new_required_states
    | A.AG(dir,strict,phi) ->
	let new_required_states = get_reachable dir m required_states in
	let res = loop unchecked required new_required_states phi in
	strict_A1 strict satAG satEF dir m res new_required_states
    | A.EU(dir,phi1,phi2) ->
	let new_required_states = get_reachable dir m required_states in
	(match loop unchecked required new_required_states phi2 with
	  [] when !pLazyOpt -> []
	| s2 ->
	    let new_required = extend_required s2 required in
	    let s1 = loop unchecked new_required new_required_states phi1 in
	    satEU dir m s1 s2 new_required_states
	      (fun y ctr -> print_graph_c grp new_required_states y ctr phi))
    | A.AW(dir,strict,phi1,phi2) ->
	let new_required_states = get_reachable dir m required_states in
	(match loop unchecked required new_required_states phi2 with
	  [] when !pLazyOpt -> []
	| s2 ->
	    let new_required = extend_required s2 required in
	    let s1 = loop unchecked new_required new_required_states phi1 in
	    strict_A2 strict satAW satEF dir m s1 s2 new_required_states)
    | A.AU(dir,strict,phi1,phi2) ->
	(*Printf.printf "using AU\n"; flush stdout;*)
	let new_required_states = get_reachable dir m required_states in
	(match loop unchecked required new_required_states phi2 with
	  [] when !pLazyOpt -> []
	| s2 ->
	    let new_required = extend_required s2 required in
	    let s1 = loop unchecked new_required new_required_states phi1 in
	    let res =
	      strict_A2au strict satAU satEF dir m s1 s2 new_required_states
		(fun y ctr ->
		  print_graph_c grp new_required_states y ctr phi) in
	    match res with
	      AUok res -> res
	    | AUfailed tmp_res ->
		(* found a loop, have to try AW *)
		(* the formula is
		   A[E[phi1 U phi2] & phi1 W phi2]
		   the and is nonstrict *)
		(* tmp_res is bigger than s2, so perhaps closer to s1 *)
		(*Printf.printf "using AW\n"; flush stdout;*)
		let s1 =
		  triples_conj
		    (satEU dir m s1 tmp_res new_required_states
		       (* no graph, for the moment *)
		       (fun y str -> ()))
		    s1 in
		strict_A2 strict satAW satEF dir m s1 s2 new_required_states
		)
    | A.Implies(phi1,phi2) ->
	loop unchecked required required_states (A.Or(A.Not phi1,phi2))
    | A.Exists (keep,v,phi) ->
	let new_required = drop_required v required in
	triples_witness v unchecked (not keep)
	  (loop unchecked new_required required_states phi)

    | A.Let(v,phi1,phi2) ->
	(* should only be used when the properties unchecked, required,
	   and required_states are known to be the same or at least
	   compatible between all the uses.  this is not checked. *)
	let res = loop unchecked required required_states phi1 in
	satloop unchecked required required_states m phi2 ((v,res) :: env)
    | A.LetR(dir,v,phi1,phi2) ->
	(* should only be used when the properties unchecked, required,
	   and required_states are known to be the same or at least
	   compatible between all the uses.  this is not checked. *)
	(* doesn't seem to be used any more *)
	let new_required_states = get_reachable dir m required_states in
	let res = loop unchecked required new_required_states phi1 in
	satloop unchecked required required_states m phi2 ((v,res) :: env)
    | A.Ref(v) ->
	let res = List.assoc v env in
	if unchecked
	then List.map (function (s,th,_) -> (s,th,[])) res
	else res
    | A.XX(phi) -> failwith "should have been removed" in
    if !Flag_ctl.bench > 0 then triples := !triples + (List.length res);
    let res = drop_wits required_states res phi (* ) *) in
    print_graph grp required_states res "" phi;
    res in

  loop unchecked required required_states phi
;;


(* SAT with tracking *)
let output str =
  Printf.printf "%s\n" str

let rec sat_verbose_loop unchecked required required_states annot maxlvl lvl
    ((_,label,states) as m) phi env =
  let anno res children = (annot lvl phi res children,res) in
  let satv unchecked required required_states phi0 env =
    sat_verbose_loop unchecked required required_states annot maxlvl (lvl+1)
      m phi0 env in
  if (lvl > maxlvl) && (maxlvl > -1) then
    anno (satloop unchecked required required_states m phi env) []
  else
    let (child,res) =
      match phi with
      A.False              -> anno [] []
    | A.True               -> anno (triples_top states) []
    | A.Pred(p)            ->
	output "label";
	anno (satLabel label required p) []
    | A.Uncheck(phi1) ->
	let unchecked = if !pUNCHECK_OPT then true else false in
	let (child1,res1) = satv unchecked required required_states phi1 env in
	output "uncheck";
	anno res1 [child1]
    | A.Not(phi1)          ->
	let (child,res) =
	  satv unchecked required required_states phi1 env in
	output "not";
	anno (triples_complement (mkstates states required_states) res) [child]
    | A.Or(phi1,phi2)      ->
	let (child1,res1) =
	  satv unchecked required required_states phi1 env in
	let (child2,res2) =
	  satv unchecked required required_states phi2 env in
	output "or";
	anno (triples_union res1 res2) [child1; child2]
    | A.SeqOr(phi1,phi2)      ->
	let (child1,res1) =
	  satv unchecked required required_states phi1 env in
	let (child2,res2) =
	  satv unchecked required required_states phi2 env in
	let res1neg =
	  List.map (function (s,th,_) -> (s,th,[])) res1 in
	output "seqor";
	let pm = !Flag_ctl.partial_match in
	(match (pm,res1,res2) with
	  (false,res1,[]) -> anno res1 [child1; child2]
	| (false,[],res2) -> anno res2 [child1; child2]
	| _ ->
	    anno (triples_union res1
		    (triples_conj
		       (triples_complement (mkstates states required_states)
			  res1neg)
		       res2))
	      [child1; child2])
    | A.And(strict,phi1,phi2)     ->
	let pm = !Flag_ctl.partial_match in
	(match (pm,satv unchecked required required_states phi1 env) with
	  (false,(child1,[])) ->
	    output "and"; anno [] [child1]
	| (_,(child1,res1)) ->
	    let new_required = extend_required res1 required in
	    let new_required_states = get_required_states res1 in
	    (match (pm,satv unchecked new_required new_required_states phi2
		      env) with
	      (false,(child2,[])) ->
		output "and"; anno [] [child1;child2]
	    | (_,(child2,res2)) ->
		output "and";
		let res =
		  strict_triples_conj strict
		    (mkstates states required_states)
		    res1 res2 in
		anno res [child1; child2]))
    | A.AndAny(dir,strict,phi1,phi2)     ->
	let pm = !Flag_ctl.partial_match in
	(match (pm,satv unchecked required required_states phi1 env) with
	  (false,(child1,[])) ->
	    output "and"; anno [] [child1]
	| (_,(child1,res1)) ->
	    let new_required = extend_required res1 required in
	    let new_required_states = get_required_states res1 in
	    let new_required_states =
	      get_reachable dir m new_required_states in
	    (match (pm,satv unchecked new_required new_required_states phi2
		env) with
	      (false,(child2,[])) ->
		output "andany";
		anno res1 [child1;child2]
	    | (_,(child2,res2)) ->
		(match res1 with
		  [] -> (* !Flag_ctl.partial_match must be true *)
		    if res2 = []
		    then anno [] [child1; child2]
		    else
		      let res =
			let s = mkstates states required_states in
			List.fold_left
			  (function a -> function b ->
			    strict_triples_conj strict s a [b])
			  [List.hd res2] (List.tl res2) in
		      anno res [child1; child2]
		| [(state,_,_)] ->
		    let res2 =
		      List.map (function (s,e,w) -> [(state,e,w)]) res2 in
		    output "andany";
		    let res =
		      let s = mkstates states required_states in
		      List.fold_left
			(function a -> function b ->
			  strict_triples_conj strict s a b)
			res1 res2 in
		    anno res [child1; child2]
		| _ ->
		    failwith
		      "only one result allowed for the left arg of AndAny")))
    | A.HackForStmt(dir,strict,phi1,phi2)     ->
	let pm = !Flag_ctl.partial_match in
	(match (pm,satv unchecked required required_states phi1 env) with
	  (false,(child1,[])) ->
	    output "and"; anno [] [child1]
	| (_,(child1,res1)) ->
	    let new_required = extend_required res1 required in
	    let new_required_states = get_required_states res1 in
	    let new_required_states =
	      get_reachable dir m new_required_states in
	    (match (pm,satv unchecked new_required new_required_states phi2
		env) with
	      (false,(child2,[])) ->
		output "andany";
		anno res1 [child1;child2]
	    | (_,(child2,res2)) ->
		let res =
		  let s = mkstates states required_states in
		  List.fold_left
		    (function acc ->
		      function (st,th,_) as phi2_elem ->
			let inverse =
			  triples_complement [st] [(st,th,[])] in
			strict_triples_conj_none strict s acc
			  (phi2_elem::inverse))
		    res1 res2 in
		anno res [child1; child2]))
    | A.InnerAnd(phi1) ->
	let (child1,res1) = satv unchecked required required_states phi1 env in
	output "uncheck";
	anno (inner_and res1) [child1]
    | A.EX(dir,phi1)       ->
	let new_required_states =
	  get_children_required_states dir m required_states in
	let (child,res) =
	  satv unchecked required new_required_states phi1 env in
	output "EX";
	anno (satEX dir m res required_states) [child]
    | A.AX(dir,strict,phi1)       ->
	let new_required_states =
	  get_children_required_states dir m required_states in
	let (child,res) =
	  satv unchecked required new_required_states phi1 env in
	output "AX";
	let res = strict_A1 strict satAX satEX dir m res required_states in
	anno res [child]
    | A.EF(dir,phi1)       ->
	let new_required_states = get_reachable dir m required_states in
	let (child,res) =
	  satv unchecked required new_required_states phi1 env in
	output "EF";
	anno (satEF dir m res new_required_states) [child]
    | A.AF(dir,strict,phi1) ->
	if !Flag_ctl.loop_in_src_code
	then
	  satv unchecked required required_states
	    (A.AU(dir,strict,A.True,phi1))
	    env
	else
	  (let new_required_states = get_reachable dir m required_states in
	  let (child,res) =
	    satv unchecked required new_required_states phi1 env in
	  output "AF";
	  let res =
	    strict_A1 strict satAF satEF dir m res new_required_states in
	  anno res [child])
    | A.EG(dir,phi1)       ->
	let new_required_states = get_reachable dir m required_states in
	let (child,res) =
	  satv unchecked required new_required_states phi1 env in
	output "EG";
	anno (satEG dir m res new_required_states) [child]
    | A.AG(dir,strict,phi1)       ->
	let new_required_states = get_reachable dir m required_states in
	let (child,res) =
	  satv unchecked required new_required_states phi1 env in
	output "AG";
	let res = strict_A1 strict satAG satEF dir m res new_required_states in
	anno res [child]

    | A.EU(dir,phi1,phi2)  ->
	let new_required_states = get_reachable dir m required_states in
	(match satv unchecked required new_required_states phi2 env with
	  (child2,[]) ->
	    output "EU";
	    anno [] [child2]
	| (child2,res2) ->
	    let new_required = extend_required res2 required in
	    let (child1,res1) =
	      satv unchecked new_required new_required_states phi1 env in
	    output "EU";
	    anno (satEU dir m res1 res2 new_required_states (fun y str -> ()))
	      [child1; child2])
    | A.AW(dir,strict,phi1,phi2)      ->
	failwith "should not be used" (*
	  let new_required_states = get_reachable dir m required_states in
	  (match satv unchecked required new_required_states phi2 env with
	    (child2,[]) ->
	      output (Printf.sprintf "AW %b" unchecked); anno [] [child2]
	  | (child2,res2) ->
	      let new_required = extend_required res2 required in
	      let (child1,res1) =
		satv unchecked new_required new_required_states phi1 env in
	      output (Printf.sprintf "AW %b" unchecked);
	      let res =
		strict_A2 strict satAW satEF dir m res1 res2
		  new_required_states in
	      anno res [child1; child2]) *)
    | A.AU(dir,strict,phi1,phi2)      ->
	let new_required_states = get_reachable dir m required_states in
	(match satv unchecked required new_required_states phi2 env with
	  (child2,[]) ->
	    output "AU"; anno [] [child2]
	| (child2,s2) ->
	    let new_required = extend_required s2 required in
	    let (child1,s1) =
	      satv unchecked new_required new_required_states phi1 env in
	    output "AU";
	    let res =
	      strict_A2au strict satAU satEF dir m s1 s2 new_required_states
		(fun y str -> ()) in
	    (match res with
	      AUok res ->
		anno res [child1; child2]
	    | AUfailed tmp_res ->
		(* found a loop, have to try AW *)
		(* the formula is
		   A[E[phi1 U phi2] & phi1 W phi2]
		   the and is nonstrict *)
		(* tmp_res is bigger than s2, so perhaps closer to s1 *)
	      output "AW";
	      let s1 =
		triples_conj
		  (satEU dir m s1 tmp_res new_required_states
		     (* no graph, for the moment *)
		     (fun y str -> ()))
		  s1 in
	      let res =
		strict_A2 strict satAW satEF dir m s1 s2 new_required_states in
	      anno res [child1; child2]))
    | A.Implies(phi1,phi2) ->
	satv unchecked required required_states
	  (A.Or(A.Not phi1,phi2))
	  env
    | A.Exists (keep,v,phi1)    ->
	let new_required = drop_required v required in
	let (child,res) =
	  satv unchecked new_required required_states phi1 env in
	output "exists";
	anno (triples_witness v unchecked (not keep) res) [child]
    | A.Let(v,phi1,phi2)   ->
	let (child1,res1) =
	  satv unchecked required required_states phi1 env in
	let (child2,res2) =
	  satv unchecked required required_states phi2 ((v,res1) :: env) in
	anno res2 [child1;child2]
    | A.LetR(dir,v,phi1,phi2)   ->
	let new_required_states = get_reachable dir m required_states in
	let (child1,res1) =
	  satv unchecked required new_required_states phi1 env in
	let (child2,res2) =
	  satv unchecked required required_states phi2 ((v,res1) :: env) in
	anno res2 [child1;child2]
    | A.Ref(v)             ->
	output "Ref";
	let res = List.assoc v env in
	let res =
	  if unchecked
	  then List.map (function (s,th,_) -> (s,th,[])) res
	  else res in
	anno res []
    | A.XX(phi) -> failwith "should have been removed" in
    let res1 = drop_wits required_states res phi in
    if not(res1 = res)
    then
      begin
	print_required_states required_states;
      print_state "after drop_wits" res1 end;
    (child,res1)

;;

let sat_verbose annotate maxlvl lvl m phi =
  sat_verbose_loop false [] None annotate maxlvl lvl m phi []

(* Type for annotations collected in a tree *)
type ('a) witAnnoTree = WitAnno of ('a * ('a witAnnoTree) list);;

let sat_annotree annotate m phi =
  let tree_anno l phi res chld = WitAnno(annotate l phi res,chld) in
    sat_verbose_loop false [] None tree_anno (-1) 0 m phi []
;;

(*
let sat m phi = satloop m phi []
;;
*)

let simpleanno l phi res =
  let pp s =
    Format.print_string ("\n" ^ s ^ "\n------------------------------\n");
    print_generic_algo (List.sort compare res);
    Format.print_string "\n------------------------------\n\n" in
  let pp_dir = function
      A.FORWARD -> ()
    | A.BACKWARD -> pp "^" in
  match phi with
    | A.False              -> pp "False"
    | A.True               -> pp "True"
    | A.Pred(p)            -> pp ("Pred" ^ (Dumper.dump p))
    | A.Not(phi)           -> pp "Not"
    | A.Exists(_,v,phi)    -> pp ("Exists " ^ (Dumper.dump(v)))
    | A.And(_,phi1,phi2)   -> pp "And"
    | A.AndAny(dir,_,phi1,phi2) -> pp "AndAny"
    | A.HackForStmt(dir,_,phi1,phi2) -> pp "HackForStmt"
    | A.Or(phi1,phi2)      -> pp "Or"
    | A.SeqOr(phi1,phi2)   -> pp "SeqOr"
    | A.Implies(phi1,phi2) -> pp "Implies"
    | A.AF(dir,_,phi1)     -> pp "AF"; pp_dir dir
    | A.AX(dir,_,phi1)     -> pp "AX"; pp_dir dir
    | A.AG(dir,_,phi1)     -> pp "AG"; pp_dir dir
    | A.AW(dir,_,phi1,phi2)-> pp "AW"; pp_dir dir
    | A.AU(dir,_,phi1,phi2)-> pp "AU"; pp_dir dir
    | A.EF(dir,phi1)       -> pp "EF"; pp_dir dir
    | A.EX(dir,phi1)	   -> pp "EX"; pp_dir dir
    | A.EG(dir,phi1)	   -> pp "EG"; pp_dir dir
    | A.EU(dir,phi1,phi2)  -> pp "EU"; pp_dir dir
    | A.Let (x,phi1,phi2)  -> pp ("Let"^" "^x)
    | A.LetR (dir,x,phi1,phi2) -> pp ("LetR"^" "^x); pp_dir dir
    | A.Ref(s)             -> pp ("Ref("^s^")")
    | A.Uncheck(s)         -> pp "Uncheck"
    | A.InnerAnd(s)        -> pp "InnerAnd"
    | A.XX(phi1)           -> pp "XX"
;;


(* pad: Rene, you can now use the module pretty_print_ctl.ml to
   print a ctl formula more accurately if you want.
   Use the print_xxx provided in the different module to call
   Pretty_print_ctl.pp_ctl.
 *)

let simpleanno2 l phi res =
  begin
    Pretty_print_ctl.pp_ctl (P.print_predicate, SUB.print_mvar) false phi;
    Format.print_newline ();
    Format.print_string "----------------------------------------------------";
    Format.print_newline ();
    print_generic_algo (List.sort compare res);
    Format.print_newline ();
    Format.print_string "----------------------------------------------------";
    Format.print_newline ();
    Format.print_newline ();
  end


(* ---------------------------------------------------------------------- *)
(* Benchmarking                                                           *)
(* ---------------------------------------------------------------------- *)

type optentry = bool ref * string
type options = {label : optentry; unch : optentry;
		 conj : optentry; compl1 : optentry; compl2 : optentry;
		 newinfo : optentry;
		 reqenv : optentry; reqstates : optentry}

let options =
  {label = (pSATLABEL_MEMO_OPT,"satlabel_memo_opt");
    unch = (pUNCHECK_OPT,"uncheck_opt");
    conj = (pTRIPLES_CONJ_OPT,"triples_conj_opt");
    compl1 = (pTRIPLES_COMPLEMENT_OPT,"triples_complement_opt");
    compl2 = (pTRIPLES_COMPLEMENT_SIMPLE_OPT,"triples_complement_simple_opt");
    newinfo = (pNEW_INFO_OPT,"new_info_opt");
    reqenv = (pREQUIRED_ENV_OPT,"required_env_opt");
    reqstates = (pREQUIRED_STATES_OPT,"required_states_opt")}

let baseline =
  [("none                    ",[]);
    ("label                   ",[options.label]);
    ("unch                    ",[options.unch]);
    ("unch and label          ",[options.label;options.unch])]

let conjneg =
  [("conj                    ", [options.conj]);
    ("compl1                  ", [options.compl1]);
    ("compl12                 ", [options.compl1;options.compl2]);
    ("conj/compl12            ", [options.conj;options.compl1;options.compl2]);
    ("conj unch satl          ", [options.conj;options.unch;options.label]);
(*
    ("compl1 unch satl        ", [options.compl1;options.unch;options.label]);
    ("compl12 unch satl       ",
     [options.compl1;options.compl2;options.unch;options.label]); *)
    ("conj/compl12 unch satl  ",
     [options.conj;options.compl1;options.compl2;options.unch;options.label])]

let path =
  [("newinfo                 ", [options.newinfo]);
    ("newinfo unch satl       ", [options.newinfo;options.unch;options.label])]

let required =
  [("reqenv                  ", [options.reqenv]);
    ("reqstates               ", [options.reqstates]);
    ("reqenv/states           ", [options.reqenv;options.reqstates]);
(*  ("reqenv unch satl        ", [options.reqenv;options.unch;options.label]);
    ("reqstates unch satl     ",
     [options.reqstates;options.unch;options.label]);*)
    ("reqenv/states unch satl ",
     [options.reqenv;options.reqstates;options.unch;options.label])]

let all_options =
  [options.label;options.unch;options.conj;options.compl1;options.compl2;
    options.newinfo;options.reqenv;options.reqstates]

let all =
  [("all                     ",all_options)]

let all_options_but_path =
  [options.label;options.unch;options.conj;options.compl1;options.compl2;
    options.reqenv;options.reqstates]

let all_but_path = ("all but path            ",all_options_but_path)

let counters =
  [(satAW_calls, "satAW", ref 0);
    (satAU_calls, "satAU", ref 0);
    (satEF_calls, "satEF", ref 0);
    (satAF_calls, "satAF", ref 0);
    (satEG_calls, "satEG", ref 0);
    (satAG_calls, "satAG", ref 0);
  (satEU_calls, "satEU", ref 0)]

let perms =
  map
    (function (opt,x) ->
      (opt,x,ref 0.0,ref 0,
       List.map (function _ -> (ref 0, ref 0, ref 0)) counters))
    [List.hd all;all_but_path]
  (*(all@baseline@conjneg@path@required)*)

exception Out

let rec iter fn = function
    1 -> fn()
  | n -> let _ = fn() in
    (Hashtbl.clear reachable_table;
     Hashtbl.clear memo_label;
     triples := 0;
     iter fn (n-1))

let copy_to_stderr fl =
  let i = open_in fl in
  let rec loop _ =
    Printf.fprintf stderr "%s\n" (input_line i);
    loop() in
  try loop() with _ -> ();
  close_in i

let bench_sat (_,_,states) fn =
  List.iter (function (opt,_) -> opt := false) all_options;
  let answers =
    concatmap
      (function (name,options,time,trips,counter_info) ->
	let iterct = !Flag_ctl.bench in
	if !time > float_of_int timeout then time := -100.0;
	if not (!time = -100.0)
	then
	  begin
	    Hashtbl.clear reachable_table;
	    Hashtbl.clear memo_label;
	    List.iter (function (opt,_) -> opt := true) options;
	    List.iter (function (calls,_,save_calls) -> save_calls := !calls)
	      counters;
	    triples := 0;
	    let res =
	      let bef = Sys.time() in
	      try
		Common.timeout_function "bench" timeout
		  (fun () ->
		    let bef = Sys.time() in
		    let res = iter fn iterct in
		    let aft = Sys.time() in
		    time := !time +. (aft -. bef);
		    trips := !trips + !triples;
		    List.iter2
		      (function (calls,_,save_calls) ->
			function (current_calls,current_cfg,current_max_cfg) ->
			  current_calls :=
			    !current_calls + (!calls - !save_calls);
			  if (!calls - !save_calls) > 0
			  then
			    (let st = List.length states in
			    current_cfg := !current_cfg + st;
			    if st > !current_max_cfg
			    then current_max_cfg := st))
		      counters counter_info;
		    [res])
	      with
		Common.Timeout ->
		  begin
		    let aft = Sys.time() in
		    time := -100.0;
		    Printf.fprintf stderr "Timeout at %f on: %s\n"
		      (aft -. bef) name;
		    []
		  end in
	    List.iter (function (opt,_) -> opt := false) options;
	    res
	  end
	else [])
      perms in
  Printf.fprintf stderr "\n";
  match answers with
    [] -> []
  | res::rest ->
      (if not(List.for_all (function x -> x = res) rest)
      then
	(List.iter (print_state "a state") answers;
	 Printf.printf "something doesn't work\n");
      res)

let print_bench _ =
  let iterct = !Flag_ctl.bench in
  if iterct > 0
  then
    (List.iter
       (function (name,options,time,trips,counter_info) ->
	 Printf.fprintf stderr "%s Numbers: %f %d "
	   name (!time /. (float_of_int iterct)) !trips;
	 List.iter
	   (function (calls,cfg,max_cfg) ->
	     Printf.fprintf stderr "%d %d %d " (!calls / iterct) !cfg !max_cfg)
	   counter_info;
	 Printf.fprintf stderr "\n")
       perms)

(* ---------------------------------------------------------------------- *)
(* preprocessing: ignore irrelevant functions *)

let preprocess (cfg,label,preproc,_) = function
    [] -> true (* no information, try everything *)
  | l ->
      let sz = G.size cfg in
      let verbose_output pred = function
	  false ->
	    Printf.printf "did not find:\n";
	    P.print_predicate pred; Format.print_newline()
	| true ->
	    Printf.printf "found:\n";
	    P.print_predicate pred; Format.print_newline();
	    Printf.printf "but it was not enough\n" in
      let get_any verbose x =
	let res = preproc x in
	(if verbose then verbose_output x res);
	res in
      let get_all l =
	(* don't bother testing when there are more patterns than nodes *)
	if List.length l > sz-2
	then false
	else List.for_all (get_any false) l in
      if List.exists get_all l
      then true
      else
	(if !Flag_ctl.verbose_match
	then
	   List.iter (List.iter (function x -> let _ = get_any true x in ()))
	    l;
	 false)

let filter_partial_matches trips =
  if !Flag_ctl.partial_match
  then
    let anynegwit = (* if any is neg, then all are *)
      List.exists (function A.NegWit _ -> true | A.Wit _ -> false) in
    let (bad,good) =
      List.partition (function (s,th,wit) -> anynegwit wit) trips in
    (match bad with
      [] -> ()
    | _ -> print_state "partial matches" bad; Format.print_newline());
    good
  else trips

(* ---------------------------------------------------------------------- *)
(* Main entry point for engine *)
let sat m phi reqopt =
  try
    (match !Flag_ctl.steps with
      None -> step_count := 0
    | Some x -> step_count := x);
    Hashtbl.clear reachable_table;
    Hashtbl.clear memo_label;
    let (x,label,preproc,states) = m in
    if (!Flag_ctl.bench > 0) || preprocess m reqopt
    then
      ((* to drop when Yoann initialized this flag *)
      if List.exists (G.extract_is_loop x) states
      then Flag_ctl.loop_in_src_code := true;
      let m = (x,label,List.sort compare states) in
      let res =
	if(!Flag_ctl.verbose_ctl_engine)
	then
	  let fn _ = snd (sat_annotree simpleanno2 m phi) in
	  if !Flag_ctl.bench > 0
	  then bench_sat m fn
	  else fn()
	else
	  let fn _ = satloop false [] None m phi [] in
	  if !Flag_ctl.bench > 0
	  then bench_sat m fn
	  else Common.profile_code "ctl" (fun _ -> fn()) in
      let res = filter_partial_matches res in
      (*
      Printf.printf "steps: start %d, stop %d\n"
	(match !Flag_ctl.steps with Some x -> x | _ -> 0)
	!step_count;
      Printf.printf "triples: %d\n" !triples;
      print_state "final result" res;
      *)
      List.sort compare res)
    else
      (if !Flag_ctl.verbose_ctl_engine
      then Common.pr2 "missing something required";
       [])
  with Steps -> []

(* ********************************************************************** *)
(* End of Module: CTL_ENGINE                                              *)
(* ********************************************************************** *)
end
;;
