(* **********************************************************************
 *
 * Implementation of a Witness Tree model checking engine for CTL-FVex 
 * 
 *
 * $Id$
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
    val predecessors: cfg -> node -> node list
    val print_node : node -> unit
  end
;;

module OGRAPHEXT_GRAPH = 
  struct
    type node = int;;
    type cfg = (string,unit) Ograph_extended.ograph_extended;;
    let predecessors cfg n = List.map fst ((cfg#predecessors n)#tolist);;
    let print_node i = Format.print_string (Common.i_to_s i)
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


exception TODO_CTL            (* implementation still not quite done so... *)
exception NEVER_CTL			(* Some things should never happen *)

(* ---------------------------------------------------------------------- *)
(* Misc. useful generic functions                                         *)
(* ---------------------------------------------------------------------- *)

let head = List.hd

let tail l = 
  match l with
    [] -> []
  | (x::xs) -> xs
;;

let foldl = List.fold_left;;

let foldl1 f xs = foldl f (head xs) (tail xs)

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

let some_tolist_alt opts = concatmap (maybe (fun x -> [x]) []) opts

let rec some_tolist opts =
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

let rec nub ls = nubBy (=) ls

let setifyBy eq xs = List.sort compare (nubBy eq xs);;

let setify xs = List.sort compare (nub xs);;

let unionBy eq xs = function
    [] -> xs
  | ys ->
      let rec loop = function
	  [] -> ys
	| x::xs -> if memBy eq x ys then loop xs else x::(loop xs) in
      List.sort compare (loop xs)
;;

let union xs ys = unionBy (=) xs ys;;

let setdiff xs ys = filter (fun x -> not (List.mem x ys)) xs;;

let subseteqBy eq xs ys = List.for_all (fun x -> memBy eq x ys) xs;;

let subseteq xs ys = List.for_all (fun x -> List.mem x ys) xs;;

let setequalBy eq xs ys = (subseteqBy eq xs ys) & (subseteqBy eq ys xs);;

let setequal xs ys = (subseteq xs ys) & (subseteq ys xs);;

(* Fix point calculation *)
let rec fix eq f x =
  let x' = f x in if (eq x x') then x' else fix eq f x'
;;

(* Fix point calculation on set-valued functions *)
let setfix f x = setify (fix setequal f x);;

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
  | A.NegWit  (state, subst, anno, childrens) -> 
      Format.print_string "!";
      print_generic_witness(A.Wit  (state, subst, anno, childrens))

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
    l;
  Printf.printf "\n"
    
    
    
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
  match (sub,sub',eqx (dom_sub sub) (dom_sub sub')) with
    | (A.Subst (x,v),A.Subst (x',v'),true) -> 
	if (v === v')
	then Some [A.Subst(x, v >+< v')]
	else None
    | (A.NegSubst(x,v),A.Subst(x',v'),true) ->
	if (not (v === v'))
	then Some [A.Subst(x',v')]
	else None
    | (A.Subst(x,v),A.NegSubst(x',v'),true) ->
	if (not (v === v'))
	then Some [A.Subst(x,v)]
	else None
    | (A.NegSubst(x,v),A.NegSubst(x',v'),true) ->
	if (v === v')
	then Some [A.NegSubst(x,v)]
	else Some [A.NegSubst(x,v);A.NegSubst(x',v')]
    | _ -> Some [sub;sub']
;;

(* NOTE: functor *)
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
	  | _ -> 0
	else res)
      theta in
  let rec loop = function
      [] -> []
    | (A.Subst(x,v)::A.NegSubst(y,v')::rest) when x = y -> (*generic enough?*)
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
	try
	  Some (clean_subst (
		  foldl
		    (function rest ->
		       function sub ->
			 foldl
			   (function rest ->
			      function sub' ->
				match (merge_sub sub sub') with
				  | Some subs -> 
				      subs @ rest
				  | _       -> raise SUBST_MISMATCH)
			   rest theta')
		    [] theta))
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

let union_wit wit wit' = union wit wit';;

let negate_wit wit =
  match wit with
    | A.Wit(s,th,anno,ws)    -> A.NegWit(s,th,anno,ws)
    | A.NegWit(s,th,anno,ws) -> A.Wit(s,th,anno,ws)
;;

let negate_wits wits =
  List.sort compare (map (fun wit -> [negate_wit wit]) wits);;


(* ************************* *)
(* Triples                   *)
(* ************************* *)

(* Triples are equal when the constituents are equal *)
let eq_trip (s,th,wit) (s',th',wit') =
  (s = s') && (eq_subst th th') && (eq_wit wit wit');;

let triples_top states = map (fun s -> (s,top_subst,top_wit)) states;;

let triples_union trips trips' = unionBy eq_trip trips trips';;

let triples_conj trips trips' =
  setify (
    foldl
      (function rest ->
	 function (s1,th1,wit1) ->
	   foldl
	     (function rest ->
		function (s2,th2,wit2) ->
		  if (s1 = s2) then
		    match (conj_subst th1 th2) with
		      | Some th -> (s1,th,union_wit wit1 wit2)::rest
		      | _       -> rest
		  else rest)
	     rest trips')
      [] trips)
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
    setify (
    foldl
      (function rest ->
	 function (s1,th1,wit1) ->
	   foldl
	     (function rest ->
		function (s2,th2,wit2) ->
		  match compatible_states(s1,s2) with
		    Some s ->
		      (match (conj_subst th1 th2) with
			Some th -> (s,th,union_wit wit1 wit2)::rest
		      | _       -> rest)
		  | _ -> rest)
	     rest trips')
      [] trips)
;;

let triple_negate (s,th,wits) = 
  let negstates = [(NegState [s],top_subst,top_wit)] in
  let negths = map (fun th -> (PosState s,th,top_wit)) (negate_subst th) in
  let negwits = map (fun nwit -> (PosState s,th,nwit)) (negate_wits wits) in
    triples_union negstates (triples_union negths negwits)

(* FIX ME: it is not necessary to do full conjunction *)
let triples_complement states (trips : ('pred, 'anno) triples) =
  let cleanup (s,th,wit) =
    match s with
      | PosState s' -> [(s',th,wit)]
      | NegState ss ->
	  assert (th=top_subst);
	  assert (wit=top_wit);
	  map (fun st -> (st,top_subst,top_wit)) (setdiff states ss) in
  let rec compl trips =
    match trips with
      | [] -> List.map (function s -> (PosState s, top_subst, top_wit)) states
      | (t::[]) -> triple_negate t
      | (t::ts) -> triples_state_conj (triple_negate t) (compl ts) in
  setify(concatmap cleanup (compl trips))
;;


(* ********************************** *)
(* END OF NEGATION (NegState style)   *)
(* ********************************** *)

type 'a wit = Wit of 'a | And of 'a wit * 'a wit | Not of 'a wit | True

(* Conjunction on triples with "special states" *)
let triples_state_wit_conj trips trips' =
  let mkand = function
      (True,x) -> x
    | (x,True) -> x
    | (Not(True),x) -> Not(True)
    | (x,Not(True)) -> Not(True)
    | (x,y) when x = y -> x
    | (x,y) -> And(x,y) in
  setify (
    foldl
      (function rest ->
	 function (s1,th1,wit1) ->
	   foldl
	     (function rest ->
		function (s2,th2,wit2) ->
		  match compatible_states(s1,s2) with
		    Some s ->
		      (match (conj_subst th1 th2) with
			Some th -> (s,th,mkand(wit1,wit2))::rest
		      | _       -> rest)
		  | _ -> rest)
	     rest trips')
      [] trips)
;;

let negate_wit_wits wits =
  match wits with
    True -> []
  | wits -> [Not(wits)];;

let triple_wit_negate (s,th,wits) = 
  let negstates = [(NegState [s],top_subst,True)] in
  let negths = map (fun th -> (PosState s,th,True)) (negate_subst th) in
  let negwits =
    map (fun nwit -> (PosState s,th,nwit)) (negate_wit_wits wits) in
    triples_union negstates (triples_union negths negwits)

(* FIX ME: it is not necessary to do full conjunction *)
let triples_wit_complement states trips =
  let cleanup (s,th,wit) =
    match s with
      | PosState s' -> [(s',th,wit)]
      | NegState ss ->
	  assert (th=top_subst);
	  assert (wit=True);
	  map (fun st -> (st,top_subst,True)) (setdiff states ss) in
  let rec compl trips =
    match trips with
      | [] -> List.map (function s -> (PosState s, top_subst, True)) states
      | (t::[]) -> triple_wit_negate t
      | (t::ts) -> triples_state_wit_conj (triple_wit_negate t) (compl ts) in
  (* setify not needed here, will be done by others *)
  concatmap cleanup (compl trips)
;;

(* even makes a wit for true, which manages to keep around some extra info,
eg (15,p = paren(1),[]) v (15,p != paren(1),[]) double negated is
(15,p = paren(1),[]) v (15,p != paren(1),[]), not (15,[],[]) *)
let witify trips =
  List.map (function (s,th,wit) -> (s,th,Wit wit)) trips

let rec wit2c = function
    Wit(_) -> "wit"
  | And(a1,a2) -> Printf.sprintf "%s & %s" (wit2c a1) (wit2c a2)
  | Not(a) -> Printf.sprintf "(not %s)" (wit2c a)
  | True -> "true"

let unwitify trips =
  setify
    (List.concat
       (List.map
	  (function (s,th,wit) ->
	    let push_not =
	      let rec posloop = function
		  Wit(w) -> [w]
		| And(a1,a2) ->
		    let a1res = posloop a1 in
		    let a2res = posloop a2 in
		    foldl
		      (function rest -> function a1 ->
			foldl
			  (function rest -> function a2 ->
			    (a1@a2)::rest)
			  rest a2res)
		      [] a1res
		| Not(a) -> negloop a
		| True -> [[]]
	      and negloop = function
		  Wit(w) -> failwith "unexpected negated witness"
		| And(a1,a2) -> (negloop a1) @ (negloop a2)
		| Not(a) -> posloop a
		| True -> [] in
	      posloop in
	    List.map (function wit -> (s,th,setify wit)) (push_not wit))
	  trips))

(* ********************************** *)
(* END OF NEGATION (NegState/Wit style)   *)
(* ********************************** *)


let triples_witness x trips = 
  let mkwit (s,th,wit) =
    let (th_x,newth) = split_subst th x in
    if th_x = []
    then
      (SUB.print_mvar x; Format.print_flush();
      print_state ": empty witness from" [(s,th,wit)]);
    (s,newth,[A.Wit(s,th_x,[],wit)]) in	(* [] = annotation *)
  (* not sure that nub is needed here.  would require empty witness case to
     make a duplicate. *)
  setify (map mkwit trips)
;;



(* ---------------------------------------------------------------------- *)
(* SAT  - Model Checking Algorithm for CTL-FVex                           *)
(*                                                                        *)
(* TODO: Implement _all_ operators (directly)                             *)
(* ---------------------------------------------------------------------- *)


(* ************************************* *)
(* The SAT algorithm and special helpers *)
(* ************************************* *)

(* doesn't call setify, because pre_forall works better without it *)
let rec pre_exist (grp,_,_) y =
  let exp (s,th,wit) = map (fun s' -> (s',th,wit)) (G.predecessors grp s) in
  let res = concatmap exp y in
  res
;;

let pre_forall ((_,_,states) as m) y = 
  let arg = triples_wit_complement states (witify y) in
  let res = unwitify (triples_wit_complement states (pre_exist m arg)) in
  res
;;

let satEX m s = setify (pre_exist m s);;

let satAX m s = pre_forall m s
;;
  

(* A[phi1 U phi2] == phi2 \/ (phi1 /\ AXA[phi1 U phi2]) *)
let satAU m s1 s2 = 
  if s1 = []
  then s2
  else
    let f y = 
      let first = pre_forall m y in
      let second = triples_conj s1 first in
      triples_union s2 second in
    let res = setfix f s2 in
    res
;;

(* E[phi1 U phi2] == phi2 \/ (phi1 /\ EXE[phi1 U phi2]) *)
let satEU m s1 s2 = 
  if s1 = []
  then s2
  else
    let f y = triples_union s2 (triples_conj s1 (setify (pre_exist m y))) in 
    setfix f s2
;;

let satAF ((_,_,states) as m) s =
  let f y =
    let pre = pre_forall m y in
    union y pre in
  setfix f s

let satEG ((_,_,states) as m) s = satEU m s (triples_top states)

let satEF ((_,_,states) as m) s = satEU m (triples_top states) s

let satAG ((_,_,states) as m) s = satAU m s (triples_top states)

let drop_wits keep_negwits s =
  let contains_poswits =
    List.exists
      (function
	  A.Wit(_,_,_,_) -> print_state "dropping a witness" s; true
	| _ -> false) in
  let contains_negwits =
    List.exists
      (function
	  A.NegWit(_,_,_,_) -> print_state "dropping a witness" s; true
	| _ -> false) in
  List.filter
    (function (s,th,wits) ->
      if keep_negwits
      then not(contains_poswits wits)
      else not(contains_negwits wits))
    s

type ('code,'value) cell = Frozen of 'code | Thawed of 'value

let rec satloop keep_negwits ((grp,label,states) as m) phi env check_conj =
  let rec loop keep_negwits phi : ('pred, 'anno) triples =
    let res =
    match A.unwrap phi with
      A.False              -> []
    | A.True               -> triples_top states
    | A.Pred(p)            -> (label p) (* NOTE: Assume well-formed *)
    | A.Not(phi)           ->
	triples_complement states (loop (not keep_negwits) phi)
    | A.Or(phi1,phi2)      ->
	triples_union (loop keep_negwits phi1) (loop keep_negwits phi2)
    | A.And(phi1,phi2)     ->
	(* phi1 is considered to be more likely to be [], because of the
	   definition of asttoctl.  Could use heuristics such as the size of
	   the term *)
	(match loop keep_negwits phi1 with
	  [] -> []
	| phi1res ->
	    (match loop keep_negwits phi2 with
	      [] -> []
	    | phi2res ->
		let res = triples_conj phi1res phi2res in
		check_conj phi phi1res phi2res res;
		res))
    | A.EX(phi)            -> satEX m (loop keep_negwits phi)
    | A.AX(phi)            -> satAX m (loop keep_negwits phi)
    | A.EF(phi)            -> satEF m (loop keep_negwits phi)
    | A.AF(phi)            -> satAF m (loop keep_negwits phi)
    | A.EG(phi)            -> satEG m (loop keep_negwits phi)
    | A.AG(phi1)           -> satAG m (loop keep_negwits phi)
    | A.EU(phi1,phi2)      ->
	(match loop keep_negwits phi2 with
	  [] -> []
	| s2 -> satEU m (loop keep_negwits phi1) s2)
    | A.AU(phi1,phi2)      ->
	(match loop keep_negwits phi2 with
	  [] -> []
	| s2 -> satAU m (loop keep_negwits phi1) s2)
    | A.Implies(phi1,phi2) ->
	loop keep_negwits (A.rewrap phi (A.Or(A.rewrap phi (A.Not phi1),phi2)))
    | A.Exists (v,phi)     -> triples_witness v (loop keep_negwits phi)
    | A.Let(v,phi1,phi2)   ->
	satloop keep_negwits m phi2
	  ((v,(ref (Frozen phi1),ref (Frozen phi1))) :: env)
	  check_conj
    | A.Ref(v)             ->
	let cell =
	  match (keep_negwits,List.assoc v env) with
	    (false,(cell,_)) -> cell
	  | (true,(_,cell)) -> cell in
	(match !cell with
	  Thawed v -> v
	| Frozen phi ->
	    let res = loop keep_negwits phi in cell := Thawed res; res) in
    drop_wits keep_negwits res in
  
  loop keep_negwits phi
;;    


(* SAT with tracking *)
let rec sat_verbose_loop keep_negwits annot maxlvl lvl
    ((_,label,states) as m) phi env check_conj =
  let anno res children = (annot lvl phi res children,res) in
  let satv keep_negwits phi0 env =
    sat_verbose_loop keep_negwits annot maxlvl (lvl+1)
      m phi0 env check_conj in
  if (lvl > maxlvl) && (maxlvl > -1) then
    anno (satloop keep_negwits m phi env check_conj) []
  else
    let (child,res) =
      match A.unwrap phi with
      A.False              -> anno [] []
    | A.True               -> anno (triples_top states) []
    | A.Pred(p)            -> anno (label p) []
    | A.Not(phi1)          -> 
	let (child,res) = satv (not keep_negwits) phi1 env in
	Printf.printf "not\n"; flush stdout;
	anno (triples_complement states res) [child]
    | A.Or(phi1,phi2)      -> 
	let (child1,res1) = satv keep_negwits phi1 env in
	let (child2,res2) = satv keep_negwits phi2 env in
	Printf.printf "or\n"; flush stdout;
	anno (triples_union res1 res2) [child1; child2]
    | A.And(phi1,phi2)     -> 
	(match satv keep_negwits phi1 env with
	  (child1,[]) -> anno [] [child1]
	| (child1,res1) ->
	    (match satv keep_negwits phi2 env with
	      (child2,[]) -> anno [] [child1;child2]
	    | (child2,res2) ->
		Printf.printf "and\n"; flush stdout;
		anno (triples_conj res1 res2) [child1; child2]))
    | A.EX(phi1)           -> 
	let (child,res) = satv keep_negwits phi1 env in
	Printf.printf "EX\n"; flush stdout;
	anno (satEX m res) [child]
    | A.AX(phi1)           -> 
	let (child,res) = satv keep_negwits phi1 env in
	Printf.printf "AX\n"; flush stdout;
	anno (pre_forall m res) [child]
    | A.EF(phi1)           -> 
	let (child,res) = satv keep_negwits phi1 env in
	Printf.printf "EF\n"; flush stdout;
	anno (satEF m res) [child]
    | A.AF(phi1)           -> 
	let (child,res) = satv keep_negwits phi1 env in
	Printf.printf "AF\n"; flush stdout;
	anno (satAF m res) [child]
    | A.EG(phi1)           -> 
	let (child,res) = satv keep_negwits phi1 env in
	Printf.printf "EG\n"; flush stdout;
	anno (satEG m res) [child]
    | A.AG(phi1)            -> 
	let (child,res) = satv keep_negwits phi1 env in
	Printf.printf "AG\n"; flush stdout;
	anno (satAG m res) [child]
	  
    | A.EU(phi1,phi2)      -> 
	(match satv keep_negwits phi2 env with
	  (child2,[]) -> anno [] [child2]
	| (child2,res2) ->
	    let (child1,res1) = satv keep_negwits phi1 env in
	    Printf.printf "EU\n"; flush stdout;
	    anno (satEU m res1 res2) [child1; child2])
    | A.AU(phi1,phi2)      -> 
	(match satv keep_negwits phi2 env with
	  (child2,[]) -> anno [] [child2]
	| (child2,res2) ->
	    let (child1,res1) = satv keep_negwits phi1 env in
	    Printf.printf "AU\n"; flush stdout;
	    anno (satAU m res1 res2) [child1; child2])
    | A.Implies(phi1,phi2) -> 
	let (child1,res1) = satv (not keep_negwits) phi1 env in
	let (child2,res2) = satv keep_negwits phi2 env in
	anno (triples_union (triples_complement states res1) res2)
	  [child1; child2]
    | A.Exists (v,phi1)    -> 
	let (child,res) = satv keep_negwits phi1 env in
	anno (triples_witness v res) [child]
    | A.Let(v,phi1,phi2)   ->
	let (child2,res2) =
	  satv keep_negwits phi2
	    ((v,(ref (Frozen phi1),ref (Frozen phi1))) :: env) in
	anno res2 [child2]
    | A.Ref(v)             ->
	let cell =
	  match (keep_negwits,List.assoc v env) with
	    (false,(cell,_)) -> cell
	  | (true,(_,cell)) -> cell in
	(match !cell with
	  Thawed v -> anno v []
	| Frozen phi ->
	    let (child,res) = satv keep_negwits phi env in
	    cell := Thawed res;
	    anno res [child]) in
    (child,drop_wits keep_negwits res)
	
;;

let sat_verbose annotate maxlvl lvl m phi check_conj =
  sat_verbose_loop false annotate maxlvl lvl m phi [] check_conj

(* Type for annotations collected in a tree *)
type ('a) witAnnoTree = WitAnno of ('a * ('a witAnnoTree) list);;

let sat_annotree annotate m phi check_conj =
  let tree_anno l phi res chld = WitAnno(annotate l phi res,chld) in
    sat_verbose_loop false tree_anno (-1) 0 m phi [] check_conj
;;

(*
let sat m phi = satloop m phi []
;;
*)

let simpleanno l phi res =
  let pp s = 
    Format.print_string ("\n" ^ s ^ "\n------------------------------\n"); 
    print_generic_algo res;
    Format.print_string "\n------------------------------\n\n"
  in
  match A.unwrap phi with
    | A.False              -> pp "False"
    | A.True               -> pp "True"
    | A.Pred(p)            -> pp ("Pred" ^ (Dumper.dump p))
    | A.Not(phi)           -> pp "Not"
    | A.Exists(v,phi)      -> pp ("Exists " ^ (Dumper.dump(v)))
    | A.And(phi1,phi2)     -> pp "And"
    | A.Or(phi1,phi2)      -> pp "Or"
    | A.Implies(phi1,phi2) -> pp "Implies"
    | A.AF(phi1)           -> pp "AF"
    | A.AX(phi1)           -> pp "AX"
    | A.AG(phi1)           -> pp "AG"
    | A.AU(phi1,phi2)      -> pp "AU"
    | A.EF(phi1)           -> pp "EF"
    | A.EX(phi1)	   -> pp "EX"
    | A.EG(phi1)	   -> pp "EG"
    | A.EU(phi1,phi2)	   -> pp "EU"
    | A.Let (x,phi1,phi2)  -> pp ("Let"^" "^x)
    | A.Ref(s)             -> pp ("Ref("^s^")")
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
    print_generic_algo res;
    Format.print_newline ();
    Format.print_string "----------------------------------------------------";
    Format.print_newline ();
    Format.print_newline ();
  end
  



(* Main entry point for engine *)
let sat m phi check_conj = 
  let (x,y,states) = m in
  let m = (x,y,List.sort compare states) in
  let res =
    if(!Flag_ctl.verbose_ctl_engine)
    then snd (sat_annotree simpleanno2 m phi check_conj)
    else satloop false m phi [] check_conj in
(* print_state "final result" res;*)
  res
;;

(* ********************************************************************** *)
(* End of Module: CTL_ENGINE                                              *)
(* ********************************************************************** *)
end
;;

