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
  end
;;

module OGRAPHEXT_GRAPH = 
  struct
    type node = int;;
    type cfg = (string,unit) Ograph_extended.ograph_extended;;
    let predecessors cfg n = List.map fst ((cfg#predecessors n)#tolist);;
  end
;;


(* ********************************************************************** *)


exception TODO_CTL            (* implementation still not quite done so... *)
exception NEVER_CTL			(* Some things should never happen *)

(* ---------------------------------------------------------------------- *)
(* Misc. useful generic functions                                         *)
(* ---------------------------------------------------------------------- *)

(* Sanity-preserving definitions *)
let itos i = string_of_int i;;

let head = List.hd

let tail l = 
  match l with
    [] -> []
  | (x::xs) -> xs
;;

let foldl = List.fold_left;;

let foldl1 f xs = foldl f (head xs) (tail xs)

let foldr = List.fold_right;;

let rec scanl f q xs = 
  q :: (match xs with
	  | []      -> []
	  | (x::xs) -> scanl f (f q x) xs)
;;

let scanl1 f xs =
  match xs with
    | []      -> []
    | (x::xs) -> scanl f x xs
;;

let rec mapAccumL f s xs =
  match xs with
    | []      -> (s,[])
    | (x::xs) -> 
	let (s',y) = f s x in
	let (s'',ys) = mapAccumL f s' xs in
	  (s'',y::ys)
;;

let append = List.append;;

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

(* FIX ME: rename *)
let rec nubBy eq ls =
  match ls with
    [] -> []
  | (x::xs) when (memBy eq x xs) -> nubBy eq xs
  | (x::xs) -> x::(nubBy eq xs)
;;

(* FIX ME: rename *)
let rec nub ls = nubBy (=) ls

let setifyBy eq xs = List.sort compare (nubBy eq xs);;

let setify xs = List.sort compare (nub xs);;

let unionBy eq xs ys =
  let rec preunion xs ys =
    match (xs,ys) with
      ([],ys') -> ys'
    | (xs',[]) -> xs'
    | (x::xs',y::ys') when eq x y -> x::(preunion xs' ys')
    | (x::xs',y::ys') when not (eq x y) -> x::(preunion xs' (y::ys')) 
    |  _ -> raise NEVER_CTL
  in
  setifyBy eq (nubBy eq (preunion xs ys))
;;

let union xs ys = setify (unionBy (=) xs ys);;

let setdiff xs ys = setify (filter (fun x -> not (List.mem x ys)) xs);;

let subseteqBy eq xs ys = List.for_all (fun x -> memBy eq x ys) xs;;

let subseteq xs ys = List.for_all (fun x -> List.mem x ys) xs;;

let setequalBy eq xs ys = (subseteqBy eq xs ys) & (subseteqBy eq ys xs);;

let setequal xs ys = (subseteq xs ys) & (subseteq ys xs);;

let subset xs ys = 
  (subseteq xs ys) & (List.length (nub xs) < List.length (nub ys));;

(* Fix point calculation *)
let rec fix eq f x =
  let x' = f x in if (eq x x') then x' else fix eq f x'
;;

(* Fix point calculation on set-valued functions *)
let rec setfix f x = fix setequal f x;;

let rec allpairs f l1 l2 = foldr (fun x -> append (map (f x) l2)) l1 [];;

let lmerge f m l1 l2 =
  let mrg x1 x2 = if (f x1 x2) then [m x1 x2] else []
  in concat (allpairs mrg l1 l2)
;;


(* ********************************************************************** *)
(* Module: CTL_ENGINE                                                     *)
(* ********************************************************************** *)

module CTL_ENGINE =
  functor (SUB : SUBST) -> 
    functor (G : GRAPH) ->
struct


open Ast_ctl

type ('state,'subst,'anno) generic_triple = 
    'state * 'subst * ('state,'subst,'anno) generic_witnesstree;;

type ('state,'subst,'anno) generic_algo = 
    ('state,'subst,'anno) generic_triple list;;


(* ---------------------------------------------------------------------- *)
(*                                                                        *)
(* ---------------------------------------------------------------------- *)

(* FIX ME: optimise under assumption that xs and ys are sorted *)
let conflictBy conf xs ys =
  List.exists (fun x -> List.exists (fun y -> conf x y) ys) xs;;

let remove_conflictsBy conf xss = filter (fun xs -> not (conf xs)) xss;;


(* ************************* *)
(* Substitutions             *)
(* ************************* *)

let dom_sub sub =
  match sub with
    | Subst(x,_)    -> x
    | NegSubst(x,_) -> x
;;

let eq_subBy eqx eqv sub sub' =
  match (sub,sub') with 
    | (Subst(x,v),Subst(x',v'))       -> (eqx x x') && (eqv v v')
    | (NegSubst(x,v),NegSubst(x',v')) -> (eqx x x') && (eqv v v')
    | _                               -> false
;;

(* NOTE: functor *)
let eq_sub sub sub' = eq_subBy SUB.eq_mvar SUB.eq_val sub sub'

let eq_subst th th' = setequalBy eq_sub th th';;

let merge_subBy eqx (===) (>+<) sub sub' =
  match (sub,sub',eqx (dom_sub sub) (dom_sub sub')) with
    | (Subst (x,v),Subst (x',v'),true) -> 
	if (v === v')
	then Some [Subst(x, v >+< v')]
	else None
    | (NegSubst(x,v),Subst(x',v'),true) ->
	if (not (v === v'))
	then Some [Subst(x',v')]
	else None
    | (Subst(x,v),NegSubst(x',v'),true) ->
	if (not (v === v'))
	then Some [Subst(x,v)]
	else None
    | _ -> Some [sub;sub']
;;

(* NOTE: functor *)
let merge_sub sub sub' = 
  merge_subBy SUB.eq_mvar SUB.eq_val SUB.merge_val sub sub'

(*
let rec fold_subst theta =
  let rec foo acc_th sub =
    match acc_th with
      | [] -> []
      | (s::ss) -> 
	  match (merge_sub s sub) with
	    | None -> raise NEVER_CTL
	    | Some subs -> subs @ (foo ss sub)
  in
    foldl foo [] theta
;;
*)

let clean_substBy eq cmp theta = List.sort cmp (nubBy eq theta);;

(* NOTE: we sort by using the generic "compare" on (meta-)variable
 *   names; we could also require a definition of compare for meta-variables 
 *   or substitutions but that seems like overkill for sorting
 *)
let clean_subst theta = 
  clean_substBy eq_sub (fun s s' -> compare (dom_sub s) (dom_sub s')) theta;;

let top_subst = [];;			(* Always TRUE subst. *)


(* Split a theta in two parts: one with (only) "x" and one without *)
(* NOTE: functor *)
let split_subst theta x = 
  partition (fun sub -> SUB.eq_mvar (dom_sub sub) x) theta;;

(* Returns an option because it is possible that no non-conflicting
 * (sub-)theta could be constructed 
 * NOTE: remove
 *)
let merge_subst theta theta' =
  match(theta,theta') with
    | ([],th') -> Some (map (fun x -> [x]) th')
    | (th,[])  -> Some (map (fun x -> [x]) th)
    | _        -> 
	let tmp = some_tolist (allpairs merge_sub theta theta') in
	  if tmp = [] 
	  then None 
	  else Some tmp
;;

(* We only want to know if there is a conflict so we don't care
 * about merging of values
 *)
let conflict_subBy eqx (===) sub sub' = 
  (merge_subBy eqx (===) (fun x _ -> x) sub sub') = None;;

(* NOTE: functor *)
let conflict_sub sub sub' = 
  conflict_subBy SUB.eq_mvar SUB.eq_val sub sub';;

let conflict_subst theta theta' = conflictBy conflict_sub theta theta';;

(* Returns an option since conjunction may fail (incompatible subs.) *)
let conj_subst theta theta' =
  if (conflict_subst theta theta') 
  then None 
  else Some (clean_subst (unionBy eq_sub theta theta'))
;;

let negate_sub sub =
  match sub with
    | Subst(x,v)    -> NegSubst (x,v)
    | NegSubst(x,v) -> Subst(x,v)
;;

(* Turn a (big) theta into a list of (small) thetas *)
let negate_subst theta = map (fun sub -> [negate_sub sub]) theta;;


(* ************************* *)
(* Witnesses                 *)
(* ************************* *)

let top_wit = [];;			(* Always TRUE witness *)

let eq_wit wit wit' = wit = wit';;

let union_wit wit wit' = union wit wit';;

let negate_wit wit =
  match wit with
    | Wit(s,th,anno,ws)    -> NegWit(s,th,anno,ws)
    | NegWit(s,th,anno,ws) -> Wit(s,th,anno,ws)
;;

let negate_wits wits = map (fun wit -> [negate_wit wit]) wits;;


(* ************************* *)
(* Triples                   *)
(* ************************* *)

(* Triples are equal when the constituents are equal *)
let eq_trip (s,th,wit) (s',th',wit') =
  (s = s') && (eq_subst th th') && (eq_wit wit wit');;

let triples_cleanBy eq cmp trips = List.sort cmp (nubBy eq trips);;

let triples_clean trips = triples_cleanBy eq_trip compare trips;;

let triples_top states = map (fun s -> (s,top_subst,top_wit)) states;;

let triples_union trips trips' = unionBy eq_trip trips trips';;

let triples_conj trips trips' =
  let mrg (s1,th1,wit1) (s2,th2,wit2) =
    if (s1 = s2) then
      match (conj_subst th1 th2) with
	| Some th -> [(s1,th,union_wit wit1 wit2)]
	| _       -> []
    else
      []
  in concat (allpairs mrg trips trips')
;;

let triple_negate states (s,th,wits) = 
  let negstates = map (fun st -> (st,top_subst,top_wit)) (setdiff states [s]) in
  let negths = map (fun th -> (s,th,top_wit)) (negate_subst th) in
  let negwits = map (fun nwit -> (s,th,nwit)) (negate_wits wits) in
    triples_union negstates (triples_union negths negwits)
;;

(* FIX ME: optimise; it is not necessary to do full conjunction *)
let rec triples_complement states trips =
  match trips with
    | [] -> []
    | (t::[]) -> triple_negate states t
    | (t::ts) -> 
	triples_conj (triple_negate states t) (triples_complement states ts)
;;


let triples_witness x trips = 
  let mkwit (s,th,wit) =
    let (th_x,newth) = split_subst th x in
      (s,newth,[Wit(s,th_x,[],wit)]) in	(* [] = annotation *)
    map mkwit trips
;;



(* ---------------------------------------------------------------------- *)
(* SAT  - Model Checking Algorithm for CTL-FVex                           *)
(*                                                                        *)
(* TODO: Implement _all_ operators (directly)                             *)
(* ---------------------------------------------------------------------- *)


(* ************************************* *)
(* The SAT algorithm and special helpers *)
(* ************************************* *)

let rec pre_exist (grp,_,_) y =
  let exp (s,th,wit) = map (fun s' -> (s',th,wit)) (G.predecessors grp s) in
  (concatmap exp y)
;;

let pre_forall ((_,_,states) as m) y = 
  triples_complement states (pre_exist m (triples_complement states y));;

let satAF m s =
  let f y = union y (pre_forall m y) in
  setfix f s
;;

let satEX m s = pre_exist m s;;

(* A[phi1 U phi2] == phi2 \/ (phi1 /\ AXA[phi1 U phi2]) *)
let satAU m s1 s2 = 
  let f y = triples_union s2 (triples_conj s1 (pre_forall m y)) in 
    setfix f []
;;

(* E[phi1 U phi2] == phi2 \/ (phi1 /\ EXE[phi1 U phi2]) *)
let satEU m s1 s2 = 
  let f y = triples_union s2 (triples_conj s1 (pre_exist m y)) in 
    setfix f []
;;




let rec sat ((grp,label,states) as m) phi =
  match phi with
    False              -> []
  | True               -> triples_top states
  | Pred(p)            -> label(p)		(* NOTE: Assume well-formed *)
  | Not(phi)           -> triples_complement states (sat m phi)
  | Or(phi1,phi2)      -> triples_union (sat m phi1) (sat m phi2)
  | And(phi1,phi2)     -> triples_conj (sat m phi1) (sat m phi2)
  | EX(phi)            -> satEX m (sat m phi)
  | AX(phi)            -> sat m (Not (EX(Not phi)))
  | EF(phi)            -> sat m (EU(True,phi))
  | AF(phi)            -> satAF m (sat m phi)
  | EG(phi)            -> sat m (Not(AF(Not phi)))
  | AG(phi)            -> sat m (Not(EF(Not(phi))))
  | EU(phi1,phi2)      -> satEU m (sat m phi1) (sat m phi2)
  | AU(phi1,phi2)      -> satAU m (sat m phi1) (sat m phi2) 
  | Implies(phi1,phi2) -> sat m (Or(Not phi1,phi2))
  | Exists (v,phi)     -> triples_witness v (sat m phi)
;;



(* SAT with tracking *)
let rec sat_verbose annotate maxlvl lvl ((grp,label,states) as m) phi =
  let anno res children = (annotate lvl phi res children,res) in
  let satv phi0 = sat_verbose annotate maxlvl (lvl+1) m phi0 in
    if (lvl > maxlvl) && (maxlvl > -1) then
      anno (sat m phi) []
    else
      match phi with
	  False              -> anno [] []
	| True               -> anno (triples_top states) []
	| Pred(p)            -> anno (label(p)) []
	| Not(phi1)          -> 
	    let (child,res) = satv phi1 in
	      anno (triples_complement states res) [child]
	| Or(phi1,phi2)      -> 
	    let (child1,res1) = satv phi1 in
	    let (child2,res2) = satv phi2 in
	      anno (triples_union res1 res2) [child1; child2]
	| And(phi1,phi2)     -> 
	    let (child1,res1) = satv phi1 in
	    let (child2,res2) = satv phi2 in
	      anno (triples_conj res1 res2) [child1; child2]
	| EX(phi1)           -> 
	    let (child,res) = satv phi1 in
	      anno (satEX m res) [child]
	| AX(phi1)           -> 
	    let (child,res) = satv phi1 in
	      anno (pre_forall m res) [child]
	| EF(phi1)           -> 
	    let (child,_) = satv phi1 in
	      anno (sat m (EU(True,phi1))) [child]
	| AF(phi1)           -> 
	    let (child,res) = satv phi1 in
	      anno (satAF m res) [child]
	| EG(phi1)           -> 
	    let (child,_) = satv phi1 in
	      anno (sat m (Not(AF(Not phi1)))) [child]
	| AG(phi1)            -> 
	    let (child,_) = satv phi1 in
	      anno (sat m (Not(EF(Not(phi1))))) [child]
	| EU(phi1,phi2)      -> 
	    let (child1,res1) = satv phi1 in
	    let (child2,res2) = satv phi2 in
	      anno (satEU m res1 res2) [child1; child2]
	| AU(phi1,phi2)      -> 
	    let (child1,res1) = satv phi1 in
	    let (child2,res2) = satv phi2 in
	      anno (satAU m res1 res2) [child1; child2]
	| Implies(phi1,phi2) -> 
	    let (child1,_) = satv phi1 in
	    let (child2,_) = satv phi2 in
	      anno (sat m (Or(Not phi1,phi2))) [child1; child2]
	| Exists (v,phi1)    -> 
	    let (child,res) = satv phi1 in
	      anno (triples_witness v res) [child]
;;

(* Type for annotations collected in a tree *)
type ('a) witAnnoTree = WitAnno of ('a * ('a witAnnoTree) list);;

let sat_annotree annotate m phi =
  let tree_anno l phi res chld = WitAnno(annotate l phi res,chld) in
    sat_verbose tree_anno (-1) 0 m phi
;;


(* ********************************************************************** *)
(* End of Module: CTL_ENGINE                                              *)
(* ********************************************************************** *)
end
;;

