(* **********************************************************************
 *
 * Implementation of a Witness Tree model checking engine for CTL-FVex 
 * 
 *
 * $Id$
 *
 * **********************************************************************)



exception TODO_CTL            (* implementation still not quite done so... *)
exception NEVER_CTL			(* Some things should never happen *)

(* ----------------------------------------------------------------- *)
(* Misc. useful generic functions                                    *)
(* ----------------------------------------------------------------- *)

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
  nubBy eq (preunion xs ys)
;;

let union xs ys = setify (unionBy (=) xs ys);;

let setdiff xs ys = setify (filter (fun x -> not (List.mem x ys)) xs);;

let subseteq xs ys = List.for_all (fun x -> List.mem x ys) xs;;

let setequal xs ys = (subseteq xs ys) & (subseteq ys xs);;

let subset xs ys = (subseteq xs ys) & (not (setequal xs ys));;

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

(* ---------------------------------------------------------------------- *)
(* Types                                                                  *)
(* ---------------------------------------------------------------------- *)

open Ast_ctl

(* CTL parameterised on basic predicates and metavar's*)
(* NOW IN ctl.ml 
type ('pred,'mvar) generic_ctl = 
  | False
  | True
  | Pred of 'pred
  | Not of ('pred,'mvar) generic_ctl
  | Exists of 'mvar * ('pred,'mvar) generic_ctl		
  | And of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl
  | Or  of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl
  | Implies of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl
  | AF of ('pred,'mvar) generic_ctl
  | AX of ('pred,'mvar) generic_ctl
  | AG of ('pred,'mvar) generic_ctl
  | AU of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl
  | EF of ('pred,'mvar) generic_ctl
  | EX of ('pred,'mvar) generic_ctl
  | EG of ('pred,'mvar) generic_ctl
  | EU of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl
*)

type state = int;;
type states = state list;;

(* NOTE: No explicit representation of the bottom subst., i.e., FALSE *)
(* NOW IN in ctl.ml
type ('mvar,'value) subst = 
  | Subst of 'mvar * 'value
  | NegSubst of 'mvar * 'value
type ('mvar,'value) generic_substitution = ('mvar,'value) subst list;;

type ('state,'subst,'anno) generic_witness =
  | Wit of 'state * 'subst * 'anno * ('state,'subst,'anno) generic_witness list
  | NegWit of 'state * 'subst * 'anno * ('state,'subst,'anno)generic_witness list
*)

type ('state,'subst,'anno) generic_witnesstree = 
    ('state,'subst,'anno) generic_witness list;;

type ('state,'subst,'anno) generic_triple = 
    'state * 'subst * ('state,'subst,'anno) generic_witnesstree;;

type ('state,'subst,'anno) generic_algo = 
    ('state,'subst,'anno) generic_triple list;;


(* ---------------------------------------------------------------------- *)
(*                                                                        *)
(* ---------------------------------------------------------------------- *)

let conflictBy conf xs ys =
  List.exists (fun x -> List.exists (fun y -> conf x y) ys) xs;;

let remove_conflictsBy conf xss = filter (fun xs -> not (conf xs)) xss;;


(* ************************* *)
(* Substitutions             *)
(* ************************* *)

let clean_substBy eq cmp theta = List.sort cmp (nubBy eq theta);;

let clean_subst theta = clean_substBy (=) compare theta;;

let top_subst = [];;			(* Always TRUE subst. *)

let dom_sub sub =
  match sub with
    | Subst(x,_)    -> x
    | NegSubst(x,_) -> x
;;

(* Split a theta in two parts: one with (only) "x" and one without *)
let split_subst theta x = partition (fun sub -> (dom_sub sub) = x) theta;;

let conflict_subBy eqx eqv sub sub' =
  match (sub,sub') with
    | (Subst (x,v),Subst (x',v'))  when (eqx x x') -> not (eqv v v')
    | (NegSubst(x,v),Subst(x',v')) when (eqx x x') -> eqv v v'
    | (Subst(x,v),NegSubst(x',v')) when (eqx x x') -> eqv v v'
    | _                                            -> false
;;

let conflict_sub sub sub' = conflict_subBy (=) (=) sub sub';;

let conflict_subst theta theta' = conflictBy conflict_sub theta theta';;

let merge_sub sub sub' =
  match (sub,sub',(dom_sub sub) = (dom_sub sub')) with
    | (Subst (x,v),Subst (x',v'),true) -> 
	if (v = v')
	then Some [Subst(x,v)]
	else None
    | (NegSubst(x,v),Subst(x',v'),true) ->
	if (not (v = v'))
	then Some [Subst(x',v')]
	else None
    | (Subst(x,v),NegSubst(x',v'),true) ->
	if (not (v = v'))
	then Some [Subst(x,v)]
	else None
    | _ -> Some [sub;sub']
;;

(* Returns an option because it is possible that no non-conflicting
 * (sub-)theta could be constructed 
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

(* NOTE: returns an option since conjunction may fail (incompatible subs.) *)
let conj_subst theta theta' =
  if (conflict_subst theta theta') 
  then None 
  else Some (clean_subst (union theta theta'))
;;

let negate_sub sub =
  match sub with
    | Subst(x,v)    -> NegSubst (x,v)
    | NegSubst(x,v) -> Subst(x,v)
;;

(* Turn a (big) theta into a list of (small) thetas *)
let negate_subst theta = map (fun sub -> [negate_sub sub]) theta;;

let rec negate_substs thetas =
(*  let mrg [sub] [sub'] = maybe (fun x -> x) [] (merge_sub sub sub') in*)
  let mrg th th' = 
    maybe (fun x -> clean_subst (concat x)) [] (merge_subst th th') in
  let cln th = clean_subst (filter (fun sub -> sub <> []) th) in
  match thetas with
    | [] -> []
    | (th::[]) -> negate_subst th
    | (th::ths) -> cln (allpairs mrg (negate_subst th) (negate_substs ths))
;;

let rec negate_substs_alt thetas =
(*  let mrg th th' = 
    if conflict_subst th th' 
    then []
    else allpairs union th th' in*)
  let cln ths = 
    clean_subst (filter (fun th -> not (conflict_subst th th)) ths) in
  match thetas with
    | [] -> []
    | (th::[]) -> negate_subst th
    | (th::ths) -> cln (allpairs union (negate_subst th) (negate_substs_alt ths))
;;


(* ************************* *)
(* Witnesses                 *)
(* ************************* *)

let top_wit = [];;			(* Always TRUE witness *)

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

let triples_cleanBy eq cmp trips = List.sort cmp (nubBy eq trips);;

let triples_clean trips = triples_cleanBy (=) compare trips;;

let triples_top states = map (fun s -> (s,top_subst,top_wit)) states;;

let triples_group trips = 
  let part =  groupBy (fun (s,_,_) (s',_,_) -> s = s') trips in
  let tmp = map (fun ts -> (map (fun (s,th,wit) -> (s,[th],wit)) ts)) part in
  let fld (s,ths,wit) (s',ths',wit') = (s,union ths ths',union_wit wit wit') in
    map (foldl1 fld) tmp
;;

let triples_union trips trips' = union trips trips';;

let triples_conj trips trips' =
  let mrg (s1,th1,wit1) (s2,th2,wit2) =
    let conj = conj_subst th1 th2 in
    match (s1 = s2, conj) with
      | (true,Some th) -> [(s1,th,union_wit wit1 wit2)]
      | _             -> []
  in concat (allpairs mrg trips trips')
;;

(* WRONG: it is not enough to do a massive conjunction over the final list
   of substitutions; must consider all pairs

let triples_complement states trips =
  let s_in_trips s = memBy (fun st (st',_,_) -> st = st') s trips in
  let states' = filter (fun s -> not (s_in_trips s)) states in
  let negstates = map (fun s -> (s,top_subst,top_wit)) states' in
  let negth (s,th,wit) = 
    map (fun th' -> (s,th',wit)) (negate_subst th) in
  let negsubsts = concatmap negth trips in
  let negwit (s,th,wit) = 
    map (fun wit' -> (s,th,wit')) (negate_wits wit) in
  let negwits = concatmap negwit trips in
    (triples_clean (union negstates (union negsubsts negwits)))
;;
*)

let triples_complement states trips =
  let s_in_trips s = memBy (fun st (st',_,_) -> st = st') s trips in
  let states' = filter (fun s -> not (s_in_trips s)) states in
  let negstates = map (fun s -> (s,top_subst,top_wit)) states' in
  let negts (s,ths,wits) = 
    union 
      (map (fun th -> (s,th,wits)) (negate_substs ths))
      (concatmap (fun th -> (map (fun wit -> (s,th,wit)) (negate_wits wits))) ths)
  in
    union negstates (concatmap negts (triples_group trips))
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
  let preds g s = (g#predecessors s)#tolist in
  let exp (s,th,wit) = map (fun (s',_) -> (s',th,wit)) (preds grp s) in
  (concatmap exp y)
;;

let pre_forall ((grp,_,states) as m) y = 
  triples_complement states (pre_exist m (triples_complement states y));;

let satAF m s =
  let f y = union y (pre_forall m y) in
  setfix f s
;;

let satEX m s = pre_exist m s;;

let rec (sat: ('pred, 'mvar, 'valu, 'hole,   'a, 'b) Ast_ctl.model -> 
              ('pred, 'mvar) generic_ctl -> 
              (Ograph_extended.nodei * ('mvar, 'valu) Ast_ctl.generic_substitution *
               (Ograph_extended.nodei, ('mvar, 'valu) Ast_ctl.generic_substitution, 'hole list) Ast_ctl.generic_witness list) list
         ) = fun ((grp,label,states) as m) phi ->
  match phi with
    False              -> []
  | True               -> triples_top states
  | Pred(p, _vTODO)            -> label(p)		(* NOTE: Assume well-formed *)
  | Not(phi)           -> triples_complement states (sat m phi)
  | Or(phi1,phi2)      -> triples_union (sat m phi1) (sat m phi2)
  | And(phi1,phi2)     -> triples_conj (sat m phi1) (sat m phi2)
  | EX(phi)            -> satEX m (sat m phi)
  | AX(phi)          -> sat m (Not (EX(Not phi)))
(*  | AX(phi)            -> pre_forall m (sat m phi)*)
  | AF(phi)            -> satAF m (sat m phi)
  | EG(phi)            -> sat m (Not(AF(Not phi)))
  | Implies(phi1,phi2) -> sat m (Or(Not phi1,phi2))
  | Exists (v,phi)     -> triples_witness v (sat m phi)
  | AU(_phi,phi)            -> satAF m (sat m phi) (* TOFIX *)
  | _ -> raise TODO_CTL
;;


(* NOW IN test.ml
(* ******************************************************************** *)
(*                                                                      *)
(* EXAMPLES                                                             *)
(*                                                                      *)
(* ******************************************************************** *)

(* ---------------------------------------------------------------------- *)
(* Helpers                                                                *)
(* ---------------------------------------------------------------------- *)

let (-->) x v = Subst (x,v);;

let (-/->) x v = NegSubst(x,v);;

let mkgraph nodes edges = 
  let g = ref (new Ograph_extended.ograph_extended) in
  let addn (n,x) = 
    (* let (g',i) = (!g)#add_node x in *) 
    (* now I need to force the nodei of a node, because of the state(vx) predicates 
       hence add_node -> add_nodei
     *)
    let (g', i) = !g#add_nodei n x in
    assert (i = n);
    g := g'; (n,i) in
  let adde anodes (n1,n2,x) = 
    let g' = (!g)#add_arc ((List.assoc n1 anodes,List.assoc n2 anodes),x) in
    g := g'; () in
  let add_nodes = map addn nodes in
  let add_edges = map (adde add_nodes) edges in
  !g
;;


(* ******************************************************************** *)
(* Example 1                                                            *)
(*   CTL: f(x) /\ AF(Ey.g(y))                                           *)
(* ******************************************************************** *)

let ex1lab s =
  match s with
    "f(x)" -> [(0,["x" --> "1"],top_wit); (1,["x" --> "2"],top_wit)]
  | "g(y)" -> [(3,["y" --> "1"],top_wit); (4,["y" --> "2"],top_wit)]
  | _ -> []
;;

let ex1graph = 
  let nodes = 
    [(0,"f(1)");(1,"f(2)");(2,"< >");(3,"g(1)");(4,"g(2)");(5,"<exit>")] in
  let edges = [(0,2); (1,2); (2,3); (2,4); (3,5); (4,5); (5,5)] in
  mkgraph nodes (map (fun (x,y) -> (x,y,())) edges)
;;

let ex1states = List.map fst (ex1graph#nodes)#tolist;;

let ex1model = (ex1graph,ex1lab,ex1states);;

(*
  # ex1 ex1phi1;;

  [(0, [Subst ("x", "1")], 
    [Wit (3, [Subst ("y", "1")], [], []); Wit (4, [Subst ("y", "2")], [], [])]);
   (1, [Subst ("x", "2")],
    [Wit (3, [Subst ("y", "1")], [], []); Wit (4, [Subst ("y", "2")], [], [])])]

  # ex1 ex1phi2;;

  [(0, [Subst ("x", "1")],
    [Wit (3, [Subst ("y", "1")], [], []); Wit (4, [Subst ("y", "2")], [], [])]);
   (1, [Subst ("x", "2")],
    [Wit (3, [Subst ("y", "1")], [], []); Wit (4, [Subst ("y", "2")], [], [])])]
*)
let ex1phi1 = And(Pred "f(x)", AF(Exists ("y",Pred "g(y)")));;
let ex1phi2 = And(Pred "f(x)", AX(AX(Exists ("y",Pred "g(y)"))));;


let ex1s0 = Pred "f(x)";;
let ex1s1 = Pred "g(y)";;
let ex1s2 = Exists("y",ex1s1);;
let ex1s3 = AF(ex1s2);;
let ex1s4 = And(ex1s0,ex1s3);;

let ex1s3a = AX(ex1s2);;
let ex1s4a = AX(AX(ex1s2));;
let ex1s5a = And(ex1s0,ex1s4a);;

let ex1 phi = sat ex1model phi;;


(* ******************************************************************** *)
(* Example 2                                                            *)
(*   CTL: h(z) /\ AF(Ex.(f(x) /\ AF(Ey.g(...,x,y,...))))                *)
(* ******************************************************************** *)

let ex2graph = 
   let nodes = [(0,"h(7)");(1,"f(1)");(2,"f(2)");(3,"g(1,2,3)");(4,"<exit>")] in
   let edges = [(0,1,()); (0,2,()); (1,3,()); (2,3,()); (3,4,()); (4,4,())] in
   mkgraph nodes edges
;;

let ex2lab s =
  match s with
    "h(z)" -> [(0,["z" --> "7"],top_wit)]
  | "f(x)" -> [(1,["x" --> "1"],top_wit); (2,["x" --> "2"],top_wit)]
  | "g(...,x,y,...)" -> [(3,["x" --> "1"; "y" --> "2"],top_wit);
			 (3,["x" --> "2"; "y" --> "3"],top_wit)]
  | _ -> []
;;

let ex2states = List.map fst ex2graph#nodes#tolist;;

let ex2model = (ex2graph,ex2lab,ex2states);;

let ex2 phi = sat ex2model phi;;

let ex2s0 = Pred "g(...,x,y,...)";;
let ex2s1 = Exists("y",Pred "g(...,x,y,...)");;
let ex2s2 = AF(Exists("y",Pred "g(...,x,y,...)"));;
let ex2s2a = AX(Exists("y",Pred "g(...,x,y,...)"));;
let ex2s2b = AX(AX(Exists("y",Pred "g(...,x,y,...)")));;
let ex2s3 = Pred "f(x)";;
let ex2s4 = And(ex2s3,ex2s2);;
let ex2s5 = Exists("x",ex2s4);;
let ex2s6 = AF(ex2s5);;
let ex2s7 = Pred "h(z)";;
let ex2s8 = And(ex2s7,ex2s6);;

let ex2phi1 = ex2s8;;


*)
