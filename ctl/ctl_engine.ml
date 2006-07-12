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
    (G.node * substitution * ('pred,'anno) witness) list


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

let rec (print_generic_witnesstree : ('pred,'anno) witness -> unit) =
  fun witnesstree ->
  let rec loop = function (* no brace or paren needed *)
      A.AndWits(c1,c2) ->
	paren_loop c1; Format.print_string " &"; Format.force_newline();
	paren_loop c2
    | A.OrWits(c1,c2) ->
	paren_loop c1; Format.print_string " v"; Format.force_newline();
	paren_loop c2
    | A.Wit (state, subst, anno, wit) -> 
	Format.print_string "wit ";
	G.print_node state;
	print_generic_substitution subst;
	(match wit with
	  A.TopWit -> Format.print_string "{}"
	| _ ->
	    Format.force_newline(); Format.print_string "   ";
	    brace_loop wit "{" "}")
    | A.NegWit wit -> Format.print_string "!"; paren_loop wit
    | A.TopWit -> ()
	    
  and paren_loop = function (* paren needed for complex things *)
      A.AndWits(_,_) | A.OrWits(_,_) as wit ->
	brace_loop wit "(" ")"
    | A.TopWit -> Format.print_string "()"
    | wit -> loop wit

  and brace_loop witnesstree l r =
    Format.open_box 1;
    Format.print_string l;
    loop witnesstree;
    Format.print_string r;
    Format.close_box() in

  brace_loop witnesstree "{" "}"
      
      
and (print_generic_triple :
       G.node * substitution * ('pred,'anno) witness -> unit) =
  fun (node,subst,tree) ->
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
  clean_substBy eq_sub (fun s s' -> compare (dom_sub s) (dom_sub s')) theta;;

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

let top_wit = A.TopWit;;			(* Always TRUE witness *)

let eq_wit wit wit' = wit = wit';;

let union_wit wit wit' =
  match (wit,wit') with
    (A.TopWit,_) -> wit'
  | (_,A.TopWit) -> wit
  | (A.NegWit(A.TopWit),_) -> wit
  | (_,A.NegWit(A.TopWit)) -> wit'
  | _ -> if eq_wit wit wit' then wit else A.AndWits(wit,wit')

let disj_wit wit wit' =
  match (wit,wit') with
    (A.TopWit,_) -> wit
  | (_,A.TopWit) -> wit'
  | (A.NegWit(A.TopWit),_) -> wit'
  | (_,A.NegWit(A.TopWit)) -> wit
  | _ -> if eq_wit wit wit' then wit else A.OrWits(wit,wit')

let negate_wits = function
    A.TopWit -> []
  | A.NegWit(ws) -> [ws]
  | wit -> [A.NegWit(wit)]


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
    List.fold_left
      (function rest ->
	 function (s1,th1,wit1) ->
	   List.fold_left
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
    List.fold_left
      (function rest ->
	 function (s1,th1,wit1) ->
	   List.fold_left
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
  let negwits =
    map
      (fun nwit -> (PosState s,th,nwit))
      (negate_wits wits) in
    triples_union negstates (triples_union negths negwits)
;;

(* goal: get rid of the negations that pile up at the top of a tree when
   double negation is done
   invariant: there are no such undesired double negations in the child of
   a witness *)
let propagate_neg trips =
  let rec loop neg = function
      A.AndWits(c1,c2) ->
	if neg
	then disj_wit (loop neg c1) (loop neg c2)
	else union_wit (loop neg c1) (loop neg c2)
    | A.OrWits(c1,c2) ->
	if neg
	then union_wit (loop neg c1) (loop neg c2)
	else disj_wit (loop neg c1) (loop neg c2)
    | A.NegWit(wit) -> loop (not neg) wit
    | w -> if neg then A.NegWit w else w (* children are taken care of *) in
  List.map (function (st,th,wit) -> (st,th,loop false wit)) trips

(* FIX ME: it is not necessary to do full conjunction *)
let triples_complement states trips =
  let cleanup (s,th,wit) =
    match s with
      | PosState s' -> [(s',th,wit)]
      | NegState ss ->
	  assert (th=top_subst);
	  assert (wit=top_wit);
	  map (fun st -> (st,top_subst,top_wit)) (setdiff states ss) in
  let rec compl trips =
    match trips with
      | [] -> []
      | (t::[]) -> triple_negate t
      | (t::ts) -> triples_state_conj (triple_negate t) (compl ts) in
    concatmap cleanup (compl trips)

;;

(* ********************************** *)
(* END OF NEGATION (NegState style)   *)
(* ********************************** *)


let triples_witness x trips = 
  let mkwit (s,th,wit) =
    let (th_x,newth) = split_subst th x in
      (s,newth,A.Wit(s,th_x,[],wit)) in	(* [] = annotation *)
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

let pre_concatmap f l =
  List.fold_left (function rest -> function cur -> union (f cur) rest) [] l

let rec pre_exist (grp,_,_) y =
  let exp (s,th,wit) = map (fun s' -> (s',th,wit)) (G.predecessors grp s) in
  let res = setify (pre_concatmap exp y) in
  res
;;

let pre_forall ((_,_,states) as m) y = 
  propagate_neg
    (triples_complement states (pre_exist m (triples_complement states y)))
;;

let satAF m s =
  let f y = union y (pre_forall m y) in
  setfix f s
;;

let satEG ((_,_,states) as m) s =
  propagate_neg
    (triples_complement states (satAF m (triples_complement states s)))

let satEX m s = pre_exist m s;;

let satAX m s = pre_forall m s
;;
  

(* A[phi1 U phi2] == phi2 \/ (phi1 /\ AXA[phi1 U phi2]) *)
let satAU m s1 s2 = 
  let f y = triples_union s2 (triples_conj s1 (pre_forall m y)) in
  setfix f [] 				(* NOTE: is [] right? *)
;;

(* E[phi1 U phi2] == phi2 \/ (phi1 /\ EXE[phi1 U phi2]) *)
let satEU m s1 s2 = 
  let f y = triples_union s2 (triples_conj s1 (pre_exist m y)) in 
  setfix f []                		(* NOTE: is [] right? *)
;;

let rec satloop ((grp,label,states) as m) phi env check_conj =
  let rec loop phi =
    match A.unwrap phi with
      A.False              -> []
    | A.True               -> triples_top states
    | A.Pred(p)            -> (label p) (* NOTE: Assume well-formed *)
    | A.Not(phi)           ->
	propagate_neg (triples_complement states (loop phi))
    | A.Or(phi1,phi2)      ->
	triples_union (loop phi1) (loop phi2)
    | A.And(phi1,phi2)     ->
	let phi1res = loop phi1 in
	let phi2res = loop phi2 in
	let res = triples_conj phi1res phi2res in
	check_conj phi phi1res phi2res res;
	res
    | A.EX(phi)            -> satEX m (loop phi)
    | A.AX(phi)            -> satAX m (loop phi)
    | A.EF(phi)            ->
	loop (A.rewrap phi (A.EU(A.rewrap phi A.True,phi)))
    | A.AF(phi)            -> satAF m (loop phi)
    | A.EG(phi)            -> satEG m (loop phi)
    | A.AG(phi1)           ->(* should rewrite to only do propagate_neg once *)
	loop (A.rewrap phi
		(A.Not(A.rewrap phi (A.EF(A.rewrap phi (A.Not phi1))))))
    | A.EU(phi1,phi2)      -> satEU m (loop phi1) (loop phi2)
    | A.AU(phi1,phi2)      -> satAU m (loop phi1) (loop phi2)
      (* old: satAF m (satloop m phi2 env) *)
    | A.Implies(phi1,phi2) ->
	loop (A.rewrap phi (A.Or(A.rewrap phi (A.Not phi1),phi2)))
    | A.Exists (v,phi)     -> triples_witness v (loop phi)
    | A.Let(v,phi1,phi2)   -> satloop m phi2 ((v,(loop phi1)) :: env)check_conj
    | A.Ref(v)             -> List.assoc v env in
  loop phi
;;    


(* SAT with tracking *)
let rec sat_verbose_loop annot maxlvl lvl ((_,label,states) as m) phi env
    check_conj =
  let anno res children = (annot lvl phi res children,res) in
  let satv phi0 env =
    sat_verbose_loop annot maxlvl (lvl+1) m phi0 env check_conj in
  if (lvl > maxlvl) && (maxlvl > -1) then
    anno (satloop m phi env check_conj) []
  else
    match A.unwrap phi with
      A.False              -> anno [] []
    | A.True               -> anno (triples_top states) []
    | A.Pred(p)            -> anno (label p) []
    | A.Not(phi1)          -> 
	let (child,res) = satv phi1 env in
	anno (propagate_neg (triples_complement states res)) [child]
    | A.Or(phi1,phi2)      -> 
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 env in
	anno (triples_union res1 res2) [child1; child2]
    | A.And(phi1,phi2)     -> 
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 env in
	anno (triples_conj res1 res2) [child1; child2]
    | A.EX(phi1)           -> 
	let (child,res) = satv phi1 env in
	anno (satEX m res) [child]
    | A.AX(phi1)           -> 
	let (child,res) = satv phi1 env in
	anno (pre_forall m res) [child]
    | A.EF(phi1)           -> 
	let (child,_) = satv phi1 env in
	anno (satloop m
		(A.rewrap phi (A.EU(A.rewrap phi A.True,phi1)))
		env check_conj)
	  [child]
    | A.AF(phi1)           -> 
	let (child,res) = satv phi1 env in
	anno (satAF m res) [child]
    | A.EG(phi1)           -> 
	let (child,_) = satv phi1 env in
	anno
	  (satloop m
	     (A.rewrap phi
		(A.Not(A.rewrap phi (A.AF(A.rewrap phi (A.Not phi1))))))
	     env check_conj)
	  [child]
    | A.AG(phi1)            -> 
	let (child,_) = satv phi1 env in
	anno
	  (satloop m
	     (A.rewrap phi
		(A.Not(A.rewrap phi (A.EF(A.rewrap phi (A.Not phi1))))))
	     env check_conj)
	  [child]
    | A.EU(phi1,phi2)      -> 
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 env in
	anno (satEU m res1 res2) [child1; child2]
    | A.AU(phi1,phi2)      -> 
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 env in
	anno (satAU m res1 res2) [child1; child2]
    | A.Implies(phi1,phi2) -> 
	let (child1,_) = satv phi1 env in
	let (child2,_) = satv phi2 env in
	anno
	  (satloop m
	     (A.rewrap phi (A.Or(A.rewrap phi (A.Not phi1),phi2)))
	     env check_conj)
	  [child1; child2]
    | A.Exists (v,phi1)    -> 
	let (child,res) = satv phi1 env in
	anno (triples_witness v res) [child]
    | A.Let(v,phi1,phi2)   ->
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 ((v,res1) :: env) in
	anno res2 [child1;child2]
    | A.Ref(v)             -> anno (List.assoc v env) []
;;

let sat_verbose annotate maxlvl lvl m phi check_conj =
  sat_verbose_loop annotate maxlvl lvl m phi [] check_conj

(* Type for annotations collected in a tree *)
type ('a) witAnnoTree = WitAnno of ('a * ('a witAnnoTree) list);;

let sat_annotree annotate m phi check_conj =
  let tree_anno l phi res chld = WitAnno(annotate l phi res,chld) in
    sat_verbose_loop tree_anno (-1) 0 m phi [] check_conj
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


(* drops negwits, not always appropriate, but more concise *)
let not_negwits triples =
  let rec no_negwits = function
      A.AndWits(c1,c2) | A.OrWits(c1,c2) -> no_negwits c1 && no_negwits c2
    | A.Wit(st,th,anno,wit) -> no_negwits wit
    | A.NegWit(_) -> false
    | A.TopWit -> true in
  let rec loop = function
      A.AndWits(c1,c2) ->
	(match (loop c1,loop c2) with
	  (Some c1,Some c2) -> Some (A.AndWits(c1,c2))
	| _ -> None)
    | A.OrWits(c1,c2) ->
	(match (loop c1,loop c2) with
	  (Some c1,Some c2) -> Some (A.OrWits(c1,c2))
	| (None,Some c2) -> Some c2
	| (Some c1,None) -> Some c1
	| _ -> None)
    | A.Wit(st,th,anno,wit) ->
	(match loop wit with
	  None -> None
	| Some wit -> Some(A.Wit(st,th,anno,wit)))
    | A.NegWit(wit) ->
	if no_negwits wit then None else failwith "nested negwits"
    | A.TopWit -> Some A.TopWit in
  List.concat
    (List.map
       (function (st,th,wit) ->
	 match loop wit with None -> [] | Some wit -> [(st,th,wit)])
       triples)

(* pad: Rene, you can now use the module pretty_print_ctl.ml to
   print a ctl formula more accurately if you want.
   Use the print_xxx provided in the different module to call 
   Pretty_print_ctl.pp_ctl.
 *)

let simpleanno2 l phi res = 
  begin
    Pretty_print_ctl.pp_ctl (P.print_predicate, SUB.print_mvar) phi;
    Format.print_newline ();
    Format.print_string "-----------------------------------------------------";
    Format.print_newline ();
    print_generic_algo
      (if !Flag_ctl.poswits_only then not_negwits res else res);
    Format.print_newline ();
    Format.print_string "-----------------------------------------------------";
    Format.print_newline ();
    Format.print_newline ();
  end
  



(* Main entry point for engine *)
let sat m phi check_conj = 
  let res =
    if(!Flag_ctl.verbose_ctl_engine)
    then snd (sat_annotree simpleanno2 m phi check_conj)
    else satloop m phi [] check_conj in
(* print_state "final result" res;*)
  res
;;

(* ********************************************************************** *)
(* End of Module: CTL_ENGINE                                              *)
(* ********************************************************************** *)
end
;;

