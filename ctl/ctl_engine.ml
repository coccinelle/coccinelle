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
struct


type ('state,'subst,'anno) local_witnesstree =
    AndWits of ('state,'subst,'anno) local_witnesstree list
  | OrWits of ('state,'subst,'anno) local_witnesstree list
  | Wits of 'state * 'subst * 'anno * ('state,'subst,'anno) local_witnesstree
  | NegWits of ('state,'subst,'anno) local_witnesstree

let rec clean_triple (st,th,wits) =
  let isnone = function None -> true | _ -> false in
  let drop_some = function None -> failwith "impossible" | Some x -> x in
  let rec no_negwits = function
      AndWits(children) | OrWits(children) -> List.for_all no_negwits children
    | Wits(st,th,anno,children) -> no_negwits children
    | NegWits(children) -> false  in
  let rec loop2 = function
      AndWits(children) ->
	let res = List.map loop2 children in
	if List.exists isnone res
	then None
	else
	  let children = List.map drop_some res in
	  Some (AndWits(children))
    | OrWits(children) ->
	let res = List.map loop2 children in
	if List.for_all isnone res
	then None
	else 
	  let children =
	    List.map drop_some
	      (List.filter (function x -> not(isnone x)) res) in
	  Some (OrWits(children))
    | Wits(st,th,anno,children) ->
	(match loop2 children with
	  None -> None
	| Some children -> Some (Wits(st,th,anno,children)))
    | NegWits(children) ->
	if no_negwits children then None else Some (NegWits(children)) in
  match loop2 wits with
    Some x -> [(st,th,x)]
  | None -> []

open Ast_ctl

type ('state,'subst,'anno) generic_triple = 
    'state * 'subst * ('state,'subst,'anno) generic_witnesstree;;

type ('state,'subst,'anno) generic_algo = 
    ('state,'subst,'anno) generic_triple list;;


let (print_generic_subst: (SUB.mvar, SUB.value) Ast_ctl.generic_subst -> unit) = fun subst ->
  match subst with
  | Subst (mvar, value) ->
      SUB.print_mvar mvar; 
      Format.print_string " --> ";
      SUB.print_value value
  | NegSubst (mvar, value) -> 
      SUB.print_mvar mvar; 
      Format.print_string " -/-> ";
      SUB.print_value value

let (print_generic_substitution:  (SUB.mvar, SUB.value) Ast_ctl.generic_substitution -> unit) = fun substxs ->
  begin
    Format.print_string "[";
    Common.print_between (fun () -> Format.print_string ";" ) print_generic_subst substxs;
    Format.print_string "]";
  end

let rec (print_generic_witness :
	   (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_substitution, 'anno) 
           Ast_ctl.generic_witness -> unit) = function
  | Wit (state, subst, anno, childrens) -> 
      Format.print_string "wit ";
      G.print_node state;
      print_generic_substitution subst;
      (match childrens with
	[] -> Format.print_string "{}"
      |	_ -> 
	  Format.force_newline(); Format.print_string "   "; Format.open_box 0;
	  print_generic_witnesstree childrens; Format.close_box())
  | NegWit  (state, subst, anno, childrens) -> 
      Format.print_string "-wit ";
      G.print_node state;
      print_generic_substitution subst;
      (match childrens with
	[] -> Format.print_string "{}"
      |	_ -> 
	  Format.force_newline(); Format.print_string "   "; Format.open_box 0;
	  print_generic_witnesstree childrens; Format.close_box())


and (print_generic_witnesstree :
       (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_substitution, 'anno) 
       Ast_ctl.generic_witnesstree -> unit) = fun witnesstree ->
  begin
    Format.open_box 1;
    Format.print_string "{";
    Common.print_between
      (fun () -> Format.print_string ";"; Format.force_newline() ) 
      print_generic_witness witnesstree;
    Format.print_string "}";
    Format.close_box()
  end


let rec (print_local_witnesstree :
       (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_substitution, 'anno) 
       local_witnesstree -> unit) =
  fun witnesstree ->
    let rec loop = function
	AndWits(wits) ->
	  Common.print_between
	    (fun () -> Format.print_string " &"; Format.force_newline() ) 
	    paren_loop wits
      | OrWits(wits) ->
	  Common.print_between
	    (fun () -> Format.print_string " v"; Format.force_newline() ) 
	    paren_loop wits
      | Wits (state, subst, anno, childrens) -> 
	  Format.print_string "wit ";
	  G.print_node state;
	  print_generic_substitution subst;
	  (match childrens with
	    AndWits [] | OrWits [] -> Format.print_string "{}"
	  | _ -> 
	      Format.force_newline(); Format.print_string "   ";
	      Format.open_box 1;
	      Format.print_string "{";
	      loop childrens;
	      Format.print_string "}"; Format.close_box())
      | NegWits childrens -> 
	  Format.print_string "!";
	  (match childrens with
	    AndWits [] | OrWits [] -> Format.print_string "{}"
	  | _ -> paren_loop childrens)
	    
    and paren_loop = function
	AndWits(_::_::_) | OrWits(_::_::_) as wit ->
	  Format.open_box 1;
	  Format.print_string "(";
	  loop wit;
	  Format.print_string ")";
	  Format.close_box()
      | wit -> loop wit in
    Format.open_box 1;
    Format.print_string "{";
    loop witnesstree;
    Format.print_string "}";
    Format.close_box()
      
      
and (print_local_triple :
       (G.node * 
          (SUB.mvar, SUB.value) Ast_ctl.generic_substitution *
          (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_substitution, 'b list)
	  local_witnesstree)
     -> unit) =
  fun triple ->
    List.iter
      (function (node,subst,tree) ->
	G.print_node node;
	print_generic_substitution subst;
	print_local_witnesstree tree)
      (clean_triple triple)
	  
and (print_generic_triple: (G.node * 
                            (SUB.mvar, SUB.value) Ast_ctl.generic_substitution *
                            (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_substitution, 'b list) Ast_ctl.generic_witnesstree)
                         -> unit) = fun (node, subst, tree) -> 
  begin
    G.print_node node;
    print_generic_substitution subst;
    print_generic_witnesstree tree;
  end

and (print_local_algo :
       (G.node * 
	  (SUB.mvar, SUB.value) Ast_ctl.generic_substitution *
	  (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_substitution, 'b list) local_witnesstree)
       list -> unit) = fun xs -> 
	 begin
	   Format.print_string "<";
	   Common.print_between (fun () -> Format.print_string ";\n" ) print_local_triple xs;
	   Format.print_string ">";
	 end
	   
and (print_generic_algo :
       (G.node * 
	  (SUB.mvar, SUB.value) Ast_ctl.generic_substitution *
	  (G.node, (SUB.mvar, SUB.value) Ast_ctl.generic_substitution, 'b list) generic_witnesstree)
       list -> unit) = fun xs -> 
	 begin
	   Format.print_string "<";
	   Common.print_between (fun () -> Format.print_string ";\n" ) print_generic_triple xs;
	   Format.print_string ">";
	 end
	   
let print_state str l =
  Printf.printf "%s\n" str;
  List.iter (function x ->
    print_local_triple x; Format.print_flush(); Printf.printf "\n";
    flush stdout)
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
    | (NegSubst(x,v),NegSubst(x',v'),true) ->
	if (v === v')
	then Some [NegSubst(x,v)]
	else Some [NegSubst(x,v);NegSubst(x',v')]
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
    | Subst(x,v)    -> NegSubst (x,v)
    | NegSubst(x,v) -> Subst(x,v)
;;

(* Turn a (big) theta into a list of (small) thetas *)
let negate_subst theta = (map (fun sub -> [negate_sub sub]) theta);;


(* ************************* *)
(* Witnesses                 *)
(* ************************* *)

let top_wit = AndWits [];;			(* Always TRUE witness *)

let eq_wit wit wit' = wit = wit';;

let union_wit wit wit' =
  let andify = function AndWits wits -> wits | x -> [x] in
  let wit = andify wit in
  let wit' = andify wit' in
  match union wit wit' with
    [x] -> x
  | x -> AndWits x

(*
let negate_wit wit =
  match wit with
    | Wit(s,th,anno,ws)    -> NegWit(s,th,anno,ws)
    | NegWit(s,th,anno,ws) -> Wit(s,th,anno,ws)
;;

let negate_wits wits =
  setify
    (List.fold_left
       (function rest ->
	 function wit ->
	   [negate_wit wit] :: rest)
       [] wits);;
*)


let negate_wits = function
    AndWits [] | OrWits [] -> []
  | NegWits(ws) -> [ws]
  | Wits(_,_,_,_) as w -> [NegWits(w)]
  | wits -> [NegWits(wits)]


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
(* NEGATION (classic style)    *)
(* *************************** *)

(*
let triple_negate states (s,th,wits) = 
  let negstates = map (fun st -> (st,top_subst,top_wit)) (setdiff states [s]) in
  let negths = map (fun th -> (s,th,top_wit)) (negate_subst th) in
  let negwits = map (fun nwit -> (s,th,nwit)) (negate_wits wits) in
    triples_union negstates (triples_union negths negwits)
;;

(* FIX ME: optimise; it is not necessary to do full conjunction *)
let rec triples_complement states trips =
  let rec loop states trips =
  match trips with
    | [] -> []
    | (t::[]) -> triple_negate states t
    | (t::ts) -> 
	triples_conj (triple_negate states t) (loop states ts) in
  setify (loop states trips)
;;
*)

(* ********************************** *)
(* END OF NEGATION (classic style)    *)
(* ********************************** *)


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
      AndWits(children) ->
	let wits = List.map (loop neg) children in
	if neg then OrWits wits else AndWits wits
    | OrWits(children) ->
	let wits = List.map (loop neg) children in
	if neg then AndWits wits else OrWits wits
    | Wits(st,th,anno,children) -> (* children are taken care of *)
	let wits = Wits(st,th,anno,children) in
	if neg then NegWits(wits) else wits
    | NegWits(children) -> loop (not neg) children in
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
      | (t::ts) -> 
	  triples_state_conj (triple_negate t) (compl ts) in
    concatmap cleanup (compl trips)

;;

(* ********************************** *)
(* END OF NEGATION (NegState style)   *)
(* ********************************** *)


let triples_witness x trips = 
  let mkwit (s,th,wit) =
    let (th_x,newth) = split_subst th x in
      (s,newth,Wits(s,th_x,[],wit)) in	(* [] = annotation *)
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

let rec satloop ((grp,label,states) as m) phi env =
  match unwrap phi with
    False              -> []
  | True               -> triples_top states
  | Pred(p)            -> (* NOTE: Assume well-formed *)
      List.map (function (node,subst,_) -> (node,subst,AndWits[])) (label p)
  | Not(phi)           ->
      propagate_neg (triples_complement states (satloop m phi env))
  | Or(phi1,phi2)      ->
      triples_union (satloop m phi1 env) (satloop m phi2 env)
  | And(phi1,phi2)     ->
      let phi1res = satloop m phi1 env in
      let phi2res = satloop m phi2 env in
      let res = triples_conj phi1res phi2res in
      res
  | EX(phi)            -> satEX m (satloop m phi env)
  | AX(phi)            -> satAX m (satloop m phi env)
  | EF(phi)            -> satloop m (rewrap phi (EU(rewrap phi True,phi))) env
  | AF(phi)            -> satAF m (satloop m phi env)
  | EG(phi)            -> satEG m (satloop m phi env)
  | AG(phi1)           -> (* should rewrite to only do propagate_neg once *)
      satloop m
	(rewrap phi (Not(rewrap phi (EF(rewrap phi (Not phi1)))))) env
  | EU(phi1,phi2)      ->
      satEU m (satloop m phi1 env) (satloop m phi2 env)
  | AU(phi1,phi2)      -> 
      satAU m (satloop m phi1 env) (satloop m phi2 env)
      (* old: satAF m (satloop m phi2 env) *)
  | Implies(phi1,phi2) ->
      satloop m (rewrap phi (Or(rewrap phi (Not phi1),phi2))) env
  | Exists (v,phi)     -> triples_witness v (satloop m phi env)
  | Let(v,phi1,phi2)   -> satloop m phi2 ((v,(satloop m phi1 env)) :: env)
  | Ref(v)             -> List.assoc v env
;;    


(* SAT with tracking *)
let rec sat_verbose_loop annot maxlvl lvl ((_,label,states) as m) phi env =
  let anno res children = (annot lvl phi res children,res) in
  let satv phi0 env =
    sat_verbose_loop annot maxlvl (lvl+1) m phi0 env in
  if (lvl > maxlvl) && (maxlvl > -1) then
    anno (satloop m phi env) []
  else
    match unwrap phi with
      False              -> anno [] []
    | True               -> anno (triples_top states) []
    | Pred(p)            ->
	let res =
	  List.map (function (node,subst,_) -> (node,subst,AndWits[]))
	    (label p) in
	anno res []
    | Not(phi1)          -> 
	let (child,res) = satv phi1 env in
	anno (propagate_neg (triples_complement states res)) [child]
    | Or(phi1,phi2)      -> 
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 env in
	anno (triples_union res1 res2) [child1; child2]
    | And(phi1,phi2)     -> 
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 env in
	anno (triples_conj res1 res2) [child1; child2]
    | EX(phi1)           -> 
	let (child,res) = satv phi1 env in
	anno (satEX m res) [child]
    | AX(phi1)           -> 
	let (child,res) = satv phi1 env in
	anno (pre_forall m res) [child]
    | EF(phi1)           -> 
	let (child,_) = satv phi1 env in
	anno (satloop m
		(rewrap phi (EU(rewrap phi True,phi1)))
		env)
	  [child]
    | AF(phi1)           -> 
	let (child,res) = satv phi1 env in
	anno (satAF m res) [child]
    | EG(phi1)           -> 
	let (child,_) = satv phi1 env in
	anno
	  (satloop m
	     (rewrap phi (Not(rewrap phi (AF(rewrap phi (Not phi1))))))
	     env)
	  [child]
    | AG(phi1)            -> 
	let (child,_) = satv phi1 env in
	anno
	  (satloop m
	     (rewrap phi (Not(rewrap phi (EF(rewrap phi (Not phi1))))))
	     env)
	  [child]
    | EU(phi1,phi2)      -> 
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 env in
	anno (satEU m res1 res2) [child1; child2]
    | AU(phi1,phi2)      -> 
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 env in
	anno (satAU m res1 res2) [child1; child2]
    | Implies(phi1,phi2) -> 
	let (child1,_) = satv phi1 env in
	let (child2,_) = satv phi2 env in
	anno
	  (satloop m
	     (rewrap phi (Or(rewrap phi (Not phi1),phi2)))
	     env)
	  [child1; child2]
    | Exists (v,phi1)    -> 
	let (child,res) = satv phi1 env in
	anno (triples_witness v res) [child]
    | Let(v,phi1,phi2)   ->
	let (child1,res1) = satv phi1 env in
	let (child2,res2) = satv phi2 ((v,res1) :: env) in
	anno res2 [child1;child2]
    | Ref(v)             -> anno (List.assoc v env) []
;;

let sat_verbose annotate maxlvl lvl m phi =
  sat_verbose_loop annotate maxlvl lvl m phi []

(* Type for annotations collected in a tree *)
type ('a) witAnnoTree = WitAnno of ('a * ('a witAnnoTree) list);;

let sat_annotree annotate m phi =
  let tree_anno l phi res chld = WitAnno(annotate l phi res,chld) in
    sat_verbose_loop tree_anno (-1) 0 m phi []
;;

let simpleanno l phi res =
  let pp s = 
    Format.print_string ("\n" ^ s ^ "\n------------------------------\n"); 
    print_local_algo res;
    Format.print_string "\n------------------------------\n\n"
  in
  match unwrap phi with
    | False              -> pp "False"
    | True               -> pp "True"
    | Pred(p)            -> pp ("Pred" ^ (Dumper.dump(p)))
    | Not(phi)           -> pp "Not"
    | Exists(v,phi)      -> pp ("Exists " ^ (Dumper.dump(v)))
    | And(phi1,phi2)     -> pp "And"
    | Or(phi1,phi2)      -> pp "Or"
    | Implies(phi1,phi2) -> pp "Implies"
    | AF(phi1)           -> pp "AF"
    | AX(phi1)           -> pp "AX"
    | AG(phi1)           -> pp "AG"
    | AU(phi1,phi2)      -> pp "AU"
    | EF(phi1)           -> pp "EF"
    | EX(phi1)	         -> pp "EX"
    | EG(phi1)		 -> pp "EG"
    | EU(phi1,phi2)	 -> pp "EU"
    | Let (x,phi1,phi2)  -> pp ("Let"^" "^x)
    | Ref(s)             -> pp ("Ref("^s^")")
;;


let rec dnf = function
    AndWits(children) ->
      let ors = List.concat (List.map dnf children) in
      (match ors with
	[] -> []
      |	x::xs ->
	  setify
	    (List.fold_left
	       (function rest ->
		 function cur ->
		   List.map
		     (function
			 AndWits r -> AndWits (cur::r)
		       | r -> AndWits [cur;r])
		     rest)
	       [x] xs))
  | OrWits(children) -> setify (List.concat(List.map dnf children))
  | Wits(st,th,anno,AndWits[]) -> [Wits(st,th,anno,AndWits[])]
  | Wits(st,th,anno,children) ->
      let children = dnf children in
      List.map (function children -> Wits(st,th,anno,children)) children
  | NegWits(children) -> (* already pushed in as far as possible *)
      let children = dnf children in
      List.map (function children -> NegWits(children)) children

let rec witstowit (st,th,wits) =
  let wits = dnf wits in
  let rec loop = function
    AndWits(children) -> List.concat(List.map loop children)
  | OrWits(children) -> failwith "removed by dnf"
  | Wits(st,th,anno,children) -> [Ast_ctl.Wit(st,th,anno,loop children)]
  | NegWits(Wits(st,th,anno,children)) ->
      [Ast_ctl.NegWit(st,th,anno,loop children)]
  | NegWits(children) -> failwith "complex negwits should not occur" in
  let wits = List.concat(List.map loop wits) in
  (st,th,wits)


(* Main entry point for engine *)
let sat m phi = 
  let res =
    if(!Flag_ctl.verbose_ctl_engine)
    then snd (sat_annotree simpleanno m phi)
    else satloop m phi [] in
  let res = List.concat(List.map clean_triple res) in
  let res = List.map witstowit res in
  print_generic_algo res;
  res
;;

(* ********************************************************************** *)
(* End of Module: CTL_ENGINE                                              *)
(* ********************************************************************** *)
end
;;

