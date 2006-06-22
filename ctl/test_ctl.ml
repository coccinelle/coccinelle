
(* ********************************************************************** *)
(* Module: EXAMPLE_ENGINE (instance of CTL_ENGINE)                        *)
(* ********************************************************************** *)

(* FIX ME: move *)
module EXAMPLE_ENGINE = Ctl_engine.CTL_ENGINE (Ctl_engine.SIMPLE_ENV) (Ctl_engine.SIMPLE_CFG);;


let top_wit = []

(* ******************************************************************** *)
(*                                                                      *)
(* EXAMPLES                                                             *)
(*                                                                      *)
(* ******************************************************************** *)

(* For convenience in the examples *)
(* FIX ME: remove *)
open Ctl_engine.SIMPLE_ENV;;
open Ctl_engine.SIMPLE_CFG;;
open EXAMPLE_ENGINE;;

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
  let add_nodes = List.map addn nodes in
  let _add_edges = List.map (adde add_nodes) edges in
  !g
;;


(* ******************************************************************** *)
(* NEW Example 1                                                        *)
(*   CTL: f(x) /\ AF(Ey.g(y))                                           *)
(* ******************************************************************** *)

(*
let conv_label olab (p,mv) =
  let old_label = olab p in
  let penv = mv --> PredVal(p) in
  let conv_sub sub =
    match sub with
      | Subst(x,v)    -> Subst(UnModif(x),CodeVal(v))
      | NegSubst(x,v) -> NegSubst(UnModif(x),CodeVal(v))
  in
    List.map (fun (s,env,wit) -> (s,penv :: (List.map conv_sub env),wit)) old_label
;;
*)

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
  mkgraph nodes (List.map (fun (x,y) -> (x,y,())) edges)
;;

let ex1states = List.map fst (ex1graph#nodes)#tolist;;

let ex1model = (ex1graph,(*conv_label*) ex1lab,ex1states);;

let ex1s0 = Exists((*UnModif*) "v0",Pred ("f(x)",UnModif "v0"));;
let ex1s1 = Exists((*UnModif*) "v1",Pred ("g(y)",UnModif "v1"));;
let ex1s2 = Exists((*UnModif*) "y",ex1s1);;
let ex1s3 = AF(ex1s2);;
let ex1s4 = And(ex1s0,ex1s3);;

let ex1s3a = AX(ex1s2);;
let ex1s4a = AX(AX(ex1s2));;
let ex1s5a = And(ex1s0,ex1s4a);;

let ex1phi1 = ex1s4;;
let ex1phi2 = ex1s5a;;


let ex1 phi = sat ex1model phi;;

