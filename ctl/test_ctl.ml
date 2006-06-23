(* ********************************************************************** *)
(* Module: EXAMPLE_ENGINE (instance of CTL_ENGINE)                        *)
(* ********************************************************************** *)

module WRAPPER_PRED = 
  struct
    type predicate = string
  end

module EXAMPLE_ENGINE = Wrapper_ctl.CTL_ENGINE_BIS (Ctl_engine.SIMPLE_ENV) (Ctl_engine.SIMPLE_CFG) (WRAPPER_PRED)

let top_wit = []

(* ******************************************************************** *)
(*                                                                      *)
(* EXAMPLES                                                             *)
(*                                                                      *)
(* ******************************************************************** *)

(* For convenience in the examples *)
(* FIX ME: remove *)
open Ctl_engine.SIMPLE_CFG;;
open EXAMPLE_ENGINE;;
open Ast_ctl;;

(* ---------------------------------------------------------------------- *)
(* Helpers                                                                *)
(* ---------------------------------------------------------------------- *)

(* FIX ME: move to ENGINE module *)
let (-->) x v = Subst (x,v);;

(* FIX ME: move to ENGINE module *)
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

let ex1model = (ex1graph,ex1lab,ex1states);;

let ex1s0 = Exists("v0",Pred ("f(x)",UnModif "v0"));;
let ex1s1 = Exists("v1",Pred ("g(y)",Modif "v1"));;
let ex1s2 = Exists("y",ex1s1);;
let ex1s3 = AF(ex1s2);;
let ex1s4 = And(ex1s0,ex1s3);;

let ex1s3a = AX(ex1s2);;
let ex1s4a = AX(AX(ex1s2));;
let ex1s5a = And(ex1s0,ex1s4a);;

let ex1s0b = Pred ("g(y)", Modif "v0");;
let ex1s1b = Exists ("v0",ex1s0b);;
let ex1s2b = Exists ("y",ex1s1b);;
let ex1s3b = AF(ex1s2b);;
let ex1s4b = AX(ex1s3b);;
let ex1s5b = Pred ("f(x)", UnModif "v3");;
let ex1s6b = Exists ("v3", ex1s5b);;
let ex1s7b = Exists ("x", ex1s6b);;
let ex1s8b = And(ex1s7b,ex1s4b);;

let ex1s7c = And(ex1s6b,ex1s4b);;
let ex1s8c = Exists("x",ex1s7c);;

let ex1phi1 = ex1s4;;
let ex1phi2 = ex1s5a;;
let ex1phi3 = 
 And
 (Exists ("x",
  (Exists ("v3",
    Pred ("f(x)", UnModif "v3")))),
  AX
   (AF
    (Exists ("y", (* change this to Y and have strange behaviour *)
      (Exists ("v0",
       Pred ("g(y)", Modif "v0")
                      ))))));;

(*
     [(3, [("x","1");("y","1")], "g(y)");
      (4, [("x","1");("y","2")], "g(y)");
      (3, [("x","2");("y","1")], "g(y)");
      (4, [("x","2");("y","2")], "g(y)");


[(0, [],
  [Wit (0, [Subst ("x", Wrapper_ctl.ClassicVar "1")], [],
    [Wit (0, [Subst ("v3", Wrapper_ctl.PredVar (UnModif "f(x)"))], [], [])]);
   Wit (3, [Subst ("y", Wrapper_ctl.ClassicVar "1")], [],
    [Wit (3, [Subst ("v0", Wrapper_ctl.PredVar (Modif "g(y)"))], [], [])]);
   Wit (4, [Subst ("y", Wrapper_ctl.ClassicVar "2")], [],
    [Wit (4, [Subst ("v0", Wrapper_ctl.PredVar (Modif "g(y)"))], [], [])])]);
 (1, [],
  [Wit (1, [Subst ("x", Wrapper_ctl.ClassicVar "2")], [],
    [Wit (1, [Subst ("v3", Wrapper_ctl.PredVar (UnModif "f(x)"))], [], [])]);
   Wit (3, [Subst ("y", Wrapper_ctl.ClassicVar "1")], [],
    [Wit (3, [Subst ("v0", Wrapper_ctl.PredVar (Modif "g(y)"))], [], [])]);
   Wit (4, [Subst ("y", Wrapper_ctl.ClassicVar "2")], [],
    [Wit (4, [Subst ("v0", Wrapper_ctl.PredVar (Modif "g(y)"))], [], [])])])]
*)



(* let ex1 phi = satbis ex1model phi;; *)
