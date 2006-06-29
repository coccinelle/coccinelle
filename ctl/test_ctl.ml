(* -*- mode: tuareg; tuareg-interactive-program: "/home/rrh/coccinelle/coccinelle/ctl/ctl.top -I ../commons" -*- *)

(* ********************************************************************** *)
(* Module: EXAMPLE_ENGINE (instance of CTL_ENGINE)                        *)
(* ********************************************************************** *)

(* Simple env.: meta.vars and values are strings *)
module SIMPLE_ENV =
  struct
    type value = string;;
    type mvar = string;;
    let eq_mvar x x' = x = x';;
    let eq_val v v' = v = v';;
    let merge_val v v' = v;;
  end
;;

(* Simple predicates *)
module WRAPPER_PRED = 
  struct
    type predicate = string
  end

module EXAMPLE_ENGINE = 
  Wrapper_ctl.CTL_ENGINE_BIS (SIMPLE_ENV) (Ctl_engine.OGRAPHEXT_GRAPH) (WRAPPER_PRED)

let top_wit = []

(* ******************************************************************** *)
(*                                                                      *)
(* EXAMPLES                                                             *)
(*                                                                      *)
(* ******************************************************************** *)

(* For convenience in the examples *)
(* FIX ME: remove *)
open Ctl_engine.OGRAPHEXT_GRAPH;;
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
(* Example 1                                                            *)
(*   CTL: f(x) /\ AF(Ey.g(y))                                           *)
(* ******************************************************************** *)

let ex1lab s =
  match s with
    | "f(x)" -> [(0,["x" --> "1"]); (1,["x" --> "2"])]
    | "g(y)" -> [(3,["y" --> "1"]); (4,["y" --> "2"])]
    | "f(1)" -> [(0,[])]
    | "f(2)" -> [(1,[])]
    | "g(1)" -> [(3,[])]
    | "g(2)" -> [(4,[])]
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
let ex1model_wrapped = (ex1graph,wrap_label ex1lab,ex1states);;

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

let ex1phi4 = 
  Exists ("x",
	  And (
	    (Exists ("v3",
		     Pred ("f(x)", UnModif "v3"))),
	    AX
	      (AF
		 (Exists ("y", (* change this to Y and have strange behaviour *)
			  (Exists ("v0",
				   Pred ("g(y)", Modif "v0")
				  )))))));;


let ex1phi5 = AU(True,Exists("y", Exists("v0",Pred("g(y)",Modif "v0"))));;

let ex1phi6 = 
  AU(
    Not(Exists("x",Exists("v1",Pred("f(x)",UnModif "v1")))),
    Exists("y", Exists("v0",Pred("g(y)",Modif "v0")))
  );;

(* use with ex1nc *)
let ex1phi7 = 
  AU(
    Not(Or(Pred("f(1)",Control),Pred("f(2)",Control))),
    Exists("y", Exists("v0",Pred("g(y)",Modif "v0")))
  );;

let ex1 phi = satbis ex1model phi;;
let ex1nc phi = satbis_noclean ex1model phi;;


(* ******************************************************************** *)
(* Example 2                                                            *)
(* ******************************************************************** *)

let ex2lab s =
  match s with
    | "p"        -> [0,[]]
    | "{"        -> [(1,[]); (2,[])]
    | "}"        -> [(3,[]); (4,[])]
    | "paren(v)" -> [(1,["v" --> "1"]); (2,["v" --> "2"]); 
		     (3,["v" --> "2"]); (4,["v" --> "1"])]
    | _          -> []
;;

let ex2graph = 
  let nodes = 
    [(0,"p");(1,"{");(2,"{");(3,"}");(4,"}");(5,"<exit>")] in
  let edges = [(0,1); (1,2); (2,3); (3,4); (4,5); (5,5)] in
  mkgraph nodes (List.map (fun (x,y) -> (x,y,())) edges)
;;

let ex2states = List.map fst (ex2graph#nodes)#tolist;;

let ex2model = (ex2graph,ex2lab,ex2states);;
let ex2model_wrapped = (ex2graph,wrap_label ex2lab,ex2states);;

let ex2s0 = Pred("p",Control);;
let ex2s1 = Pred("{",Control);;
let ex2s2 = Pred("paren(v)",Control);;
let ex2s3 = And(ex2s1,ex2s2);;
let ex2s4 = Pred("}",Control);;
let ex2s5 = Pred("paren(v)",Control);;
let ex2s6 = And(ex2s4,ex2s5);;
let ex2s7 = AF(ex2s6);;
let ex2s8 = And(ex2s3,ex2s7);;
let ex2s9 = Exists("v",ex2s8);;
let ex2s10 = AX(ex2s9);;
let ex2s11 = And(ex2s0,ex2s10);;

let ex2phi1 = ex2s11;;

(*
           +--- s11:& ---+
           |             |
         s0:p         s10:AX
                         |
                     s9:exists v
                         |
           +---------- s8:& --------+
           |                        |
     +-- s3:& --+                 s7:AF
     |          |                   |
   s1:"{"     s2:paren(v)     +--  s6:& -+
                              |          |
		            s4:"}"     s5:paren(v)

  s0 : p                   : (0,_,_)
  s1 : "{"                 : (1,_,_); (2,_,_)
  s2 : paren(v)            : (1,v=1,_); (2,v=2,_); (3,v=2,_); (4,v=1,_)
  s3 : "{" & paren(v)      : (1,v=1,_); (2,v=2,_)
  s4 : "}"                 : (3,_,_); (4,_,_)
  s5 : paren(v)            : (1,v=1,_); (2,v=2,_); (3,v=2,_); (4,v=1,_)
  s6 : "}" & paren(v)      : (3,v=2,_); (4,v=1,_)
  s7 : AF(...)             : (0;1;2;3,v=2,_); (0;1;2;3;4,v=1,_)
  s8 : (...&...) & AF(...) : (1,v=1,_); (2,v=2,_)
  s9 : exists ...          : (1,_,(1,v=1)); (2,_,(2,v=2))
 s10 : AX(...)             : (0,_,(1,v=1)); (1,_,(2,v=2))
 s11 : p & AX(...)         : (0,_,(1,v=1))
*)
