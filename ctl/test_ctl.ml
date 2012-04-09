(*
 * Copyright 2012, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./test_ctl.ml"

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


(* CTL parameterised on basic predicates and metavar's*)
type ('pred,'mvar) old_gen_ctl =
  | False_
  | True_
  | Pred_ of 'pred
  | Not_ of ('pred,'mvar) old_gen_ctl
  | Exists_ of 'mvar * ('pred,'mvar) old_gen_ctl		(* !!! *)
  | And_ of ('pred,'mvar) old_gen_ctl * ('pred,'mvar) old_gen_ctl
  | Or_  of ('pred,'mvar) old_gen_ctl * ('pred,'mvar) old_gen_ctl
  | Implies_ of ('pred,'mvar) old_gen_ctl * ('pred,'mvar) old_gen_ctl
  | AF_ of ('pred,'mvar) old_gen_ctl
  | AX_ of ('pred,'mvar) old_gen_ctl
  | AG_ of ('pred,'mvar) old_gen_ctl
  | AU_ of ('pred,'mvar) old_gen_ctl * ('pred,'mvar) old_gen_ctl
  | EF_ of ('pred,'mvar) old_gen_ctl
  | EX_ of ('pred,'mvar) old_gen_ctl
  | EG_ of ('pred,'mvar) old_gen_ctl
  | EU_ of ('pred,'mvar) old_gen_ctl * ('pred,'mvar) old_gen_ctl
  | Let_ of string * ('pred,'mvar) old_gen_ctl * ('pred,'mvar) old_gen_ctl
  | Ref_ of string

let rec mkanno phi0 =
  let anno phi = (phi,None) in
  match phi0 with
    | False_              -> anno False
    | True_               -> anno True
    | Pred_(p)            -> anno (Pred(p))
    | Not_(phi)           -> anno (Not(mkanno phi))
    | Exists_(v,phi)      -> anno (Exists(v,mkanno phi))
    | And_(phi1,phi2)     -> anno (And(mkanno phi1,mkanno phi2))
    | Or_(phi1,phi2)      -> anno (Or(mkanno phi1,mkanno phi2))
    | Implies_(phi1,phi2) -> anno (Implies(mkanno phi1,mkanno phi2))
    | AF_(phi1)           -> anno (AF(mkanno phi1))
    | AX_(phi1)           -> anno (AX(mkanno phi1))
    | AG_(phi1)           -> anno (AG(mkanno phi1))
    | AU_(phi1,phi2)      -> anno (AU(mkanno phi1,mkanno phi2))
    | EF_(phi1)           -> anno (EF(mkanno phi1))
    | EX_(phi1)		  -> anno (EX(mkanno phi1))
    | EG_(phi1)		  -> anno (EG(mkanno phi1))
    | EU_(phi1,phi2)	  -> anno (EU(mkanno phi1,mkanno phi2))
    | Let_ (x,phi1,phi2)  -> anno (Let(x,mkanno phi1,mkanno phi2))
    | Ref_(s)             -> anno (Ref(s))


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

let ex1s0 = Exists_("v0",Pred_ ("f(x)",UnModif "v0"));;
let ex1s1 = Exists_("v1",Pred_ ("g(y)",Modif "v1"));;
let ex1s2 = Exists_("y",ex1s1);;
let ex1s3 = AF_(ex1s2);;
let ex1s4 = And_(ex1s0,ex1s3);;

let ex1s3a = AX_(ex1s2);;
let ex1s4a = AX_(AX_(ex1s2));;
let ex1s5a = And_(ex1s0,ex1s4a);;

let ex1s0b = Pred_ ("g(y)", Modif "v0");;
let ex1s1b = Exists_ ("v0",ex1s0b);;
let ex1s2b = Exists_ ("y",ex1s1b);;
let ex1s3b = AF_(ex1s2b);;
let ex1s4b = AX_(ex1s3b);;
let ex1s5b = Pred_ ("f(x)", UnModif "v3");;
let ex1s6b = Exists_ ("v3", ex1s5b);;
let ex1s7b = Exists_ ("x", ex1s6b);;
let ex1s8b = And_(ex1s7b,ex1s4b);;

let ex1s7c = And_(ex1s6b,ex1s4b);;
let ex1s8c = Exists_("x",ex1s7c);;

let ex1phi1 = ex1s4;;
let ex1phi2 = ex1s5a;;
let ex1phi3 =
  And_
 (Exists_ ("x",
  (Exists_ ("v3",
    Pred_ ("f(x)", UnModif "v3")))),
  AX_
   (AF_
    (Exists_ ("y", (* change this to Y and have strange behaviour *)
      (Exists_ ("v0",
       Pred_ ("g(y)", Modif "v0")
                      ))))));;

let ex1phi4 =
  Exists_ ("x",
	   And_ (
	    (Exists_ ("v3",
		     Pred_ ("f(x)", UnModif "v3"))),
	    AX_
	      (AF_
		 (Exists_ ("y", (* change this to Y and have strange behaviour *)
			  (Exists_ ("v0",
				   Pred_ ("g(y)", Modif "v0")
				  )))))));;


let ex1phi5 = AU_(True_,Exists_("y", Exists_("v0",Pred_("g(y)",Modif "v0"))));;

let ex1phi6 =
  AU_(
    Not_(Exists_("x",Exists_("v1",Pred_("f(x)",UnModif "v1")))),
    Exists_("y", Exists_("v0",Pred_("g(y)",Modif "v0")))
  );;

(* use with ex1nc *)
let ex1phi7 =
  AU_(
    Not_(Or_(Pred_("f(1)",Control),Pred_("f(2)",Control))),
    Exists_("y", Exists_("v0",Pred_("g(y)",Modif "v0")))
  );;

let ex1 phi = satbis ex1model (mkanno phi);;
let ex1nc phi = satbis_noclean ex1model (mkanno phi);;


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

let ex2s0 = Pred_("p",Control);;
let ex2s1 = Pred_("{",Control);;
let ex2s2 = Pred_("paren(v)",Control);;
let ex2s3 = And_(ex2s1,ex2s2);;
let ex2s4 = Pred_("}",Control);;
let ex2s5 = Pred_("paren(v)",Control);;
let ex2s6 = And_(ex2s4,ex2s5);;
let ex2s7 = AF_(ex2s6);;
let ex2s8 = And_(ex2s3,ex2s7);;
let ex2s9 = Exists_("v",ex2s8);;
let ex2s10 = AX_(ex2s9);;
let ex2s11 = And_(ex2s0,ex2s10);;

let ex2phi1 = ex2s11;;

let ex2 phi = satbis_noclean ex2model (mkanno phi)

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
