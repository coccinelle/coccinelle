(* **********************************************************************
 *
 * Examples for testing the CTL-FVex Engine
 *
 * **********************************************************************)

open Ast_ctl;;
open Ctl_engine;;


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



let top_wit = [];;			(* Always TRUE witness *) 
let fake = Modif ""


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
  mkgraph nodes (List.map (fun (x,y) -> (x,y,())) edges)
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
let ex1phi1 = And(Pred ("f(x)",fake), AF(Exists ("y",Pred( "g(y)",fake))));;
let ex1phi2 = And(Pred( "f(x)",fake), AX(AX(Exists ("y",Pred( "g(y)",fake)))));;

let ex1phi3 = 
Ast_ctl.And
 (Ast_ctl.Exists ("x",
  (Ast_ctl.Exists ("v3",
    Ast_ctl.Pred ("f(x)", fake)))),
  Ast_ctl.AX
   (Ast_ctl.AF
    (Ast_ctl.Exists ("y", (* change this to Y and have strange behaviour *)
      (Ast_ctl.Exists ("v0",
       Ast_ctl.Pred ("g(y)", fake)
                      ))))))
                   


let ex1s0 = Pred( "f(x)",fake);;
let ex1s1 = Pred( "g(y)",fake);;
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

let ex2s0 = Pred( "g(...,x,y,...)",fake);;
let ex2s1 = Exists("y",Pred( "g(...,x,y,...)",fake));;
let ex2s2 = AF(Exists("y",Pred( "g(...,x,y,...)",fake)));;
let ex2s2a = AX(Exists("y",Pred( "g(...,x,y,...)",fake)));;
let ex2s2b = AX(AX(Exists("y",Pred( "g(...,x,y,...)",fake))));;
let ex2s3 = Pred( "f(x)",fake);;
let ex2s4 = And(ex2s3,ex2s2);;
let ex2s5 = Exists("x",ex2s4);;
let ex2s6 = AF(ex2s5);;
let ex2s7 = Pred( "h(z)",fake);;
let ex2s8 = And(ex2s7,ex2s6);;

let ex2phi1 = ex2s8;;




(* ******************************************************************** *)
(* Example Pad                                                          *)
(*   show the need for the witness tree                                 *)
(* ******************************************************************** *)


let ex3graph = 
   let nodes = [(0,"f(1)");
                (1,"< >");(2,"g(2)");(3,"g(3)");
                (4,"< >");(5,"h(2,45");(6,"h(3,44");
                (7, "<exit>")
              ] in
   let edges = [(0,1,()); 
                (1,2,()); (1,3,()); (2,4,()); (3,4,()); 
                (4,5,()); (4,6,()); (5,7,()); (6,7,()); 
                (7,7,()); 
              ] 
   in
   mkgraph nodes edges
;;

let ex3lab s =
  match s with
  | "f(x)" -> [(0,["x" --> "1"],top_wit);]
  | "g(y)" -> [(2,["y" --> "2"],top_wit); (3,["y" --> "3"],top_wit)]
  | "h(y,z)" -> [(5,["y" --> "2";"z" --> "45"],top_wit); (6,["y" --> "3";"z" --> "44"],top_wit)]
  | _ -> []
;;

let ex3states = List.map fst ex3graph#nodes#tolist;;

let ex3model = (ex3graph,ex3lab,ex3states);;

let ex3 phi = sat ex3model phi;;

let phi3 = And (Pred( "f(x)",fake), 
                AX (AF (Exists("y",  And (Pred( "g(y)",fake), 
                                          EX (EX (Exists("z", Pred( "h(y,z)",fake)))))))))


(* ******************************************************************** *)
(* Example Pad bug1                                                          *)
(*                                    *)
(* ******************************************************************** *)

let ex4graph = 
   let nodes = [(0,"f(1,2,3)");
                (1,"< >");(2,"g(1)");(3,"g(1)");
                (4, "<exit>")
              ] in
   let edges = [(0,1,()); 
                (1,2,()); (1,3,()); (2,4,()); (3,4,()); 
                (4,4,()); 
              ] 
   in
   mkgraph nodes edges
;;

let ex4lab s =
  match s with
  | "f(...,x,y,...)" -> [(0,["x" --> "1";"y" --> "2"],top_wit);
                         (0,["x" --> "2";"y" --> "3"],top_wit);
                       ]
  | "g(x)" -> [(2,["x" --> "1"],top_wit); (3,["x" --> "1"],top_wit)]
  | _ -> []
;;

let ex4states = List.map fst ex4graph#nodes#tolist;;

let ex4model = (ex4graph,ex4lab,ex4states);;

let ex4 phi = sat ex4model phi;;

let phi4 = Exists("x", Exists("y", And (Pred( "f(...,x,y,...)",fake), 
                AX (AF (Pred( "g(x)",fake))))))


(* ********************************************************************** *)
(* ********************************************************************** *)
