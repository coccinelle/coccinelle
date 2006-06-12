open Common open Commonop

(* ------------------------------------------------------------------------------ *)
let test1 () = Cocci.test_cocci "../1.c" "../1.cocci"

let statement1 () = Cocci.cstatement_from_string "f(1,2,3);"
let expr1 () = Cocci.cexpression_from_string "1"
let rule_elem1 () = Cocci.rule_elem_from_string "@@ expression X,Y;@@
-f(...,X,Y,...);
+h(X)
"

let pattern_result1 () = Cocci.test_pattern 
    "f(1,2,3);" 

    "@@ expression X,Y;@@
-f(...,X,Y,...);
+h(X)
"


(* ------------------------------------------------------------------------------ *)
(* I put only in the list the match that modifies *)

let pred_for_transfo () = Cocci.rule_elem_from_string "@@ expression X,Y;@@\n-g(Y);\n+h(X,Y);\n"

let sat_result_for_transfo () = 
  [(15, 
   ["X",
    (Ast_c.MetaExpr (Ast_c.al_expr (Cocci.cexpression_from_string "1")));
    "Y",
    (Ast_c.MetaExpr (Ast_c.al_expr (Cocci.cexpression_from_string "1")));
  ],
    pred_for_transfo ()
   );
   (16,
    ["X", 
     (Ast_c.MetaExpr (Ast_c.al_expr (Cocci.cexpression_from_string "1")));
     "Y",
     (Ast_c.MetaExpr (Ast_c.al_expr (Cocci.cexpression_from_string "2")));
   ],
    pred_for_transfo ()
   )
 ]


let _display_graph = false

let test_transfo () =
  let cfile = "../1.c" in
  let flow = Cocci.one_flow cfile  in
  if _display_graph then Ograph_extended.print_ograph_extended flow;
  (* Pretty_print.pp_program (Control_flow.control_flow_to_mini_c flow); *)
  let flow = Transformation.transform (sat_result_for_transfo ()) flow in
     if _display_graph then Ograph_extended.print_ograph_extended flow;
  let def = (Control_flow_c.control_flow_to_ast flow) in
  Unparse_c.pp_program "../1.c" [Ast_c.Definition def, Unparse_c.PPnormal]




(* ------------------------------------------------------------------------------ *)
let ctl1 = 

Ast_ctl.And
 (Ast_ctl.Exists ("x",
  (Ast_ctl.Exists ("v3",
    Ast_ctl.Pred
     (Ast0toctl.Match (Cocci.rule_elem_from_string "@@expression x;@@
 f(x);")
       ,
     Ast_ctl.UnModif "v3")))),
  Ast_ctl.AX
   (Ast_ctl.AF
    (Ast_ctl.Exists ("y",
      (Ast_ctl.Exists ("v0",
       Ast_ctl.Pred
        (Ast0toctl.Match (Cocci.rule_elem_from_string "@@expression x,y;@@
- g(y);
+ h(x,y);
"
)
          ,
        Ast_ctl.Modif "v0")
                         ))))))


let ctl2 = 

Ast_ctl.Exists ("x",
 Ast_ctl.And
  (Ast_ctl.Exists ("v3",
    Ast_ctl.Pred
     (Ast0toctl.Match (Cocci.rule_elem_from_string "@@expression x;@@
 f(x);"
                      ),
      Ast_ctl.UnModif "v3")),
   Ast_ctl.AX
   (Ast_ctl.Exists ("y",
     Ast_ctl.AF (
(*
     Ast_ctl.AU
      (Ast_ctl.Not
        (Ast_ctl.Or
          (Ast_ctl.Exists ("v2",
            Ast_ctl.Pred
             (Ast0toctl.Match  (Cocci.rule_elem_from_string "@@expression X;@@
 f(X);"
                      ),
              Ast_ctl.UnModif "v2")),
          Ast_ctl.Exists ("v1",
           Ast_ctl.Pred
            (Ast0toctl.Match (Cocci.rule_elem_from_string "@@expression X;@@
- g(Y);
+ h(X,Y);
"
                             ),
             Ast_ctl.Modif "v1")))),
 *)
      Ast_ctl.Exists ("v0",
       Ast_ctl.Pred
        (Ast0toctl.Match (Cocci.rule_elem_from_string "@@expression x,y;@@
- g(y);
+ h(x,y);
"
                            ),
        Ast_ctl.Modif "v0")))))))




let test_ctl_sat ctl = 

  let ctl  = ctl in
  let flow = Cocci.one_flow "../1.c" in

  let model_ctl  = Ctlcocci_integration.model_for_ctl flow ctl in
  let _labels = (Ctlcocci_integration.labels_for_ctl (flow#nodes#tolist) (Ctlcocci_integration.ctl_get_all_predicates ctl)) in

  Ctl_engine.sat model_ctl  ctl




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
(*
    "f(x)" -> [(9,["x" --> "1"],top_wit); (10,["x" --> "2"],top_wit)]
  | "g(y)" -> [(15,["y" --> "1"],top_wit); (16,["y" --> "2"],top_wit)]
*)
  | _ -> []
;;

let ex1graph = 
  let nodes = 

   [(0,"f(1)");(1,"f(2)");(2,"< >");(3,"g(1)");(4,"g(2)");(5,"<exit>")] in
  let edges = [(0,2); (1,2); (2,3); (2,4); (3,5); (4,5); (5,5)] in
(*
    [(9,"f(1)");(10,"f(2)");(11,"< >");(15,"g(1)");(16,"g(2)");(2,"<exit>");
     (0, "fake"); (3, "fake");(5, "fake");(6, "fake");(7, "fake");(8, "fake");(12, "fake");
     (13, "fake"); (14, "fake");(4, "fake");
   ] in
  let edges = [
    (0,3);(3,5);(5,6);(5,7);(6,9);(7,10);(9,8);(10,8);(8,11);
    (11,12);(11,13);(12,15);(13,16);(15,14);(16,14);(14,3);(4,2)
             ] in
*)
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


let ex1phi3bis = 
 Ast_ctl.Exists ("x",
 Ast_ctl.And
  (Ast_ctl.Exists ("v3",
    Ast_ctl.Pred ("f(x)", fake)),
  Ast_ctl.AX
   (Ast_ctl.Exists ("y",
     Ast_ctl.AF (
      Ast_ctl.Exists ("v0",
       Ast_ctl.Pred ("g(y)", fake)))))))


                   


let ex1s0 = Pred( "f(x)",fake);;
let ex1s1 = Pred( "g(y)",fake);;
let ex1s2 = Exists("y",ex1s1);;
let ex1s3 = AF(ex1s2);;
let ex1s4 = And(ex1s0,ex1s3);;

let ex1s3a = AX(ex1s2);;
let ex1s4a = AX(AX(ex1s2));;
let ex1s5a = And(ex1s0,ex1s4a);;

let ex1 phi = sat ex1model phi;;
