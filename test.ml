open Common open Commonop

(* ------------------------------------------------------------------------------ *)
(* let test1 () = Cocci.test_cocci "mytests/1.c" "mytests/1.cocci" *)

let statement1 () = Cocci.cstatement_from_string "f(1,2,3);"
let expr1      () = Cocci.cexpression_from_string "1"
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


let rule_elem_if () = Cocci.rule_elem_from_string "@@@@
 if(1) { 2; }
"


let type_annoted () = Type_annoter_c.annotate_expr [] None (Cocci.cexpression_from_string "1+1")

(* ------------------------------------------------------------------------------ *)

let test_pattern_bis () = 
  let cfile = "evo9.c" in
  let flow = Cocci.one_flow (Cocci.flows (Cocci.cprogram_from_file cfile)) in


  let coccifile = "rule9p2.cocci" in
  let rule_with_metavars_list = Cocci.spbis_from_file coccifile in

  let (all_nodes: Control_flow_c.node list) = flow#nodes#tolist +> List.map snd in
  let (all_rule_elem: Ast_cocci.rule_elem list) = 
    rule_with_metavars_list +> List.hd +> snd +> List.hd +> (fun x -> 
      match x with
      | Ast_cocci.CODE rule_elem_dots -> Ast_cocci.undots rule_elem_dots
      | Ast_cocci.FUNCTION rule_elem_dots -> Ast_cocci.undots rule_elem_dots
      | _ -> raise Todo
      ) 
  in
  let cartesian = cartesian_product all_nodes all_rule_elem in

  cartesian +> map_filter (fun (node,rule) -> 
    let bindings = Pattern.match_re_node  rule node (Ast_c.emptyMetavarsBinding) in
    if not (null bindings) 
    then Some ((node, rule), bindings)
    else None
   )
   +> filter (fun ((node, rule), bindings) -> 
     match rule with
     | Ast_cocci.SeqStart _ 
     | Ast_cocci.SeqEnd _ -> false
     | _ -> true
             )
    +> List.iter (fun ((node, rule), bindings) -> 
         pr2 (Unparse_cocci.rule_elem_to_string rule)
       )

    
  

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
  let cfile = "mytests/1.c" in
  let flow = Cocci.one_flow (Cocci.flows (Cocci.cprogram_from_file cfile))  in
  if _display_graph then Ograph_extended.print_ograph_extended flow;
  (* Pretty_print.pp_program (Control_flow.control_flow_to_mini_c flow); *)
  let flow = Transformation.transform (sat_result_for_transfo ()) flow in
     if _display_graph then Ograph_extended.print_ograph_extended flow;
  let def = (Control_flow_c.control_flow_to_ast flow) in
  Unparse_c.pp_program "mytests/1.c" [Ast_c.Definition def, Unparse_c.PPnormal]




(* ------------------------------------------------------------------------------ *)
(*

let ctl1 () = 

Ast_ctl.And
 (Ast_ctl.Exists ("x",
  (Ast_ctl.Exists ("v3",
    Ast_ctl.Pred
     (Lib_engine.Match (Cocci.rule_elem_from_string "@@expression x;@@
 f(x);")
       ,
     Lib_engine.UnModif "v3")))),
  Ast_ctl.AX
   (Ast_ctl.AF
    (Ast_ctl.Exists ("y",
      (Ast_ctl.Exists ("v0",
       Ast_ctl.Pred
        (Lib_engine.Match (Cocci.rule_elem_from_string "@@expression x,y;@@
- g(y);
+ h(x,y);
"
)
          ,
        Lib_engine.Modif "v0")
                         ))))))


let ctl2 () = 

Ast_ctl.Exists ("x",
 Ast_ctl.And
  (Ast_ctl.Exists ("v3",
    Ast_ctl.Pred
     (Lib_engine.Match (Cocci.rule_elem_from_string "@@expression x;@@
 f(x);"
                      ),
      Lib_engine.UnModif "v3")),
   Ast_ctl.AX
   (Ast_ctl.Exists ("y",
     Ast_ctl.AF (
(*
     Ast_ctl.AU
      (Ast_ctl.Not
        (Ast_ctl.Or
          (Ast_ctl.Exists ("v2",
            Ast_ctl.Pred
             (Lib_engine.Match  (Cocci.rule_elem_from_string "@@expression X;@@
 f(X);"
                      ),
              Ast_ctl.UnModif "v2")),
          Ast_ctl.Exists ("v1",
           Ast_ctl.Pred
            (Lib_engine.Match (Cocci.rule_elem_from_string "@@expression X;@@
- g(Y);
+ h(X,Y);
"
                             ),
             Ast_ctl.Modif "v1")))),
 *)
      Ast_ctl.Exists ("v0",
       Ast_ctl.Pred
        (Lib_engine.Match (Cocci.rule_elem_from_string "@@expression x,y;@@
- g(y);
+ h(x,y);
"
                            ),
        Lib_engine.Modif "v0")))))))

let ctl3 () = 

Ast_ctl.Exists ("x",
 Ast_ctl.And
  (Ast_ctl.Exists ("v3",
    Ast_ctl.Pred
     (Lib_engine.Match (Cocci.rule_elem_from_string "@@expression x;@@
 f(x);"
                      ),
      Lib_engine.UnModif "v3")),
   Ast_ctl.AX (
     Ast_ctl.AF (
     Ast_ctl.Exists ("y",
      Ast_ctl.Exists ("v0",
       Ast_ctl.Pred
        (Lib_engine.Match (Cocci.rule_elem_from_string "@@expression x,y;@@
- g(y);
+ h(x,y);
"
                            ),
        Lib_engine.Modif "v0")))))))
*)




let test_ctl_sat ctl = 

  let flow = Cocci.one_flow (Cocci.flows (Cocci.cprogram_from_file  "mytests/1.c")) in

  let _model_ctl  = Ctlcocci_integration.model_for_ctl flow in
  raise Todo
  (* Ast_ctl.sat model_ctl  ctl *)







(* ********************************************************************** *)
(* Module: EXAMPLE_ENGINE (instance of CTL_ENGINE)                        *)
(* ********************************************************************** *)

(* FIX ME: move *)
module EXAMPLE_ENGINE_BIS = Wrapper_ctl.CTL_ENGINE_BIS (Ctl_engine.SIMPLE_ENV) (Ctl_engine.SIMPLE_CFG);;


let top_wit = []

(* ******************************************************************** *)
(*                                                                      *)
(* EXAMPLES                                                             *)
(*                                                                      *)
(* ******************************************************************** *)

(* For convenience in the examples *)
(* FIX ME: remove *)
open Ast_ctl
open Ctl_engine.SIMPLE_ENV;;
open Ctl_engine.SIMPLE_CFG;;
open EXAMPLE_ENGINE_BIS;;


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
  let _add_edges = map (adde add_nodes) edges in
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
  mkgraph nodes (map (fun (x,y) -> (x,y,())) edges)
;;

let ex1states = List.map fst (ex1graph#nodes)#tolist;;

let ex1model = (ex1graph, ex1lab, ex1states);;

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
                      ))))))

let test_satbis () = 
  assert 
    (satbis ex1model ex1phi3
       =
     [(3, [("x","1");("y","1")], "g(y)");
      (4, [("x","1");("y","2")], "g(y)");
      (3, [("x","2");("y","1")], "g(y)");
      (4, [("x","2");("y","2")], "g(y)");
    ]
    )



(*
let ex1phi3bis = 
 Exists ("x",
 And
  (Exists ("v3",
    Pred ("f(x)")),
  AX
   (Exists ("y",
     AF (
      Exists ("v0",
       Pred ("g(y)")))))))
*)

