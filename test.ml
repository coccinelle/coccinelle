open Common open Commonop


let test1 () = Cocci.test_cocci "tests/1.c" "tests/1.cocci"

let test2 = Cocci.one_flow "tests/1.c" +> Control_flow_c.control_flow_to_ast



(*
(* ------------------------------------------------------------------------------ *)

let rule_elem1 = 
      B.ExprSt (
         B.FunCall (  ("f",  j0),
                      ("(",  j0),
                      [
(*                        B.MetaExpr ("X", (B.CONTEXT (ref B.NOTHING))); *)
                        B.Edots (("..." ,   (j0)));
                        B.MetaExpr ("X", (j0));
                        B.MetaExpr ("Y", (j0));
                        B.Edots (("..." ,   (j0)));

                      ],
                      (")",  (j0))
                    ) 
              ,
              (";",  (j0))
             ) 
let node1 = Control_flow.Statement  (Misc.mini_c_add_token (
    A.ExprSt (A.FunCall ("f", [(A.Int 1, i0), i0;
                               (A.Int 2, i0), i0;
                               (A.Int 3, i0), i0;
                              ]), i0), i0
    )), "f(1,2,3"

let pattern_result1 = Pattern.match_re_node 
                       rule_elem1   node1
                      (Mini_c.empty_metavars_binding)






(* ------------------------------------------------------------------------------ *)




let res1 = Full_engine.test_cocci minic1 minicocci1a
let res2 = Full_engine.test_cocci minic2 minicocci2a



(* ------------------------------------------------------------------------------ *)
(* I put only in the list the match that modifies *)

(*
- g(Y);
+ h(X,Y);
*)

let pred_for_transfo = 
      B.ExprSt (
         B.FunCall (  ("g",  (B.MINUS (ref []))),
                      ("(",  (B.MINUS (ref []))),
                      [
                        B.MetaExpr ("Y", (B.MINUS (ref [])));
                      ],
                      (")",  (B.MINUS (ref [])))
                    ) 
              ,
              (";",  (B.MINUS (ref 
                       [[
                         B.Rule_elemTag (
                                 B.ExprSt (
                                       B.FunCall (  ("h",  ((B.CONTEXT (ref B.NOTHING)))),
                                                    ("(",  ((B.CONTEXT (ref B.NOTHING)))),
                                                    [
                                                      B.MetaExpr ("X", ((B.CONTEXT (ref B.NOTHING))));
                                                      B.EComma (",", ((B.CONTEXT (ref B.NOTHING))));
                                                      B.MetaExpr ("Y", ((B.CONTEXT (ref B.NOTHING))));
                                                     
                                                    ],
                                                    (")",  ((B.CONTEXT (ref B.NOTHING))))
                                                  ) 
                                            ,
                                            (";",  ((B.CONTEXT (ref B.NOTHING))))
                                          )
                                      )
                       ]]
                                 )))
                      
             )



let sat_result_for_transfo = 
  [(14, 
   ["X",
    (Mini_c.MetaExpr
         (Mini_c.Int 1,
          [(("1", Mini_cocci.CONTEXT {contents = Mini_cocci.NOTHING}), [])]));
    "Y",
    (Mini_c.MetaExpr
         (Mini_c.Int 1,
          [(("1", Mini_cocci.CONTEXT {contents = Mini_cocci.NOTHING}), [])]));
  ],
    pred_for_transfo
   );
   (15,
    ["X",
     (Mini_c.MetaExpr
         (Mini_c.Int 1,
          [(("1", Mini_cocci.CONTEXT {contents = Mini_cocci.NOTHING}), [])]));
     "Y",
     (Mini_c.MetaExpr
         (Mini_c.Int 2,
          [(("2", Mini_cocci.CONTEXT {contents = Mini_cocci.NOTHING}), [])]));
   ],
    pred_for_transfo
   )
 ]


let _display_graph = false

let test_transfo () =
  let ast = minic1 in
  let flow = Control_flow.mini_c_to_control_flow ast in
     if _display_graph then Ograph_extended.print_control_flow flow;
  Pretty_print.pp_program (Control_flow.control_flow_to_mini_c flow);
  let flow = Transformation.transform sat_result_for_transfo flow in
     if _display_graph then Ograph_extended.print_control_flow flow;
  let ast =(Control_flow.control_flow_to_mini_c flow) in
  Pretty_print.pp_program ast;
  flow


(* ******************************************************************** *)
(* ******************************************************************** *)


let main () =
  ignore (test_transfo ())
  

let _ = if not !Sys.interactive then main () 

*)
