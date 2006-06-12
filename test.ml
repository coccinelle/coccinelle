open Common open Commonop

(* ------------------------------------------------------------------------------ *)
let test1 () = Cocci.test_cocci "../1.c" "../1.cocci"
let test2 () = 
  Cocci.one_flow "../1.c"
    +> Control_flow_c.control_flow_to_ast
    +> (fun def -> Unparse_c.pp_program "../1.c" [Ast_c.Definition def, Unparse_c.PPnormal])
    

let statement1 = Cocci.cstatement_from_string "f(1,2,3);"
let expr1 = Cocci.cexpression_from_string "1"
let rule_elem1 = Cocci.rule_elem_from_string "@@ expression X,Y;@@\n-f(...,X,Y,...);\n+h(X)"

let pattern_result1 = 
  Pattern.match_re_node 
    rule_elem1   (Control_flow_c.Statement statement1, "str")
    (Ast_c.empty_metavars_binding)



let pred_for_transfo = Cocci.rule_elem_from_string "@@ expression X,Y;@@\n-g(Y);\n+h(X,Y);\n"


(* ------------------------------------------------------------------------------ *)
(* I put only in the list the match that modifies *)


let sat_result_for_transfo = 
  [(15, 
   ["X",
    (Ast_c.MetaExpr (Ast_c.al_expr (Cocci.cexpression_from_string "1")));
    "Y",
    (Ast_c.MetaExpr (Ast_c.al_expr (Cocci.cexpression_from_string "1")));
  ],
    pred_for_transfo
   );
   (16,
    ["X", 
     (Ast_c.MetaExpr (Ast_c.al_expr (Cocci.cexpression_from_string "1")));
     "Y",
     (Ast_c.MetaExpr (Ast_c.al_expr (Cocci.cexpression_from_string "2")));
   ],
    pred_for_transfo
   )
 ]


let _display_graph = false

let test_transfo () =
  let cfile = "../1.c" in
  let flow = Cocci.one_flow cfile  in
  if _display_graph then Ograph_extended.print_ograph_extended flow;
  (* Pretty_print.pp_program (Control_flow.control_flow_to_mini_c flow); *)
  let flow = Transformation.transform sat_result_for_transfo flow in
     if _display_graph then Ograph_extended.print_ograph_extended flow;
  let def = (Control_flow_c.control_flow_to_ast flow) in
  Unparse_c.pp_program "../1.c" [Ast_c.Definition def, Unparse_c.PPnormal]



(* TODO

let minic1 = Cocci.cstatement_from_string ""

let res1 = Full_engine.test_cocci minic1 minicocci1a
let res2 = Full_engine.test_cocci minic2 minicocci2a

*)

