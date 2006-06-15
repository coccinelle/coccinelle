open Common open Commonop

open Ograph_extended





let (-->) x v = Ast_ctl.Subst (x,v);;


(*
Take list of pred  and for each pred return where in control flow
it matches (and the set of subsitutions for this match).
*)

let top_wit = []

let (labels_for_ctl: 
  (nodei * Control_flow_c.node) list -> 
  (Lib_engine.predicate ->
   (nodei * 
    (Lib_engine.mvar, Lib_engine.metavar_binding_kind2) Ast_ctl.generic_substitution * 'a list)
   list))
  = fun nodes ->

   (fun pred -> 
       let nodes = nodes +> map (fun (nodei, (node, nodestring)) -> 
         (* todo? put part of this code in pattern ? *)
         (match pred, node with
         | Lib_engine.Paren s,  (Control_flow_c.StartBrace (bracelevel, _)) -> 
             [(nodei,         [(s -->   (Lib_engine.ParenVar (i_to_s bracelevel)))], top_wit)]
         | Lib_engine.Paren s,  (Control_flow_c.EndBrace bracelevel) -> 
             [(nodei,         [(s -->   (Lib_engine.ParenVar (i_to_s bracelevel)))], top_wit)]
         | Lib_engine.Paren _, _ -> 
             []

         | Lib_engine.Match (re),    node -> 
             let substs = Pattern.match_re_node re (node, nodestring)  (Ast_c.emptyMetavarsBinding) in
             if substs <> []
             then
               substs +> List.map (fun subst -> 
                 (nodei, 
                  subst +> List.map (fun (s, meta) -> 
                    s --> Lib_engine.NormalMetaVar meta
                                    ),
                  top_wit
                 )
                )
             else []

         | Lib_engine.TrueBranch , Control_flow_c.TrueNode ->  [nodei, [], top_wit]
         | Lib_engine.FalseBranch, Control_flow_c.FalseNode -> [nodei, [], top_wit]
         | Lib_engine.After,       Control_flow_c.AfterNode -> [nodei, [], top_wit]
         | Lib_engine.TrueBranch , _ -> []
         | Lib_engine.FalseBranch, _ -> []
         | Lib_engine.After, _ -> []
         )
       ) +> List.concat
       in
       nodes
       ) 

let (control_flow_for_ctl: (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> ('a, 'b) ograph_extended) = fun cflow ->
 (* could erase info on nodes, and edge,  because they are not used by rene *)
  cflow


(* Just make the final node of the control flow loop over itself. 
   It seems that one hypothesis of the SAT algorithm is that each node as at least a successor.
*)
let (fix_flow_ctl: (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> (Control_flow_c.node, Control_flow_c.edge) ograph_extended) = fun  flow ->
  let (exitnodei, (node, nodestr)) = flow#nodes#tolist +> List.find (function (nodei, (Control_flow_c.Exit, nodes)) -> true | _ -> false) in
  let flow = flow#add_arc ((exitnodei, exitnodei), Control_flow_c.Direct) in
  assert (flow#nodes#tolist +> List.for_all (fun (nodei, node) -> 
    List.length ((flow#successors nodei)#tolist) >= 1 
    (* no:  && List.length ((flow#predecessors nodei)#tolist) >= 1  
       because    the enter node at least have no predecessors 
     *)
      ));
  flow



let model_for_ctl  cflow = 
 let newflow = fix_flow_ctl (control_flow_for_ctl cflow) in
 let labels = labels_for_ctl (cflow#nodes#tolist)  in
 let states = List.map fst  newflow#nodes#tolist  in
 newflow, labels, states
 




(* let sattrans = convert_... give = *)
