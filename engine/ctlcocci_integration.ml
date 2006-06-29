open Common open Commonop

open Ograph_extended

let (-->) x v = Ast_ctl.Subst (x,v);;

(*
Take list of pred  and for each pred return where in control flow
it matches (and the set of subsitutions for this match).
*)

let (labels_for_ctl: 
  (nodei * Control_flow_c.node) list -> 
   (Lib_engine.predicate ->
     (nodei * (Lib_engine.mvar, Lib_engine.metavar_binding_kind2) Ast_ctl.generic_substitution) list
   ))
  = fun nodes ->

   (fun pred -> 
       pr2 "start label";
       pr2 (Dumper.dump pred);
       let nodes' = nodes +> map (fun (nodei, (node, nodestring)) -> 
         (* todo? put part of this code in pattern ? *)
         (match pred, node with
         | Lib_engine.Paren s,  (Control_flow_c.StartBrace (bracelevel, _)) -> 
             [(nodei,         [(s -->   (Lib_engine.ParenVar (i_to_s bracelevel)))])]
         | Lib_engine.Paren s,  (Control_flow_c.EndBrace bracelevel) -> 
             [(nodei,         [(s -->   (Lib_engine.ParenVar (i_to_s bracelevel)))])]
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
                                    )
                 )
                )
             else []

         | Lib_engine.TrueBranch , Control_flow_c.TrueNode ->  [nodei, []]
         | Lib_engine.FalseBranch, Control_flow_c.FalseNode -> [nodei, []]
         | Lib_engine.After,       Control_flow_c.AfterNode -> [nodei, []]
         | Lib_engine.TrueBranch , _ -> []
         | Lib_engine.FalseBranch, _ -> []
         | Lib_engine.After, _ -> []
         | Lib_engine.Return, node -> 
             (match node with
             (* todo? should match the Exit code ? *)
             | Control_flow_c.Statement (Ast_c.Jump (Ast_c.Return), _) -> 
                 [nodei, []]
             | Control_flow_c.Statement (Ast_c.Jump (Ast_c.ReturnExpr _), _) -> 
                 [nodei, []]
             | _ -> []
             )
         )
       ) +> List.concat
       in
       pr2 "end label";
       pr2 (Dumper.dump nodes');
       nodes'
       ) 

let (control_flow_for_ctl: (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> ('a, 'b) ograph_extended) = fun cflow ->
 (* could erase info on nodes, and edge,  because they are not used by rene *)
  cflow


(* Just make the final node of the control flow loop over itself. 
   It seems that one hypothesis of the SAT algorithm is that each node as at least a successor.
   todo?: erase some fake nodes ? (and adjust the edges accordingly)
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
 


module PRED = 
  struct
    type predicate = Lib_engine.predicate
  end

module ENV =
  struct
    type value = Lib_engine.metavar_binding_kind2
    type mvar = string
    let eq_mvar x x' = x = x';;
    let eq_val v v' = v = v';;
    let merge_val v v' = v;;	       
  end


module CFG = 
  struct
    type node = int;;
    type cfg = (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended;;
    let predecessors cfg n = List.map fst ((cfg#predecessors n)#tolist);;
  end


module WRAPPED_ENGINE = Wrapper_ctl.CTL_ENGINE_BIS (ENV) (CFG) (PRED)


let (mysat:
       ((Control_flow_c.node, Control_flow_c.edge) ograph_extended *
        (Lib_engine.predicate ->
           (nodei * (Lib_engine.mvar, Lib_engine.metavar_binding_kind2) Ast_ctl.generic_substitution)
            list) *
         nodei list)
         -> (Lib_engine.predicate, Lib_engine.mvar, unit) Wrapper_ctl.wrapped_ctl
           -> (nodei * (Lib_engine.mvar * Lib_engine.metavar_binding_kind2) list *
                 Lib_engine.predicate) list

    ) = fun (flow, label, states) ctl -> 
      WRAPPED_ENGINE.satbis (flow, label, states) ctl

let mysat_noclean = fun (flow, label, states) ctl -> 
      WRAPPED_ENGINE.satbis_noclean (flow, label, states) ctl


let (satbis_to_trans_info: 
  (nodei * (Lib_engine.mvar * Lib_engine.metavar_binding_kind2) list *  Lib_engine.predicate) list
     -> (nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list) = fun xs -> 
       xs +> List.map (fun (nodei, binding, pred) -> 
         let binding' = binding +> map_filter (fun (s, kind2) -> 
             (match kind2 with
             | Lib_engine.NormalMetaVar kind -> Some (s, kind)
             | Lib_engine.ParenVar _ -> 
                 (* I thought it was Impossible, but it does not seems so *)
                 None
             )
           ) in
         let pred' = 
           (match pred with
           | Lib_engine.Match rule_elem -> rule_elem
           | _ -> raise Impossible
           ) in
         
         nodei, binding', pred'
         )
