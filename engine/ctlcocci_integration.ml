open Common open Commonop

open Ograph_extended

let (-->) x v = Ast_ctl.Subst (x,v);;


(******************************************************************************)

(* Take list of pred  and for each pred return where in control flow
it matches (and the set of subsitutions for this match). *)
let (labels_for_ctl: 
 (nodei * Control_flow_c.node) list -> Lib_engine.label_ctlcocci) =
  fun nodes ->

   (fun pred -> 

     if !Flag_engine.debug_engine
     then begin 
       pp_init (fun () -> 
         pp "labeling: pred =";
         Format.print_space ();
         Pretty_print_engine.pp_predicate pred;
         );
     end;

     let nodes' = nodes +> map (fun (nodei, node) -> 
      (* todo? put part of this code in pattern ? *)
      (match pred, Control_flow_c.unwrap node with
      | Lib_engine.Paren s,  (Control_flow_c.StartBrace (bracelevel, _, _)) -> 
         [(nodei,     [(s --> (Lib_engine.ParenVal (i_to_s bracelevel)))])]
      | Lib_engine.Paren s,  (Control_flow_c.EndBrace (bracelevel, _)) -> 
          [(nodei,    [(s --> (Lib_engine.ParenVal (i_to_s bracelevel)))])]
      | Lib_engine.Paren _, _ -> []

      | Lib_engine.Label s, _ -> 
          let labels = Control_flow_c.extract_labels node in
          [(nodei, [(s --> (Lib_engine.LabelVal labels))])]
      | Lib_engine.PrefixLabel s, _ -> 
          let labels = Control_flow_c.extract_labels node in
          let prefixes = Common.inits labels +> Common.tail in
          prefixes +> List.map (fun prefixlabels -> 
            (nodei, [(s --> (Lib_engine.LabelVal prefixlabels))])
          )
          

      | Lib_engine.Match (re), _node -> 
          let substs = Pattern.match_re_node re node Ast_c.emptyMetavarsBinding
          in
          if substs <> []
          then
            substs +> List.map (fun subst -> 
              (nodei, 
               subst +> List.map (fun (s, meta) -> 
                 s --> Lib_engine.NormalMetaVal meta
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
            (* todo: one day try also to match the special function
               such as panic(); *)
          | Control_flow_c.Statement (Ast_c.Jump (Ast_c.Return), _) -> 
              [nodei, []]
          | Control_flow_c.Statement (Ast_c.Jump (Ast_c.ReturnExpr _), _) -> 
              [nodei, []]
          | _ -> []
          )
      )
                               ) +> List.concat
     in
     if !Flag_engine.debug_engine
     then begin 
       pp_init (fun () -> 
         pp "labeling: result =";
         Format.print_space ();
         
         pp_do_in_box (fun () -> 
           pp "{";
           Common.print_between 
             (fun () -> pp ";"; Format.print_cut())
             (fun (nodei, subst) -> 
               Format.print_int nodei;
               pp_do_in_box (fun () -> Pretty_print_engine.pp_binding2_ctlsubst subst)
             ) nodes';
           pp "}";
                      );
               )
     end;
     nodes'
   ) 

let (control_flow_for_ctl: 
       (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
         ('a, 'b) ograph_extended) = 
 fun cflow ->
 (* could erase info on nodes, and edge,  because they are not used by rene *)
  cflow


(* Just make the final node of the control flow loop over itself. 
   It seems that one hypothesis of the SAT algorithm is that each node as at least a successor.
   todo?: erase some fake nodes ? (and adjust the edges accordingly) *)
let (fix_flow_ctl: 
   (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
   (Control_flow_c.node, Control_flow_c.edge) ograph_extended) = 
 fun  flow ->
  let (exitnodei, node) = flow#nodes#tolist +> List.find (fun (nodei, node) -> 
    match Control_flow_c.unwrap node with
    | Control_flow_c.Exit -> true 
    | _ -> false
    )
  in
  let flow = flow#add_arc ((exitnodei, exitnodei), Control_flow_c.Direct) in

  assert (flow#nodes#tolist +> List.for_all (fun (nodei, node) -> 
    List.length ((flow#successors nodei)#tolist) >= 1 
    (* no:  && List.length ((flow#predecessors nodei)#tolist) >= 1  
       because    the enter node at least have no predecessors *)
      ));

  flow



let model_for_ctl  cflow = 
 let newflow = fix_flow_ctl (control_flow_for_ctl cflow) in
 let labels = labels_for_ctl (cflow#nodes#tolist)  in
 let states = List.map fst  newflow#nodes#tolist  in
 newflow, labels, states
 

(******************************************************************************)
module PRED = 
  struct
    type t = Lib_engine.predicate
    let print_predicate x = 
      Pretty_print_cocci.print_plus_flag := false;
      Pretty_print_cocci.print_minus_flag := false;
      Pretty_print_engine.pp_predicate x
  end

module ENV =
  struct
    type value = Lib_engine.metavar_binding_kind2
    type mvar = string
    let eq_mvar x x' = x = x';;
    let eq_val v v' = v = v';;
    let merge_val v v' = v;;	       

    let print_mvar s = Format.print_string s
    let print_value x = Pretty_print_engine.pp_binding_kind2 x
  end


module CFG = 
  struct
    type node = int;;
    type cfg = (Control_flow_c.node, Control_flow_c.edge) Ograph_extended.ograph_extended;;
    let predecessors cfg n = List.map fst ((cfg#predecessors n)#tolist);;
    let print_node i = Format.print_string (i_to_s i)
  end


module WRAPPED_ENGINE = Wrapper_ctl.CTL_ENGINE_BIS (ENV) (CFG) (PRED)


(******************************************************************************)
let (mysat:
       ((Control_flow_c.node, Control_flow_c.edge) ograph_extended *
        Lib_engine.label_ctlcocci *
        nodei list) -> 
       Lib_engine.ctlcocci -> 
       (nodei * 
        (Lib_engine.mvar * Lib_engine.metavar_binding_kind2) list *
         Lib_engine.predicate) 
       list) = 
 fun (flow, label, states) ctl -> 
      WRAPPED_ENGINE.satbis (flow, label, states) ctl


let (satbis_to_trans_info: 
  (nodei * 
   (Lib_engine.mvar * Lib_engine.metavar_binding_kind2) list *  
   Lib_engine.predicate) 
  list -> 
  (nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list) = 
  fun xs -> 
    xs +> List.map (fun (nodei, binding, pred) -> 
         let binding' = binding +> map_filter (fun (s, kind2) -> 
             (match kind2 with
             | Lib_engine.NormalMetaVal kind -> Some (s, kind)
             (* I thought it was Impossible, but it does not seems so *)
             | Lib_engine.ParenVal _ -> None
             | Lib_engine.LabelVal _ -> None
             )
           ) in
         let pred' = 
           (match pred with
           | Lib_engine.Match rule_elem -> rule_elem
           | _ -> raise Impossible
           ) in
         
         nodei, binding', pred'
         )


