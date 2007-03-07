open Common open Commonop

open Ograph_extended

(*****************************************************************************)
(* Debugging functions *)
(*****************************************************************************)
let show_or_not_predicate pred = 
  if !Flag_engine.debug_engine then begin 
    indent_do (fun () -> 
      adjust_pp_with_indent_and_header "labeling: pred = " (fun () -> 
        Pretty_print_engine.pp_predicate pred;
      );
    )
  end

let show_or_not_nodes nodes =
  if !Flag_engine.debug_engine  then begin 

    indent_do (fun () -> 
      adjust_pp_with_indent_and_header "labeling: result = " (fun () -> 
        Common.pp_do_in_box (fun () -> 
          pp "{";
          Common.print_between 
            (fun () -> pp ";"; Format.print_cut())
            (fun (nodei, (_predTODO, subst)) -> 
              Format.print_int nodei;
              Common.pp_do_in_box (fun () -> 
                Pretty_print_engine.pp_binding2_ctlsubst subst
              )
            ) nodes;
          pp "}";
        );
      )
    )
  end


(*****************************************************************************)
(* Labeling function *)
(*****************************************************************************)
let (-->) x v = Ast_ctl.Subst (x,v);;


(* Take list of predicate and for each predicate returns where in the
 * control flow it matches, and the set of subsitutions for this match. 
 *)
let (labels_for_ctl: 
 (nodei * Control_flow_c.node) list -> Lib_engine.metavars_binding -> 
 Lib_engine.label_ctlcocci) =
  fun nodes binding ->

   (fun p -> 
     show_or_not_predicate p;

     let nodes' = nodes +> map (fun (nodei, node) -> 
      (* todo? put part of this code in pattern ? *)
      (match p, Control_flow_c.unwrap node with
      | Lib_engine.Paren s,  (Control_flow_c.SeqStart (_, bracelevel, _)) -> 
          [(nodei, (p,[(s --> (Lib_engine.ParenVal (i_to_s bracelevel)))]))]
      | Lib_engine.Paren s,  (Control_flow_c.SeqEnd (bracelevel, _)) -> 
          [(nodei, (p,[(s --> (Lib_engine.ParenVal (i_to_s bracelevel)))]))]
      | Lib_engine.Paren _, _ -> []

      | Lib_engine.Label s, _ -> 
          let labels = Control_flow_c.extract_labels node in
          [(nodei, (p,[(s --> (Lib_engine.LabelVal labels))]))]
      | Lib_engine.PrefixLabel s, _ -> 
          let labels = Control_flow_c.extract_labels node in
          let prefixes = Common.inits labels +> Common.tail in
          prefixes +> List.map (fun prefixlabels -> 
            (nodei, (p,[(s --> (Lib_engine.LabelVal prefixlabels))]))
          )
          

      | Lib_engine.Match (re), _unwrapnode -> 
          let substs = 
                Pattern3.match_re_node re node binding
                  +> List.map (fun (re', subst) -> 
                    Lib_engine.Match (re'), subst
                  )
          in
          substs +> List.map (fun (p', subst) -> 
            (nodei, 
              (p', 
                subst +> List.map (fun (s, meta) -> 
                  s --> Lib_engine.NormalMetaVal meta
                )
              )
            )
          )

      | Lib_engine.TrueBranch , Control_flow_c.TrueNode ->  [nodei, (p,[])]
      | Lib_engine.FalseBranch, Control_flow_c.FalseNode -> [nodei, (p,[])]
      | Lib_engine.After,       Control_flow_c.AfterNode -> [nodei, (p,[])]
      | Lib_engine.FallThrough, Control_flow_c.FallThroughNode ->[nodei,(p,[])]
      | Lib_engine.Exit,        Control_flow_c.Exit ->      [nodei, (p,[])]
      | Lib_engine.ErrorExit,   Control_flow_c.ErrorExit -> [nodei, (p,[])]

      | Lib_engine.TrueBranch , _ -> []
      | Lib_engine.FalseBranch, _ -> []
      | Lib_engine.After, _ -> []
      | Lib_engine.FallThrough, _ -> []
      | Lib_engine.Exit, _  -> []
      | Lib_engine.ErrorExit, _  -> []

      | Lib_engine.Return, node -> 
          (match node with
            (* todo? should match the Exit code ? 
             * todo: one day try also to match the special function
             * such as panic(); 
             *)
          | Control_flow_c.Return _ ->  [nodei, (p,[])]
          | Control_flow_c.ReturnExpr _ -> [nodei, (p,[])]
          | _ -> []
          )
      )
     ) +> List.concat
     in

     show_or_not_nodes nodes';
     nodes'
   ) 



(*****************************************************************************)
(* Some fix flow, for CTL, for unparse *)
(*****************************************************************************)
(* could erase info on nodes, and edge, because they are not used by rene *)
let (control_flow_for_ctl: Control_flow_c.cflow -> ('a, 'b) ograph_extended) = 
 fun cflow -> cflow



(* Just make the final node of the control flow loop over itself. 
 * It seems that one hypothesis of the SAT algorithm is that each node as at
 * least a successor.
 * update: do same for errorexit node.
 * 
 * Addon: also erase the fake nodes (and adjust the edges accordingly), so that
 * AX in CTL can now work.
 * Indeed, à la fin de la branche then (et else), on devrait aller directement
 * au suivant du endif, sinon si ecrit if(1) { foo(); }; bar();
 * sans '...' entre le if et bar(), alors ca matchera pas car le CTL
 * generera un AX bar()  qui il tombera d'abord sur le [endif] :( 
 * Mais chiant de changer l'algo de generation, marche pas tres bien avec 
 * ma facon de faire recursive et compositionnel.
 * => faire une fonction qui applique des fixes autour de ce control flow,
 * comme ca passe un bon flow a rene, mais garde un flow a moi pour pouvoir 
 * facilement generate back the ast.
 *
 * alt: faire un wrapper autourde mon graphe pour lui passer dans le module CFG
 * une fonction qui passe a travers les Fake, mais bof.
 * 
 * update: also make loop the deadcode nodes, the one that have
 * no predecessor.
 *)
let (fix_flow_ctl2: Control_flow_c.cflow -> Control_flow_c.cflow) = fun flow ->
  let g = ref flow in

  (* ------------------------ *)
  (* helpers *)
  (* ------------------------ *)
    
  let adjust_g (newg)        = begin  g := newg;    end in
  let adjust_g_i (newg,newi) = begin  g := newg;   newi end in

  let find_node f = 
    !g#nodes#tolist 
     +> List.find (fun (nodei, node) -> f (Control_flow_c.unwrap node)) 
     +> fst
  in

  (* remove an intermediate node and redirect the connexion  *)
  let remove_one_node nodei = 
    let preds = (!g#predecessors nodei)#tolist in
    let succs = (!g#successors nodei)#tolist in
    assert (not (null preds));

    preds +> List.iter (fun (predi, Control_flow_c.Direct) -> 
      !g#del_arc ((predi, nodei), Control_flow_c.Direct) +> adjust_g;
      );
    succs +> List.iter (fun (succi, Control_flow_c.Direct) -> 
      !g#del_arc ((nodei, succi), Control_flow_c.Direct) +> adjust_g;
      );
    
    !g#del_node nodei +> adjust_g;

    (* connect in-nodes to out-nodes *)
    preds +> List.iter (fun (pred, Control_flow_c.Direct) -> 
      succs +> List.iter (fun (succ, Control_flow_c.Direct) -> 
        !g#add_arc ((pred, succ), Control_flow_c.Direct) +> adjust_g;
        );
      );
  in



  (* ------------------------ *)

  (* note that must choose a kind that will not be deleted after *)
  let topi = !g#add_node ((Control_flow_c.Fake, []), "start") +> adjust_g_i
  in
  let enteri = 
    find_node (function Control_flow_c.FunHeader _ -> true | _ -> false)
  in
  let exitnodei  = find_node (fun x -> x = Control_flow_c.Exit) in
  let errornodei = find_node (fun x -> x = Control_flow_c.ErrorExit) in


  !g#add_arc ((topi, topi), Control_flow_c.Direct) +> adjust_g;
  !g#add_arc ((topi, enteri), Control_flow_c.Direct) +> adjust_g;
  !g#add_arc ((exitnodei, exitnodei), Control_flow_c.Direct) +> adjust_g;

  if null ((!g#successors   errornodei)#tolist) &&
     null ((!g#predecessors errornodei)#tolist)
  then
    !g#del_node errornodei +> adjust_g
  else 
    !g#add_arc ((errornodei, errornodei), Control_flow_c.Direct) +> adjust_g;


  let fake_nodes = !g#nodes#tolist +> List.filter (fun (nodei, node) -> 
    match Control_flow_c.unwrap node with
    | Control_flow_c.CaseNode _ 
    | Control_flow_c.Enter
    (*| Control_flow_c.Fake*) (* [endif], [endswitch], ... *)
      -> true
    | _ -> false 
    ) in
  
  fake_nodes +> List.iter (fun (nodei, node) -> remove_one_node nodei);

  (* even when have deadcode, julia want loop over those nodes *)
  !g#nodes#tolist +> List.iter (fun (nodei, node) -> 
    if (!g#predecessors nodei)#null 
    then begin
      let fakei = !g#add_node ((Control_flow_c.Fake,[]), "DEADCODELOOP") 
        +> adjust_g_i
      in
      !g#add_arc ((fakei, nodei), Control_flow_c.Direct) +> adjust_g;
      !g#add_arc ((fakei, fakei), Control_flow_c.Direct) +> adjust_g;
    end
  );


  !g#nodes#tolist +> List.iter (fun (nodei, node) -> 
    assert (List.length ((!g#successors nodei)#tolist) >= 1); 
    (* no:  && List.length ((!g#predecessors nodei)#tolist) >= 1  
       because    the enter node at least have no predecessors *)
    );

  !g
let fix_flow_ctl a = 
  Common.profile_code "fix_flow" (fun () -> fix_flow_ctl2 a)





let (fix_simple_flow_ctl: Control_flow_c.cflow -> Control_flow_c.cflow) = 
 fun flow -> 
  let nodes = flow#nodes#tolist in
  match nodes with 
  | [(nodei, n)] -> 
      flow#add_arc ((nodei, nodei), Control_flow_c.Direct)
  | _ -> failwith "simple flow can contain only one node"

(*****************************************************************************)
(* subtil: the label must operate on newflow, not (old) cflow 
 * update: now I supposed that we give me a fixed_flow
 *)
let model_for_ctl  cflow binding = 
 let newflow = cflow (* old: fix_flow_ctl (control_flow_for_ctl cflow) *) in
 let labels = labels_for_ctl (newflow#nodes#tolist) binding  in
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
    let eq_mvar x x' = x = x'
    let eq_val v v' = v = v'
    let merge_val v v' = v	       

    let print_mvar s = Format.print_string s
    let print_value x = Pretty_print_engine.pp_binding_kind2 x
  end


module CFG = 
  struct
    type node = int
    type cfg = 
        (Control_flow_c.node, Control_flow_c.edge) 
        Ograph_extended.ograph_extended
    let predecessors cfg n = List.map fst ((cfg#predecessors n)#tolist)
    let successors   cfg n = List.map fst ((cfg#successors n)#tolist)
    let print_node i = Format.print_string (i_to_s i)
  end


module WRAPPED_ENGINE = Wrapper_ctl.CTL_ENGINE_BIS (ENV) (CFG) (PRED)

let print_bench _ = WRAPPED_ENGINE.print_bench()

type pred = Lib_engine.predicate * string Ast_ctl.modif

(*****************************************************************************)
let metavars_binding2_to_binding   binding2 = 
  binding2 +> Common.map_filter (fun (s, kind2) -> 
    match kind2 with
    | Lib_engine.NormalMetaVal kind -> Some (s, kind)
    (* I thought it was Impossible to have this when called from
       satbis_to_trans_info but it does not seems so *)
    | Lib_engine.ParenVal _ -> None
    | Lib_engine.LabelVal _ -> None
   )

let metavars_binding_to_binding2 binding = 
  binding +> List.map (fun (s, kind) -> s, Lib_engine.NormalMetaVal kind)


let (satbis_to_trans_info: 
  (nodei * Lib_engine.metavars_binding2 * Lib_engine.predicate) list -> 
  (nodei * Lib_engine.metavars_binding * Ast_cocci.rule_elem) list) = 
  fun xs -> 
    xs +> List.map (fun (nodei, binding2, pred) -> 
         let rule_elem = 
           (match pred with
           | Lib_engine.Match rule_elem -> rule_elem
           | _ -> raise Impossible
           ) in
         nodei, metavars_binding2_to_binding binding2, rule_elem
         )

(*****************************************************************************)
(* Call ctl engine *)
(*****************************************************************************)
let (mysat2:
  Lib_engine.model ->
  (Lib_engine.ctlcocci * (pred list * pred list)) -> 
  (Lib_engine.mvar list * Lib_engine.metavars_binding) ->
  (Lib_engine.transformation_info * bool * Lib_engine.metavars_binding list)) =
  fun (flow, label, states) ctl (used_after, binding) -> 
    let binding2 = metavars_binding_to_binding2 binding in
    let (triples,(trans_info2, returned_any_states, used_after_envs)) = 
      WRAPPED_ENGINE.satbis (flow, label, states) ctl (used_after, binding2)
    in
    if not (!Flag_parsing_cocci.sgrep_mode)
    then Check_reachability.check_reachability triples flow;
    let (trans_info2,used_after_fresh_env) =
      Postprocess_transinfo.process used_after binding2 trans_info2 in
    let used_after_envs =
      (* not ideal, fresh envs should be tree specific *)
      List.map (function x -> used_after_fresh_env @ x) used_after_envs in
    let trans_info = satbis_to_trans_info trans_info2 in
    let newbindings = List.map metavars_binding2_to_binding used_after_envs in
    (trans_info, returned_any_states, newbindings)

let mysat a b c = 
  Common.profile_code "mysat" (fun () -> mysat2 a b c)
