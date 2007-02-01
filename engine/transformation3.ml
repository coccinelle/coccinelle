open Common open Commonop

module F = Control_flow_c

module XTRANS = struct

  type tin = Lib_engine.metavars_binding
  type 'x tout = 'x option


  let (>>=) m f = fun tin -> 
     match m tin with
     | None -> None
     | Some (a,b) -> f a b tin

  let return = fun x -> fun tin -> 
    Some x

  (* can have fail in transform now that the process is deterministic ? *)
  let fail = fun tin -> 
    None

  let (>||>) m1 m2 = fun tin -> 
    match m1 tin with
    | None -> m2 tin
    | Some x -> Some x (* stop as soon as have found something *)

  let (cocciexp : 
      (Ast_cocci.expression->Ast_c.expression -> tin -> (Ast_cocci.expression*Ast_c.expression)tout) ->
      Ast_cocci.expression -> Control_flow_c.node -> (tin -> (Ast_cocci.expression * Control_flow_c.node) tout))
   = fun expf expa node -> fun binding -> 

    let bigf = { 
      Visitor_c.default_visitor_c_s with 
      Visitor_c.kexpr_s = (fun (k, bigf) expb ->
	match expf expa expb binding with
	| None -> (* failed *) k expb
	| Some (x, expb) -> expb);
    }
    in
    Some (expa, Visitor_c.vk_node_s bigf node)



  (***************************************************************************)
  (* Tokens *) 
  (***************************************************************************)
  (* todo: check not already tagged ? *)
  let tokenf ia ib = fun binding -> 
    let (_s1,_i,mck) = ia in

    let pos = Ast_c.get_pos_of_info ib in

    let (s2, cocciinforef) = ib in
    let (oldmcode, _oldenv) = (*!*)cocciinforef in

    (match mck with
    | Ast_cocci.PLUS -> raise Impossible
    | Ast_cocci.CONTEXT (Some (i1,i2),_) 
    | Ast_cocci.MINUS   (Some (i1,i2),_) -> 
        if pos <= i2 && pos >= i1
        then
          (match (oldmcode,mck) with
          | (Ast_cocci.CONTEXT(_,Ast_cocci.NOTHING), _)
          | (_, Ast_cocci.CONTEXT(_,Ast_cocci.NOTHING)) ->

              let cocciinforef = (mck, binding) in
              (* cocciinforef := (mck, binding); *)
              Some (ia, (s2, cocciinforef)) (* useless now *)
                

          | _ -> failwith "already tagged"
          )
        else 
          fail binding
    | _ -> failwith "wierd: dont have position info for the mcodekind"      
    )

  (***************************************************************************)
  (* Environment *) 
  (***************************************************************************)

  let envf _inherited (s, value) = fun env -> 
    try Some (s, List.assoc s env)
    with Not_found -> 
      pr2 ("Don't find value for metavariable " ^ s ^ " in the environment");
      None


  (***************************************************************************)
  (* Misc *) 
  (***************************************************************************)

(*
  module D = Distribute_mcodekind

  type 'a tdistr = 'a D.distributer

  let distrf distrop mck x   = fun binding -> 
    Some (D.distribute_mck mck distrop x binding)
      
  let distrf_e = D.distribute_mck_e
*)
   
end


module TRANS  = Cocci_vs_c_3.COCCI_VS_C (XTRANS)


let (transform2: Lib_engine.transformation_info -> F.cflow -> F.cflow) = 
 fun xs cflow -> 
  (* find the node, transform, update the node,  and iter for all elements *)

   xs +> List.fold_left (fun acc (nodei, binding, rule_elem) -> 
      (* subtil: not cflow#nodes but acc#nodes *)
      let node  = acc#nodes#assoc nodei in 

      if !Flag_engine.show_misc 
      then pr2 "transform one node";

      let node' = TRANS.rule_elem_node rule_elem node binding in

      match node' with
      | None -> raise Impossible
      | Some (_sp, node') -> 

          (* assert that have done something. But with metaruleElem sometimes 
             dont modify fake nodes. So special case before on Fake nodes. *)
          (match F.unwrap node with
          | F.Enter | F.Exit | F.ErrorExit
          | F.EndStatement _ | F.CaseNode _        
          | F.Fake
          | F.TrueNode | F.FalseNode | F.AfterNode | F.FallThroughNode 
              -> ()
          | _ -> () (* assert (not (node =*= node')); *)
          );
          
          acc#replace_node (nodei, node')
     ) cflow

let transform a b = 
  Common.profile_code "Transformation2.transform(proto)?" 
    (fun () -> transform2 a b)

