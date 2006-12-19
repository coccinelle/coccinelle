open Common open Commonop

module F = Control_flow_c

module XTRANS = struct

  (***************************************************************************)
  (* Combinators *) 
  (***************************************************************************)
  (*
   * version0: 
   *  type ('a, 'b) transformer = 
   *    'a -> 'b -> Lib_engine.metavars_binding -> 'b
   *  exception NoMatch 
   * 
   * version1:
   *   type ('a, 'b) transformer = 
   *    'a -> 'b -> Lib_engine.metavars_binding -> 'b option
   * use an exception monad 
   *)

  type tin = Lib_engine.metavars_binding
  type 'b tout = 'b option


  let ((>>=):(tin -> 'c tout) -> ('c -> (tin -> 'b tout)) -> (tin -> 'b tout))
   = fun  m f -> fun tin -> 
     match m tin with
     | None -> None
     | Some x -> f x tin

  let (return: 'b -> tin -> 'b tout) = fun x -> fun tin -> 
    Some x

  let (fail: tin -> 'b tout) = fun tin -> 
    None

  let (>||>) m1 m2 = fun tin -> 
    match m1 tin with
    | None -> m2 tin
    | Some x -> Some x


  (***************************************************************************)
  (* Tokens *) 
  (***************************************************************************)
  (* todo: check not already tagged ? *)
  let tokenf_one ia ib = fun binding -> 
    let (s1,_,x) = ia in
    let (s2, (oldmcode, oldenv)) = ib in
    Some (s2, (x, binding))
      

  (***************************************************************************)
  (* Environment *) 
  (***************************************************************************)

  let envf _inherited (s, value) = fun env -> 
    try Some (List.assoc s env)
    with Not_found -> 
      pr2 ("Don't find value for metavariable " ^ s ^ " in the environment");
      None

  (***************************************************************************)
  (* Misc *) 
  (***************************************************************************)

  module D = Distribute_mcodekind

  type 'a tdistr = 'a D.distributer

  let distrf distrop mck x   = fun binding -> 
    Some (D.distribute_mck mck distrop x binding)
      
  let distrf_e = D.distribute_mck_e
    
  let distrf_node = D.distribute_mck_node
    
  let distrf_type = D.distribute_mck_type

end


module TRANS  = Cocci_vs_c.COCCI_VS_C (XTRANS)


let (transform2: Lib_engine.transformation_info -> F.cflow -> F.cflow) = 
 fun xs cflow -> 
  (* find the node, transform, update the node,  and iter for all elements *)

   xs +> List.fold_left (fun acc (nodei, binding, rule_elem) -> 
      (* subtil: not cflow#nodes but acc#nodes *)
      let node  = acc#nodes#assoc nodei in 

      if !Flag_engine.show_misc then pr2 "transform one node";
      let node' = TRANS.rule_elem_node rule_elem node binding in
      match node' with
      | None -> raise Impossible
      | Some node' -> 

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

