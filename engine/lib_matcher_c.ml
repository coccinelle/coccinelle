open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type protocol_match = 
  | MatchPos of Ograph_extended.nodei
  | MatchNeg of Ograph_extended.nodei
  | NoMatch 
  (* could generate exn instead, but in many cases as for my acomment gui
   * I still want to print the match for the other elements, so one failure
   * should not stop everything
   *)
  | MatchProblem of string


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Specific finder wrappers *)
(*****************************************************************************)
let (find_nodes_satisfying_pattern: 
    Control_flow_c.cflow -> Ast_cocci.rule_elem -> Ograph_extended.nodei list)= 
 fun flow pattern -> 

  let nodes = flow#nodes in
  let nodes = nodes#tolist in
  nodes +> Common.map_filter (fun (nodei, node) -> 
    let res = 
      Pattern_c.match_re_node [] (* dropped isos *)
        pattern node 
        [] 
    in
    if List.length res > 0
    then Some nodei
    else None
  )


let (find_nodes_containing_expr: 
    Control_flow_c.cflow -> Ast_c.expression -> Ograph_extended.nodei list)= 
 fun flow expr -> 

  let expr = Lib_parsing_c.real_al_expr expr in

  let nodes = flow#nodes in
  let nodes = nodes#tolist in
  nodes +> Common.map_filter (fun (nodei, node) -> 
    let node = Lib_parsing_c.real_al_node node in 

    let found = ref false in 
    
    Visitor_c.vk_node { Visitor_c.default_visitor_c with
      Visitor_c.kexpr = (fun (k, bigf) e2 -> 
        if e2 =*= expr
        then found := true
        else k e2
      );
    } node;

    if !found
    then Some nodei
    else None
  )



(*****************************************************************************)
(* Main entries *)
(*****************************************************************************)

(*
 * 
 * todo: Check for all path upwards ?
 *)

let (find_nodes_upward_satisfying_protocol: 
  Ograph_extended.nodei -> Control_flow_c.cflow -> 
  Ast_cocci.rule_elem * Ast_cocci.rule_elem -> 
  protocol_match
  ) = 
 fun nodei flow (pattern1, pattern2) ->

   let already_done = ref [nodei] in
   let found = ref [] in

   let rec aux nodei = 
     let pred = 
       List.map fst ((flow#predecessors nodei)#tolist)
     in
     pred +> List.iter (fun nodei2 -> 
       if List.mem nodei2 !already_done
       then ()
       else begin
         Common.push2 nodei2 already_done;

         let node2 = flow#nodes#assoc nodei2 in

         let res1 = 
           Pattern_c.match_re_node [] 
             pattern1 node2
             [] 
         in
         let res2 = 
           Pattern_c.match_re_node [] 
             pattern2 node2
             [] 
         in
         match List.length res1 > 0, List.length res2 > 0 with
         | true, false -> 
             Common.push2 (MatchPos nodei2) found
         | false, true -> 
             Common.push2 (MatchNeg nodei2) found
         | true, true -> 
             failwith "wierd, node match both rule_elem"
         | false, false -> 
             aux nodei2
       end
     );
   in
   aux nodei;
   (match !found with
   | [] -> NoMatch
   | [x] -> x
   | x::y::ys -> 
       failwith "multiple found";
   )




