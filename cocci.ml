open Common open Commonop


let c  file = Parse_c.parse_print_error_heuristic file

let (cstatement_from_string: string -> Ast_c.statement) = fun s ->
  begin
    write_file "/tmp/__cocci.c" ("void main() { \n" ^ s ^ "\n}");
    let (program, _stat) = c "/tmp/__cocci.c" in
    program +> map_filter (fun (e,_) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, c,_)) -> 
          (match c with
          | [Right st] -> Some st
          | _ -> None
          )
      | _ -> None
                          )
      +> List.hd
    
  end

let (cexpression_from_string: string -> Ast_c.expression) = fun s ->
  begin
    write_file "/tmp/__cocci.c" ("void main() { \n" ^ s ^ ";\n}");
    let (program, _stat) = c "/tmp/__cocci.c" in
    program +> map_filter (fun (e,_) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, c,_)) -> 
          (match c with
          | [Right (Ast_c.ExprStatement (Some e),ii)] -> Some e
          | _ -> None
          )
      | _ -> None
                          )
      +> List.hd
    
  end
  

let sp file    = Parse_cocci.process_for_ctl file false
let spbis file = Parse_cocci.process file false

let (rule_elem_from_string: string -> Ast_cocci.rule_elem) = fun s -> 
  begin
    write_file "/tmp/__cocci.cocci" (s);
    let rule_with_metavars_list = spbis "/tmp/__cocci.cocci" in
    rule_with_metavars_list +> List.hd +> snd +> List.hd +> (function
      | Ast_cocci.CODE rule_elem_dots -> Ast_cocci.undots rule_elem_dots +> List.hd
      | _ -> raise Not_found
    )
  end


let ctls file = 
  sp file
  +> List.split 
  +> (fun (ast_lists,ast0_lists) -> 
        ast0_lists +> List.map Ast0toctl.ast0toctl
     )
let one_ctl file = List.hd (List.hd (ctls file))

let flows file = 
  let (program, _stat) = c file in
  program +> map_filter (fun (e,_) -> 
    match e with
    | Ast_c.Definition ((funcs, _, _, c,_) as def) -> 
        pr2 funcs;
        (try 
          Some (Control_flow_c.ast_to_control_flow def)
        with 
        | Control_flow_c.DeadCode None      -> pr2 "deadcode detected, but cant trace back the place"; None
        | Control_flow_c.DeadCode Some info -> pr2 ("deadcode detected: " ^ (Common.error_message file ("", info.charpos) )); None
        )
          
    | _ -> None
   )
let one_flow file = List.hd (flows file)

let print_flow file =
  flows file +> List.iter Ograph_extended.print_ograph_extended




let test_unparser cfile = 
  one_flow cfile
    +> Control_flow_c.control_flow_to_ast
    +> (fun def -> Unparse_c.pp_program "../1.c" [Ast_c.Definition def, Unparse_c.PPnormal])
    +> (fun () -> cat "/tmp/output.c")


let test_pattern statement_str rule_elem_str = 
  let statement = cstatement_from_string statement_str in
  let rule_elem = rule_elem_from_string rule_elem_str in
  Pattern.match_re_node 
    rule_elem   (Control_flow_c.Statement statement, "str")
    (Ast_c.empty_metavars_binding)



let test_cocci cfile coccifile = 

  let ctl  = one_ctl coccifile in
  let flow = one_flow cfile in
  

  let model_ctl  = Ctlcocci_integration.model_for_ctl flow ctl in
  let _labels = (Ctlcocci_integration.labels_for_ctl (flow#nodes#tolist) (Ctlcocci_integration.ctl_get_all_predicates ctl)) in

  Ctl_engine.sat model_ctl  ctl

      

  


