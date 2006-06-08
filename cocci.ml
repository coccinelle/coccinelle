open Common open Commonop


let c  file = Parse_c.parse_print_error_heuristic file

let sp file    = Parse_cocci.process_for_ctl file false
let spbis file = Parse_cocci.process file false

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



let test_cocci cfile coccifile = 

  let ctl  = one_ctl coccifile in
  let flow = one_flow cfile in
  

  let model_ctl  = Ctlcocci_integration.model_for_ctl flow ctl in
  let _labels = (Ctlcocci_integration.labels_for_ctl (flow#nodes#tolist) (Ctlcocci_integration.ctl_get_all_predicates ctl)) in

  Ctl_engine.sat model_ctl  ctl

      

  


