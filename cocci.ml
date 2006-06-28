open Common open Commonop

(*
 This file is a kind of driver. It gathers all the important functions 
 from Coccinelle in one place. The different entities of the Coccinelle system:
  - files

  - astc
  - astcocci

  - flow (contain nodes)
  - ctl  (contain rule_elem)

  There are functions to transform one in another.

*)

(* --------------------------------------------------------------------- *)
let cprogram_from_file  file = Parse_c.parse_print_error_heuristic file

let (cstatement_from_string: string -> Ast_c.statement) = fun s ->
  begin
    write_file "/tmp/__cocci.c" ("void main() { \n" ^ s ^ "\n}");
    let (program, _stat) = cprogram_from_file "/tmp/__cocci.c" in
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
    let (program, _stat) = cprogram_from_file "/tmp/__cocci.c" in
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
  

(* --------------------------------------------------------------------- *)
let sp_from_file file    = Parse_cocci.process_for_ctl file None false
let spbis_from_file file = Parse_cocci.process file None false

let (rule_elem_from_string: string -> Ast_cocci.rule_elem) = fun s -> 
  begin
    write_file "/tmp/__cocci.cocci" (s);
    let rule_with_metavars_list = spbis_from_file "/tmp/__cocci.cocci" in
    let stmt =
      rule_with_metavars_list +> List.hd +> snd +> List.hd +> (function
	| Ast_cocci.CODE stmt_dots -> Ast_cocci.undots stmt_dots +> List.hd
	| _ -> raise Not_found) in
    match stmt with
      Ast_cocci.Atomic(re) -> re
    | _ -> failwith "only atomic patterns allowed"
  end





(* --------------------------------------------------------------------- *)
let flows astc = 
  let (program, stat) = astc in
  program +> map_filter (fun (e,_) -> 
    match e with
    | Ast_c.Definition ((funcs, _, _, c,_) as def) -> 
        (try 
          Some (Control_flow_c.ast_to_control_flow def)
        with 
        | Control_flow_c.DeadCode None      -> pr2 "deadcode detected, but cant trace back the place"; None
        | Control_flow_c.DeadCode Some info -> pr2 ("deadcode detected: " ^ (Common.error_message stat.Parse_c.filename ("", info.charpos) )); None
        )
          
    | _ -> None
   )

let one_flow flows = List.hd flows

let print_flow flow = Ograph_extended.print_ograph_extended flow

(* --------------------------------------------------------------------- *)
let ctls = List.map Asttoctl.asttoctl
let one_ctl ctls = List.hd (List.hd ctls)

(* --------------------------------------------------------------------- *)

