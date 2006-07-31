open Common open Commonop

(* --------------------------------------------------------------------- *)
(*

 This file is a kind of driver. It gathers all the important functions 
 from Coccinelle in one place. The different entities of the Coccinelle system
 are :
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
      | Ast_c.Definition ((funcs, _, _, compound,_)) -> 
          (match compound with
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
      | Ast_c.Definition ((funcs, _, _, compound,_)) -> 
          (match compound with
          | [Right (Ast_c.ExprStatement (Some e),ii)] -> Some e
          | _ -> None
          )
      | _ -> None
      )
      +> List.hd
    
  end
  

(* --------------------------------------------------------------------- *)
let sp_from_file file iso    = Parse_cocci.process file iso false

let (rule_elem_from_string: string -> filename option -> Ast_cocci.rule_elem) = 
 fun s iso -> 
  begin
    write_file "/tmp/__cocci.cocci" (s);
    let (astcocci, _,_,_) = sp_from_file "/tmp/__cocci.cocci" iso in
    let stmt =
      astcocci +> List.hd +> List.hd +> (function x ->
	match Ast_cocci.unwrap x with
	| Ast_cocci.CODE stmt_dots -> Ast_cocci.undots stmt_dots +> List.hd
	| _ -> raise Not_found)
    in
    match Ast_cocci.unwrap stmt with
    | Ast_cocci.Atomic(re) -> re
    | _ -> failwith "only atomic patterns allowed"
  end

(* --------------------------------------------------------------------- *)
let flows astc = 
  let (program, stat) = astc in
  program +> map_filter (fun (e,_) -> 
    match e with
    | Ast_c.Definition ((funcs, _, _, c,_) as def) -> 
        let flow = Control_flow_c.ast_to_control_flow def in
        (try begin Control_flow_c.deadcode_detection flow; Some flow end
        with
           | Control_flow_c.DeadCode None      -> 
               pr2 "deadcode detected, but cant trace back the place"; 
               None
           | Control_flow_c.DeadCode Some info -> 
               pr2 ("deadcode detected: " ^ 
                    (Common.error_message 
                       stat.Parse_c.filename ("", info.charpos) )); 
               None
          )
          
    | _ -> None
   )

let one_flow flows = List.hd flows

let print_flow flow = Ograph_extended.print_ograph_extended flow

(* --------------------------------------------------------------------- *)
let ctls ast ft ex ua =
  List.map2
    (function (ast,ft) -> function (ex,ua) -> Asttoctl.asttoctl ast ft ex ua)
    (List.combine ast ft) (List.combine ex ua)
let one_ctl ctls = List.hd (List.hd ctls)

(* --------------------------------------------------------------------- *)

let full_engine ?(print_input_file=true) cfile coccifile_and_iso_or_ctl = 

  if print_input_file then begin
    Common.print_xxxxxxxxxxxxxxxxx ();
    pr2 ("processing C file: " ^ cfile);
    Common.print_xxxxxxxxxxxxxxxxx ();
    Common.command2 ("cat " ^ cfile);
  end;

  
  let (ctls, error_words) = 
    (match coccifile_and_iso_or_ctl with
    | Left (coccifile, isofile) -> 

        if print_input_file then begin
          print_xxxxxxxxxxxxxxxxx ();
          pr2 ("processing semantic patch file: " ^ coccifile);
          isofile +> do_option (fun s -> pr2 ("with isos from: " ^ s));
          print_xxxxxxxxxxxxxxxxx ();
          command2 ("cat " ^ coccifile);
          pr2 "";
        end;
  
        let (astcocci,free_tables,used_after_lists,extenders) =
	  sp_from_file coccifile isofile in

        (* extract_all_error_words *)
        let (all_error_words: string list) = 
          astcocci +> List.hd +> (fun xs -> 
            let res = ref [] in
            xs +> List.iter (function x ->
	      match Ast_cocci.unwrap x with
                Ast_cocci.ERRORWORDS es -> 
                  es +> List.iter (fun e -> 
                    (match Ast_cocci.unwrap e with
                    | Ast_cocci.Ident id ->
			(match Ast_cocci.unwrap id with 
			  Ast_cocci.Id (s,_,_) -> push2 s res
			| _ ->
                            pr2 "warning: does not support complex error words")
                    | _ -> pr2 "warning: does not support complex error words"
                    )
                                  );
              | _ -> ()
                            );
            List.rev !res
                                                       ) 
        in

        let ctls = (ctls astcocci free_tables extenders used_after_lists) in

        if !Flag.show_ctl then
          begin
            Ctltotex.totex "/tmp/__cocci_ctl.tex" astcocci ctls;
            command2 ("cd /tmp; latex __cocci_ctl.tex; " ^
                      "dvips __cocci_ctl.dvi -o __cocci_ctl.ps;" ^
                      "gv __cocci_ctl.ps &");
          end;

        (zip ctls used_after_lists, all_error_words)
    | Right ctl ->([[ctl], []]), []
    )
  in


  command2("cp " ^ cfile ^ " /tmp/input.c");

  let _current_bindings = ref [Ast_c.emptyMetavarsBinding] in

  (* 1: iter ctl *)  
  ctls +> List.iter (fun  (ctl_toplevel_list, used_after_list) -> 
    
    if List.length ctl_toplevel_list <> 1 
    then failwith "I handle only one toplevel element per region";
    
    let ctl = List.hd ctl_toplevel_list in
    
    if print_input_file then begin
      print_xxxxxxxxxxxxxxxxx();
      pr2 "ctl";
      print_xxxxxxxxxxxxxxxxx();
      Pretty_print_engine.pp_ctlcocci 
        !Flag.show_mcodekind_in_ctl !Flag.inline_let_ctl ctl;
      Format.print_newline();
    end;
    
    (* 2: iter binding *)
    let lastround_bindings = !_current_bindings in
    _current_bindings := [];
    lastround_bindings +> List.iter (fun binding -> 

      let (cprogram, _stat)   = cprogram_from_file "/tmp/input.c" in
     
      (* 3: iter function *)
      cprogram +> List.map (fun (e, (filename, (pos1, pos2), s, il)) -> 
        match e with
        | Ast_c.Definition ((funcs, _, _, c,_) as def) -> 
	    Printf.printf "starting function %s\n" funcs; flush stdout;
            
            (* Cos caml regexp dont like \n ... *)
            let str = Str.global_replace (Str.regexp "\n") " " s in 
            (* Call the engine algorithms only if have found a flag word. *)
            if not (!Flag.process_only_when_error_words) 
              || error_words +> List.exists (fun error -> str =~ (".*" ^ error))
            then
              begin
                let flow = Control_flow_c.ast_to_control_flow def in
                
                begin
                  try Control_flow_c.deadcode_detection flow
                  with Control_flow_c.DeadCode Some info -> 
                    pr2 "PBBBBBBBBBBBBBBBBBB";
                    pr2 (Common.error_message filename ("", info.charpos));
                    pr2 ("at least 1 deadcode detected (there may be more)," ^
                         "but I continue")
                end;
                  
                (* remove some fake nodes *)
                let fixed_flow = Ctlcocci_integration.fix_flow_ctl flow in
                
                if !Flag.show_flow 
                then print_flow flow;
                if !Flag.show_fixed_flow 
                then print_flow fixed_flow;

                let current_binding = binding in
                let current_binding2 = 
                  Ctlcocci_integration.metavars_binding_to_binding2 binding
                in

                let model_ctl  =
		  Ctlcocci_integration.model_for_ctl flow current_binding in
		let satres =
		  Ctlcocci_integration.mysat model_ctl ctl 
                    (used_after_list, current_binding2) in
		match satres with
		| Some (trans_info2,used_after_env) ->
                    let trans_info = 
                      Ctlcocci_integration.satbis_to_trans_info trans_info2
                    in

                    print_xxxxxxxxxxxxxxxxx();
                    pr2 "transformation info returned:";
                    print_xxxxxxxxxxxxxxxxx();
                    Pretty_print_engine.pp_transformation_info trans_info;
                    Format.print_newline();
                    
                    if trans_info <> []
                    then 
                      begin

                        let flow' = Transformation.transform trans_info flow  in
                        let def' = Control_flow_c.control_flow_to_ast flow' in

                        (* TODO some union *)
                        _current_bindings := 
                          Ctlcocci_integration.metavars_binding2_to_binding
                            used_after_env 
                          :: !_current_bindings;

                        (Ast_c.Definition def', Unparse_c.PPnormal)
                      end
                    else 
                      (Ast_c.Definition def, Unparse_c.PPviatok il)
		| None -> 
                    (Ast_c.Definition def, Unparse_c.PPviatok il)
              end
            else 
              (Ast_c.Definition def, Unparse_c.PPviatok il)
        | x -> 
            (x, Unparse_c.PPviatok il)
        ) (* end 3: iter function *)
        +> Unparse_c.pp_program "/tmp/input.c";
      command2("cp /tmp/output.c /tmp/input.c");    

      ) (* end 2: iter bindings *)
  ); (* end 1: iter ctl *)
  Common.command2 ("diff -u  --strip-trailing-cr -b -B " ^ 
                   cfile ^ " /tmp/output.c")

