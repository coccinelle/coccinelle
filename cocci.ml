open Common open Commonop

(* --------------------------------------------------------------------- *)
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

let print_xxxxxxxxxxxxxxxxx () = 
  pr2 "-----------------------------------------------------------------------"



let full_engine cfile coccifile_and_iso_or_ctl = 
  print_xxxxxxxxxxxxxxxxx ();
  pr2 ("processing C file: " ^ cfile);
  print_xxxxxxxxxxxxxxxxx ();
  command2 ("cat " ^ cfile);
  
  let astc     = cprogram_from_file cfile in
  
  let (ctl, error_words, used_after_list) = 
    (match coccifile_and_iso_or_ctl with
    | Left (coccifile, isofile) -> 

        print_xxxxxxxxxxxxxxxxx ();
        pr2 ("processing semantic patch file: " ^ coccifile);
        isofile +> do_option (fun s -> pr2 ("with isos from: " ^ s));
        print_xxxxxxxxxxxxxxxxx ();
        command2 ("cat " ^ coccifile);
        pr2 "";
  
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

        if List.length ctls <> 1 
        then failwith "I handle cocci patch with only one region";

        let ctl = one_ctl ctls in
	let used_after_list = List.hd used_after_lists in (* drop this! *)

        if !Flag.show_ctl then
          begin
            Ctltotex.totex "/tmp/__cocci_ctl.tex" astcocci ctls;
            command2 ("cd /tmp; latex __cocci_ctl.tex; " ^
                      "dvips __cocci_ctl.dvi -o __cocci_ctl.ps;" ^
                      "gv __cocci_ctl.ps &");
          end;

        (ctl, all_error_words, used_after_list)
    | Right ctl -> (ctl, [], [])
    )
  in

  print_xxxxxxxxxxxxxxxxx();
  pr2 "ctl";
  print_xxxxxxxxxxxxxxxxx();
  Pretty_print_engine.pp_ctlcocci_no_mcodekind !Flag.inline_let_ctl ctl;
  Format.print_newline();

  let (program, _stat) = astc in
  begin
    program +> List.map (fun (e, (filename, (pos1, pos2), s, il)) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, c,_) as def) -> 
          (* Cos caml regexp dont like \n ... *)
          let str = Str.global_replace (Str.regexp "\n") " " s in 
          (* Call the engine algorithms only if have found a flag word. *)
          if not (!Flag.process_only_when_error_words) || 
              error_words +> List.exists (fun error -> str =~ (".*" ^ error)) 
          then
            begin
              if !Flag.process_only_when_error_words 
              then pr2 "found error word: ";
              
              let flow = Control_flow_c.ast_to_control_flow def in
              
              (try Control_flow_c.deadcode_detection flow
              with Control_flow_c.DeadCode Some info -> 
                pr2 "PBBBBBBBBBBBBBBBBBB";
                pr2 (Common.error_message filename ("", info.charpos));
                pr2 ("at least 1 deadcode detected (there may be more)," ^
                     "but I continue")
              );

              (* remove some fake nodes *)
              let fixed_flow = Ctlcocci_integration.fix_flow_ctl flow in

              if !Flag.show_flow 
              then print_flow fixed_flow;

              let model_ctl  =
		Ctlcocci_integration.model_for_ctl flow in
              let (trans_info,_used_after_env) =
		Ctlcocci_integration.mysat model_ctl ctl used_after_list in
              let trans_info' = 
                Ctlcocci_integration.satbis_to_trans_info trans_info 
              in

              print_xxxxxxxxxxxxxxxxx();
              pr2 "transformation' info returned:";
              print_xxxxxxxxxxxxxxxxx();
              Pretty_print_engine.pp_transformation_info trans_info';
              Format.print_newline();



              let flow' = Transformation.transform trans_info' flow  in
              let def' = Control_flow_c.control_flow_to_ast flow' in
              (Ast_c.Definition def', Unparse_c.PPnormal)
            end
          else 
           (Ast_c.Definition def, Unparse_c.PPviatok il)
      | x -> 
          (x, Unparse_c.PPviatok il)
     )
    +> Unparse_c.pp_program cfile;

    Common.command2 ("diff -u " ^ cfile ^ " /tmp/output.c");
  end
