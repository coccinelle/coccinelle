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
let sp_from_file file iso    = Parse_cocci.process_for_ctl file iso false
let spbis_from_file file iso = Parse_cocci.process file iso false

let (rule_elem_from_string: string -> filename option -> Ast_cocci.rule_elem) = 
 fun s iso -> 
  begin
    write_file "/tmp/__cocci.cocci" (s);
    let rule_with_metavars_list = spbis_from_file "/tmp/__cocci.cocci" iso in
    let stmt =
      rule_with_metavars_list +> List.hd +> snd +> List.hd +> (function x ->
	match Ast_cocci.unwrap x with
	| Ast_cocci.CODE stmt_dots -> Ast_cocci.undots stmt_dots +> List.hd
	| _ -> raise Not_found) in
    match Ast_cocci.unwrap stmt with
      Ast_cocci.Atomic(re) -> re
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
               pr2 ("deadcode detected: " ^ (Common.error_message stat.Parse_c.filename ("", info.charpos) )); 
               None
          )
          
    | _ -> None
   )

let one_flow flows = List.hd flows

let print_flow flow = Ograph_extended.print_ograph_extended flow

(* --------------------------------------------------------------------- *)
let ctls = List.map Asttoctl.asttoctl
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

  let (ctl, error_words) = 
    (match coccifile_and_iso_or_ctl with
    | Left (coccifile, isofile) -> 
        print_xxxxxxxxxxxxxxxxx ();
        pr2 ("processing semantic patch file: " ^ coccifile);
	(match isofile with
	  Some isofile -> pr2 ("with isos from: " ^ isofile)
	| None -> ());
        let astcocci = spbis_from_file coccifile isofile in
        print_xxxxxxxxxxxxxxxxx ();
        command2 ("cat " ^ coccifile);
        pr2 "";
  
        let rule_with_metavars_list = astcocci in

        (* extract_all_error_words *)
        let (all_error_words: string list) = 
          rule_with_metavars_list +> List.hd +> snd +> (fun xs -> 
            let res = ref [] in
            xs +> List.iter (function x ->
	      match Ast_cocci.unwrap x with
                Ast_cocci.ERRORWORDS es -> 
                  es +> List.iter (fun e -> 
                    (match Ast_cocci.unwrap e with
                    | Ast_cocci.Ident id ->
			(match Ast_cocci.unwrap id with 
			  Ast_cocci.Id (s,_,_) -> push2 s res
			| _ -> pr2 "warning: does not support complex error words")
                    | _ -> pr2 "warning: does not support complex error words"
                    )
                                  );
              | _ -> ()
                            );
            List.rev !res
                                                       ) 
        in

        let sp = sp_from_file coccifile isofile in
        let ctls = (ctls sp) in
        if List.length ctls <> 1 
        then failwith "I handle cocci patch with only one region for the moment";
        let ctl = one_ctl ctls in

        if !Flag.show_ctl then
          begin
            Ctltotex.totex "/tmp/__cocci_ctl.tex" sp ctls;
            command2 "cd /tmp; latex __cocci_ctl.tex; dvips __cocci_ctl.dvi -o __cocci_ctl.ps; gv __cocci_ctl.ps &";
          end;

        ctl, all_error_words
    | Right ctl -> 
        ctl, []
    )
  in

  print_xxxxxxxxxxxxxxxxx();
  pr2 "ctl";
  print_xxxxxxxxxxxxxxxxx();
  Ctlcocci_integration.pp_ctlcocci_no_mcodekind ctl;
  Format.print_newline();


  print_xxxxxxxxxxxxxxxxx();
  let (program, _stat) = astc in
  begin
   program 
    +> List.map (fun (e, (filename, (pos1, pos2), s, il)) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, c,_) as def) -> 

          (* call the engine algorithms only if have found a flag word *)
          let str = Str.global_replace (Str.regexp "\n") " " s in (* cos caml regexp dont like \n ... *)
          if not (!Flag.process_only_when_error_words) || 
            error_words +> List.exists (fun error -> str =~ (".*" ^ error)) 
          then
              let _ = if !Flag.process_only_when_error_words then pr2 "found error word: " in
              let flow = Control_flow_c.ast_to_control_flow def in
              let _ =
                try Control_flow_c.deadcode_detection flow
                with Control_flow_c.DeadCode Some info -> 
                  pr2 "PBBBBBBBBBBBBBBBBBB";
                  pr2 (Common.error_message filename ("", info.charpos));
                  pr2 "at least 1 deadcode detected (there may be more), but I continue"
              in
          
              if !Flag.show_flow then print_flow  flow;
              let model_ctl  = Ctlcocci_integration.model_for_ctl flow in
              (*
              pr2 "calling sat_noclean";
              let _trans_info_noclean = Ctlcocci_integration.mysat_noclean model_ctl ctl in
               *)
              pr2 "calling sat";
              let trans_info = Ctlcocci_integration.mysat model_ctl ctl in
              pr2 "ending sat";
              (* pr2 (Dumper.dump trans_info); *)


              let trans_info' = Ctlcocci_integration.satbis_to_trans_info trans_info in

              pr2 "transformation' info returned:";
              trans_info' +> List.iter (fun (i, subst, pred) -> 
                Format.print_string ("transform state:" ^ (i_to_s i));
                Format.print_string " with binding ";
                Pretty_print_c.pp_binding subst;
                Format.force_newline();
                                      );
              Format.print_string "\n"; Format.print_flush();


              let flow' = Transformation.transform trans_info' flow  in
              let def' = flow' +> Control_flow_c.control_flow_to_ast in
              (Ast_c.Definition def', Unparse_c.PPnormal)
          else 
           (Ast_c.Definition def, Unparse_c.PPviatok il)
      | x -> 
          (x, Unparse_c.PPviatok il)
     )
    +> Unparse_c.pp_program cfile;
    Common.command2 ("diff -u " ^ cfile ^ " /tmp/output.c")
  end
