open Common open Commonop

module CCI = Ctlcocci_integration
(* --------------------------------------------------------------------- *)
(*

 This file is a kind of driver. It gathers all the important functions 
 from Coccinelle in one place. The different entities of the Coccinelle 
 system are:
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
    program +> find_some (fun (e,_) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, [st]),_) -> Some st
      | _ -> None
      )
  end

let (cexpression_from_string: string -> Ast_c.expression) = fun s ->
  begin
    write_file "/tmp/__cocci.c" ("void main() { \n" ^ s ^ ";\n}");
    let (program, _stat) = cprogram_from_file "/tmp/__cocci.c" in
    program +> find_some (fun (e,_) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, compound),_) -> 
          (match compound with
          | [(Ast_c.ExprStatement (Some e),ii)] -> Some e
          | _ -> None
          )
      | _ -> None
      )
  end
  

(* --------------------------------------------------------------------- *)
let sp_from_file file iso    = Parse_cocci.process file iso false

let (rule_elem_from_string: string -> filename option -> Ast_cocci.rule_elem) =
 fun s iso -> 
  begin
    write_file "/tmp/__cocci.cocci" (s);
    let (astcocci, _,_) = sp_from_file "/tmp/__cocci.cocci" iso in
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
    | Ast_c.Definition (((funcs, _, _, c),_) as def) -> 
        let flow = Ast_to_flow.ast_to_control_flow def in
        (try begin Ast_to_flow.deadcode_detection flow; Some flow end
        with
           | Ast_to_flow.DeadCode None      -> 
               pr2 "deadcode detected, but cant trace back the place"; 
               None
           | Ast_to_flow.DeadCode Some info -> 
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
let ctls ast ua  = List.map2 Asttoctl.asttoctl ast ua
let one_ctl ctls = List.hd (List.hd ctls)

(* --------------------------------------------------------------------- *)

exception NotWorthTrying

let full_engine cfile coccifile_and_iso_or_ctl = 

  try
  assert (lfile_exists cfile);

  if !Flag.show_c then begin
    Common.print_xxxxxxxxxxxxxxxxx ();
    pr2 ("processing C file: " ^ cfile);
    Common.print_xxxxxxxxxxxxxxxxx ();
    Common.command2 ("cat " ^ cfile);
  end;

  
  let (ctls, error_words) = 
    (match coccifile_and_iso_or_ctl with
    | Left (coccifile, isofile) -> 

        if !Flag.show_cocci then begin
          print_xxxxxxxxxxxxxxxxx ();
          pr2 ("processing semantic patch file: " ^ coccifile);
          isofile +> do_option (fun s -> pr2 ("with isos from: " ^ s));
          print_xxxxxxxxxxxxxxxxx ();
          command2 ("cat " ^ coccifile);
          pr2 "";
        end;
  
        let (astcocci,used_after_lists,tokens) = sp_from_file coccifile isofile
        in

	(match
	  Sys.command
	    (Printf.sprintf "egrep -q '(%s)' %s"
	       (String.concat "|" tokens) cfile) with
	| 0 -> (* success*) ()
	| _ -> (* failure *) pr2 "raised NotWorthTrying"; raise NotWorthTrying
        );

        (* extract_all_error_words *)
        let (all_error_words: string list) = 
          astcocci +> List.hd +> (fun xs -> 
            let res = ref [] in
            xs +> List.iter (fun x ->
	      match Ast_cocci.unwrap x with
              | Ast_cocci.ERRORWORDS es -> 
                  es +> List.iter (fun e -> 
                    (match Ast_cocci.unwrap e with
                    | Ast_cocci.Ident id ->
		      (match Ast_cocci.unwrap id with 
		      | Ast_cocci.Id (s,_,_) -> push2 s res
		      | _ -> pr2 "warning: does not support complex error words"
                      )
                    | _ -> pr2 "warning: does not support complex error words"
                    )
                  );
              | _ -> ()
            );
            List.rev !res
          ) 
        in

        let ctls = ctls astcocci used_after_lists in

        if !Flag.show_ctl_tex then begin
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
  let _hack_funheader = ref [] in

  (* 1: iter ctl *)  
  ctls +> List.iter (fun  (ctl_toplevel_list, used_after_list) -> 
    
   if List.length ctl_toplevel_list = 1 
   then begin
    
    let ctl = List.hd ctl_toplevel_list in
    
    (* 2: iter binding *)
    let lastround_bindings = !_current_bindings in
    _current_bindings := [];

    (* I have not to look at used_after_list to decide to restart from scratch.
     * I just need to look if the binding list is empty. Indeed, let's suppose
     * that a SP have 3 regions/rules. If we don't find a match for the 
     * first region, then if this first region does not bind metavariable
     * used after, that is if used_after_list is empty, then 
     * mysat, even if does not find a match, will return a Left, with
     * an empty transformation_info, and so current_binding will grow.
     * On the contrary if the first region must bind some metavariables used
     * after, and that we dont find any such region, then mysat will
     * returns lots of Right, and current_binding will not grow, and so
     * we will have an empty list of binding, and we will catch such a case.
     *)

    if null(lastround_bindings) then 
      begin 
        pr2 "Empty list of bindings, I start from scratch";
        _current_bindings := [Ast_c.emptyMetavarsBinding];
      end;

    if !Flag.show_ctl_text then begin
      print_xxxxxxxxxxxxxxxxx();
      pr2 "ctl";
      print_xxxxxxxxxxxxxxxxx();
      Pretty_print_engine.pp_ctlcocci 
        !Flag.show_mcodekind_in_ctl !Flag.inline_let_ctl ctl;
      Format.print_newline();
    end;
    
    lastround_bindings +> List.iter (fun binding -> 

      let (cprogram, _stat)  = cprogram_from_file "/tmp/input.c" in
     
      (* 3: iter function *)
      cprogram +> List.map (fun (e, (filename, pos, s, il)) -> 
        match e with
        | Ast_c.Definition (((funcs, _, _, c),_) as def) -> 
	    if !Flag.show_misc then pr2 ("starting function " ^ funcs);
            
            (* Cos caml regexp dont like \n ... *)
            let str = Str.global_replace (Str.regexp "\n") " " s in 

            (* Call the engine algorithms only if have found a flag word. *)
            if not (!Flag.process_only_when_error_words) ||
               error_words +> List.exists (fun errw -> str =~ (".*" ^ errw))
            then
              begin
                let flow = Ast_to_flow.ast_to_control_flow def in
                
                begin
                  try Ast_to_flow.deadcode_detection flow
                  with Ast_to_flow.DeadCode Some info -> 
                    pr2 "PBBBBBBBBBBBBBBBBBB";
                    pr2 (Common.error_message filename ("", info.charpos));
                    pr2 ("At least 1 DEADCODE detected (there may be more)," ^
                         "but I continue.");
                    pr2 "Maybe because of cpp #ifdef side effects.";
                      
                end;
                  
                (* remove some fake nodes *)
                let fixed_flow = CCI.fix_flow_ctl flow in
                
                if !Flag.show_flow              then print_flow fixed_flow;
                if !Flag.show_before_fixed_flow then print_flow flow;

		let old_loop_in_src_code = !Flag_ctl.loop_in_src_code in
		if not old_loop_in_src_code
		then
                  (Flag_ctl.loop_in_src_code := false;
                   def +> Visitor_c.visitor_def_k { Visitor_c.default_visitor_c
                    with Visitor_c.kstatement = (fun (k, bigf) stat -> 
                      match stat with 
                       | Ast_c.Iteration _, ii
                       | Ast_c.Jump (Ast_c.Goto _), ii
                           -> Flag_ctl.loop_in_src_code := true
                       | st -> k st
                             )
                     });

                let current_binding = binding in
                let current_binding2 = CCI.metavars_binding_to_binding2 binding
                in


                let model_ctl  = CCI.model_for_ctl flow current_binding in
		(* change this!!! *)
		let used_after_list =
		  List.fold_left Common.union_set [] used_after_list in
		let satres = 
                  CCI.mysat model_ctl ctl (used_after_list, current_binding2) 
                in

		Flag_ctl.loop_in_src_code := old_loop_in_src_code;
		match satres with
		| Left (trans_info2, returned_any_states, used_after_env) ->
                    let trans_info = CCI.satbis_to_trans_info trans_info2 in
                    if !Flag.show_transinfo then begin
                      print_xxxxxxxxxxxxxxxxx();
                      pr2 "transformation info returned:";
                      print_xxxxxxxxxxxxxxxxx();
                      Pretty_print_engine.pp_transformation_info trans_info;
                      Format.print_newline();
                    end;

                    (* Some union. julia say that because the binding is
                     * determined by the used_after_list, the items
                     * in the list are kind of sorted, so could
                     * optimise the union.
                     *)
                    _current_bindings := 
                      Common.insert_set 
                        (CCI.metavars_binding2_to_binding used_after_env)
                        !_current_bindings;

                    (* modify also the proto. *)
                    trans_info +> List.iter (fun (_nodei, binding, re) -> 
                      match re with
                      | Ast_cocci.FunHeader (a,b,c,d,e,f,g),info,fv,dots -> 
                          push2  (binding, ((a,b,c,d,e,f,g),info,fv,dots))
                            _hack_funheader
                      | _ -> ()
                      );
                    
                    if trans_info <> []
                    then 
                      (* I do the transformation on flow, not fixed_flow, 
                         because the flow_to_ast need my extra information. *)
                      let flow' = Transformation.transform trans_info flow in
                      let def' = Flow_to_ast.control_flow_to_ast flow' in
                      (Ast_c.Definition def', Unparse_c.PPnormal)
                    else 
                      (Ast_c.Definition def, Unparse_c.PPviatok il)
		| Right x -> 
                    pr2 ("Unable to find a value for " ^ x);
                    (Ast_c.Definition def, Unparse_c.PPviatok il)
              end
            else 
              (Ast_c.Definition def, Unparse_c.PPviatok il)


        | x -> 
            (x, Unparse_c.PPviatok il)
        ) (* end 3: iter function *)
        +> Unparse_c.pp_program "/tmp/input.c" "/tmp/output.c";
      command2("cp /tmp/output.c /tmp/input.c");    

      ) (* end 2: iter bindings *)
   end
   else begin
     (* todo: use current_binding
      * todo: merge with the case when List.length = 1
      *) 
     (* (binding *  (funcname * transformed) list) list *)
     let _current_bindings_multictl = ref [(Ast_c.emptyMetavarsBinding,[])] in
     let (cprogram, _stat)  = cprogram_from_file "/tmp/input.c" in
     
     let _compteur1 = ref 0 in 
     (* iter ctl_toplevel 1 bis *)

     
     let rec union_after already = function
       | [] -> []
       | x::xs -> 
           let u = Common.union_set x already in
           u::union_after u xs
     in
     let used_after_list2 = 
       union_after [] used_after_list  (*   *)
     in

     zip ctl_toplevel_list used_after_list2 +> List.iter 
      (fun (ctl, used_after_one_ctl) -> 
       let lastround_bindings_multi = List.rev !_current_bindings_multictl in
       _current_bindings_multictl := [];
       incr _compteur1;
       pr2 ("CTL part:" ^ i_to_s !_compteur1);
       let _compteur2 = ref 0 in 
       (* iter binding and already *)
       lastround_bindings_multi +> List.iter (fun (binding, already) -> 

         incr _compteur2;
         pr2 ("binding part:" ^ i_to_s !_compteur2);

         (* iter program *)
         cprogram +> List.iter (fun (e, (filename, pos, s, il)) -> 
           match e with
           | Ast_c.Definition (((funcs, _, _, c),_) as def) -> 
               if not (List.mem funcs (Common.keys already))
               then begin
                 let flow = Ast_to_flow.ast_to_control_flow def in
                 let _fixed_flow = CCI.fix_flow_ctl flow in
                 let current_binding = binding in
                 let current_binding2 = CCI.metavars_binding_to_binding2 current_binding in

                 pr2 "start binding = ";
                 Pretty_print_c.pp_binding current_binding;
                 Format.print_newline();

                 let model_ctl  = CCI.model_for_ctl flow current_binding in
		 let satres =
		   CCI.mysat model_ctl ctl
		     (used_after_one_ctl, current_binding2) in
		 match satres with
		 | Left (trans_info2, returned_any_states, used_after_env) ->
                     let trans_info = CCI.satbis_to_trans_info trans_info2 in
                     if !Flag.show_transinfo then begin
                       pr2 ("FOUND STUFF: in " ^ funcs);
                       Pretty_print_engine.pp_transformation_info trans_info;
                       Format.print_newline();
                     end;

                     if !_compteur1 = 3 && !_compteur2 = 1  && funcs = "main"
                     then 
                       pr2 "THIS ONE SHOULD MATCH";

                     let flow' = Transformation.transform trans_info flow in
                     let def' = Flow_to_ast.control_flow_to_ast flow' in
                     
                     if returned_any_states (* not (null trans_info) *)
                     then begin
                      (* when used_after is not good *)
                      (* let newbinding = snd3 (List.hd trans_info) in *)
                      let newbinding = 
                        (CCI.metavars_binding2_to_binding used_after_env)
                      in
                      push2 (newbinding, (funcs, def')::already)
                       _current_bindings_multictl;
                      end
                 | Right x -> 
                     pr2 ("Unable to find a value for " ^ x);
               end
                 
           | x -> 
               ()
          )
         )
       );
     (* assert no conflict. can concern different function ? *)
     assert (List.length !_current_bindings_multictl = 1);
     let (binding, list_func) = List.hd !_current_bindings_multictl in
     cprogram +> List.map (fun (e, (filename, pos, s, il)) -> 
       match e with
       | Ast_c.Definition (((funcs, _, _, c),_) as def) -> 
           if List.mem funcs (Common.keys list_func)
           then (Ast_c.Definition (List.assoc funcs list_func), 
                 Unparse_c.PPnormal)
           else (Ast_c.Definition def, Unparse_c.PPviatok il) 
             
       | x -> (x, Unparse_c.PPviatok il)
       )
        +> Unparse_c.pp_program "/tmp/input.c" "/tmp/output.c";
      command2("cp /tmp/output.c /tmp/input.c");    
     end

  ); (* end 1: iter ctl *)



  !_hack_funheader +>
  List.iter (fun ((binding, ((a,b,c,d,e,f,g),info,fv,dots))) -> 
   
      let (cprogram, _stat)  = cprogram_from_file "/tmp/input.c" in
      cprogram +> List.map (fun (ebis, (filename, pos, s, il)) -> 
        match ebis with
        | Ast_c.Declaration 
            (Ast_c.DeclList 
               ([((Some ((s, None), iisini)), 
                  (qu, (Ast_c.FunctionType ft, iity)), 
                  storage),
                 []
               ], iiptvirg::iisto))  -> 
             (try 
               Transformation.transform_proto
                   (Ast_cocci.FunHeader (a,b,c,d,e,f,g), info, fv, dots)
                   (((Control_flow_c.FunHeader ((s, ft, storage), 
                                                iisini++iity++iisto)), []),"")
                   binding (qu, iiptvirg, storage) g
                 +> (fun x -> x,  Unparse_c.PPnormal)
             with Transformation.NoMatch -> 
               (ebis, Unparse_c.PPviatok il)
             )
             
        | x -> 
            (x, Unparse_c.PPviatok il)
        ) 
        +> Unparse_c.pp_program "/tmp/input.c" "/tmp/output.c";
      command2("cp /tmp/output.c /tmp/input.c");    
   );

  (* may need --strip-trailing-cr under windows *)
  ignore(Sys.command ("diff -u -b -B " ^ cfile ^ " /tmp/output.c"))

  with NotWorthTrying -> command2("cp " ^ cfile ^ " /tmp/output.c");    
