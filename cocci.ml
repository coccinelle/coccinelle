open Common open Commonop

module CCI = Ctlcocci_integration

(*****************************************************************************)
(*
 * This file is a kind of driver. It gathers all the important functions 
 * from coccinelle in one place. The different entities in coccinelle are:
 *  - files
 *  - astc
 *  - astcocci
 *  - flow (contain nodes)
 *  - ctl  (contain rule_elems)
 * There are functions to transform one in another.
 *)
(*****************************************************************************)

(* --------------------------------------------------------------------- *)
(* C related *)
(* --------------------------------------------------------------------- *)
let cprogram_from_file  file = 
  let (program2, _stat) = Parse_c.parse_print_error_heuristic file in
  program2 
    +> Common.unzip 
    +> (fun (program, infos) -> Type_annoter_c.annotate_program program, infos)
    +> uncurry Common.zip

let (cstatement_from_string: string -> Ast_c.statement) = fun s ->
  begin
    write_file ("/tmp/__cocci.c") ("void main() { \n" ^ s ^ "\n}");
    let program = cprogram_from_file ("/tmp/__cocci.c") in
    program +> find_some (fun (e,_) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, [st]),_) -> Some st
      | _ -> None
      )
  end

let (cexpression_from_string: string -> Ast_c.expression) = fun s ->
  begin
    write_file ("/tmp/__cocci.c") ("void main() { \n" ^ s ^ ";\n}");
    let program = cprogram_from_file ("/tmp/__cocci.c") in
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
(* Cocci related *)
(* --------------------------------------------------------------------- *)
let sp_from_file file iso    = Parse_cocci.process file iso false

let (rule_elem_from_string: string -> filename option -> Ast_cocci.rule_elem) =
 fun s iso -> 
  begin
    write_file ("/tmp/__cocci.cocci") (s);
    let (astcocci, _,_) = sp_from_file ("/tmp/__cocci.cocci") iso in
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
(* Flow related *)
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
(* Ctl related *)
(* --------------------------------------------------------------------- *)
let ctls ast ua  =
  List.map2
    (function ast -> function ua ->
      List.combine
	(Asttoctl2.asttoctl ast ua) (Asttomember.asttomember ast ua))
    ast ua

let one_ctl ctls = List.hd (List.hd ctls)




(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

let show_or_not_cfile cfile =
  if !Flag.show_c then begin
    Common.print_xxxxxxxxxxxxxxxxx ();
    pr2 ("processing C file: " ^ cfile);
    Common.print_xxxxxxxxxxxxxxxxx ();
    Common.command2 ("cat " ^ cfile);
  end

let show_or_not_cocci coccifile isofile = 
  if !Flag.show_cocci then begin
    print_xxxxxxxxxxxxxxxxx ();
    pr2 ("processing semantic patch file: " ^ coccifile);
    isofile +> do_option (fun s -> pr2 ("with isos from: " ^ s));
    print_xxxxxxxxxxxxxxxxx ();
    command2 ("cat " ^ coccifile);
    pr2 "";
  end


let show_or_not_ctl_tex astcocci ctls =
  if !Flag.show_ctl_tex then begin
    Ctltotex.totex ("/tmp/__cocci_ctl.tex") astcocci ctls;
    command2 ("cd /tmp; latex __cocci_ctl.tex; " ^
              "dvips __cocci_ctl.dvi -o __cocci_ctl.ps;" ^
              "gv __cocci_ctl.ps &");
  end

let show_or_not_ctl_text ctl =
  if !Flag.show_ctl_text then begin
    print_xxxxxxxxxxxxxxxxx();
    pr2 "ctl";
    print_xxxxxxxxxxxxxxxxx();
    let (ctl,_) = ctl in
    Pretty_print_engine.pp_ctlcocci 
      !Flag.show_mcodekind_in_ctl !Flag.inline_let_ctl ctl;
    Format.print_newline();
  end


let show_or_not_trans_info trans_info = 
  if !Flag.show_transinfo then begin
    print_xxxxxxxxxxxxxxxxx();
    pr2 "transformation info returned:";
    print_xxxxxxxxxxxxxxxxx();
    Pretty_print_engine.pp_transformation_info trans_info;
    Format.print_newline();
  end


let show_or_not_binding binding =
  begin
  pr2 "start binding = ";
  Pretty_print_c.pp_binding binding;
  Format.print_newline()
  end


(*****************************************************************************)
(* Some  helpers functions *)
(*****************************************************************************)


let extract_all_error_words astcocci = 
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


let worth_trying cfile tokens = 
  if not !Flag.windows && not (null tokens)
  then
    (match Sys.command (sprintf "egrep -q '(%s)' %s" (join "|" tokens) cfile)
    with
    | 0 (* success *) -> true
    | _ (* failure *) -> false (* no match, so not worth trying *)
    )
  else true



let contain_loop def = 
  let res = ref false in
  def +> Visitor_c.visitor_def_k { Visitor_c.default_visitor_c with
   Visitor_c.kstatement = (fun (k, bigf) stat -> 
     match stat with 
     | Ast_c.Iteration _, ii
     (* overapproximation cos a goto doesn't always lead to a loop *)
     | Ast_c.Jump (Ast_c.Goto _), ii -> 
         res := true
     | st -> k st
     )
     };
  !res



let ast_to_flow_with_error_messages def filename =
  let flow = 
    try Ast_to_flow.ast_to_control_flow def 
    with Ast_to_flow.DeadCode Some info -> 
      pr2 "PBBBBBBBBBBBBBBBBBB";
      pr2 (Common.error_message filename ("", info.charpos));
      failwith 
        ("At least 1 DEADCODE detected (there may be more)," ^
         "but I can't continue." ^ 
         "Maybe because of cpp #ifdef side effects."
        );
  in
  (* This time even if there is a deadcode, we still have a
   * flow graph, so I can try the transformation and hope the
   * deadcode will not bother us. 
   *)
  begin
    try Ast_to_flow.deadcode_detection flow
    with Ast_to_flow.DeadCode Some info -> 
      pr2 "PBBBBBBBBBBBBBBBBBB";
      pr2 (Common.error_message filename ("", info.charpos));
      pr2 ("At least 1 DEADCODE detected (there may be more)," ^
           "but I continue.");
      pr2 "Maybe because of cpp #ifdef side effects.";
              
  end;
  flow

                  
  
(*****************************************************************************)
(* The main functions *)
(*****************************************************************************)

(* This function returns a triplet. First the C element (modified),
 * then a binding option if there is new info brought by the matching,
 * and finally a hack_funheaders list. 
 *)
let program_elem_vs_ctl = fun cinfo cocciinfo binding -> 
  let (elem, (filename, _pos, str_elem, il)) = cinfo in
  let (ctl, used_after_list, error_words) = cocciinfo in

  match elem, ctl  with

  (* In cocci we have 2 elements for include, in C we have only 1. For
   * the moment I use only the header element (ex "<devfs.h>"), and
   * not the keywordd element ("#include"). It would be complicated to
   * put 2 elements in C because sometime the lexer consider an
   * include as a comment, and the algo currently treats tokens
   * separately. 
   *)
  | Ast_c.CPPInclude ii, 
    ((Ast_ctl.Pred (Lib_engine.Include (kwd, header), _modif), _i), _preds) -> 
      (match ii with
      | [(iinclude, mcodebinding)] -> 
          let sheader = Ast_cocci.unwrap_mcode header in
          if iinclude.str =~ ("#[ \t]*include[ \t]*" ^ sheader)
          then
            (Ast_c.CPPInclude 
              (Transformation.tag_symbols [header] ii  binding), 
             Unparse_c.PPnormal),
            None, []
          else 
            (Ast_c.CPPInclude ii, Unparse_c.PPnormal),
            None, []
      | _ -> raise Impossible
      )
      

  | celem, ((Ast_ctl.Pred (Lib_engine.Include (kwd, header), _m), _i), _p) -> 
      (celem, Unparse_c.PPviatok il),
      None, []

  | Ast_c.Definition (((funcs, _, _, c),_) as def),   ctl -> 
      if !Flag.show_misc then pr2 ("starting function " ^ funcs);

      (* cos caml regexp dont like \n ... *)
      let str = Str.global_replace (Str.regexp "\n") " " str_elem in 
      (* call the engine algorithms only if have found a flag word *)
      if not (!Flag.process_only_when_error_words) ||
         error_words +> List.exists (fun errw -> str =~ (".*" ^ errw))
      then
        begin
          let flow = ast_to_flow_with_error_messages def filename in

          (* remove the fake nodes for julia *)
          let fixed_flow = CCI.fix_flow_ctl flow in
                
          if !Flag.show_flow              then print_flow fixed_flow;
          if !Flag.show_before_fixed_flow then print_flow flow;


          let satres = 
            Common.save_excursion Flag_ctl.loop_in_src_code (fun () -> 
              Flag_ctl.loop_in_src_code := 
                !Flag_ctl.loop_in_src_code || contain_loop def;
              
              (***************************************)
              (* !Main point! The call to the engine *)
              (***************************************)
              let model_ctl  = CCI.model_for_ctl flow binding in
	      CCI.mysat model_ctl ctl (used_after_list, binding)

          ) in

	  match satres with
	  | Left (trans_info, returned_any_states, newbinding) ->
              show_or_not_trans_info trans_info;

              (* modify also the proto if FunHeader was touched *)
              let hack_funheaders = 
               trans_info +> map_filter (fun (_nodei, binding, rule_elem) -> 
                match rule_elem with
                | Ast_cocci.FunHeader (a,b,c,d,e,f,g),info,fv,dots -> 
                    Some  (binding, ((a,b,c,d,e,f,g),info,fv,dots))
                | _ -> None
                )  
              in
                    
              if not (null trans_info) (* TODOOOOO returned_any_states *)
              then 
                (* I do the transformation on flow, not fixed_flow, 
                   because the flow_to_ast need my extra information. *)
                let flow' = Transformation.transform trans_info flow in
                let def' = Flow_to_ast.control_flow_to_ast flow' in

                (Ast_c.Definition def', Unparse_c.PPnormal), 
                Some newbinding, hack_funheaders
              else 
                (Ast_c.Definition def, Unparse_c.PPviatok il), 
                Some newbinding, hack_funheaders
	  | Right x -> 
              pr2 ("Unable to find a value for " ^ x);
              (Ast_c.Definition def, Unparse_c.PPviatok il), 
              None, []
        end
      else 
        (Ast_c.Definition def, Unparse_c.PPviatok il),
        None, []
  | x, ctl -> 
      (x, Unparse_c.PPviatok il),
      None, []






(* --------------------------------------------------------------------- *)

(* Returns nothing. The output is in the file "/tmp/output.c". *)
let full_engine cfile coccifile_and_iso_or_ctl = 
  assert (lfile_exists cfile);

  (* preparing the inputs (c, cocci, ctl) *)
  let (ctls, error_words, error_words_julia) = 
    (match coccifile_and_iso_or_ctl with
    | Left (coccifile, isofile) -> 
        let (astcocci,used_after_lists,toks)= sp_from_file coccifile isofile in
        let all_error_words = extract_all_error_words astcocci in
        let ctls = ctls astcocci used_after_lists in

        show_or_not_cfile  cfile;
        show_or_not_cocci coccifile isofile;
        show_or_not_ctl_tex astcocci ctls;
        (zip ctls used_after_lists, all_error_words, toks)
    | Right ctl ->([[(ctl,([],[]))], []]), [], []
    )
  in

  (* optimisation allowing to launch coccinelle on all the drivers *)
  if not (worth_trying cfile error_words_julia)
  then command2("cp " ^ cfile ^ " /tmp/output.c")
  else begin


  (* ----------------------------------------- *)
  (* And now the main algorithm *)
  (* ----------------------------------------- *)

  command2("cp " ^ cfile ^ " /tmp/input.c");

  (* The algorithm is roughly: 
   *  for_all ctl rules in SP
   *   for_all minirule in rule
   *    for_all binding (computed during previous phase)
   *      for_all C elements in "input.c"
   *         match control flow of function vs minirule 
   *         with the binding and update the set of possible 
   *         bindings, and returned the possibly modified function.
   *      pretty print modified C elements in "output.c"
   *      copy "output.c" to "input.c"
   *)

  let _current_bindings = ref [Ast_c.emptyMetavarsBinding] in

  let _hack_funheader = ref [] in


  (* ----------------- *)
  (* 1: iter ctl *)  
  (* ----------------- *)
  ctls +> List.iter (fun  (ctl_toplevel_list, used_after_list) -> 
    
   if List.length ctl_toplevel_list = 1 
   then begin
    
    let ctl = List.hd ctl_toplevel_list in
    show_or_not_ctl_text ctl;
    
    (* 2: prepare to iter binding *)

    (* I have not to look at used_after_list to decide to restart from
     * scratch. I just need to look if the binding list is empty.
     * Indeed, let's suppose that a SP have 3 regions/rules. If we
     * don't find a match for the first region, then if this first
     * region does not bind metavariable used after, that is if
     * used_after_list is empty, then mysat(), even if does not find a
     * match, will return a Left, with an empty transformation_info,
     * and so current_binding will grow. On the contrary if the first
     * region must bind some metavariables used after, and that we
     * dont find any such region, then mysat() will returns lots of
     * Right, and current_binding will not grow, and so we will have
     * an empty list of binding, and we will catch such a case. 
     *)

    if null (!_current_bindings) then begin 
      pr2 "Empty list of bindings, I restart from scratch";
      _current_bindings := [Ast_c.emptyMetavarsBinding];
    end;
    let lastround_bindings = !_current_bindings in
    _current_bindings := [];

    (* ------------------ *)
    (* 2: iter binding *)
    (* ------------------ *)
    lastround_bindings +> List.iter (fun binding -> 

      (* ------------------ *)
      (* 3: iter function *)
      (* ------------------ *)
      let cprogram = cprogram_from_file ("/tmp/input.c") in
      let cprogram' = cprogram +> List.map (fun (elem,(filename, pos, s,il)) ->

        (************************************************************)
        (* !Main point! The call to the function that will call the
         * ctl engine and all the machinery *)
        (************************************************************)
        let full_used_after_list = 
	  List.fold_left Common.union_set [] used_after_list 
        in

        let elem', newbinding, hack_funheaders = 
          program_elem_vs_ctl 
            (elem, (filename, pos, s, il))
            (ctl, full_used_after_list, error_words) 
            binding 
        in

        hack_funheaders +> List.iter (fun hack -> push2 hack _hack_funheader);

        (* opti: julia says that because the binding is
         * determined by the used_after_list, the items in the list
         * are kind of sorted, so could optimise the union.
         *)
        newbinding +> do_option (fun newbinding -> 
          _current_bindings := Common.insert_set newbinding !_current_bindings;
        );
        
        elem'
        ) (* end 3: iter function *)
      in

      Unparse_c.pp_program cprogram' ("/tmp/input.c") ("/tmp/output.c");
      command2("cp /tmp/output.c /tmp/input.c");    
      ) (* end 2: iter bindings *)
   end
   else begin
     (* (binding *  (funcname * transformed) list) list *)
     let _current_bindings_multictl = 
       if List.length !_current_bindings = 1
       then ref [(List.hd !_current_bindings, [])]
       else failwith "too many inherited environments for mini-rules"
     in
     let cprogram = cprogram_from_file ("/tmp/input.c") in
     
     (* iter regions *)
     zip ctl_toplevel_list used_after_list +> List.iter 
       (fun (ctl, used_after_one_ctl) -> 

         show_or_not_ctl_text ctl;


         let lastround_bindings_multi = List.rev !_current_bindings_multictl in
         _current_bindings_multictl := [];


         (* iter binding and already *)
         lastround_bindings_multi +> List.iter (fun (binding, already) -> 

           (* iter program *)
           cprogram +> List.iter (fun (elem, (filename, pos, s, il)) -> 

             if (not (List.mem pos (Common.keys already)))
             then begin
             let elem', newbinding, hack_funheaders = 
               program_elem_vs_ctl 
                 (elem, (filename, pos, s, il))
                 (ctl, used_after_one_ctl, error_words) 
                 binding 
             in

             (* hack_funheaders +> List.iter (fun hack -> push2 hack _hack_funheader); *)

             (* opti: julia says that because the binding is
              * determined by the used_after_list, the items in the list
              * are kind of sorted, so could optimise the union.
              *)
             newbinding +> do_option (fun newbinding -> 
               push2 (newbinding, (pos, fst elem')::already)
                       _current_bindings_multictl;
               );
             end
             )
       );
     );



     (* assert no conflict. can concern different function ? *)
     (match (List.length !_current_bindings_multictl) with
     | 0 ->  command2("cp /tmp/input.c /tmp/output.c");    
     | 1 -> 
       let (binding, list_func) = List.hd !_current_bindings_multictl in
       let cprogram' = cprogram +> List.map (fun (e, (filename, pos, s, il)) ->
         if List.mem pos (Common.keys list_func)
         then List.assoc pos list_func, Unparse_c.PPnormal
         else e, Unparse_c.PPviatok il
         )
       in
       Unparse_c.pp_program cprogram' ("/tmp/input.c") ("/tmp/output.c");
       command2("cp /tmp/output.c /tmp/input.c");    
       _current_bindings := [binding]
       
     | n -> failwith "multiple candidates for multi mini-rules, TODO"
           );
     end

  ); (* end 1: iter ctl *)

  (* ----------------------------------------------------------------- *)
  (* Last fix *)
  (* ----------------------------------------------------------------- *)
  (* todo: what if the function is modified two times ? we should
   * modify the prototype as soon as possible, not wait until the end
   * of all the ctl rules 
   *)
 
  !_hack_funheader +> List.iter (fun 
    ((binding, ((a,b,c,d,e,f,g),info,fv,dots))) -> 
   
    let cprogram = cprogram_from_file ("/tmp/input.c") in
    let cprogram' = 
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
    in
    Unparse_c.pp_program cprogram' ("/tmp/input.c") ("/tmp/output.c");
    command2("cp /tmp/output.c /tmp/input.c");    
   );

  (* ----------------------------------------- *)

  (* may need --strip-trailing-cr under windows *)
  ignore(Sys.command ("diff -u -b -B " ^ cfile ^ " /tmp/output.c"))
 end


