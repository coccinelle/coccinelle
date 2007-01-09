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
let cprogram_from_file file = 
  let (program2, _stat) = Parse_c.parse_print_error_heuristic file in
  program2 

let cfile_from_program program2_with_method outf = 
  Unparse_c.pp_program program2_with_method outf


  

let (cstatement_from_string: string -> Ast_c.statement) = fun s ->
  begin
    Common.write_file ("/tmp/__cocci.c") ("void main() { \n" ^ s ^ "\n}");
    let program = cprogram_from_file ("/tmp/__cocci.c") in
    program +> Common.find_some (fun (e,_) -> 
      match e with
      | Ast_c.Definition ((funcs, _, _, [st]),_) -> Some st
      | _ -> None
      )
  end

let (cexpression_from_string: string -> Ast_c.expression) = fun s ->
  begin
    Common.write_file ("/tmp/__cocci.c") ("void main() { \n" ^ s ^ ";\n}");
    let program = cprogram_from_file ("/tmp/__cocci.c") in
    program +> Common.find_some (fun (e,_) -> 
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
    Common.write_file ("/tmp/__cocci.cocci") (s);
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
  program +> Common.map_filter (fun (e,_) -> 
    match e with
    | Ast_c.Definition (((funcs, _, _, c),_) as def) -> 
        let flow = Ast_to_flow.ast_to_control_flow def in
        (try begin Ast_to_flow.deadcode_detection flow; Some flow end
        with
           | Ast_to_flow.DeadCode None      -> 
               pr2 "deadcode detected, but cant trace back the place"; 
               None
           | Ast_to_flow.DeadCode (Some info) -> 
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
    Common.print_xxxxxxxxxxxxxxxxx ();
    pr2 ("processing semantic patch file: " ^ coccifile);
    isofile +> Common.do_option (fun s -> pr2 ("with isos from: " ^ s));
    Common.print_xxxxxxxxxxxxxxxxx ();
    Common.command2 ("cat " ^ coccifile);
    pr2 "";
  end


let show_or_not_ctl_tex astcocci ctls =
  if !Flag.show_ctl_tex then begin
    Ctltotex.totex ("/tmp/__cocci_ctl.tex") astcocci ctls;
    Common.command2 ("cd /tmp; latex __cocci_ctl.tex; " ^
              "dvips __cocci_ctl.dvi -o __cocci_ctl.ps;" ^
              "gv __cocci_ctl.ps &");
  end

let show_or_not_ctl_text ctl =
  if !Flag.show_ctl_text then begin
    Common.print_xxxxxxxxxxxxxxxxx();
    pr2 "ctl";
    Common.print_xxxxxxxxxxxxxxxxx();
    let (ctl,_) = ctl in
    Pretty_print_engine.pp_ctlcocci 
      !Flag.show_mcodekind_in_ctl !Flag.inline_let_ctl ctl;
    Format.print_newline();
  end


let show_or_not_trans_info trans_info = 
  if !Flag.show_transinfo then begin
    if null trans_info then pr2 "transformation info is empty"
    else begin
      Common.print_xxxxxxxxxxxxxxxxx();
      pr2 "transformation info returned:";
      Common.print_xxxxxxxxxxxxxxxxx();
      Pretty_print_engine.pp_transformation_info trans_info;
      Format.print_newline();
    end
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



let ast_to_flow_with_error_messages2 def filename =
  let flow = 
    try Ast_to_flow.ast_to_control_flow def 
    with Ast_to_flow.DeadCode (Some info) -> 
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
    with Ast_to_flow.DeadCode (Some info) -> 
      pr2 "PBBBBBBBBBBBBBBBBBB";
      pr2 (Common.error_message filename ("", info.charpos));
      pr2 ("At least 1 DEADCODE detected (there may be more)," ^
           "but I continue.");
     (* not a failwith this time *)
      pr2 "Maybe because of cpp #ifdef side effects."; 
              
  end;
  flow

let flow_to_ast2 flow = 
  let nodes = flow#nodes#tolist in
  match nodes with
  | [_, node] -> 
      (match Control_flow_c.unwrap node with
      | Control_flow_c.Decl decl -> 
          Ast_c.Declaration decl
      | _ -> raise Impossible
      )
  | _ -> 
      Ast_c.Definition (Flow_to_ast.control_flow_to_ast flow)



let ast_to_flow_with_error_messages a b = 
  Common.profile_code "flow" (fun () -> ast_to_flow_with_error_messages2 a b)

let flow_to_ast a = 
  Common.profile_code "unflow" (fun () -> flow_to_ast2 a)
                  
(*****************************************************************************)
(* Optimisation. Try not unparse/reparse the whole file when have modifs  *)
(*****************************************************************************)

type celem_info = { 
  flow: Control_flow_c.cflow;
  fixed_flow: Control_flow_c.cflow;
  contain_loop: bool;
}

type celem_with_info = (Parse_c.programElement2 * celem_info option)


let build_maybe_info e cfile = 
  match e with 
  | Ast_c.Definition (((funcs, _, _, c),_) as def) -> 
      if !Flag.show_misc then pr2 ("build info function " ^ funcs);
      
      let flow = ast_to_flow_with_error_messages def cfile in
      
      (* remove the fake nodes for julia *)
      let fixed_flow = CCI.fix_flow_ctl flow in
      
      if !Flag.show_flow              then print_flow fixed_flow;
      if !Flag.show_before_fixed_flow then print_flow flow;
      Some
        { flow = flow; 
          fixed_flow = fixed_flow; 
          contain_loop = contain_loop def 
        }
  | Ast_c.Declaration decl -> 
      let flow = Ast_to_flow.simple_cfg  (Control_flow_c.Decl decl) "decl" in
      let fixed_flow = CCI.fix_simple_flow_ctl flow in
      Some { 
        flow = flow;
        fixed_flow = fixed_flow;
        contain_loop = false;
      }

  | _ -> None

let (build_info_program: filename -> celem_with_info list) = fun cfile -> 
  let cprogram = cprogram_from_file cfile in
  let cprogram' = cprogram
    +> Common.unzip 
    +> (fun (program, infos) -> Type_annoter_c.annotate_program program, infos)
    +> Common.uncurry Common.zip
  in
  cprogram' +> List.map (fun (e, info_item) -> 
    (e, info_item), build_maybe_info e cfile)
    
        

(* bool is wether or not have to unparse and reparse the c element *)
let (rebuild_info_program : 
  (celem_with_info * bool) list -> celem_with_info list) = fun xs ->
  let xxs = xs +> List.map (fun (((elem, info_item), flow), modified) -> 
    if modified 
    then begin
      let file = "/tmp/small_output.c" in
      cfile_from_program [(elem, info_item), Unparse_c.PPnormal]  file;
      (* Common.command2 ("cat " ^ file); *)
      let xs = build_info_program file  in
      Common.list_init xs (* get rid of the FinalDef *)
    end
    else 
      [((elem, info_item), flow)]
  )
  in
  List.concat xxs



(*****************************************************************************)
(* The main functions *)
(*****************************************************************************)

(* This function returns a triplet. First the C element (modified),
 * then a binding option if there is new info brought by the matching,
 * and finally a hack_funheaders list. 
 *)
let program_elem_vs_ctl2 = fun cinfo cocciinfo binding -> 
  let (elem, info) = cinfo in
  let (ctl, used_after_list) = cocciinfo in

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
                (Transformation.tag_symbols [header] ii  binding), true),
          None, []
          else 
            (Ast_c.CPPInclude ii, true),
          None, []
      | _ -> raise Impossible
      )
        

  | celem, ((Ast_ctl.Pred (Lib_engine.Include (kwd, header), _m), _i), _p) -> 
      (celem, false),   None, []

  | celem, ctl -> 
      (match info with
      | None -> (celem, false), None, []
      | Some info -> 

          (match celem with 
          | Ast_c.Definition ((funcs, _, _, _c),_) -> 
              if !Flag.show_misc then pr2 ("starting function " ^ funcs);

          | Ast_c.Declaration 
              (Ast_c.DeclList ([(Some ((s, _),_), typ, sto), _], _)) -> 
              if !Flag.show_misc then pr2 ("starting variable " ^ s);
          | _ -> 
              if !Flag.show_misc then pr2 ("starting something else");
          );

          let satres = 
            Common.save_excursion Flag_ctl.loop_in_src_code (fun () -> 
              Flag_ctl.loop_in_src_code := 
                !Flag_ctl.loop_in_src_code || info.contain_loop;
              
              (***************************************)
              (* !Main point! The call to the engine *)
              (***************************************)
              (* model_ctl internally build a fixed_flow *)
              let model_ctl  = CCI.model_for_ctl info.fixed_flow binding in
	      CCI.mysat model_ctl ctl (used_after_list, binding)

            ) in

          (match satres with
          | Left (trans_info, returned_any_states, newbinding) ->
              show_or_not_trans_info trans_info;

              (* modify also the proto if FunHeader was touched *)
              let hack_funheaders = 
                trans_info +> Common.map_filter (fun (_nodi, binding, rule_elem) ->
                  match rule_elem with
                  | Ast_cocci.FunHeader (a,b,c,d,e,f,g,h),info,fv,dots -> 
                      Some  (binding, ((a,b,c,d,e,f,g,h),info,fv,dots))
                  | _ -> None
                )  
              in
              
              if not (null trans_info) (* TODOOOOO returned_any_states *)
              then 
                (* I do the transformation on flow, not fixed_flow, 
                   because the flow_to_ast need my extra information. *)
                let flow' = 
                  if !Flag_engine.use_cocci_vs_c
                  then Transformation2.transform trans_info info.flow 
                  else Transformation.transform trans_info info.flow 
                in
                let celem' = flow_to_ast flow' in
                (celem', true), Some newbinding, hack_funheaders
              else 
                (celem, false), Some newbinding, hack_funheaders
          | Right x -> 
              pr2 ("Unable to find a value for " ^ x);
              (celem, false),   None, []
          )
      )


let program_elem_vs_ctl a b c = 
  Common.profile_code "program_elem_vs_ctl" 
    (fun () -> program_elem_vs_ctl2 a b c)




(* --------------------------------------------------------------------- *)

(* Returns nothing. The output is in the file "/tmp/output.c". *)
let full_engine2 cfile coccifile_and_iso_or_ctl = 
  assert (Common.lfile_exists cfile);

  (* preparing the inputs (c, cocci, ctl) *)
  let (ctls, error_words_julia) = 
    (match coccifile_and_iso_or_ctl with
    | Left (coccifile, isofile) -> 
        let (astcocci,used_after_lists,toks)= sp_from_file coccifile isofile in
        let ctls = ctls astcocci used_after_lists in

        show_or_not_cfile  cfile;
        show_or_not_cocci coccifile isofile;
        show_or_not_ctl_tex astcocci ctls;
        (zip ctls used_after_lists, toks)
    | Right ctl ->([[(ctl,([],[]))], []]), []
    )
  in

  (* optimisation allowing to launch coccinelle on all the drivers *)
  if not (worth_trying cfile error_words_julia)
  then Common.command2("cp " ^ cfile ^ " /tmp/output.c")
  else begin


    (* parsing and build CFG *)
    let cprogram = ref (build_info_program cfile) in

    (* And now the main algorithm *)
    (* The algorithm is roughly: 
     *  for_all ctl rules in SP
     *   for_all minirule in rule
     *    for_all binding (computed during previous phase)
     *      for_all C elements
     *         match control flow of function vs minirule 
     *         with the binding and update the set of possible 
     *         bindings, and returned the possibly modified function.
     *      pretty print modified C elements and reparse it.
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
          let cprogram' = !cprogram +> List.map (fun ((elem, info_item), info) ->

            let full_used_after_list = 
	      List.fold_left Common.union_set [] used_after_list 
            in

            (************************************************************)
            (* !Main point! The call to the function that will call the
             * ctl engine and all the machinery *)
            (************************************************************)
            let (elem',modified), newbinding, hack_funheaders = 
              program_elem_vs_ctl 
                (elem, info)
                (ctl, full_used_after_list) 
                binding 
            in

            hack_funheaders +> List.iter 
              (fun hack -> Common.push2 hack _hack_funheader);

            (* opti: julia says that because the binding is
             * determined by the used_after_list, the items in the list
             * are kind of sorted, so could optimise the union.
             *)
            newbinding +> Common.do_option (fun newbinding -> 
              _current_bindings := 
                Common.insert_set newbinding !_current_bindings;
            );
            
            ((elem', info_item), info), modified 
          ) (* end 3: iter function *)
          in
          cprogram := rebuild_info_program cprogram';
        ) (* end 2: iter bindings *)
      end
      else failwith "not hanling multiple minirules"

    ); (* end 1: iter ctl *)

    (* ----------------------------------------------------------------- *)
    (* Last fix *)
    (* ----------------------------------------------------------------- *)
    (* todo: what if the function is modified two times ? we should
     * modify the prototype as soon as possible, not wait until the end
     * of all the ctl rules 
     *)
    if !Flag.show_misc then pr2 ("hack headers");
    Common.profile_code "hack_headers" (fun () -> 
      !_hack_funheader +> List.iter (fun 
        ((binding, ((a,b,c,d,e,f,g,h),info,fv,dots))) -> 
          
          let cprogram' = 
            !cprogram +> List.map (fun ((ebis, info_item), flow) -> 
              let ebis', modified = 
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
                          (Ast_cocci.FunHeader (a,b,c,d,e,f,g,h),info,fv,dots)
                          (((Control_flow_c.FunHeader ((s, ft, storage), 
                                                      iisini++iity++iisto)), []),"")
                          binding (qu, iiptvirg, storage) h
                        +> (fun x -> x,  true)
                      with Transformation.NoMatch -> (ebis, false)
                    )
                | x -> (x, false)
              in
              (((ebis', info_item), flow), modified)
            ) 
              
          in
          cprogram := rebuild_info_program cprogram';
      );
    );

    (* and now unparse everything *)
    let cprogram' = !cprogram +> List.map (fun ((ebis, info_item), _flow) -> 
      (ebis, info_item), Unparse_c.PPviastr) 
    in
    cfile_from_program cprogram' "/tmp/output.c";

    if !Flag.show_diff then begin
      (* may need --strip-trailing-cr under windows *)
      pr2 "diff = ";
      Common.command2 ("diff -u -b -B " ^ cfile ^ " /tmp/output.c");
    end
  end


let full_engine a b = 
  Common.profile_code "full_engine" (fun () -> full_engine2 a b)
