open Common open Commonop

module CCI = Ctlcocci_integration
module TAC = Type_annoter_c

(*****************************************************************************)
(*
 * This file is a kind of driver. It gathers all the important functions 
 * from coccinelle in one place. The different entities in coccinelle are:
 *  - files
 *  - astc
 *  - astcocci
 *  - flow (contain nodes)
 *  - ctl  (contain rule_elems)
 * This file contains functions to transform one in another.
 *)
(*****************************************************************************)

(* --------------------------------------------------------------------- *)
(* C related *)
(* --------------------------------------------------------------------- *)
let cprogram_from_file file = 
  let (program2, _stat) = Parse_c.parse_print_error_heuristic file in
  program2 

let cfile_from_program program2_with_ppmethod outf = 
  Unparse_c.pp_program program2_with_ppmethod outf


  

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
  astc +> Common.map_filter (fun e -> 
    match e with
    | Ast_c.Definition (((funcs, _, _, c),_) as def) -> 
        let flow = Ast_to_flow.ast_to_control_flow def in
        let everythings_fine = 
          (try Ast_to_flow.deadcode_detection flow; true
           with Ast_to_flow.Error (Ast_to_flow.DeadCode x) -> 
             Ast_to_flow.report_error (Ast_to_flow.DeadCode x);
             false 
          )
        in
        if everythings_fine then Some flow else None
    | _ -> None
   )

let one_flow flows = List.hd flows

let print_flow flow = 
  Ograph_extended.print_ograph_extended "/tmp/test.dot" flow

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

let show_or_not_cfile2 cfile =
  if !Flag.show_c then begin
    Common.pr2_xxxxxxxxxxxxxxxxx ();
    pr2 ("processing C file: " ^ cfile);
    Common.pr2_xxxxxxxxxxxxxxxxx ();
    Common.command2 ("cat " ^ cfile);
  end
let show_or_not_cfile a = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_cfile2 a)

let show_or_not_cocci2 coccifile isofile = 
  if !Flag.show_cocci then begin
    Common.pr2_xxxxxxxxxxxxxxxxx ();
    pr2 ("processing semantic patch file: " ^ coccifile);
    isofile +> Common.do_option (fun s -> pr2 ("with isos from: " ^ s));
    Common.pr2_xxxxxxxxxxxxxxxxx ();
    Common.command2 ("cat " ^ coccifile);
    pr2 "";
  end
let show_or_not_cocci a b = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_cocci2 a b)


let show_or_not_ctl_tex2 astcocci ctls =
  if !Flag.show_ctl_tex then begin
    Ctltotex.totex ("/tmp/__cocci_ctl.tex") astcocci ctls;
    Common.command2 ("cd /tmp; latex __cocci_ctl.tex; " ^
              "dvips __cocci_ctl.dvi -o __cocci_ctl.ps;" ^
              "gv __cocci_ctl.ps &");
  end
let show_or_not_ctl_tex a b  = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_ctl_tex2 a b)



let show_or_not_ctl_text2 ctl ast rulenb =
  if !Flag.show_ctl_text then begin

    ast +> do_option (fun ast -> 
      Common.pr_xxxxxxxxxxxxxxxxx ();
      pr ("rule " ^ i_to_s rulenb ^ " = ");
      Common.pr_xxxxxxxxxxxxxxxxx ();
      adjust_pp_with_indent (fun () -> 
        Format.force_newline();
        Pretty_print_cocci.print_plus_flag := true;
        Pretty_print_cocci.print_minus_flag := true;
        Pretty_print_cocci.unparse ast;
      );
    );

    pr "CTL = ";
    let (ctl,_) = ctl in
    adjust_pp_with_indent (fun () -> 
      Format.force_newline();
      Pretty_print_engine.pp_ctlcocci 
        !Flag.show_mcodekind_in_ctl !Flag.inline_let_ctl ctl;
    );
    pr "";
  end
let show_or_not_ctl_text a b c = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_ctl_text2 a b c)



let show_or_not_celem2 prelude celem = 
  if !Flag.show_misc then 
  (match celem with 
  | Ast_c.Definition ((funcs,_,_,_c),_) -> 
      pr2 (prelude ^ " function: " ^ funcs);
  | Ast_c.Declaration (Ast_c.DeclList ([(Some ((s, _),_), typ, sto), _], _)) ->
      pr2 (prelude ^ " variable " ^ s);
  | _ -> 
      pr2 (prelude ^ " something else");
  )
let show_or_not_celem a b  = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_celem2 a b)



let show_or_not_trans_info2 trans_info = 
  if !Flag.show_transinfo then begin
    if null trans_info then pr2 "transformation info is empty"
    else begin
      pr2 "transformation info returned:";
      let trans_info =
        List.sort (function (i1,_,_) -> function (i2,_,_) -> compare i1 i2)
          trans_info 
      in
      indent_do (fun () -> 
        trans_info +> List.iter (fun (i, subst, re) -> 
          pr2 ("transform state: " ^ (Common.i_to_s i));
          indent_do (fun () -> 
            adjust_pp_with_indent_and_header "with rule_elem: " (fun () -> 
              Pretty_print_cocci.print_plus_flag := true;
              Pretty_print_cocci.print_minus_flag := true;
              Pretty_print_cocci.rule_elem "" re;
            );
            adjust_pp_with_indent_and_header "with binding: " (fun () -> 
              Pretty_print_c.pp_binding subst;
            );
          )
        );
      )
    end
  end
let show_or_not_trans_info a  = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_trans_info2 a)



let show_or_not_binding2 s binding =
  if !Flag.show_binding_in_out then begin
    adjust_pp_with_indent_and_header ("binding " ^ s ^ " = ") (fun () -> 
      Pretty_print_c.pp_binding binding;
    )
  end
let show_or_not_binding a b  = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_binding2 a b)



let show_or_not_diff2 cfile outfile = 
  if !Flag.show_diff then begin
    (* may need --strip-trailing-cr under windows *)
    pr2 "diff = ";
    Common.command2 ("diff -u -b -B " ^ cfile ^ " " ^ outfile);
  end
let show_or_not_diff a b  = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_diff2 a b)



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
  def +> Visitor_c.vk_def { Visitor_c.default_visitor_c with
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

let sp_contain_typed_metavar toplevel_list_list = 
  let bind x y = x or y in
  let option_default = false in
  let mcode _ _ = option_default in
  let donothing r k e = k e in

  let expression r k e =
    match Ast_cocci.unwrap e with
    | (Ast_cocci.MetaExpr (_,_,Some t,_)| Ast_cocci.MetaConst (_,_,Some t,_)) 
      -> true
    | _ -> k e 
  in

  let combiner = 
    Visitor_ast.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing
      donothing expression donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing 
  in
  toplevel_list_list +> 
    List.exists (List.exists combiner.Visitor_ast.combiner_top_level)
    




let ast_to_flow_with_error_messages2 def =
  let flowopt = 
    try Some (Ast_to_flow.ast_to_control_flow def)
    with Ast_to_flow.Error x -> 
      pr2 "PBBBBBBBBBBBBBBBBBB";
      Ast_to_flow.report_error x;
      pr2
        ("At least 1 DEADCODE detected (there may be more)," ^
            "but I can't continue :(" ^ 
            "Maybe because of cpp #ifdef side effects."
        );
      None
  in
  flowopt +> do_option (fun flow -> 
    (* This time even if there is a deadcode, we still have a
     * flow graph, so I can try the transformation and hope the
     * deadcode will not bother us. 
     *)
    try Ast_to_flow.deadcode_detection flow
    with Ast_to_flow.Error (Ast_to_flow.DeadCode x) -> 
      pr2 "PBBBBBBBBBBBBBBBBBB";
      Ast_to_flow.report_error (Ast_to_flow.DeadCode x);
      pr2 ("At least 1 DEADCODE detected (there may be more)," ^
              "but I continue.");
      (* not a failwith this time *)
      pr2 "Maybe because of cpp #ifdef side effects."; 
  );
  flowopt
let ast_to_flow_with_error_messages a = 
  Common.profile_code "flow" (fun () -> ast_to_flow_with_error_messages2 a)


(* obsolete with -use_ref *)
let flow_to_ast2 flow = 
  let nodes = flow#nodes#tolist in
  match nodes with
  | [_, node] -> 
      (match Control_flow_c.unwrap node with
      | Control_flow_c.Decl decl -> Ast_c.Declaration decl
      | Control_flow_c.CPPInclude x -> Ast_c.CPPInclude x
      | Control_flow_c.CPPDefine x -> Ast_c.CPPDefine x
      | _ -> raise Impossible
      )
  | _ -> 
      Ast_c.Definition (Flow_to_ast.control_flow_to_ast flow)
let flow_to_ast a = 
  Common.profile_code "unflow" (fun () -> flow_to_ast2 a)

                  
(*****************************************************************************)
(* Optimisation. Try not unparse/reparse the whole file when have modifs  *)
(*****************************************************************************)

type celem_info = { 
  flow: Control_flow_c.cflow; (* obsolete with -use_ref *)
  fixed_flow: Control_flow_c.cflow;
  contain_loop: bool;
}

type celem_with_info = 
  Parse_c.programElement2 * celem_info option * (TAC.environment Common.pair)


let build_maybe_info e = 
  match e with 
  | Ast_c.Definition (((funcs, _, _, c),_) as def) -> 
      if !Flag.show_misc then pr2 ("build info function " ^ funcs);
      
      let flowopt = ast_to_flow_with_error_messages def in
      flowopt +> map_option (fun flow -> 
      
        (* remove the fake nodes for julia *)
        let fixed_flow = CCI.fix_flow_ctl flow in

        if !Flag.show_flow then print_flow fixed_flow;
        if !Flag.show_before_fixed_flow then print_flow flow;
        { flow = flow; 
          fixed_flow = fixed_flow; 
          contain_loop = contain_loop def 
        }
      )
  | Ast_c.Declaration _ | Ast_c.CPPInclude _ | Ast_c.CPPDefine _  -> 
      let (elem, str) = 
        match e with 
        | Ast_c.Declaration decl -> (Control_flow_c.Decl decl),  "decl"
        | Ast_c.CPPInclude x -> (Control_flow_c.CPPInclude x), "#include"
        | Ast_c.CPPDefine x -> (Control_flow_c.CPPDefine x), "#define"
        | _ -> raise Impossible
      in
      let flow = Ast_to_flow.simple_cfg elem str  in
      let fixed_flow = CCI.fix_simple_flow_ctl flow in
      Some { 
        flow = flow;
        fixed_flow = fixed_flow;
        contain_loop = false;
      }

  | _ -> None






let fakeEnv = (TAC.initial_env, TAC.initial_env)

let (build_info_program: 
 filename -> bool -> TAC.environment -> celem_with_info list) = 
 fun cfile contain_typedmetavar env -> 
  let cprogram = cprogram_from_file cfile in
  let cprogram' = 
    if contain_typedmetavar
    then
      cprogram
      +> Common.unzip 
      +> (fun (program, infos) -> 
        TAC.annotate_program env program, infos)
      +> Common.uncurry Common.zip
    else 
      cprogram +> List.map (fun (e, info_item) -> ((e,fakeEnv),  info_item))
  in
  cprogram' +> List.map (fun ((e, beforeafterenv), info_item) -> 
    (e, info_item), build_maybe_info e, beforeafterenv
  )
  
    
        

(* bool is wether or not have to unparse and reparse the c element *)
let (rebuild_info_program : 
  (celem_with_info * bool) list -> bool -> celem_with_info list) = 
 fun xs contain_typedmetavar ->
  let xxs = xs +> List.map 
    (fun (x, modified) ->
      if modified 
      then begin
        let ((elem, info_item), flow, (beforeenv, afterenvTOUSE)) = x in
        let file = Common.new_temp_file "cocci_small_output" ".c" in

        cfile_from_program [(elem, info_item), Unparse_c.PPnormal]  file;
        (* Common.command2 ("cat " ^ file); *)
        let xs = build_info_program file contain_typedmetavar beforeenv in
        (* TODO: assert env has not changed,
         * if yes then must also reparse what follows even if not modified.
         * Do that only if contain_typedmetavar of course, so good opti.
         *)
        Common.list_init xs (* get rid of the FinalDef *)
      end
      else [x]
  )
  in
  List.concat xxs



(*****************************************************************************)
(* The main functions *)
(*****************************************************************************)

(* The main algorithm =~
 * The algorithm is roughly: 
 *  for_all ctl rules in SP
 *   for_all minirule in rule
 *    for_all binding (computed during previous phase)
 *      for_all C elements
 *         match control flow of function vs minirule 
 *         with the binding and update the set of possible 
 *         bindings, and returned the possibly modified function.
 *      pretty print modified C elements and reparse it.
 *
 * 
 * On ne prends que les newbinding ou returned_any_state est vrai.
 * Si ca ne donne rien, on prends ce qu'il y avait au depart.
 * Mais au nouveau depart de quoi ?  
 * - si ca donne rien apres avoir traité toutes les fonctions avec ce binding ?
 * - ou alors si ca donne rien, apres avoir traité toutes les fonctions 
 *   avec tous les bindings du round d'avant ?
 * 
 * Julia pense qu'il faut prendre la premiere solution.
 * Example: on a deux environnements candidats, E1 et E2 apres avoir traité
 * la regle ctl 1. On arrive sur la regle ctl 2.
 * E1 ne donne rien pour la regle 2, on garde quand meme E1 pour la regle 3.
 * E2 donne un match a un endroit et rend E2' alors on utilise ca pour
 * la regle 3.
 * 
 * I have not to look at used_after_list to decide to restart from
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
 *
 * opti: julia says that because the binding is
 * determined by the used_after_list, the items in the list
 * are kind of sorted, so could optimise the insert_set operations.
 *)


let g_cprogram = ref [] 
let g_hack_funheaders = ref []
let g_contain_typedmetavar = ref false 

(* --------------------------------------------------------------------- *)
let rec process_ctls ctls envs = 
  match ctls with
  | [] -> ()
  | ctl::ctls_remaining -> 
      let ((ctl_toplevel_list,ast),used_after_list), rulenb = ctl in

      if not (List.length ctl_toplevel_list = 1)
      then failwith "not handling multiple minirules";
        
      let ctl = List.hd ctl_toplevel_list in
      show_or_not_ctl_text ctl ast rulenb;

      let newenvs = process_a_ctl (ctl, used_after_list, rulenb) envs in
      process_ctls ctls_remaining newenvs


and process_a_ctl ctl envs = 
  match envs with
  | [] -> []
  | env::envs_remaining -> 
        let children_envs = process_a_ctl_a_env ctl env in
        Common.union_set children_envs (process_a_ctl ctl envs_remaining)


and process_a_ctl_a_env (ctl, used_after_list, rulenb) env = 

  let new_c_elems = ref [] in
  show_or_not_binding "in" env;

  let children_envs = !g_cprogram +> List.fold_left 
    (fun acc ((elem,info_item),info,_env) ->

      let full_used_after_list = 
	List.fold_left Common.union_set [] used_after_list 
      in

      match process_a_ctl_a_env_a_celem
        (elem,info) (ctl,full_used_after_list, rulenb) env
      with
      | None -> 
          push2 None new_c_elems ;
          acc
      | Some (elem, modified, newbinding, hacks) ->  
          push2 (if modified then (Some elem) else None) new_c_elems;
          hacks +>List.iter (fun x ->Common.push2 x g_hack_funheaders);
          Common.insert_set newbinding acc
    ) []
  in
  let new_c_elems = List.rev !new_c_elems in
  let cprogram' = zip new_c_elems !g_cprogram  +> List.map 
    (fun (optnewelem, ((elem, info_item), flow, env))  -> 
      match optnewelem with 
      | None ->         ((elem, info_item),    flow, env), false
      | Some newelem -> ((newelem, info_item), flow, env), true
    )
  in
  g_cprogram := rebuild_info_program cprogram' !g_contain_typedmetavar;

  if not (null children_envs)
  then children_envs
  else begin
    pr2 "Empty list of bindings, I will restart from old env";
    [env]
  end




(* This function returns a triplet option. First the C element (modified),
 * then a binding because there is could be new info brought by the matching,
 * and finally a hack_funheaders list. 
 * 
 * This function does not use the global, so could put it before,
 * but more logical to make it follows the other process_xxx functions.
 *)
and process_a_ctl_a_env_a_celem2 = 
 fun (celem, info) (ctl, used_after_list, rulenb) binding -> 
  indent_do (fun () -> 
  match info with
  | None -> None
  | Some info -> 

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
      | Right x -> pr2 ("Unable to find a value for " ^ x); None
      | Left (trans_info, returned_any_states, newbinding) ->
          (* modify also the proto if FunHeader was touched *)
          let hack_funheaders = 
            trans_info +> Common.map_filter (fun (_nodi, binding, rule_elem) ->
              match Ast_cocci.unwrap rule_elem with
              | Ast_cocci.FunHeader (a,b,c,d,e,f,g,h) -> 
		  let res = (binding, (a,b,c,d,e,f,g,h)) in
                  Some (Ast_cocci.rewrap rule_elem res)
              | _ -> None
            )  
          in
          
          if not returned_any_states
          then None
          else begin
            show_or_not_celem "found match in" celem;
            show_or_not_trans_info trans_info;
            show_or_not_binding "out" newbinding;
            if (null trans_info)
            then Some (celem, false, newbinding, hack_funheaders)
            else 

              (* I do the transformation on flow, not fixed_flow, 
                 because the flow_to_ast need my extra information. *)
              let flow' = (* can do via side effect now *)
                match () with 
                | _ when !Flag_engine.use_cocci_vs_c_3 -> 
                       Transformation3.transform trans_info info.flow
                | _ -> Transformation.transform trans_info info.flow 
              in
              let celem' = 
                if !Flag_engine.use_ref
                then celem (* done via side effect *)
                else flow_to_ast flow' 
              in
              Some (celem', true, newbinding, hack_funheaders)
          end
      )
  )
and process_a_ctl_a_env_a_celem  a b c = 
  Common.profile_code "process_a_ctl_a_env_a_celem" 
    (fun () -> process_a_ctl_a_env_a_celem2 a b c)


(* --------------------------------------------------------------------- *)
let process_hack_funheaders2 hack_funheaders = 

  (* Last fix.
   *
   * todo: what if the function is modified two times ? we should
   * modify the prototype as soon as possible, not wait until the end
   * of all the ctl rules 
   *)
  if !Flag.show_misc then pr2 ("hack headers");
      
  hack_funheaders +> List.iter (fun info ->
    let (binding, (a,b,c,d,e,f,g,h)) = Ast_cocci.unwrap info in
        
    let cprogram' = 
      !g_cprogram +> List.map (fun ((ebis, info_item), flow, env) -> 
        let ebis', modified = 
          match ebis with
          | Ast_c.Declaration 
              (Ast_c.DeclList 
                  ([((Some ((s, None), iis)), 
                    (qu, (Ast_c.FunctionType ft, iity)), 
                    storage),
                   []
                  ], iiptvirg::iifake::iisto))  -> 
              (try
                  Transformation.transform_proto
                    (Ast_cocci.rewrap info
			(Ast_cocci.FunHeader (a,b,c,d,e,f,g,h)))
                    (((Control_flow_c.FunHeader 
                          ((s, ft, storage), 
                          iis++iity++[iifake]++iisto)), []),"")
                    binding (qu, iiptvirg) h
                  +> (fun x -> x,  true)
                with Transformation.NoMatch -> (ebis, false)
              )
          | x -> (x, false)
        in
        (((ebis', info_item), flow, env), modified)
      ) 
    in
    g_cprogram := rebuild_info_program cprogram' !g_contain_typedmetavar;
  )

let process_hack_funheaders a = 
  Common.profile_code "hack_headers" (fun () -> process_hack_funheaders2 a)




(* --------------------------------------------------------------------- *)

(* Returns nothing. The output is in the file outfile *)
let full_engine2 cfile coccifile_and_iso_or_ctl outfile = 
  if not (Common.lfile_exists cfile)
  then failwith (Printf.sprintf "can't find file %s" cfile);

  (* preparing the inputs (c, cocci, ctl) *)
  let (ctls, error_words_julia, contain_typedmetavar) = 
    (match coccifile_and_iso_or_ctl with
    | Left (coccifile, isofile) -> 
        let (astcocci,used_after_lists,toks)= sp_from_file coccifile isofile in
        let ctls = ctls astcocci used_after_lists in
        let ctls_asts = zip ctls (List.map (fun x -> Some x) astcocci) in

        show_or_not_cfile   cfile;
        show_or_not_cocci   coccifile isofile;
        show_or_not_ctl_tex astcocci ctls;

        (zip ctls_asts used_after_lists,
         toks,
         sp_contain_typed_metavar astcocci
        )

    | Right ctl ->([([(ctl,([],[]))],None), []]), [], true (* typed metavar *)
    )
  in

  (* optimisation allowing to launch coccinelle on all the drivers *)
  if not (worth_trying cfile error_words_julia)
  then Common.command2 ("cp " ^ cfile ^ " " ^ outfile)
  else begin
    
   (* parsing and build CFG *)
    g_cprogram:= build_info_program cfile contain_typedmetavar TAC.initial_env;
    g_hack_funheaders := [];
    g_contain_typedmetavar := contain_typedmetavar;

    flush stdout;
    flush stderr;
    Format.print_newline();
    Common.pr_xxxxxxxxxxxxxxxxx();
    pr "let's go";
    Common.pr_xxxxxxxxxxxxxxxxx();

    process_ctls (Common.index_list_1 ctls) [Ast_c.emptyMetavarsBinding];
    process_hack_funheaders !g_hack_funheaders;

    Common.pr_xxxxxxxxxxxxxxxxx ();
    pr "Finished";
    Common.pr_xxxxxxxxxxxxxxxxx();

    (* and now unparse everything *)
    let cprogram' = !g_cprogram +> List.map (fun ((ebis,info_item),_cfg,_e) ->
      (ebis, info_item), Unparse_c.PPviastr) 
    in
    cfile_from_program cprogram' outfile;

    show_or_not_diff cfile outfile;
  end


let full_engine a b c = 
  Common.profile_code "full_engine" (fun () -> full_engine2 a b c)

