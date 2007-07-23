open Common open Commonop

module CCI = Ctlcocci_integration
module TAC = Type_annoter_c

(*****************************************************************************)
(* This file is a kind of driver. It gathers all the important functions 
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
let cprogram_of_file file = 
  let (program2, _stat) = Parse_c.parse_print_error_heuristic file in
  program2 

let cfile_of_program program2_with_ppmethod outf = 
  Unparse_c.pp_program program2_with_ppmethod outf

(* --------------------------------------------------------------------- *)
(* Cocci related *)
(* --------------------------------------------------------------------- *)
let sp_of_file file iso    = Parse_cocci.process file iso false

(* --------------------------------------------------------------------- *)
(* Flow related *)
(* --------------------------------------------------------------------- *)
let print_flow flow = 
  Ograph_extended.print_ograph_mutable flow "/tmp/test.dot" 


let ast_to_flow_with_error_messages2 x =
  let flowopt = 
    try Ast_to_flow.ast_to_control_flow x
    with Ast_to_flow.Error x -> 
      Ast_to_flow.report_error x;
      None
  in
  flowopt +> do_option (fun flow -> 
    (* This time even if there is a deadcode, we still have a
     * flow graph, so I can try the transformation and hope the
     * deadcode will not bother us. 
     *)
    try Ast_to_flow.deadcode_detection flow
    with Ast_to_flow.Error (Ast_to_flow.DeadCode x) -> 
      Ast_to_flow.report_error (Ast_to_flow.DeadCode x);
  );
  flowopt
let ast_to_flow_with_error_messages a = 
  Common.profile_code "flow" (fun () -> ast_to_flow_with_error_messages2 a)


(* --------------------------------------------------------------------- *)
(* Ctl related *)
(* --------------------------------------------------------------------- *)
let ctls_of_ast ast ua  =
  List.map2
    (function ast -> function ua ->
      List.combine
	(if !Flag.popl
	then Popl.popl ast
	else Asttoctl2.asttoctl ast ua)
	(Asttomember.asttomember ast ua))
    ast ua

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

(* the inputs *)

let show_or_not_cfile2 cfile =
  if !Flag.show_c then begin
    Common.pr2_xxxxxxxxxxxxxxxxx ();
    pr2 ("processing C file: " ^ cfile);
    Common.pr2_xxxxxxxxxxxxxxxxx ();
    Common.command2 ("cat " ^ cfile);
  end
let show_or_not_cfile a = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_cfile2 a)

let show_or_not_cfiles cfiles = List.iter show_or_not_cfile cfiles


let show_or_not_cocci2 coccifile isofile = 
  if !Flag.show_cocci then begin
    Common.pr2_xxxxxxxxxxxxxxxxx ();
    pr2 ("processing semantic patch file: " ^ coccifile);
    isofile +> (fun s -> pr2 ("with isos from: " ^ s));
    Common.pr2_xxxxxxxxxxxxxxxxx ();
    Common.command2 ("cat " ^ coccifile);
    pr2 "";
  end
let show_or_not_cocci a b = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_cocci2 a b)


(* the output *)

let show_or_not_diff2 cfile outfile show_only_minus = 
  if !Flag.show_diff then begin
    (* may need --strip-trailing-cr under windows *)
    pr2 "diff = ";

    let xs =
      match !Flag_parsing_c.diff_lines with
      | None ->   Common.cmd_to_list ("diff -u -b -B " ^ cfile ^ " " ^ outfile)
      | Some n -> Common.cmd_to_list ("diff -U "^n^" -b -B "^cfile^" "^outfile)
    in
    xs +> List.iter (fun s -> 
      if s =~ "^\\+[^+]" && show_only_minus
      then ()
      else pr s
    )
  end
let show_or_not_diff a b c  = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_diff2 a b c)


(* the derived input *)

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

    Common.pr_xxxxxxxxxxxxxxxxx ();
    pr ("rule " ^ i_to_s rulenb ^ " = ");
    Common.pr_xxxxxxxxxxxxxxxxx ();
      adjust_pp_with_indent (fun () -> 
        Format.force_newline();
        Pretty_print_cocci.print_plus_flag := true;
        Pretty_print_cocci.print_minus_flag := true;
        Pretty_print_cocci.unparse ast;
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



(* running information *)

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
              Pretty_print_engine.pp_binding subst;
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
      Pretty_print_engine.pp_binding binding;
    )
  end
let show_or_not_binding a b  = 
  Common.profile_code "show_xxx" (fun () -> show_or_not_binding2 a b)



(*****************************************************************************)
(* Some  helpers functions *)
(*****************************************************************************)
let worth_trying cfiles tokens = 
  if not !Flag.windows && not (null tokens)
  then
   (* could also modify the code in get_constants.ml *)
    let tokens = tokens +> List.map (fun s -> 
      match () with 
      | _ when s =~ "^[A-Za-z_][A-Za-z_0-9]*$" -> 
          "\\b" ^ s ^ "\\b"

      | _ when s =~ "^[A-Za-z_]" -> 
          "\\b" ^ s

      | _ when s =~ ".*[A-Za-z_]$" -> 
          s ^ "\\b"
      | _ -> s

    ) in
    let com = sprintf "egrep -q '(%s)' %s" (join "|" tokens) (join " " cfiles)
    in
    (match Sys.command com with
    | 0 (* success *) -> true
    | _ (* failure *) ->
	(if !Flag.show_misc
	then Printf.printf "grep failed: %s\n" com);
	false (* no match, so not worth trying *)
    )
  else true

let check_macro_in_sp_and_adjust tokens = 
  tokens +> List.iter (fun s -> 
    if Hashtbl.mem !Parsing_hacks._defs s
    then begin
      pr2 "warning: macro in semantic patch was in macro definitions";
      pr2 ("disabling macro expansion for " ^ s);
      Hashtbl.remove !Parsing_hacks._defs s
    end
  )


let contain_loop top = 
  let res = ref false in
  top +> Visitor_c.vk_program { Visitor_c.default_visitor_c with
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
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode
      donothing donothing donothing donothing
      donothing expression donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing 
  in
  toplevel_list_list +> 
    List.exists
    (function (nm,deps,rule) ->
      (List.exists combiner.Visitor_ast.combiner_top_level rule))
    

(* --------------------------------------------------------------------- *)
(* #include relative position in the file *)
(* --------------------------------------------------------------------- *)

(* compute the set of new prefixes
 * on 
 *  "a/b/x"; (* in fact it is now a list of string so  ["a";"b";"x"] *)
 *  "a/b/c/x";
 *  "a/x";
 *  "b/x";
 * it would give for the first element 
 *   ""; "a"; "a/b"; "a/b/x"
 * for the second
 *   "a/b/c/x"
 *)

let compute_new_prefixes xs = 
  xs +> Common.map_withenv (fun already xs -> 
    let subdirs_prefixes = Common.inits xs in
    let new_first = subdirs_prefixes +> List.filter (fun x -> 
      not (List.mem x already)
    )
    in
    new_first, 
    new_first @ already
  ) []
  +> fst


(* does via side effect on the ref in the Include in Ast_c *)
let rec update_include_rel_pos cs =
  let only_include = cs +> Common.map_filter (fun c -> 
    match c with 
    | Ast_c.Include ((x,_),aref) ->
        (match x with
        | Ast_c.Wierd _ -> None
        | _ -> Some (x, aref)
        )
    | _ -> None
  )
  in
  let (locals, nonlocals) = 
    only_include +> Common.partition_either (fun (c, aref)  -> 
      match c with
      | Ast_c.Local x -> Left (x, aref)
      | Ast_c.NonLocal x -> Right (x, aref)
      | Ast_c.Wierd x -> raise Impossible
    ) in

  update_rel_pos_bis locals;
  update_rel_pos_bis nonlocals;
  cs
and update_rel_pos_bis xs = 
  let xs' = List.map fst xs in
  let the_first = compute_new_prefixes xs' in
  let the_last  = List.rev (compute_new_prefixes (List.rev xs')) in
  let merged = Common.zip xs (Common.zip the_first the_last) in
  merged +> List.iter (fun ((x, aref), (the_first, the_last)) -> 
    aref := Some 
      { 
        Ast_c.first_of = the_first;
        Ast_c.last_of = the_last;
      }
  )
        




(*****************************************************************************)
(* All the information needed around the C elements and Cocci rules *)
(*****************************************************************************)

type toplevel_c_info = { 
  ast_c: Ast_c.toplevel; (* contain refs so can be modified *)
  tokens_c: Parser_c.token list;
  fullstring: string;

  flow: Control_flow_c.cflow option; (* it's the "fixed" flow *)
  contain_loop: bool;
  
  env_typing_before: TAC.environment;
  env_typing_after:  TAC.environment;

  was_modified: bool ref;

  (* id: int *)
}

type toplevel_cocci_info = {
  ctl: Lib_engine.ctlcocci * (CCI.pred list list);
  ast_rule: Ast_cocci.rule;

  rulename: string;
  dependencies: Ast_cocci.dependency list;
  used_after: Ast_cocci.meta_name list;

  ruleid: int;

  was_matched: bool ref;
}

type kind_file = Header | Source 
type file_info = { 
  fname : string;
  was_modified_once: bool ref;
  asts: toplevel_c_info list;
  fpath : string;
  fkind : kind_file;
}

let g_contain_typedmetavar = ref false 

let fake_env = TAC.initial_env



let last_env_toplevel_c_info xs =
  (Common.last xs).env_typing_after

let concat_headers_and_c ccs = 
  (List.concat (ccs +> List.map (fun x -> x.asts)))

let for_unparser xs = 
  xs +> List.map (fun x -> 
    (x.ast_c, (x.fullstring, x.tokens_c)), Unparse_c.PPviastr
  )

(* --------------------------------------------------------------------- *)
let prepare_cocci ctls used_after_lists astcocci = 

  let gathered = Common.index_list_1 (zip (zip ctls astcocci) used_after_lists)
  in
  gathered +> List.map 
    (fun (((ctl_toplevel_list,ast),used_after_list),rulenb) -> 
      
      if not (List.length ctl_toplevel_list = 1)
      then failwith "not handling multiple minirules";

      let (rulename, dependencies, restast) = ast in
      { 
        ctl = List.hd ctl_toplevel_list;
        ast_rule = ast;
        rulename = rulename;
        dependencies = dependencies;
        used_after = List.hd used_after_list;
        ruleid = rulenb;
        was_matched = ref false;
      }
    )


(* --------------------------------------------------------------------- *)

let build_info_program cprogram env = 
  let (cs, parseinfos) = Common.unzip cprogram in
  let (cs, envs) = 
    if !g_contain_typedmetavar 
    then Common.unzip (TAC.annotate_program env cs)
    else Common.unzip (cs +> List.map (fun c -> c, (fake_env, fake_env)))
  in

  zip (zip cs parseinfos) envs +> List.map (fun ((c, parseinfo), (enva,envb))->
    let (fullstr, tokens) = parseinfo in

    {
      ast_c = c; (* contain refs so can be modified *)
      tokens_c =  tokens;
      fullstring = fullstr;

      flow = ast_to_flow_with_error_messages c +> map_option (fun flow -> 
        (* remove the fake nodes for julia *)
        let fixed_flow = CCI.fix_flow_ctl flow in

        if !Flag.show_flow then print_flow fixed_flow;
        if !Flag.show_before_fixed_flow then print_flow flow;

        fixed_flow
      );

      contain_loop = contain_loop c;
  
      env_typing_before = enva;
      env_typing_after = envb;

      was_modified = ref false;
    }
  )



(* Optimisation. Try not unparse/reparse the whole file when have modifs  *)
let rebuild_info_program cs = 
  cs +> List.map (fun c ->
    if !(c.was_modified)
    then begin
      let file = Common.new_temp_file "cocci_small_output" ".c" in
      cfile_of_program 
        [(c.ast_c, (c.fullstring, c.tokens_c)), Unparse_c.PPnormal] file;

      (* Common.command2 ("cat " ^ file); *)
      let cprogram = cprogram_of_file file in
      let xs = build_info_program cprogram c.env_typing_before in

      (* TODO: assert env has not changed,
       * if yes then must also reparse what follows even if not modified.
       * Do that only if contain_typedmetavar of course, so good opti.
       *)
      (* Common.list_init xs *) (* get rid of the FinalDef *)
      xs
    end
    else [c]
  ) +> List.concat


let rebuild_info_c_and_headers ccs = 
  ccs +> List.iter (fun c_or_h -> 
    if c_or_h.asts +> List.exists (fun c -> !(c.was_modified))
    then c_or_h.was_modified_once := true;
  );
  ccs +> List.map (fun c_or_h -> 
    { c_or_h with asts = rebuild_info_program c_or_h.asts }
  )





(* finding among the #include the one that we need to parse
 * because they may contain useful type definition or because
 * we may have to modify them
 * 
 * For the moment we base in part our heuristic on the name of the file.
 * serio.c is related to #include <linux/serio.h> 
 *)

let includes_to_parse xs = 
  xs +> List.map (fun (file, cs) -> 
    let dir = Common.dirname file in

    cs +> Common.map_filter (fun (c,_info_item) -> 
      match c with
      | Ast_c.Include ((x,ii),info_h_pos)  -> 
          (match x with
          | Ast_c.Local xs -> 
              Some (Filename.concat dir (Common.join "/" xs))
          | Ast_c.NonLocal xs -> 
              if Common.fileprefix (Common.last xs) = Common.fileprefix file 
              then 
                Some (Filename.concat !Flag.include_path (Common.join "/" xs))
              else None
        | Ast_c.Wierd _ -> None
          )

      | _ -> None
    )
  )
  +> List.concat
  +> Common.uniq




let prepare_c files = 
  let cprograms = List.map cprogram_of_file files in
  let includes = includes_to_parse (zip files cprograms) in

  (* todo?: may not be good to first have all the headers and then all the c *)
  let all = 
    (includes +> List.map (fun hpath -> Right hpath))
    ++
    ((zip files cprograms) +> List.map (fun (file, asts) -> Left (file, asts)))
  in

  let env = ref TAC.initial_env in

  let ccs = all +> Common.map_filter (fun x -> 
    match x with 
    | Right hpath -> 
        if not (Common.lfile_exists hpath) 
        then begin 
          pr2 ("TYPE: header " ^ hpath ^ " not found"); 
          None 
        end
        else 
          let h_cs = cprogram_of_file hpath in
          let info_h_cs = build_info_program h_cs !env in
          env := 
            if null info_h_cs
            then !env
            else last_env_toplevel_c_info info_h_cs
          ;
          Some { 
            fname = Common.basename hpath;
            asts = info_h_cs;
            was_modified_once = ref false;
            fpath = hpath;
            fkind = Header;
          }
    | Left (file, cprogram) -> 
        (* todo?: don't update env ? *)
        let cs = build_info_program cprogram !env in
        (* we do that only for the c, not for the h *)
        ignore(update_include_rel_pos (cs +> List.map (fun x -> x.ast_c)));
        Some { 
          fname = Common.basename file;
          asts = cs;
          was_modified_once = ref false;
          fpath = file;
          fkind = Source;
        }
  ) 
  in
  ccs        


(*****************************************************************************)
(* Processing the ctls and toplevel C elements *)
(*****************************************************************************)

(* The main algorithm =~
 * The algorithm is roughly: 
 *  for_all ctl rules in SP
 *   for_all minirule in rule (no more)
 *    for_all binding (computed during previous phase)
 *      for_all C elements
 *         match control flow of function vs minirule 
 *         with the binding and update the set of possible 
 *         bindings, and returned the possibly modified function.
 *   pretty print modified C elements and reparse it.
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


(* r(ule), c(element in C code), e(nvironment) *)

let rec bigloop2 rs ccs = 
  let es = ref [Ast_c.emptyMetavarsBinding] in
  let ccs = ref ccs in
  let rules_that_have_matched = ref [] in

  (* looping over the rules *)
  rs +> List.iter (fun r -> 
   show_or_not_ctl_text r.ctl r.ast_rule r.ruleid;

   if not
       (List.for_all (function
         | Ast_cocci.Dep s     ->     (List.mem s !rules_that_have_matched)
         | Ast_cocci.AntiDep s -> not (List.mem s !rules_that_have_matched)
       ) r.dependencies)
   then
     begin
       if !Flag.show_misc
       then pr2 ("dependencies for rule " ^ r.rulename ^ " not satisfied")
     end
   else begin

    let newes = ref [] in (* envs for next round/rule *)

    (* looping over the environments *)
    !es +> List.iter (fun e -> 
      show_or_not_binding "in" e;

      let children_e = ref [] in
      
      (* looping over the functions and toplevel elements in .h and .h *)
      concat_headers_and_c !ccs +> List.iter (fun c -> 
        if c.flow <> None 
        then
          (* does also some side effects on c and r *)
          match process_a_ctl_a_env_a_toplevel r e c with
          | None -> ()
          | Some newbindings -> 
              newbindings +> List.iter (fun newbinding -> 
                children_e := Common.insert_set newbinding !children_e;
              )
      ); (* end iter cs *)

      let children_e_final = 
        if not (null !children_e)
        then !children_e
        else begin
          if !Flag_ctl.partial_match
          then printf "Empty list of bindings, I will restart from old env";
          [e +> List.filter (fun (s,v) -> List.mem s r.used_after)]
        end
      in
          
      newes := Common.union_set !newes children_e_final;
      
    ); (* end iter es *)
    es := !newes;

    (* apply the tagged modifs and reparse *)
    if not !Flag_parsing_cocci.sgrep_mode2
    then ccs := rebuild_info_c_and_headers !ccs;
      
    if !(r.was_matched) then Common.push2 r.rulename rules_that_have_matched
   end
  ); (* end iter rs *)

  (if !Flag_parsing_cocci.sgrep_mode2
  then begin
    Flag_parsing_c.verbose_parsing := false;
    ccs := rebuild_info_c_and_headers !ccs
  end);
  !ccs (* return final C asts *)

and bigloop a b = 
  Common.profile_code "bigloop" (fun () -> bigloop2 a b)





(* does side effects on C ast and on Cocci info rule *)
and process_a_ctl_a_env_a_toplevel2 r e c = 
 indent_do (fun () -> 

  let (trans_info, returned_any_states, newbindings) = 
    Common.save_excursion Flag_ctl.loop_in_src_code (fun () -> 
      Flag_ctl.loop_in_src_code := !Flag_ctl.loop_in_src_code||c.contain_loop;
      
      (***************************************)
      (* !Main point! The call to the engine *)
      (***************************************)
      let model_ctl  = CCI.model_for_ctl (Common.some c.flow) e in
      CCI.mysat model_ctl r.ctl (r.used_after, e)
    ) 
  in
  if not returned_any_states 
  then None
  else begin
    show_or_not_celem "found match in" c.ast_c;
    show_or_not_trans_info trans_info;
    List.iter (show_or_not_binding "out") newbindings;    

    r.was_matched := true;

    if not (null trans_info)
    then begin
      c.was_modified := true;
      (* modify ast via side effect *)
      ignore(Transformation3.transform r.rulename trans_info (some c.flow));
    end;

    Some newbindings
  end
 )
   
and process_a_ctl_a_env_a_toplevel  a b c = 
  Common.profile_code "process_a_ctl_a_env_a_toplevel" 
    (fun () -> process_a_ctl_a_env_a_toplevel2 a b c)
   


(*****************************************************************************)
(* The main function *)
(*****************************************************************************)

let _hparse = Hashtbl.create 101
let _hctl = Hashtbl.create 101

(* Returns nothing. The output is in the file outfile *)
let full_engine2 (coccifile, isofile) cfiles = 

  show_or_not_cfiles  cfiles;
  show_or_not_cocci   coccifile isofile;

  let (astcocci,used_after_lists,toks) = 
    Common.memoized _hparse (coccifile, isofile) (fun () -> 
      sp_of_file coccifile (Some isofile) 
    )
  in
  let ctls = 
    Common.memoized _hctl (coccifile, isofile) (fun () -> 
      ctls_of_ast  astcocci used_after_lists
    )
  in

  let contain_typedmetavar = sp_contain_typed_metavar astcocci in

  (* optimisation allowing to launch coccinelle on all the drivers *)
  if not (worth_trying cfiles toks)
  then begin 
    pr2 ("not worth trying:" ^ Common.join " " cfiles);
    cfiles +> List.map (fun s -> s, None)
  end
  else begin

    if !Flag.show_misc then Common.pr_xxxxxxxxxxxxxxxxx();
    if !Flag.show_misc then pr "let's go";
    if !Flag.show_misc then Common.pr_xxxxxxxxxxxxxxxxx();

    g_contain_typedmetavar := contain_typedmetavar;

    check_macro_in_sp_and_adjust toks;

    let cocci_infos = prepare_cocci ctls used_after_lists astcocci in
    let c_infos  = prepare_c cfiles in

    show_or_not_ctl_tex astcocci ctls;

    (* ! the big loop ! *)
    let c_infos' = bigloop cocci_infos c_infos in

    if !Flag.show_misc then Common.pr_xxxxxxxxxxxxxxxxx ();
    if !Flag.show_misc then pr "Finished";
    if !Flag.show_misc then Common.pr_xxxxxxxxxxxxxxxxx();

    c_infos' +> List.map (fun c_or_h -> 
      if !(c_or_h.was_modified_once)
      then begin
        let outfile = Common.new_temp_file "cocci-output" ("-" ^ c_or_h.fname) 
        in

        if c_or_h.fkind = Header 
        then pr2 ("a header file was modified: " ^ c_or_h.fname);

        (* and now unparse everything *)
        cfile_of_program (for_unparser c_or_h.asts) outfile;

        let show_only_minus = !Flag_parsing_cocci.sgrep_mode2 in
        show_or_not_diff c_or_h.fpath outfile show_only_minus;

        (c_or_h.fpath, 
        if !Flag_parsing_cocci.sgrep_mode2 then None else Some outfile
        )
      end
      else 
        (c_or_h.fpath, None)
    );
  end

let full_engine a b = 
  Common.profile_code "full_engine" (fun () -> full_engine2 a b)
