(*
 * Copyright 2012, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./ctlcocci_integration.ml"
open Common

open Ograph_extended

module F = Control_flow_c

(*****************************************************************************)
(* Debugging functions *)
(*****************************************************************************)
let show_or_not_predicate pred =
  if !Flag_matcher.debug_engine then begin
    indent_do (fun () ->
      adjust_pp_with_indent_and_header "labeling: pred = " (fun () ->
        Pretty_print_engine.pp_predicate pred;
      );
    )
  end

let show_or_not_nodes nodes =
  if !Flag_matcher.debug_engine  then begin
    indent_do (fun () ->
      adjust_pp_with_indent_and_header "labeling: result = " (fun () ->
        Common.pp_do_in_box (fun () ->
          pp "{";
          Common.print_between
            (fun () -> pp ";"; Format.print_cut())
            (fun (nodei, (_predTODO, subst)) ->
              Format.print_int nodei;
              Common.pp_do_in_box (fun () ->
                Pretty_print_engine.pp_binding2_ctlsubst subst
              )
            ) nodes;
          pp "}";
        );
      )
    )
  end

let show_isos rule_elem =
  match Ast_cocci.get_isos rule_elem with
    [] -> ()
  | isos ->
      let line = Ast_cocci.get_line rule_elem in
      Printf.printf "rule elem: ";
      Pretty_print_cocci.rule_elem "" rule_elem;
      Format.print_newline();
      List.iter
	(function (nm,x) ->
	  Printf.printf "    iso: %s(%d): " nm line;
	  Pretty_print_cocci.pp_print_anything x;
	  Format.print_newline())
	isos

(*****************************************************************************)
(* Labeling function *)
(*****************************************************************************)
let (-->) x v = Ast_ctl.Subst (x,v);;

(* Take list of predicate and for each predicate returns where in the
 * control flow it matches, and the set of substitutions for this match.
 *)
let (labels_for_ctl: string list (* dropped isos *) ->
 (nodei * F.node) list -> Lib_engine.metavars_binding ->
 Lib_engine.label_ctlcocci) =
  fun dropped_isos nodes binding ->

   (fun p ->
     show_or_not_predicate p;

     let nodes' = nodes +> List.map (fun (nodei, node) ->
      (* todo? put part of this code in pattern ? *)
      (match p, F.unwrap node with
      | Lib_engine.Paren s,  (F.SeqStart (_, bracelevel, _)) ->
	  let make_var x = ("",i_to_s x) in
          [(nodei, (p,[(s --> (Lib_engine.ParenVal (make_var bracelevel)))]))]
      | Lib_engine.Paren s,  (F.SeqEnd (bracelevel, _)) ->
	  let make_var x = ("",i_to_s x) in
          [(nodei, (p,[(s --> (Lib_engine.ParenVal (make_var bracelevel)))]))]
      | Lib_engine.Paren _, _ -> []
      | Lib_engine.Label s, _ ->
          let labels = F.extract_labels node in
          [(nodei,
	    (p,[(s --> (Lib_engine.LabelVal (Lib_engine.Absolute labels)))]))]
      | Lib_engine.BCLabel s, _ ->
	  (match F.extract_bclabels node with
	    [] -> [] (* null for all nodes that are not break or continue *)
	  | labels ->
              [(nodei,
		(p,[(s -->
		  (Lib_engine.LabelVal (Lib_engine.Absolute labels)))]))])
      | Lib_engine.PrefixLabel s, _ ->
          let labels = F.extract_labels node in
          [(nodei,
	    (p,[(s --> (Lib_engine.LabelVal (Lib_engine.Prefix labels)))]))]

      | Lib_engine.Match (re), _unwrapnode ->
          let substs =
            Pattern_c.match_re_node dropped_isos re node binding
            +> List.map (fun (re', subst) ->
              Lib_engine.Match (re'), subst
            )
          in
          substs +> List.map (fun (p', subst) ->
            (nodei,
              (p',
                subst +> List.map (fun (s, meta) ->
                  s --> Lib_engine.NormalMetaVal meta
                ))))

      | Lib_engine.InLoop,      F.InLoopNode ->  [nodei, (p,[])]
      | Lib_engine.TrueBranch , F.TrueNode _ ->  [nodei, (p,[])]
      | Lib_engine.EscTrueBranch , F.TrueNode esc when !esc ->  [nodei, (p,[])]
      | Lib_engine.FalseBranch, F.FalseNode -> [nodei, (p,[])]
      | Lib_engine.After,       F.AfterNode _ -> [nodei, (p,[])]
      | Lib_engine.FallThrough, F.FallThroughNode -> [nodei,(p,[])]
      | Lib_engine.LoopFallThrough, F.LoopFallThroughNode -> [nodei,(p,[])]
      | Lib_engine.FunHeader,   F.FunHeader _ -> [nodei, (p,[])]
      | Lib_engine.Top,         F.TopNode ->   [nodei, (p,[])]
      | Lib_engine.Exit,        F.Exit ->      [nodei, (p,[])]
      | Lib_engine.ErrorExit,   F.ErrorExit -> [nodei, (p,[])]
      |	Lib_engine.Goto,        F.Goto(_,_,_) -> [nodei, (p,[])]

      | Lib_engine.UnsafeBrace, node ->
	  (* cases where it it not safe to put something on the outer side
	     of braces *)
	  (match node with
	    F.FunHeader _ | F.DoHeader _ | F.TrueNode _ | F.Else _
	  | F.InLoopNode (* while, for *) | F.SwitchHeader _ ->
	      [nodei, (p,[])]
	  | _ -> [])

      | Lib_engine.InLoop , _ -> []
      | Lib_engine.TrueBranch , _ -> []
      | Lib_engine.EscTrueBranch , _ -> []
      | Lib_engine.FalseBranch, _ -> []
      | Lib_engine.After, _ -> []
      | Lib_engine.FallThrough, _ -> []
      | Lib_engine.LoopFallThrough, _ -> []
      | Lib_engine.FunHeader, _  -> []
      | Lib_engine.Top, _  -> []
      | Lib_engine.Exit, _  -> []
      | Lib_engine.ErrorExit, _  -> []
      | Lib_engine.Goto, _  -> []

      |	Lib_engine.BindGood s, _ -> [(nodei, (p,[(s --> Lib_engine.GoodVal)]))]
      |	Lib_engine.BindBad s, _ ->  [(nodei, (p,[(s --> Lib_engine.BadVal)]))]
      |	Lib_engine.FakeBrace, _ ->
	  if F.extract_is_fake node then [nodei, (p,[])] else []

      | Lib_engine.Return, node ->
          (match node with
            (* todo? should match the Exit code ?
             * todo: one day try also to match the special function
             * such as panic();
             *)
          | F.Return _ ->  [nodei, (p,[])]
          | F.ReturnExpr _ -> [nodei, (p,[])]
          | _ -> []
          )
      )
     ) +> List.concat
     in

     show_or_not_nodes nodes';
     nodes'
   )

(*****************************************************************************)
(* Some fix flow, for CTL, for unparse *)
(*****************************************************************************)
(* could erase info on nodes, and edge, because they are not used by rene *)
let (control_flow_for_ctl: F.cflow -> ('a, 'b) ograph_mutable) =
 fun cflow -> cflow



(* Just make the final node of the control flow loop over itself.
 * It seems that one hypothesis of the SAT algorithm is that each node as at
 * least a successor.
 *
 * update: do same for errorexit node.
 *
 * update: also erase the fake nodes (and adjust the edges accordingly),
 * so that AX in CTL can now work.
 * Indeed, à la fin de la branche then (et else), on devrait aller directement
 * au suivant du endif, sinon si ecrit if(1) { foo(); }; bar();
 * sans '...' entre le if et bar(), alors ca matchera pas car le CTL
 * generera un AX bar()  qui il tombera d'abord sur le [endif] :(
 * Mais chiant de changer l'algo de generation, marche pas tres bien avec
 * ma facon de faire recursive et compositionnel.
 * => faire une fonction qui applique des fixes autour de ce control flow,
 * comme ca passe un bon flow a rene, mais garde un flow a moi pour pouvoir
 * facilement generate back the ast.
 * alt: faire un wrapper autourde mon graphe pour lui passer dans le module CFG
 * une fonction qui passe a travers les Fake, mais bof.
 *
 * update: also make loop the deadcode nodes, the one that have
 * no predecessor.
 *)
let (fix_flow_ctl2: F.cflow -> F.cflow) = fun flow ->
  let g = ref flow in

  let topi = F.first_node !g in
  !g#add_arc ((topi, topi), F.Direct);

  (* for the #define CFG who have no Exit but have at least a EndNode *)
  (try
      let endi  = F.find_node (fun x -> x =*= F.EndNode) !g in
      !g#add_arc ((endi, endi), F.Direct);
    with Not_found -> ()
  );

  (* for the regular functions *)
  (try
    let exitnodei  = F.find_node (fun x -> x =*= F.Exit) !g in
    let errornodei = F.find_node (fun x -> x =*= F.ErrorExit) !g in

    !g#add_arc ((exitnodei, exitnodei), F.Direct);

    if null ((!g#successors   errornodei)#tolist) &&
       null ((!g#predecessors errornodei)#tolist)
    then !g#del_node errornodei
    else !g#add_arc ((errornodei, errornodei), F.Direct);
   with Not_found -> ()
  );

  let fake_nodes = !g#nodes#tolist +> List.filter (fun (nodei, node) ->
    match F.unwrap node with
    | F.CaseNode _
    | F.Enter
    (*| F.Fake*) (* [endif], [endswitch], ... *)
      -> true
    | _ -> false
    ) in

  fake_nodes +> List.iter (fun (nodei, node) -> F.remove_one_node nodei !g);

  (* even when have deadcode, julia want loop over those nodes *)
  !g#nodes#tolist +> List.iter (fun (nodei, node) ->
    if (!g#predecessors nodei)#null
    then begin
      let fakei = !g#add_node (F.mk_node F.Fake [] [] "DEADCODELOOP") in
      !g#add_arc ((fakei, nodei), F.Direct);
      !g#add_arc ((fakei, fakei), F.Direct);
    end
  );

  !g#nodes#tolist +> List.iter (fun (nodei, node) ->
    assert (List.length ((!g#successors nodei)#tolist) >= 1);
    (* no:  && List.length ((!g#predecessors nodei)#tolist) >= 1
       because    the enter node at least have no predecessors *)
    );

  !g
let fix_flow_ctl a =
  Common.profile_code "fix_flow" (fun () -> fix_flow_ctl2 a)





(*****************************************************************************)
(* subtil: the label must operate on newflow, not (old) cflow
 * update: now I supposed that we give me a fixed_flow
 *)
let model_for_ctl dropped_isos cflow binding =
 let newflow = cflow (* old: fix_flow_ctl (control_flow_for_ctl cflow) *) in
 let labels = labels_for_ctl dropped_isos (newflow#nodes#tolist) binding  in
 let states = List.map fst  newflow#nodes#tolist  in
 newflow, labels, states


(*****************************************************************************)

module PRED =
  struct
    type t = Lib_engine.predicate
    let print_predicate x =
      Pretty_print_cocci.print_plus_flag := false;
      Pretty_print_cocci.print_minus_flag := false;
      Pretty_print_engine.pp_predicate x
  end

(* prefix has to be nonempty *)
let prefix l1 l2 =
  let rec loop = function
      ([],_) -> true
    | (_,[]) -> false
    | (x::xs,y::ys) when x = y -> loop (xs,ys)
    | _ -> false in
  loop(l1,l2)

let compatible_labels l1 l2 =
  match (l1,l2) with
    (Lib_engine.Absolute(l1),Lib_engine.Absolute(l2)) -> l1 =*= l2
  | (Lib_engine.Absolute(l1),Lib_engine.Prefix(l2))   -> prefix l1 l2
  | (Lib_engine.Prefix(l1),Lib_engine.Absolute(l2))   -> prefix l2 l1
  | (Lib_engine.Prefix(l1),Lib_engine.Prefix(l2))     ->
      not (l1 = []) && not (l2 = []) &&
      List.hd l1 =*= List.hd l2 (* labels are never empty *)

let merge_labels l1 l2 =
  match (l1,l2) with
    (* known to be compatible *)
    (Lib_engine.Absolute(_),Lib_engine.Absolute(_)) -> l1
  | (Lib_engine.Absolute(_),Lib_engine.Prefix(_))   -> l1
  | (Lib_engine.Prefix(_),Lib_engine.Absolute(_))   -> l2
  | (Lib_engine.Prefix(l1),Lib_engine.Prefix(l2))   ->
      let rec max_prefix = function
	  (x::xs,y::ys) when x = y -> x::(max_prefix(xs,ys))
	| (l1,l2) -> [] in
      Lib_engine.Prefix(max_prefix(l1,l2))

module ENV =
  struct
    type value = Lib_engine.metavar_binding_kind2
    type mvar = Ast_cocci.meta_name
    let eq_mvar x x' = x =*= x'
    let eq_val v v' =
      (* v = v' *)
      match (v,v') with
	(Lib_engine.NormalMetaVal(Ast_c.MetaPosVal(min1,max1)),
	 Lib_engine.NormalMetaVal(Ast_c.MetaPosVal(min2,max2))) ->
	   ((min1 <= min2) && (max1 >= max2)) or
	   ((min2 <= min1) && (max2 >= max1))
      |	(Lib_engine.NormalMetaVal(Ast_c.MetaTypeVal a),
	 Lib_engine.NormalMetaVal(Ast_c.MetaTypeVal b)) ->
          C_vs_c.eq_type a b
      |	(Lib_engine.LabelVal(l1),Lib_engine.LabelVal(l2)) ->
	  compatible_labels l1 l2
      |	_ -> v =*= v'
    let merge_val v v' = (* values guaranteed to be compatible *)
      (* v *)
      match (v,v') with
	(Lib_engine.NormalMetaVal(Ast_c.MetaPosVal(min1,max1)),
	 Lib_engine.NormalMetaVal(Ast_c.MetaPosVal(min2,max2))) ->
	   if (min1 <= min2) && (max1 >= max2)
	   then Lib_engine.NormalMetaVal(Ast_c.MetaPosVal(min1,max1))
	   else
	     if (min2 <= min1) && (max2 >= max1)
	     then Lib_engine.NormalMetaVal(Ast_c.MetaPosVal(min2,max2))
	     else failwith "incompatible positions give to merge"
      |	(Lib_engine.NormalMetaVal(Ast_c.MetaTypeVal a),
	 Lib_engine.NormalMetaVal(Ast_c.MetaTypeVal b)) ->
          Lib_engine.NormalMetaVal (Ast_c.MetaTypeVal (C_vs_c.merge_type a b))
      |	(Lib_engine.LabelVal(l1),Lib_engine.LabelVal(l2)) ->
	  Lib_engine.LabelVal(merge_labels l1 l2)

      |	_ -> v
    let print_mvar (_,s) = Format.print_string s
    let print_value x = Pretty_print_engine.pp_binding_kind2 x
  end

module CFG =
  struct
    type node = Ograph_extended.nodei
    type cfg = (F.node, F.edge) Ograph_extended.ograph_mutable
    let predecessors cfg n = List.map fst ((cfg#predecessors n)#tolist)
    let successors   cfg n = List.map fst ((cfg#successors n)#tolist)
    let extract_is_loop cfg n =
      Control_flow_c.extract_is_loop (cfg#nodes#find n)
    let print_node i = Format.print_string (i_to_s i)
    let size cfg = cfg#nodes#length

    (* In ctl_engine, we use 'node' for the node but in the Ograph_extended
     * terminology, this 'node' is in fact an index to access the real
     * node information (that ctl/ wants to abstract away to be more generic),
     * the 'Ograph_extended.nodei'.
     *)
    let print_graph cfg label border_colors fill_colors filename =
      Ograph_extended.print_ograph_mutable_generic cfg label
        (fun (nodei, (node: F.node)) ->
          (* the string julia wants to put ? *)
          let bc = try Some(List.assoc nodei border_colors) with _ -> None in
          let fc = try Some(List.assoc nodei fill_colors) with _ -> None in
          (* the string yoann put as debug information in the cfg *)
          let str = snd node in
          (str,bc,fc)
        )
        ~output_file:filename
        ~launch_gv:false
  end


module WRAPPED_ENGINE = Wrapper_ctl.CTL_ENGINE_BIS (ENV) (CFG) (PRED)

let print_bench _ = WRAPPED_ENGINE.print_bench()

type pred = Lib_engine.predicate * Ast_cocci.meta_name Ast_ctl.modif

(*****************************************************************************)
let metavars_binding2_to_binding   binding2 =
  binding2 +> Common.map_filter (fun (s, kind2) ->
    match kind2 with
    | Lib_engine.NormalMetaVal kind -> Some (s, kind)
    (* I thought it was Impossible to have this when called from
       satbis_to_trans_info but it does not seems so *)
    | Lib_engine.ParenVal _ -> None
    | Lib_engine.LabelVal _ -> None
    | Lib_engine.BadVal     -> None (* should not occur *)
    | Lib_engine.GoodVal    -> None (* should not occur *)
   )

let metavars_binding_to_binding2 binding =
  binding +> List.map (fun (s, kind) -> s, Lib_engine.NormalMetaVal kind)


let (satbis_to_trans_info:
  (int list *
     (nodei * Lib_engine.metavars_binding2 * Lib_engine.predicate)) list ->
  (int list *
     (nodei * Lib_engine.metavars_binding * Ast_cocci.rule_elem)) list) =
  fun xs ->
    xs +> List.fold_left (fun prev (index,(nodei, binding2, pred)) ->
      match pred with
      | Lib_engine.Match (rule_elem) ->
	  if !Flag.track_iso_usage then show_isos rule_elem;
	  (index,
	   (nodei, metavars_binding2_to_binding binding2, rule_elem))
	  ::prev
	     (* see BindGood in asttotctl2 *)
      | Lib_engine.BindGood (_) -> prev
      | _ -> raise (Impossible 50)
    ) []

(*****************************************************************************)

let rec coalesce_positions = function
    [] -> []
  | (x,Ast_c.MetaPosValList l)::rest ->
      let (same,others) = List.partition (function (x1,_) -> x =*= x1) rest in
      let ls =
	List.concat
	  (List.map
	     (function
		 (_,Ast_c.MetaPosValList l) -> l
	       | _ -> failwith "unexpected non-position")
	     same) in
      let new_ls = List.sort compare (l@ls) in
      (x,Ast_c.MetaPosValList new_ls) :: coalesce_positions others
  | x::rest -> x :: coalesce_positions rest

let strip env =
  List.map
    (function (v,vl) ->
      let vl =
	match vl with
	  Ast_c.MetaExprVal (a,c) ->
	    Ast_c.MetaExprVal(Lib_parsing_c.al_inh_expr a,c)
	| Ast_c.MetaExprListVal a ->
	    Ast_c.MetaExprListVal(Lib_parsing_c.al_inh_arguments a)
	| Ast_c.MetaStmtVal a ->
	    Ast_c.MetaStmtVal(Lib_parsing_c.al_inh_statement a)
	| Ast_c.MetaInitVal a ->
	    Ast_c.MetaInitVal(Lib_parsing_c.al_inh_init a)
	| Ast_c.MetaInitListVal a ->
	    Ast_c.MetaInitListVal(Lib_parsing_c.al_inh_inits a)
	| x -> (*don't contain binding info*) x in
      (v,vl))
    env

    (* these remove constraints, at least those that contain pcre regexps,
       which cannot be compared (problem in the unparser) *)
let strip_predicate re =
  let donothing r k e = k e in
  let mcode mc = mc in
  let ident r k e =
    let e = k e in
    match Ast_cocci.unwrap e with
      Ast_cocci.MetaId(name,constraints,u,i) ->
        Ast_cocci.rewrap e
	  (Ast_cocci.MetaId(name,Ast_cocci.IdNoConstraint,u,i))
    |  Ast_cocci.MetaFunc(name,constraints,u,i) ->
        Ast_cocci.rewrap e
	  (Ast_cocci.MetaFunc(name,Ast_cocci.IdNoConstraint,u,i))
    |  Ast_cocci.MetaLocalFunc(name,constraints,u,i) ->
        Ast_cocci.rewrap e
	  (Ast_cocci.MetaLocalFunc(name,Ast_cocci.IdNoConstraint,u,i))
    |  _ -> e in
  let expression r k e =
    let e = k e in
    match Ast_cocci.unwrap e with
      Ast_cocci.MetaErr(name,constraints,u,i) ->
	Ast_cocci.rewrap e
	  (Ast_cocci.MetaErr(name,Ast_cocci.NoConstraint,u,i))
    | Ast_cocci.MetaExpr(name,constraints,u,ty,form,i) ->
        Ast_cocci.rewrap e
	  (Ast_cocci.MetaExpr(name,Ast_cocci.NoConstraint,u,ty,form,i))
    | _ -> e in
  let fn = Visitor_ast.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing
      ident expression donothing donothing donothing donothing
      donothing donothing
      donothing donothing donothing donothing donothing donothing in

  fn.Visitor_ast.rebuilder_rule_elem re

let clean_trans_info2 trans_info2 =
  List.map
    (function a ->
      (List.map
	 (function (node,env,pred) ->
	   let pred =
	     match pred with
	       Lib_engine.Match re -> Lib_engine.Match (strip_predicate re)
	     | _ -> pred in
	   (node,env,pred))
	 a))
    trans_info2

let rec nub ls =
  match ls with
    [] -> []
  | (x::xs) when (List.mem x xs) -> nub xs
  | (x::xs) -> x::(nub xs)

(*****************************************************************************)
(* Call ctl engine *)
(***************************************************** ************************)
let (mysat2:
  Lib_engine.model ->
  (Lib_engine.ctlcocci * (pred list list)) ->
  (string (*rulename*) * Lib_engine.mvar list*Lib_engine.metavars_binding) ->
  (Lib_engine.numbered_transformation_info * bool *
     Lib_engine.metavars_binding * Lib_engine.metavars_binding list)) =
  fun (flow, label, states) ctl (rulename, used_after, binding) ->
    let binding2 = metavars_binding_to_binding2 binding in
    let (triples,(trans_info2, returned_any_states, used_after_envs)) =
      WRAPPED_ENGINE.satbis (flow, label, states) ctl
	(used_after, binding2)
    in
    (* drop constraints in the predicate at the end of each match.
       constraints aren't needed for transformation, and they can contain
       regular expressions, which are incomparable. *)
    let trans_info2 = clean_trans_info2 trans_info2 in
    if not (!Flag_parsing_cocci.sgrep_mode || !Flag.sgrep_mode2 ||
            !Flag_matcher.allow_inconsistent_paths)
    then Check_reachability.check_reachability rulename triples flow;
    let (trans_info2,used_after_fresh_envs) =
      Postprocess_transinfo.process used_after binding2 trans_info2 in
    let used_after_envs =
      Common.uniq(List.map2 (@) used_after_fresh_envs used_after_envs) in
    let trans_info = satbis_to_trans_info trans_info2 in
    let newbindings = List.map metavars_binding2_to_binding used_after_envs in
    let newbindings = List.map coalesce_positions newbindings in
    let newbindings = List.map strip newbindings in
    let newbindings = nub newbindings in
    (trans_info, returned_any_states, binding, newbindings)

let mysat a b c =
  Common.profile_code "mysat" (fun () -> mysat2 a b c)
