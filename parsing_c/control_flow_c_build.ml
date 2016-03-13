(* Yoann Padioleau, extensions by Julia Lawall
 *
 * Copyright (C) 2011, 2012, 2013, INRIA.
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2006, 2007 Ecole des Mines de Nantes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Ast_c
open Control_flow_c

module Lib = Lib_parsing_c

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_cfg

(*****************************************************************************)
(* todo?: compute target level with goto (but rare that different I think)
 * ver1: just do init,
 * ver2: compute depth of label (easy, intercept compound in the visitor)
 *
 * checktodo: after a switch, need check that all the st in the
 * compound start with a case: ?
 *
 * checktodo: how ensure that when we call aux_statement recursively, we
 * pass it xi_lbl and not just auxinfo ? how enforce that ?
 * in fact we must either pass a xi_lbl or a newxi
 *
 * todo: can have code (and so nodes) in many places, in the size of an
 * array, in the init of initializer, but also in StatementExpr, ...
 *
 * todo?: steal code from CIL ? (but seems complicated ... again) *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type error =
  | DeadCode          of Common.parse_info option
  | CaseNoSwitch      of Common.parse_info
  | OnlyBreakInSwitch of Common.parse_info
  | WeirdSwitch       of Common.parse_info
  | NoEnclosingLoop   of Common.parse_info
  | GotoCantFindLabel of string * Common.parse_info
  | NoExit of Common.parse_info
  | DuplicatedLabel of string
  | NestedFunc
  | ComputedGoto
  | Define of Common.parse_info

exception Error of error

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let add_node node labels nodestr g =
   g#add_node (Control_flow_c.mk_node node labels [] nodestr)
let add_bc_node node labels parent_labels nodestr g =
   g#add_node (Control_flow_c.mk_node node labels parent_labels  nodestr)
let add_arc_opt (starti, nodei) g =
  starti +> do_option (fun starti -> g#add_arc ((starti, nodei), Direct))


let lbl_0 = []

let pinfo_of_ii ii = Ast_c.get_opi (List.hd ii).Ast_c.pinfo



(*****************************************************************************)
(* Contextual information passed in aux_statement *)
(*****************************************************************************)

(* Sometimes have a continue/break and we must know where we must jump.
 *
 * ctl_brace: The node list in context_info record the number of '}' at the
 * context point, for instance at the switch point. So that when deeper,
 * we can compute the difference between the number of '}' from root to
 * the context point to close the good number of '}' . For instance
 * where there is a 'continue', we must close only until the for.
 *)

type nodei = Control_flow_c.G.key 

module StringMap = Map.Make (String)

type braceinfo =
    (node * (after_type -> string -> nodei -> unit), node) Common.either

type context_info =
  | NoInfo
  | LoopInfo   of nodei * nodei (* start, end *) * braceinfo list * int list
  | SwitchInfo of nodei * nodei (* start, end *) * braceinfo list * int list

(* for the Compound case I need to do different things depending if
 * the compound is the compound of the function definition, the compound of
 * a switch, so this type allows to specify this and enable to factorize
 * code for the Compound
 *)
and compound_caller =
  FunctionDef | Statement | Switch of (nodei -> xinfo -> xinfo)

(* other information used internally in ast_to_flow and passed recursively *)
and xinfo =  {

  ctx: context_info; (* cf above *)
  ctx_stack: context_info list;

  (* are we under a ifthen[noelse]. Used for ErrorExit *)
  under_ifthen: bool;
  compound_caller: compound_caller;

  (* does not change recursively. Some kind of globals. *)
  labels_assoc: nodei StringMap.t;
  exiti:      nodei option;
  errorexiti: nodei option;

  (* ctl_braces: the nodei list is to handle current imbrication depth.
   * It contains the must-close '}'.
   * update: now it is instead a node list.
   *)
  braces: braceinfo list;

  (* ctl: *)
  labels: int list;
  }


let initial_info = {
  ctx = NoInfo;
  ctx_stack = [];
  under_ifthen = false;
  compound_caller = Statement;
  braces = [];
  labels = [];

  (* don't change when recurse *)
  labels_assoc = StringMap.empty;
  exiti = None;
  errorexiti = None;
}


(*****************************************************************************)
(* (Semi) Globals, Julia's style. *)
(*****************************************************************************)
(* global graph *)
let g = ref (new Control_flow_c.G.ograph_mutable)

let counter_for_labels = ref 0
let counter_for_braces = ref 0

(* For switch we use compteur too (or pass int ref) cos need know order of the
 * case if then later want to go from CFG to (original) AST.
 * update: obsolete now I think
 *)
let counter_for_switch = ref 0


(*****************************************************************************)
(* helpers *)
(*****************************************************************************)

(* alt: do via a todo list, so can do all in one pass (but more complex)
 * todo: can also count the depth level and associate it to the node, for
 * the ctl_braces:
 *)
let compute_labels_and_create_them st =

  (* map C label to index number in graph *)
  let h = ref StringMap.empty in

  begin
    st +> Visitor_c.vk_statement { Visitor_c.default_visitor_c with
      Visitor_c.kstatement = (fun (k, bigf) st ->
        match Ast_c.unwrap_st st with
        | Labeled (Ast_c.Label (name, _st)) ->
            let ii = Ast_c.get_ii_st_take_care st in
            (* at this point I put a lbl_0, but later I will put the
             * good labels. *)
            let s = Ast_c.str_of_name name in
            let newi = !g +> add_node (Label (st,name, ((),ii))) lbl_0  (s^":")
            in
            begin
              (* the C label already exists ? *)
              if (StringMap.mem s !h) then raise (Error (DuplicatedLabel s));
              h := StringMap.add s newi !h;
              (* not k _st !!! otherwise in lbl1: lbl2: i++; we miss lbl2 *)
              k st;
            end
        | _st -> k st
      )
    };
    !h;
  end


(* ctl_braces: *)
let insert_all_braces xs starti nodety str =
  xs  +> List.fold_left (fun acc nodeinfo ->
    (* Have to build a new node (clone), cos cannot share it.
     * update: This is now done by the caller. The clones are in xs.
     *)
    let (node,fn) =
      match nodeinfo with
	Common.Left(node,mkafter) ->
	  (node,mkafter) (* statements where after link needed *)
      | Common.Right node -> (node,fun _ _ _ -> ()) in (* ifdefs *)
    let newi = !g#add_node node in
    fn nodety str newi;
    !g#add_arc ((acc, newi), Direct);
    newi
  ) starti

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

(* Take in a (optional) start node, return an (optional) end node.
 *
 * history:
 *
 * ver1: old code was returning an nodei, but goto has no end, so
 * aux_statement should return nodei option.
 *
 * ver2: old code was taking a nodei, but should also take nodei
 * option.
 *
 * ver3: deadCode detection. What is dead code ? When there is no
 * starti to start from ? So make starti an option too ? Si on arrive
 * sur un label: au moment d'un deadCode, on peut verifier les
 * predecesseurs de ce label, auquel cas si y'en a, ca veut dire
 * qu'en fait c'est pas du deadCode et que donc on peut se permettre
 * de partir d'un starti à None. Mais si on a xx; goto far:; near:
 * yy; zz; far: goto near:. Bon ca doit etre un cas tres tres rare,
 * mais a cause de notre parcours, on va rejeter ce programme car au
 * moment d'arriver sur near: on n'a pas encore de predecesseurs pour
 * ce label. De meme, meme le cas simple ou la derniere instruction
 * c'est un return, alors ca va generer un DeadCode :(
 *
 * So make a first pass where dont launch exn at all. Create nodes,
 * if starti is None then dont add arc. Then make a second pass that
 * just checks that all nodes (except enter) have predecessors.
 * So make starti an option too. So type is now
 *
 *      nodei option -> statement -> nodei option.
 *
 * todo?: if the pb is at a fake node, then try first successos that
 * is non fake.
 *
 * ver4: because of special needs of coccinelle, need pass more info, cf
 * type additionnal_info defined above.
 *
 * - to complete (break, continue (and enclosing loop), switch (and
 * associated case, casedefault)) we need to pass additional info.
 * The start/exit when enter in a loop, to know the current 'for'.
 *
 * - to handle the braces, need again pass additional info.
 *
 * - need pass the labels.
 *
 * convention: xi for the auxinfo passed recursively
 *
 *)

let rec aux_statement : (nodei option * xinfo) -> statement -> nodei option =
 fun (starti, xi) stmt ->

  if not !Flag_parsing_c.label_strategy_2
  then incr counter_for_labels;

  let lbl =
    if !Flag_parsing_c.label_strategy_2
    then xi.labels
    else xi.labels @ [!counter_for_labels]
  in

  (* Normally the new auxinfo to pass recursively to the next aux_statement.
   * But in some cases we add additional stuff in which case we don't use
   * this 'xi_lbl' but a 'newxi' specially built.
   *)
  let xi_lbl =
    if !Flag_parsing_c.label_strategy_2
    then { xi with
      compound_caller = Statement;
    }
    else { xi with
      labels = xi.labels @ [ !counter_for_labels ];
      compound_caller = Statement;
    }
  in
  let ii = Ast_c.get_ii_st_take_care stmt in

  (* ------------------------- *)
  match Ast_c.unwrap_st stmt with

  (*  coupling: the Switch case copy paste parts of the Compound case *)
  | Ast_c.Compound statxs ->
      (* flow_to_ast: *)
      let (i1, i2) = tuple_of_list2 ii in

      (* ctl_braces: *)
      incr counter_for_braces;
      let brace = !counter_for_braces in

      let s1 = "{" ^ string_of_int brace in
      let s2 = "}" ^ string_of_int brace in

      let lbl = match xi.compound_caller with
        | FunctionDef -> xi.labels (* share label with function header *)
        | Statement -> xi.labels @ [!counter_for_labels]
        | Switch _ -> xi.labels
      in

      let newi = !g +> add_node (SeqStart (stmt, brace, i1)) lbl s1 in
      let endnode     = mk_node      (SeqEnd (brace, i2))    lbl [] s2 in
      let endnode_dup = mk_fake_node (SeqEnd (brace, i2))    lbl [] s2 in
(*
      let _endnode_dup =
	mk_node (SeqEnd (brace, Ast_c.fakeInfo())) lbl [] s2 in
*)

      (* This code makes a link from the top of the block to any } created
       * by a return from the braces list.  It is also called at the end
       * of treating the block.  If there is a non-return way out of the
       * block, then any link created by a } will be overwritten by a normal
       * one. This is the desired behavior. *)
      let ret_afters = ref [] in
      let mkafter ty str endi =
	if xi.compound_caller = Statement
	then
	  (let afteri = !g +> add_node (AfterNode ty) lbl str in
	  let a1 = ((newi, afteri), Direct) in
	  !g#add_arc a1;
	  let a2 = ((afteri, endi), Direct) in
	  !g#add_arc a2;
	  ret_afters := (afteri,a1,a2) :: !ret_afters) in

      let newxi = { xi_lbl with
            braces = Common.Left(endnode_dup,mkafter) :: xi_lbl.braces } in

      let newxi = match xi.compound_caller with
        | Switch todo_in_compound ->
            (* note that side effect in todo_in_compound *)
            todo_in_compound newi newxi
        | FunctionDef | Statement -> newxi
      in

      !g +> add_arc_opt (starti, newi);
      let finishi = Some newi in

      aux_statement_list finishi (xi, newxi) statxs

      (* braces: *)
      +> Common.fmap (fun finishi ->
            (* subtil: not always return a Some.
             * Note that if finishi is None, alors forcement ca veut dire
             * qu'il y'a eu un return (ou goto), et donc forcement les
             * braces auront au moins ete crée une fois, et donc flow_to_ast
             * marchera.
             * Sauf si le goto revient en arriere ? mais dans ce cas
             * ca veut dire que le programme boucle. Pour qu'il boucle pas
             * il faut forcement au moins un return.
             *)
            let endi = !g#add_node endnode in
	    List.iter
	      (function (node,a1,a2) ->
		!g#del_arc a1; !g#del_arc a2; !g#del_node node)
	      !ret_afters;
	    mkafter NormalAfterNode "[after]" endi;
            !g#add_arc ((finishi, endi), Direct);
            endi
           )


   (* ------------------------- *)
  | Labeled (Ast_c.Label (name, st)) ->
      let s = Ast_c.str_of_name name in
      let ilabel = StringMap.find s xi.labels_assoc in
      let node = mk_node (unwrap (KeyMap.find ilabel !g#nodes)) lbl [] (s ^ ":") in
      !g#replace_node (ilabel, node);
      !g +> add_arc_opt (starti, ilabel);
      aux_statement (Some ilabel, xi_lbl) st


  | Jump (Ast_c.Goto name) ->
     let s = Ast_c.str_of_name name in
     (* special_cfg_ast: *)
     let newi = !g +>
          add_node (Goto (stmt, name, ((),ii))) lbl ("goto "^s^":") in
     !g +> add_arc_opt (starti, newi);

     if !Flag_parsing_c.no_gotos
     then Some newi
     else begin
       let ilabel =
         try StringMap.find s xi.labels_assoc
         with Not_found ->
                  (* jump vers ErrorExit a la place ?
                   * pourquoi tant de "cant jump" ? pas detecté par gcc ?
                   *)
                 raise (Error (GotoCantFindLabel (s, pinfo_of_ii ii)))
         in
       (* !g +> add_arc_opt (starti, ilabel);
        * todo: special_case: suppose that always goto to toplevel of
        * function, hence the Common.init
        * todo?: can perhaps report when a goto is not a classic error_goto ?
        * that is when it does not jump to the toplevel of the function.
        *)
       let newi = insert_all_braces (Common.list_init xi.braces) newi
                                    GotoAfterNode "[goto after]" in
       !g#add_arc ((newi, ilabel), Direct);
       None
     end

  | Jump (Ast_c.GotoComputed e) ->
      raise (Error (ComputedGoto))

   (* ------------------------- *)
  | Ast_c.ExprStatement opte ->
      (* flow_to_ast:   old: when opte = None, then do not add in CFG. *)
      let s =
        match opte with
        | None -> "empty;"
        | Some e ->
            (match Ast_c.unwrap_expr e with
            | FunCall (e, _args) ->
                (match Ast_c.unwrap_expr e with
                | Ident namef ->
                    Ast_c.str_of_name namef ^ "(...)"
                | _ -> "statement"
                )
            | Assignment (e1, (SimpleAssign,_), e2) ->
                (match Ast_c.unwrap_expr e1 with
                | Ident namevar ->
                    Ast_c.str_of_name namevar ^ " = ... ;"
                | RecordAccess(e, field) ->
                    (match Ast_c.unwrap_expr e with
                    | Ident namevar ->
                        let sfield = Ast_c.str_of_name field in
                        Ast_c.str_of_name namevar ^ "." ^ sfield ^ " = ... ;"
                    | _ -> "statement"
                    )
                | _ -> "statement"
                )
            | _ -> "statement"
            )
      in
      let newi = !g +> add_node (ExprStatement (stmt, (opte, ii))) lbl s in
      !g +> add_arc_opt (starti, newi);
      Some newi

  | Ast_c.Exec(code) ->
      let s = "exec" in
      let newi = !g +> add_node (Exec (stmt, (code, ii))) lbl s in
      !g +> add_arc_opt (starti, newi);
      Some newi

   (* ------------------------- *)
  | Selection (Ast_c.If _) -> snd (mk_If starti lbl xi_lbl stmt)

   (* ------------------------- *)
  | Selection  (Ast_c.Switch (e, st)) ->
      let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in

      let ii = [i1;i2;i3] in

      (* The newswitchi is for the labels to know where to attach.
       * The newendswitch (endi) is for the 'break'. *)
      let newswitchi=
        !g +> add_node (SwitchHeader(stmt,(e,ii))) lbl "switch" in
      let newendswitch =
        !g +> add_node (EndStatement (Some iifakeend)) lbl "[endswitch]" in

      !g +> add_arc_opt (starti, newswitchi);

      (* allows multiple case labels to stack up *)
      let rec contains_default s =
	match Ast_c.unwrap_st s with
	  Labeled (Ast_c.Default _) -> true
	| Labeled (Ast_c.Case(e,s)) -> contains_default s
	| Labeled (Ast_c.CaseRange(e1,e2,s)) -> contains_default s
	| _ -> false in

       (* call compound case. Need special info to pass to compound case
        * because we need to build a context_info that need some of the
        * information build inside the compound case: the nodei of {
        *)
       let finalthen =
         match Ast_c.unwrap_st st with
         | Ast_c.Compound statxs ->

             let statxs = Lib.stmt_elems_of_sequencable statxs in

             (* todo? we should not allow to match a stmt that corresponds
              * to a compound of a switch, so really SeqStart (stmt, ...)
              * here ? so maybe should change the SeqStart labeling too.
              * So need pass a todo_in_compound2 function.
              *)
             let todo_in_compound newi newxi =
               let newxi' = { newxi with
                 ctx = SwitchInfo (newi(*!!*), newendswitch, xi.braces, lbl);
                 ctx_stack = newxi.ctx::newxi.ctx_stack
               }
               in
               !g#add_arc ((newswitchi, newi), Direct);
               (* new: if have not a default case, then must add an edge
                * between start to end.
                * todo? except if the case[range] coverthe whole spectrum
                *)
               if not (statxs +> List.exists contains_default)
               then begin
                 (* when there is no default, then a valid path is
                  * from the switchheader to the end. In between we
                  * add a Fallthrough.
                  *)

                 let newafter = !g+>add_node FallThroughNode lbl "[switchfall]"
                 in
                 !g#add_arc ((newafter, newendswitch), Direct);
                 !g#add_arc ((newswitchi, newafter), Direct);
                 (* old:
                    !g#add_arc ((newswitchi, newendswitch), Direct) +> adjust_g;
                 *)
               end;
               newxi'
             in
             let newxi = { xi_lbl with compound_caller = (* was xi *)
                 Switch todo_in_compound
             }
             in
             aux_statement (None (* no starti *), newxi) st
         | _x ->
             (* apparently gcc allows some switch body such as
              *   switch (i) case 0 : printf("here\n");
              *   cf tests-bis/switch_no_body.c
              * but I don't think it's worthwhile to handle
              * such pathological and rare case. Not worth
              * the complexity. Safe to assume a coumpound.
              *)
             raise (Error (WeirdSwitch (pinfo_of_ii [i1])))
       in
       !g +> add_arc_opt (finalthen, newendswitch);


       (* what if has only returns inside. We must  try to see if the
        * newendswitch has been used via a 'break;'  or because no
        * 'default:')
        *)
       let res =
         (match finalthen with
         | Some finalthen ->

             let afteri =
	       !g +> add_node (AfterNode NormalAfterNode) lbl "[after]" in
             !g#add_arc ((newswitchi, afteri),  Direct);
             !g#add_arc ((afteri, newendswitch), Direct);


             !g#add_arc ((finalthen, newendswitch), Direct);
             Some newendswitch
         | None ->
             if (KeyEdgeSet.is_empty (!g#predecessors newendswitch))
             then begin
                 assert (KeyEdgeSet.is_empty (!g#successors newendswitch));
                 !g#del_node newendswitch;
                 None
             end
             else begin

               let afteri =
		 !g +> add_node (AfterNode NormalAfterNode) lbl "[after]" in
               !g#add_arc ((newswitchi, afteri),  Direct);
               !g#add_arc ((afteri, newendswitch), Direct);


               Some newendswitch
             end
         )
       in
       res


  | Labeled (Ast_c.Case  (_, _))
  | Labeled (Ast_c.CaseRange  (_, _, _)) ->

      incr counter_for_switch;
      let switchrank = !counter_for_switch in
      let node, st =
        match Ast_c.get_st_and_ii stmt with
        | Labeled (Ast_c.Case  (e, st)), ii ->
            (Case (stmt, (e, ii))),  st
        | Labeled (Ast_c.CaseRange  (e, e2, st)), ii ->
            (CaseRange (stmt, ((e, e2), ii))), st
        | _ -> raise (Impossible 63)
      in

      let newi = !g +> add_node node  lbl "case:" in

      (match Common.optionise (fun () ->
        (* old: xi.ctx *)
        (xi.ctx::xi.ctx_stack) +> Common.find_some (function
        | SwitchInfo (a, b, c, _) -> Some (a, b, c)
        | _ -> None
        ))
      with
      | Some (startbrace, switchendi, _braces) ->
          (* no need to attach to previous for the first case, cos would be
           * redundant. *)
          starti +> do_option (fun starti ->
            if starti <> startbrace
            then !g +> add_arc_opt (Some starti, newi);
            );

          let s = ("[casenode] " ^ string_of_int switchrank) in
          let newcasenodei = !g +> add_node (CaseNode switchrank) lbl s in
          !g#add_arc ((startbrace, newcasenodei), Direct);
          !g#add_arc ((newcasenodei, newi), Direct);
      | None -> raise (Error (CaseNoSwitch (pinfo_of_ii ii)))
      );
      aux_statement (Some newi, xi_lbl) st


  | Labeled (Ast_c.Default st) ->
      incr counter_for_switch;
      let switchrank = !counter_for_switch in

      let newi = !g +> add_node (Default(stmt, ((),ii))) lbl "case default:" in
      !g +> add_arc_opt (starti, newi);

      (match xi.ctx with
      | SwitchInfo (startbrace, switchendi, _braces, _parent_lbl) ->
          let s = ("[casenode] " ^ string_of_int switchrank) in
          let newcasenodei = !g +> add_node (CaseNode switchrank) lbl s in
          !g#add_arc ((startbrace, newcasenodei), Direct);
          !g#add_arc ((newcasenodei, newi), Direct);
      | _ -> raise (Error (CaseNoSwitch (pinfo_of_ii ii)))
      );
      aux_statement (Some newi, xi_lbl) st






   (* ------------------------- *)
  | Selection (Ast_c.Ifdef_Ite _) -> mk_Ifdef_Ite starti lbl xi_lbl stmt
  | Selection (Ast_c.Ifdef_Ite2 _) -> mk_Ifdef_Ite2 starti lbl xi_lbl stmt

   (* ------------------------- *)
  | Iteration  (Ast_c.While (e, st)) ->
     (* starti -> newi ---> newfakethen -> ... -> finalthen -
      *             |---|-----------------------------------|
      *                 |-> newfakelse
      *)

      let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
      let ii = [i1;i2;i3] in

      let newi = !g +> add_node (WhileHeader (stmt, (e,ii))) lbl "while" in
      !g +> add_arc_opt (starti, newi);
      let newfakethen = !g +> add_node InLoopNode  lbl "[whiletrue]" in
      (* let newfakeelse = !g +> add_node FalseNode lbl "[endwhile]" in *)
      let newafter = !g +> add_node LoopFallThroughNode lbl "[whilefall]" in
      let newfakeelse =
        !g +> add_node (EndStatement (Some iifakeend)) lbl "[endwhile]" in

      let newxi = { xi_lbl with
         ctx = LoopInfo (newi, newfakeelse,  xi_lbl.braces, lbl);
         ctx_stack = xi_lbl.ctx::xi_lbl.ctx_stack
        }
      in

      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newafter, newfakeelse), Direct);
      !g#add_arc ((newi, newafter), Direct);
      let finalthen = aux_statement (Some newfakethen, newxi) st in
      !g +> add_arc_opt
	(finalthen, if !Flag_parsing_c.no_loops then newafter else newi);
      Some newfakeelse


  (* This time, may return None, for instance if goto in body of dowhile
   * (whereas While cannot return None). But if return None, certainly
   * some deadcode.
   *)
  | Iteration  (Ast_c.DoWhile (st, e)) ->
     (* starti -> doi ---> ... ---> finalthen (opt) ---> whiletaili
      *             |--------- newfakethen ---------------|  |---> newfakelse
      *)
      let is_zero =
        match Ast_c.unwrap_expr e with
        | Constant (Int ("0",_)) -> true
        | _ -> false
      in

      let (iido, iiwhiletail, iifakeend) =
        match ii with
        | [i1;i2;i3;i4;i5;i6] -> i1, [i2;i3;i4;i5], i6
        | _ -> raise (Impossible 64)
      in
      let doi = !g +> add_node (DoHeader (stmt, iido))  lbl "do" in
      !g +> add_arc_opt (starti, doi);
      let taili = !g +> add_node (DoWhileTail (e, iiwhiletail)) lbl "whiletail"
      in


      (*let newfakeelse = !g +> add_node FalseNode lbl "[enddowhile]" in *)
      let newafter = !g +> add_node FallThroughNode lbl "[dowhilefall]" in
      let newfakeelse =
        !g +> add_node (EndStatement (Some iifakeend)) lbl "[enddowhile]" in

      let afteri = !g +> add_node (AfterNode NormalAfterNode) lbl "[after]" in
      !g#add_arc ((doi,afteri), Direct);
      !g#add_arc ((afteri,newfakeelse), Direct);

      let newxi = { xi_lbl with
         ctx = LoopInfo (taili, newfakeelse, xi_lbl.braces, lbl);
         ctx_stack = xi_lbl.ctx::xi_lbl.ctx_stack
        }
      in

      if not is_zero && (not !Flag_parsing_c.no_loops)
      then begin
        let newfakethen = !g +> add_node InLoopNode lbl "[dowhiletrue]" in
        !g#add_arc ((taili, newfakethen), Direct);
        !g#add_arc ((newfakethen, doi), Direct);
      end;

      !g#add_arc ((newafter, newfakeelse), Direct);
      !g#add_arc ((taili, newafter), Direct);


      let finalthen = aux_statement (Some doi, newxi) st in
      (match finalthen with
      | None ->
          if (KeyEdgeSet.is_empty (!g#predecessors taili))
          then raise (Error (DeadCode (Some (pinfo_of_ii ii))))
          else Some newfakeelse
      | Some finali ->
          !g#add_arc ((finali, taili), Direct);
          Some newfakeelse
      )



  | Iteration  (Ast_c.For (e1opt, e2opt, e3opt, st)) ->
      let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
      let ii = [i1;i2;i3] in

      let newi =
        !g+>add_node(ForHeader(stmt,((e1opt,e2opt,e3opt),ii))) lbl "for" in
      !g +> add_arc_opt (starti, newi);
      let newfakethen = !g +> add_node InLoopNode  lbl "[fortrue]" in
      (*let newfakeelse = !g +> add_node FalseNode lbl "[endfor]" in*)
      let newafter = !g +> add_node LoopFallThroughNode lbl "[forfall]" in
      let newfakeelse =
        !g +> add_node (EndStatement (Some iifakeend)) lbl "[endfor]" in

      let newxi = { xi_lbl with
           ctx = LoopInfo (newi, newfakeelse, xi_lbl.braces, lbl);
           ctx_stack = xi_lbl.ctx::xi_lbl.ctx_stack
        }
      in

      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newafter, newfakeelse), Direct);
      !g#add_arc ((newi, newafter), Direct);
      let finalthen = aux_statement (Some newfakethen, newxi) st in
      !g +> add_arc_opt
	(finalthen,
	 if !Flag_parsing_c.no_loops then newafter else newi);
      Some newfakeelse


  (* to generate less exception with the breakInsideLoop, analyse
   * correctly the loop deguisé comme list_for_each. Add a case ForMacro
   * in ast_c (and in lexer/parser), and then do code that imitates the
   * code for the For.
   * update: the list_for_each was previously converted into Tif by the
   * lexer, now they are returned as Twhile so less pbs. But not perfect.
   * update: now I recognize the list_for_each macro so no more problems.
   *)
  | Iteration  (Ast_c.MacroIteration (s, es, st)) ->
      let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
      let ii = [i1;i2;i3] in

      let newi =
        !g+>add_node(MacroIterHeader(stmt,((s,es),ii))) lbl "foreach" in
      !g +> add_arc_opt (starti, newi);
      let newfakethen = !g +> add_node InLoopNode  lbl "[fortrue]" in
      (*let newfakeelse = !g +> add_node FalseNode lbl "[endfor]" in*)
      let newafter = !g +> add_node LoopFallThroughNode lbl "[foreachfall]" in
      let newfakeelse =
        !g +> add_node (EndStatement (Some iifakeend)) lbl "[endforeach]" in

      let newxi = { xi_lbl with
           ctx = LoopInfo (newi, newfakeelse, xi_lbl.braces, lbl);
           ctx_stack = xi_lbl.ctx::xi_lbl.ctx_stack
        }
      in

      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newafter, newfakeelse), Direct);
      !g#add_arc ((newi, newafter), Direct);
      let finalthen = aux_statement (Some newfakethen, newxi) st in
      !g +> add_arc_opt (finalthen,
                          if !Flag_parsing_c.no_loops then newafter else newi);
      Some newfakeelse



   (* ------------------------- *)
  | Jump ((Ast_c.Continue|Ast_c.Break) as x) ->
      let context_info =
	match xi.ctx with
	  SwitchInfo (startbrace, loopendi, braces, parent_lbl) ->
            if x = Ast_c.Break
	    then xi.ctx
	    else
	      (try
                xi.ctx_stack +> Common.find_some (function
                    LoopInfo (_,_,_,_) as c ->  Some c
                  | _ -> None)
              with Not_found ->
                raise (Error (OnlyBreakInSwitch (pinfo_of_ii ii))))
        | LoopInfo (loopstarti, loopendi, braces, parent_lbl) -> xi.ctx
	| NoInfo -> raise (Error (NoEnclosingLoop (pinfo_of_ii ii))) in

      let parent_label =
	match context_info with
	  LoopInfo (loopstarti, loopendi, braces, parent_lbl) -> parent_lbl
	| SwitchInfo (startbrace, loopendi, braces, parent_lbl) -> parent_lbl
	| NoInfo -> raise (Impossible 65) in

      let from_switch =
	match context_info with
	  LoopInfo (loopstarti, loopendi, braces, parent_lbl) -> false
	| SwitchInfo (startbrace, loopendi, braces, parent_lbl) -> true
	| NoInfo -> raise (Impossible 65) in

      (* flow_to_ast: *)
      let (node_info, string) =
	let parent_string =
	  String.concat "," (List.map string_of_int parent_label) in
	(match x with
          | Ast_c.Continue ->
	      (Continue (stmt, ((), ii)),
	       Printf.sprintf "continue; [%s]" parent_string)
          | Ast_c.Break    ->
	      (Break    (stmt, ((), ii), from_switch),
	       Printf.sprintf "break; [%s]" parent_string)
          | _ -> raise (Impossible 66)
          ) in

      (* idea: break or continue records the label of its parent loop or
	 switch *)
      let newi = !g +> add_bc_node node_info lbl parent_label string in
      !g +> add_arc_opt (starti, newi);

      (* let newi = some starti in *)

      (match context_info with
      | LoopInfo (loopstarti, loopendi, braces, parent_lbl) ->
          let (desti,nodety,str) =
            (match x with
            | Ast_c.Break -> (loopendi,BreakAfterNode,"[break after]")
            | Ast_c.Continue ->
		(* if no loops, then continue behaves like break - just
		   one iteration *)
		((if !Flag_parsing_c.no_loops then loopendi else loopstarti),
		ContAfterNode, "[cont after]")
            | x -> raise (Impossible 67)
            ) in
          let difference = List.length xi.braces - List.length braces in
          assert (difference >= 0);
          let toend = take difference xi.braces in
          let newi = insert_all_braces toend newi nodety str in
          !g#add_arc ((newi, desti), Direct);
          None

      | SwitchInfo (startbrace, loopendi, braces, parent_lbl) ->
	  assert (x = Ast_c.Break);
          let difference = List.length xi.braces - List.length braces in
          assert (difference >= 0);
          let toend = take difference xi.braces in
          let newi =
	    insert_all_braces toend newi SWBreakAfterNode "[swbreak after]" in
          !g#add_arc ((newi, loopendi), Direct);
          None
      | NoInfo -> raise (Impossible 68)
      )

  | Jump ((Ast_c.Return | Ast_c.ReturnExpr _) as kind) ->
     (match xi.exiti, xi.errorexiti with
     | None, None -> raise (Error (NoExit (pinfo_of_ii ii)))
     | Some exiti, Some errorexiti ->

      (* flow_to_ast: *)
      let s =
        match kind with
        | Ast_c.Return -> "return"
        | Ast_c.ReturnExpr _ -> "return ..."
        | _ -> raise (Impossible 69)
      in
      let newi =
        !g +> add_node
          (match kind with
          | Ast_c.Return ->       Return (stmt, ((),ii))
          | Ast_c.ReturnExpr e -> ReturnExpr (stmt, (e, ii))
          | _ -> raise (Impossible 70)
          )
          lbl s
      in
      !g +> add_arc_opt (starti, newi);
      let newi = insert_all_braces xi.braces newi RetAfterNode "[ret after]" in

      if xi.under_ifthen
      then !g#add_arc ((newi, errorexiti), Direct)
      else !g#add_arc ((newi, exiti), Direct)
      ;
      None
     | _ -> raise (Impossible 71)
     )


  (* ------------------------- *)
  | Ast_c.Decl decl ->
     let s =
       match decl with
       | (Ast_c.DeclList
             ([{v_namei = Some (name, _); v_type = typ; v_storage = sto}, _], _)) ->
	   "decl:" ^ Ast_c.str_of_name name
       | _ -> "decl_novar_or_multivar"
     in

     let newi = !g +> add_node (Decl (decl)) lbl s in
     !g +> add_arc_opt (starti, newi);
     Some newi

  (* ------------------------- *)
  | Ast_c.Asm body ->
      let newi = !g +> add_node (Asm (stmt, ((body,ii)))) lbl "asm;" in
      !g +> add_arc_opt (starti, newi);
      Some newi

  | Ast_c.MacroStmt ->
      let newi = !g +> add_node (MacroStmt (stmt, ((),ii))) lbl "macro;" in
      !g +> add_arc_opt (starti, newi);
      Some newi


  (* ------------------------- *)
  | Ast_c.NestedFunc def ->
      raise (Error NestedFunc)

and mk_If (starti :nodei option) (labels :int list) (xi_lbl :xinfo)
          (stmt :statement)
          : nodei (* first node of the else branch *)
          * nodei option =
  let ii = Ast_c.get_ii_st_take_care stmt in
  match Ast_c.unwrap_st stmt with
  | Selection (Ast_c.If (e, st1, st2)) ->
    let iist2 = Ast_c.get_ii_st_take_care st2 in begin
    match Ast_c.unwrap_st st2 with
    | Ast_c.ExprStatement None when iist2=[] ->
      (* We could have 'ExprStatement None' as a result of something like
       * 'if() xx else ;', so we must force to have a [] in the ii associated
       * with ExprStatement.
       *)

      let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
      let ii' = [i1;i2;i3] in
      (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
       *                  |                                      |
       *                  |->   newfakeelse -> ... -> finalelse -|
       * update: there is now also a link directly to lasti.
       *
       * because of CTL, now do different things if we are in a ifthen or
       * ifthenelse.
       *)

      (* starti -> newi *)
      let newi = !g +> add_node (IfHeader (stmt, (e, ii'))) labels "if" in
      !g +> add_arc_opt (starti, newi);

      (* newi ---> newfakethen -> ... -> lasti
       *       |                           |
       *       |-> newfakeelse ----------->|
       *)
      let escapes = ref false in
      let newfakethen = !g +> add_node (TrueNode escapes) labels "[then]" in
      let newfakeelse = !g +> add_node FallThroughNode labels "[fallthrough]" in
      let lasti  = !g +>
            add_node (EndStatement (Some iifakeend)) labels "[endif]" in
      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newi, newfakeelse), Direct);
      !g#add_arc ((newfakeelse, lasti), Direct);

      (* if -> [after] -> [endif] *)
      let afteri = !g +>
            add_node (AfterNode NormalAfterNode) labels "[after]" in
      !g#add_arc ((newi, afteri), Direct);
      !g#add_arc ((afteri, lasti), Direct);

      (* for ErrorExit heuristic *)
      let newxi = { xi_lbl with  under_ifthen = true; } in

      (* [then] -> {st1} -> lasti *)
      let finalthen = aux_statement (Some newfakethen, newxi) st1 in
      (match finalthen with None -> escapes := true | _ -> ());
      !g +> add_arc_opt (finalthen, lasti);
      lasti, Some lasti
    | __else__ ->
      (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
       *                 |                                      |
       *                 |->   newfakeelse -> ... -> finalelse -|
       * update: there is now also a link directly to lasti.
       *)
      let (iiheader, iielse, iifakeend) =
        match ii with
        | [i1;i2;i3;i4;i5] -> [i1;i2;i3], i4, i5
        | _ -> raise (Impossible 62)
        in

      (* starti -> "if" *)
      let newi = !g +> add_node (IfHeader (stmt, (e, iiheader))) labels "if" in
      !g +> add_arc_opt (starti, newi);

      (* if ---> [then] -> ...
       *     |
       *     |-> [else] -> else -> ...
       *)
      let escapes = ref false in
      let newfakethen = !g +> add_node (TrueNode escapes) labels "[then]" in
      let newfakeelse = !g +> add_node FalseNode labels "[else]" in
      let elsenode = !g +> add_node (Else iielse) labels "else" in
      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newi, newfakeelse), Direct);
      !g#add_arc ((newfakeelse, elsenode), Direct);

      let endnode =
        mk_node (EndStatement(Some iifakeend)) labels [] "[endif]" in
      let endnode_dup =
        mk_node (EndStatement(Some iifakeend)) labels [] "[endif]" in

      let ret_afters = ref [] in
      let mkafter ty str lasti = begin
            (* if -> [after] -> [endif] *)
            let afteri = !g +> add_node (AfterNode ty) labels str in
            let a1 = ((newi, afteri), Direct) in
            !g#add_arc a1;
            let a2 = ((afteri, lasti), Direct) in
            !g#add_arc a2;
            ret_afters := (afteri,a1,a2) :: !ret_afters
        end in

      let newxi = { xi_lbl with
            braces = Common.Left (endnode_dup,mkafter) :: xi_lbl.braces
        } in

      let finalthen = aux_statement (Some newfakethen, newxi) st1 in
      let finalelse = aux_statement (Some elsenode, newxi) st2 in

      (match finalthen with None -> escapes := true | _ -> ());

      (* find the first node of the 'else' branch *)
      let elsenode_succ = match finalelse with
        | Some succ -> succ
        | None      -> elsenode
        in

      elsenode_succ, begin match finalthen, finalelse with
        | (None, None) -> None
        | __else__ ->
          let lasti = !g#add_node endnode in
          !ret_afters +> List.iter (function (node,a1,a2) ->
                            !g#del_arc a1; !g#del_arc a2; !g#del_node node);
          mkafter NormalAfterNode "[after]" lasti;
          begin
            !g +> add_arc_opt (finalthen, lasti);
            !g +> add_arc_opt (finalelse, lasti);
            Some lasti
          end
      end
    end
  | x -> error_cant_have x

(* Builds the CFG for an Ifdef_Ite selection statement, i.e.
 *
 *     #ifdef A if e S1 else #endif S2
 *
 * This function works in fact as a decorator for an If statement:
 *
 * 1. We construct the CFG for 'if e S1 else S2', which coincides with
 *    the _true_ branch for the #ifdef.
 * 2. The _false_ path of is just an edge from the IfdefIteHeader to the
 *    'else' branch of the if statement.
 *
 * Why doing it in this way:
 *
 * - Coccinelle cannot match #ifdef's so we can keep the CFG thin by
 *   avoiding all the extra plumbing. We don't need, for instance, an
 *   _after_ node.
 * - We still want Coccinelle to be able to match the if statement, and
 *   we don't want to replicate (aka copy-paste) code for this purpose.
 *
 * /Iago
 *)
and mk_Ifdef_Ite (starti :nodei option) (labels :int list) (xi_lbl :xinfo)
                 (stmt :statement)
                 : nodei option =
  (* starti -> #ifdef-if ---> if -> ... -> [else] -> ... -> [endif]
   *                      |                         ^
   *                      |_________________________|
   *)
  match Ast_c.get_ii_st_take_care stmt, Ast_c.unwrap_st stmt with
    [i1;i2;i3;i4;i5;i6;i7], Selection (Ast_c.Ifdef_Ite (e, st1, st2)) ->

      let if_sel = Ast_c.If (e,st1,st2) in
      let if_stmtbis = Selection if_sel in
      let if_ii = [i2;i3;i4;i5;i7] in
      let if_stmt = if_stmtbis, if_ii in

      (* starti -> #ifdef-if *)
      let ifdefite = !g +>
        add_node (IfdefIteHeader [i1;i6]) labels "#ifdef-if" in
      !g +> add_arc_opt (starti, ifdefite);

      begin
	match mk_If (Some ifdefite) labels xi_lbl if_stmt with
	  (elsenode,endnode_opt) ->
            !g#add_arc ((ifdefite, elsenode), Direct);
            endnode_opt
      end
  | x -> error_cant_have x

(* Builds the CFG for an Ifdef_Ite selection statement, i.e.
 *
 *     #ifdef A if e S1 else #else S2 #endif S3
 *
 * The true path of the #ifdef implies:
 *
 *     if e S1 else S3
 *
 * See mk_Ifdef_Ite for further details.
 *)
and mk_Ifdef_Ite2 (starti :nodei option) (labels :int list) (xi_lbl :xinfo)
                  (stmt :statement)
                  : nodei option =
  (* starti -> #ifdef-if ---> if -> {st1} -> [else] -> {st3} -> [endif]
   *                      |                              ^
   *                      |------------> {st2} ----------|
   *)
  match Ast_c.get_ii_st_take_care stmt, Ast_c.unwrap_st stmt with
    [i1;i2;i3;i4;i5;i6;i7;i8],
    Selection (Ast_c.Ifdef_Ite2 (e, st1, st2, st3)) ->

      let if_sel = Ast_c.If (e,st1,st3) in
      let if_stmtbis = Selection if_sel in
      let if_ii = [i2;i3;i4;i5;i8] in
      let if_stmt = if_stmtbis, if_ii in

      (* starti -> #ifdef-if *)
      let ifdefite = !g +>
        add_node (IfdefIteHeader [i1;i6;i7]) labels "#ifdef-if" in
      !g +> add_arc_opt (starti, ifdefite);

      begin
	match mk_If (Some ifdefite) labels xi_lbl if_stmt with
	  (elsenode,endnode_opt) ->
	    let finalelse = aux_statement (Some ifdefite, xi_lbl) st2 in
	    begin
	      match finalelse with
		Some st2_node -> !g#add_arc ((st2_node, elsenode), Direct)
	      | None -> ()
	    end;
	    endnode_opt
      end
  | x -> error_cant_have x

and aux_statement_list starti (xi, newxi) statxs =
  statxs
  +> List.fold_left (fun starti statement_seq ->
    if !Flag_parsing_c.label_strategy_2
    then incr counter_for_labels;

    let newxi' =
      if !Flag_parsing_c.label_strategy_2
      then { newxi with labels = xi.labels @ [ !counter_for_labels ] }
      else newxi
    in

    match statement_seq with
    | Ast_c.StmtElem statement ->
        aux_statement (starti, newxi') statement

    | Ast_c.CppDirectiveStmt directive ->
        pr2_once ("ast_to_flow: filter a directive");
        starti

    | Ast_c.IfdefStmt ifdef ->
        pr2_once ("ast_to_flow: filter a directive");
        starti

    | Ast_c.IfdefStmt2 (ifdefs, xxs) ->

        let (head, body, tail) = Common.head_middle_tail ifdefs in

        let newi =
          !g +> add_node (IfdefHeader (head)) newxi'.labels "[ifdef]" in
        let taili =
          !g +> add_node (IfdefEndif (tail)) newxi'.labels "[endif]" in
        (* do like for a close brace, see endi.{c,cocci} *)
        let taili_dup =
          mk_fake_node (IfdefEndif (tail)) newxi'.labels [] "[endif]" in
        !g +> add_arc_opt (starti, newi);

        if body = [] then
        begin
          let newfakeelse = !g +> add_node FallThroughNode newxi'.labels
                                           "[fallthrough]" in
          !g#add_arc ((newi, newfakeelse), Direct);
          !g#add_arc ((newfakeelse,taili), Direct);
        end;

        let elsenodes =
          body +> List.map (fun elseif ->
            let elsei =
              !g +> add_node (IfdefElse (elseif)) newxi'.labels "[elseif]" in
            !g#add_arc ((newi, elsei), Direct);
            elsei
          ) in

        let _finalxs =
          Common.zip (newi::elsenodes) xxs +> List.map (fun (start_nodei, xs)->
              (* not sure if this is correct... newxi seems to relate to
                 the assigned level number *)
              let newerxi =
                { newxi with braces = Common.Right taili_dup:: newxi.braces } in
              let finalthen =
                aux_statement_list (Some start_nodei) (newxi, newerxi) xs in
              !g +> add_arc_opt (finalthen, taili);
              )
        in

(*
        This is an attempt to let a statement metavariable match this
	construct, but it doesn't work because #ifdef is not a statement.
        Not sure if this is a good or bad thing, at least if there is no else
	because then no statement might be there.
	let afteri =
          !g +> add_node (AfterNode NormalAfterNode) newxi'.labels "[after]" in
	!g#add_arc ((newi, afteri), Direct);
	!g#add_arc ((afteri, taili), Direct);
*)

        Some taili

  ) starti


(*****************************************************************************)
(* Definition of function *)
(*****************************************************************************)

let aux_definition: nodei -> definition -> unit = fun topi funcdef ->

  let lbl_start = [!counter_for_labels] in

  let ({f_name = namefuncs;
        f_type = functype;
        f_storage= sto;
        f_body= compound;
        f_attr= attrs;
        f_old_c_style = oldstyle;
        }, ii) = funcdef in
  let iifunheader, iicompound =
    (match ii with
    | ioparen::icparen::iobrace::icbrace::iifake::isto ->
        ioparen::icparen::iifake::isto,
        [iobrace;icbrace]
    | _ -> raise (Impossible 72)
    )
  in

  let topstatement = Ast_c.mk_st (Ast_c.Compound compound) iicompound in

  let headi = !g +> add_node
    (FunHeader ({
      Ast_c.f_name = namefuncs;
      f_type = functype;
      f_storage = sto;
      f_attr = attrs;
      f_body = [] (* empty body *);
      f_old_c_style = oldstyle;
      }, iifunheader))
    lbl_start ("function " ^ Ast_c.str_of_name namefuncs) in
  let enteri     = !g +> add_node Enter     lbl_0 "[enter]"     in
  let exiti      = !g +> add_node Exit      lbl_0 "[exit]"      in
  let errorexiti = !g +> add_node ErrorExit lbl_0 "[errorexit]" in

  !g#add_arc ((topi, headi), Direct);
  !g#add_arc ((headi, enteri), Direct);

  (* ---------------------------------------------------------------- *)
  (* todocheck: assert ? such as we have "consommer" tous les labels  *)
  let info =
    { initial_info with
      labels = lbl_start;
      labels_assoc = compute_labels_and_create_them topstatement;
      exiti      = Some exiti;
      errorexiti = Some errorexiti;
      compound_caller = FunctionDef;
    }
  in

  let lasti = aux_statement (Some enteri, info) topstatement in
  !g +> add_arc_opt (lasti, exiti)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Helpers for SpecialDeclMacro.
 *
 * could also force the coccier to define
 * the toplevel macro statement as in @@ toplevel_declarator MACRO_PARAM;@@
 * and so I would not need this hack and instead I would to a cleaner
 * match in cocci_vs_c_3.ml of a A.MacroTop vs B.MacroTop
 *
 * todo: update: now I do what I just described, so can remove this code ?
 *)
let specialdeclmacro_to_stmt (s, args, ii) =
  let (iis, iiopar, iicpar, iiptvirg) = tuple_of_list4 ii in
  let ident = Ast_c.RegularName (s, [iis]) in
  let identfinal = Ast_c.mk_e (Ast_c.Ident (ident)) Ast_c.noii in
  let f = Ast_c.mk_e (Ast_c.FunCall (identfinal, args)) [iiopar;iicpar] in
  let stmt = Ast_c.mk_st (Ast_c.ExprStatement (Some f)) [iiptvirg] in
  stmt,  (f, [iiptvirg])



let rec ast_to_control_flow e =

  (* globals (re)initialialisation *)
  g := (new Control_flow_c.G.ograph_mutable);
  counter_for_labels := 1;
  counter_for_braces := 0;
  counter_for_switch := 0;

  let topi = !g +> add_node TopNode lbl_0 "[top]" in

  match e with
  | Ast_c.Namespace (defs, _) ->
      (* todo: incorporate the other defs *)
      let rec loop defs =
	match defs with
	| [] -> None
	| def :: defs ->
	    match ast_to_control_flow def with
	    | None -> loop defs
	    | x -> x in
      loop defs
  | Ast_c.Definition ((defbis,_) as def) ->
      let _funcs = defbis.f_name in
      let _c = defbis.f_body in
      (* if !Flag.show_misc then pr2 ("build info function " ^ funcs); *)
      aux_definition topi def;
      Some !g

  | Ast_c.Declaration _
  | Ast_c.CppTop (Ast_c.Include _)
  | Ast_c.MacroTop _
    ->
      let (elem, str) =
        match e with
        | Ast_c.Declaration decl ->
            (Control_flow_c.Decl decl),  "decl"
        | Ast_c.CppTop (Ast_c.Include inc) ->
            (Control_flow_c.Include inc), "#include"
        | Ast_c.MacroTop (s, args, ii) ->
            let (st, (e, ii)) = specialdeclmacro_to_stmt (s, args, ii) in
            (Control_flow_c.ExprStatement (st, (Some e, ii))), "macrotoplevel"
          (*(Control_flow_c.MacroTop (s, args,ii), "macrotoplevel") *)
        | _ -> raise (Impossible 73)
      in
      let ei =   !g +> add_node elem    lbl_0 str in
      let endi = !g +> add_node EndNode lbl_0 "[end]" in

      !g#add_arc ((topi, ei),Direct);
      !g#add_arc ((ei, endi),Direct);
      Some !g

  | Ast_c.CppTop (Ast_c.Define ((id,ii), (defkind, defval)))  ->
      let s =
	match defkind with
	  Ast_c.Undef -> "#undef " ^ id
	| _ -> "#define " ^ id in
      let headeri = !g+>add_node (DefineHeader ((id, ii), defkind)) lbl_0 s in
      !g#add_arc ((topi, headeri),Direct);

      (match defval with
      | Ast_c.DefineExpr e ->
          let ei   = !g +> add_node (DefineExpr e) lbl_0 "defexpr" in
          let endi = !g +> add_node EndNode        lbl_0 "[end]" in
          !g#add_arc ((headeri, ei) ,Direct);
          !g#add_arc ((ei, endi) ,Direct);

      | Ast_c.DefineType ft ->
          let ei   = !g +> add_node (DefineType ft) lbl_0 "deftyp" in
          let endi = !g +> add_node EndNode         lbl_0 "[end]" in
          !g#add_arc ((headeri, ei) ,Direct);
          !g#add_arc ((ei, endi) ,Direct);

      | Ast_c.DefineStmt st ->
          (* can have some return; inside the statement *)
          let exiti      = !g +> add_node Exit      lbl_0 "[exit]"      in
          let errorexiti = !g +> add_node ErrorExit lbl_0 "[errorexit]" in
          let goto_labels = compute_labels_and_create_them st in

          let info = { initial_info with
            labels_assoc = goto_labels;
            exiti      = Some exiti;
            errorexiti = Some errorexiti;
          }
          in

          let lasti = aux_statement (Some headeri , info) st in
          lasti +> do_option (fun lasti ->
            (* todo? if don't have a lasti ? no EndNode ? CTL will work ? *)
            let endi = !g +> add_node EndNode lbl_0 "[end]" in
            !g#add_arc ((lasti, endi), Direct)
          )


      | Ast_c.DefineDoWhileZero ((st,_e), ii) ->
          let goto_labels = compute_labels_and_create_them st in
          let info = { initial_info with
            labels_assoc = goto_labels } in

          let headerdoi =
            !g +> add_node (DefineDoWhileZeroHeader ((),ii)) lbl_0 "do0" in
          !g#add_arc ((headeri, headerdoi), Direct);
          let lasti = aux_statement (Some headerdoi , info) st in
          lasti +> do_option (fun lasti ->
            let endi = !g +> add_node EndNode lbl_0 "[end]" in
            !g#add_arc ((lasti, endi), Direct)
          )

      | Ast_c.DefineFunction def ->
          aux_definition headeri def;

      | Ast_c.DefineText (s, s_ii) ->
          raise (Error(Define(pinfo_of_ii ii)))
      | Ast_c.DefineEmpty ->
          let endi = !g +> add_node EndNode lbl_0 "[end]" in
          !g#add_arc ((headeri, endi),Direct);
      | Ast_c.DefineInit _ ->
          raise (Error(Define(pinfo_of_ii ii)))
      | Ast_c.DefineMulti sts -> (* christia: todo *)
          raise (Error(Define(pinfo_of_ii ii)))
      | Ast_c.DefineTodo ->
          raise (Error(Define(pinfo_of_ii ii)))

(* old:
      | Ast_c.DefineText (s, ii) ->
          let endi = !g +> add_node EndNode lbl_0 "[end]" in
          !g#add_arc ((headeri, endi),Direct);
      | Ast_c.DefineInit _ ->
          let endi = !g +> add_node EndNode lbl_0 "[end]" in
          !g#add_arc ((headeri, endi),Direct);
      | Ast_c.DefineTodo ->
          let endi = !g +> add_node EndNode lbl_0 "[end]" in
          !g#add_arc ((headeri, endi),Direct);
*)
      );

      Some !g

  | Ast_c.CppTop (Ast_c.Pragma ((id,ii), pragmainfo))  ->
      let elem = PragmaHeader ((id,ii), pragmainfo) in
      let str = "#pragma " ^ id in
      let ei =   !g +> add_node elem    lbl_0 str in
      let endi = !g +> add_node EndNode lbl_0 "[end]" in

      !g#add_arc ((topi, ei),Direct);
      !g#add_arc ((ei, endi),Direct);
      Some !g

  | _ -> None


(*****************************************************************************)
(* CFG loop annotation *)
(*****************************************************************************)

let annotate_loop_nodes g =
  let firsti = Control_flow_c.first_node g in

  (* just for opti a little *)
  let already = Hashtbl.create 101 in

  g +> Control_flow_c.G.dfs_iter_with_path firsti (fun xi path ->
    Hashtbl.add already xi true;
    let aux (yi, _) =
      if Hashtbl.mem already yi && List.mem yi (xi::path)
      then
        let node = KeyMap.find yi g#nodes in
        let ((node2, nodeinfo), nodestr) = node in
        let node' = ((node2, {nodeinfo with is_loop = true}), (nodestr ^ "*"))
        in g#replace_node (yi, node') in
    KeyEdgeSet.iter aux (g#successors xi);
  );
  g


(*****************************************************************************)
(* CFG checks *)
(*****************************************************************************)

(* the second phase, deadcode detection. Old code was raising DeadCode if
 * lasti = None, but maybe not. In fact if have 2 return in the then
 * and else of an if ?
 *
 * alt: but can assert that at least there exist
 * a node to exiti, just check #pred of exiti.
 *
 * Why so many deadcode in Linux ? Ptet que le label est utilisé
 * mais dans le corps d'une macro et donc on le voit pas :(
 *
 *)
let deadcode_detection (g : Control_flow_c.cflow) =

  KeyMap.iter  (fun k node ->
    if KeyEdgeSet.is_empty (g#predecessors k) then
      (match unwrap node with
      | TopNode -> ()
      | FunHeader _ -> ()
      | ErrorExit -> ()
      | Exit -> ()     (* if have 'loop: if(x) return; i++; goto loop' *)
      | SeqEnd _ -> () (* todo?: certaines '}' deviennent orphelins *)
      | x ->
          (match Control_flow_c.extract_fullstatement node with
          | Some st ->
              let ii = Ast_c.get_ii_st_take_care st in
              raise (Error (DeadCode (Some (pinfo_of_ii ii))))
          | _ -> pr2 "CFG: orphan nodes, maybe something weird happened"
          )
      )
  ) g#nodes

(*------------------------------------------------------------------------*)
(* special_cfg_braces: the check are really specific to the way we
 * have build our control_flow, with the { } in the graph so normally
 * all those checks here are useless.
 *
 * ver1: to better error reporting, to report earlier the message, pass
 * the list of '{' (containing morover a brace_identifier) instead of
 * just the depth.
 *)

let check_control_flow (g : cflow) : unit =
  let nodes = g#nodes  in
  let starti = first_node g in
  let visited = ref KeyMap.empty in

  let print_trace_error xs =  pr2 "PB with flow:";  Common.pr2_gen xs; in

  let rec dfs (nodei, (* Depth depth,*) startbraces,  trace)  =
    let trace2 = nodei::trace in
    if KeyMap.mem nodei !visited
    then
      (* if loop back, just check that go back to a state where have same depth
         number *)
      let (*(Depth depth2)*) startbraces2 = KeyMap.find nodei !visited in
      if  (*(depth = depth2)*) startbraces <> startbraces2
      then
        begin
          pr2 (Printf.sprintf "PB with flow: the node %d has not same braces count"
                 nodei);
          print_trace_error trace2
        end
    else
      let children = g#successors nodei in
      visited := KeyMap.add nodei startbraces !visited;
      let newdepth =
        (match unwrap (KeyMap.find nodei g#nodes),  startbraces with
        | SeqStart (_,i,_), xs  -> i::xs
        | SeqEnd (i,_), j::xs ->
            if i = j
            then xs
            else
              begin
                pr2 (Printf.sprintf ("PB with flow: not corresponding match between }%d and excpeted }%d at node %d") i j nodei);
                print_trace_error trace2;
                xs
              end
        | SeqEnd (i,_), [] ->
            pr2 (Printf.sprintf "PB with flow: too much } at }%d " i);
            print_trace_error trace2;
            []
        | _, xs ->  xs
        )
      in


      if KeyEdgeSet.is_empty children
      then
        if (* (depth = 0) *) startbraces <> []
        then print_trace_error trace2
      else
        let aux (key,_) = dfs (key, newdepth, trace2) in
        KeyEdgeSet.iter aux children
    in

  dfs (starti, (* Depth 0*) [], [])

(*****************************************************************************)
(* Error report *)
(*****************************************************************************)

let report_error error =
  let error_from_info info =
    Common.error_message_short info.file ("", info.charpos)
  in
  match error with
  | DeadCode          infoopt ->
      (match infoopt with
      | None ->   pr2 "FLOW: deadcode detected, but cant trace back the place"
      | Some info -> pr2 ("FLOW: deadcode detected: " ^ error_from_info info)
      )
  | CaseNoSwitch      info ->
      pr2 ("FLOW: case without corresponding switch: " ^ error_from_info info)
  | OnlyBreakInSwitch info ->
      pr2 ("FLOW: only break are allowed in switch: " ^ error_from_info info)
  | WeirdSwitch info ->
      pr2 ("FLOW: weird switch: " ^ error_from_info info)
  | NoEnclosingLoop   (info) ->
      pr2 ("FLOW: can't find enclosing loop: " ^ error_from_info info)
  | GotoCantFindLabel (s, info) ->
      pr2 ("FLOW: cant jump to " ^ s ^ ": because we can't find this label")
  | NoExit info ->
      pr2 ("FLOW: can't find exit or error exit: " ^ error_from_info info)
  | DuplicatedLabel s ->
      pr2 ("FLOW: duplicate label " ^ s)
  | NestedFunc  ->
      pr2 ("FLOW: not handling yet nested function")
  | ComputedGoto ->
      pr2 ("FLOW: not handling computed goto yet")
  | Define info ->
      pr2 ("Unsupported form of #define: " ^ error_from_info info)
