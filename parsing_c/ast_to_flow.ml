open Common open Commonop

(*****************************************************************************)
(*
 * todo?: goto,  compute target level (but rare that different I think)
 *    ver1: just do init,  
 *    ver2: compute depth of label (easy, intercept compound in the visitor)
 *
 * todo: 
 * To generate less exception with the breakInsideLoop, analyse correctly
 * the loop deguisé  comme list_for_each 
 * Add a case ForMacro in ast_c (and in lexer/parser), and then do code that 
 * imitates the code for the For.
 * note: the list_for_each was previously converted into Tif by the lexer, 
 * now they are returned as Twhile so less pbs. But not perfect solutio.
 *
 * checktodo: after a switch, need check that all the st in the compound start 
 * with a case: ?
 * checktodo: how ensure that when we call aux_statement recursivly, 
 * we pass it auxinfo_label and not just auxinfo ? how enforce that ?
 *
 * todo: can have code (and so nodes) in many places, in the size of an array, 
 * in the init of initializer, but also in StatementExpr, ...
 *
 * todo?: steal code from CIL ? (but seems complicated ... again)
 *)
(*****************************************************************************)

exception DeadCode of Common.parse_info option
exception CaseNoSwitch      of Common.parse_info
exception OnlyBreakInSwitch of Common.parse_info
exception NoEnclosingLoop   of Common.parse_info
(* exception GotoCantFindLabel *)

open Ast_c
open Control_flow_c

open Ograph_extended
open Oassoc
open Oassocb

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let build_node node labels nodestr =
  let nodestr = 
    if !Flag_parsing_c.show_flow_labels
    then nodestr ^ ("[" ^ (labels +> List.map i_to_s +> join ",") ^ "]")
    else nodestr
  in
  ((node, labels), nodestr)


let lbl_empty = [] 

(*****************************************************************************)
(* Degenerated control flow graph *)
(*****************************************************************************)

(* Used to have mini-CFG containing just declaration *)
let (simple_cfg : node2 -> string -> cflow) = fun node2 nodestr -> 
  let g = (new ograph_extended) in
  let (g, _) = g#add_node (build_node node2 lbl_empty nodestr) in
  g


(*****************************************************************************)
(* Function definition to flow *)
(*****************************************************************************)

(* Information used internally in ast_to_flow and passed recursively. *) 
type additionnal_info =  { 

  ctx: context_info;
  ctx_stack: context_info list;

  (* are we under a ifthen[noelse]. Used for ErrorExit *)
  ctx_bis: bool; 

  (* ctl_braces: the nodei list is to handle current imbrication depth.
   * It contains the must-close '}'. 
   * update: now it is instead a node list. 
   *)
  braces: node list;

  (* ctl: *)
  labels: int list; 
  }

 (* Sometimes have a continue/break and we must know where we must jump.
  *    
  * ctl_brace: The node list in context_info record the number of '}' at the 
  * context point, for instance at the switch point. So that when deeper,
  * we can compute the difference between the number of '}' from root to
  * the context point to close the good number of '}' . For instance 
  * where there is a 'continue', we must close only until the for.
  *)
  and context_info =
      | NoInfo 
      | LoopInfo   of nodei * nodei (* start, end *) * node list    
      | SwitchInfo of nodei * nodei (* start, end *) * node list




let (ast_to_control_flow: definition -> cflow) = fun funcdef ->
  let g = ref (new ograph_extended) in


  (* monad like, >>= *)
  let adjust_g_i (newg,newi) = begin  g := newg;   newi end in
  (* monad like, >> *)
  let adjust_g (newg)        = begin  g := newg;    end in


  let add_node_g node labels nodestr = 
    !g#add_node (build_node node labels nodestr)  +> adjust_g_i
  in
    
  let attach_to_previous_node (starti: int option) (nodei: int) = 
    starti +> do_option (fun starti -> 
      !g#add_arc ((starti, nodei), Direct) +> adjust_g);
  in


  let counter_for_labels = ref 0 in
  incr counter_for_labels;
  let lbl_start = [!counter_for_labels] in


  let ((funcs, functype, sto, compound), ii) = funcdef in
  let iifunheader, iicompound = 
    (match ii with 
    | is::ioparen::icparen::iobrace::icbrace::iifake::isto -> 
        is::ioparen::icparen::iifake::isto,     [iobrace;icbrace]
    | _ -> raise Impossible
    )
  in

  let topstatement = Ast_c.Compound compound, iicompound in


  let headi = add_node_g (FunHeader ((funcs, functype, sto), iifunheader))
                         lbl_start ("function " ^ funcs) in
  let enteri = add_node_g Enter lbl_empty "[enter]" in
  let exiti  = add_node_g Exit  lbl_empty "[exit]" in
  !g#add_arc ((headi, enteri), Direct) +> adjust_g;

  let errorexiti = add_node_g ErrorExit lbl_empty "[errorexit]" in
  


  (* alt: do via a todo list, so can do all in one pass (but more complex) 
   * todo: can also count the depth level and associate it to the node, for 
   * the ctl_braces: 
   *)
  let compute_labels statement = 

    (* map Clabel to index number in graph *)
    let (h: (string, int) oassoc ref) = ref (new oassocb []) in

    begin
      statement +> Visitor_c.vk_statement { 
        Visitor_c.default_visitor_c with 
         Visitor_c.kstatement = (fun (k, bigf) statement -> 
           match statement with
           | Labeled (Ast_c.Label (s, st)),ii -> 
              (* at this point I put a lbl_empty, but later
               * I will put the good labels. *)
              let newi = add_node_g (Label (statement, (s, ii)))
                                     lbl_empty  (s ^ ":") in
               begin
                 (* Clabel already exist ? todo: replace assert with a raise 
                  *  DuplicatedLabel *)
                 assert (not (!h#haskey s)); 
                 h := !h#add (s, newi);
                 (* not k st !!! otherwise in lbl1: lbl2: i++; we miss lbl2 *)
                 k statement; 
               end
           | e -> k e
         )
      };
      !h;
    end
    
  in

  let labels_assoc = compute_labels topstatement in

  (* ctl_braces: *)
  let insert_all_braces xs starti = 
    xs  +> List.fold_left (fun acc e -> 
      (* Have to build a new node (clone), cos cant share it. 
       * update: This is now done by the caller. The clones are in xs.
       *)
      let node = e in
      let newi = !g#add_node node +> adjust_g_i in
      !g#add_arc ((acc, newi), Direct) +> adjust_g;
      newi
     ) starti
  in

  (* For switch, use compteur (or pass int ref) too cos need know order of the
   *  case if then later want to  go from CFG to (original) AST. *)
  let counter_for_braces = ref 0 in
  let counter_for_switch = ref 0 in


  (* Take start, return end.
   * old: old code was returning an int, but goto has no end, so aux_statement 
   * should return   int option.
   * old: old code was taking an int, but should also take int option.
   *
   * Because of special needs of coccinelle, need pass more info, cf
   * type additionnal_info defined above.
   *  - to complete (break, continue (and enclosing loop),   
   *    switch (and associated case, casedefault)) we need to pass additionnal 
   *    info. The start/exit when enter in a loop,  to know the current 'for'.
   *  - to handle the braces, need again pass additionnal info.
   *  - need pass the labels.
   *)
  let rec (aux_statement: (nodei option * additionnal_info) -> statement -> nodei option) = 
   fun (starti, auxinfo) stmt ->

    if not !Flag_parsing_c.label_strategy_2
    then 
      incr counter_for_labels;
    
    let lbl = 
      if not !Flag_parsing_c.label_strategy_2 
      then auxinfo.labels @ [!counter_for_labels]
      else auxinfo.labels 
    in

    (* Normally the new auxinfo to pass recursively to the next aux_statement.
     * But in some cases we add additionnal stuff. *)
    let auxinfo_label = 
      if not !Flag_parsing_c.label_strategy_2
      then
      { auxinfo with labels = auxinfo.labels @ [ !counter_for_labels ]; } 
      else auxinfo
    in
        
    match stmt with
  
    | Ast_c.Compound statxs, ii -> 
        (* flow_to_ast: *)
        let (i1, i2) = 
          match ii with 
          | [i1; i2] -> (i1, i2) 
          | _ -> raise Impossible
        in

        (* ctl_braces: *)
        incr counter_for_braces;
        let brace = !counter_for_braces in

        let o_info  = "{" ^ i_to_s brace in
        let c_info = "}" ^ i_to_s brace in
   
        let newi =    add_node_g (SeqStart (stmt, brace, i1)) lbl o_info in
        let endnode = build_node (SeqEnd (brace, i2))         lbl c_info in

        let newauxinfo = 
          { auxinfo_label with braces = endnode:: auxinfo_label.braces }
        in
       

        attach_to_previous_node starti newi;
        let starti = Some newi in

        statxs +> List.fold_left (fun starti statement ->
          if !Flag_parsing_c.label_strategy_2
          then 
            incr counter_for_labels;

          let newauxinfo' = 
            if !Flag_parsing_c.label_strategy_2
            then 
            { newauxinfo with 
              labels = auxinfo.labels @ [ !counter_for_labels ] 
            } 
            else newauxinfo
          in
          aux_statement (starti, newauxinfo') statement
        ) starti

        (* braces: *)
        +> fmap (fun starti -> 
              (* subtil: not always return a Some.
               * Note that if starti is None, alors forcement ca veut dire
               * qu'il y'a eu un return (ou goto), et donc forcement les 
               * braces auront au moins ete crée une fois, et donc flow_to_ast
               * marchera.
               * Sauf si le goto revient en arriere ? mais dans ce cas
               * ca veut dire que le programme boucle. Pour qu'il boucle pas
               * il faut forcement au moins un return.
               *)
              let endi = !g#add_node endnode +> adjust_g_i in
              !g#add_arc ((starti, endi), Direct) +> adjust_g;
              endi 
             ) 


     (* ------------------------- *)        
    | Labeled (Ast_c.Label (s, st)), ii -> 
        let ilabel = labels_assoc#find s in
        let node = build_node (unwrap (!g#nodes#find ilabel)) lbl (s ^ ":") in
        !g#replace_node (ilabel, node) +> adjust_g;
        attach_to_previous_node starti ilabel;
        aux_statement (Some ilabel, auxinfo_label) st


    | Jump (Ast_c.Goto s), ii -> 
       (* special_cfg_ast: *)
       let newi = add_node_g (Goto (stmt, (s,ii))) lbl ("goto " ^ s ^ ":") in
       attach_to_previous_node starti newi;

       let ilabel = 
         try labels_assoc#find s 
         with Not_found -> failwith ("cant jump to " ^ s ^ 
                                     ": because we can't find this label")
       in
       (* attach_to_previous_node starti ilabel; 
        * todo: special_case: suppose that always goto to toplevel of function,
        * hence the Common.init 
        * todo?: can perhaps report when a goto is not a classic error_goto ? 
        * that is when it does not jump to the toplevel of the function.
        *)
       let newi = insert_all_braces (Common.list_init auxinfo.braces) newi in
       !g#add_arc ((newi, ilabel), Direct) +> adjust_g;
       None
        


        
     (* ------------------------- *)        
    | Ast_c.ExprStatement opte, ii -> 
        (* flow_to_ast:   old: when opte = None, then do not add in CFG. *)
        let s = 
          match opte with
          | None -> "empty;"
          | Some e -> 
              let ((unwrap_e, typ), ii) = e in
              (match unwrap_e with
              | FunCall (((Ident f, _typ), _ii), _args) -> 
                  f ^ "(...)"
              | Assignment (((Ident var, _typ), _ii), SimpleAssign, e) -> 
                  var ^ " = ... ;"
              | Assignment 
                  (((RecordAccess (((Ident var, _typ), _ii), field), _typ2), 
                    _ii2),
                   SimpleAssign, 
                   e) -> 
                     var ^ "." ^ field ^ " = ... ;"
              | MacroCall _ -> 
                  (* todo: it can contain some goto, return, so should 
                     modify the CFG *)
                   "<macrocall>"
                     
              | _ -> "statement"
          )
        in
        let newi = add_node_g (ExprStatement (stmt, (opte, ii))) lbl s in
        attach_to_previous_node starti newi;
        Some newi
        

     (* ------------------------- *)        
    | Selection  (Ast_c.If (e, st1, (Ast_c.ExprStatement (None), ii2))), ii -> 
        let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
        let ii = [i1;i2;i3] in
       (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
        *                  |                                      |
        *                  |->   newfakeelse -> ... -> finalelse -|
        * update: there is now also a link directly to lasti.
        *  
        * because of CTL, now do different things if we are in a ifthen or
        * ifthenelse.
        *)
        let newi = add_node_g (IfHeader (stmt, (e, ii))) lbl ("if") in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode        lbl "[then]" in
        let newfakeelse = add_node_g FallThroughNode lbl "[fallthrough]" in
        let afteri = add_node_g AfterNode lbl "[after]" in
        let lasti  = add_node_g (EndStatement (Some iifakeend)) lbl "[endif]" 
        in

        (* for ErrorExit heuristic *)
        let newauxinfo = { auxinfo_label with  ctx_bis = true; } in

        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), Direct) +> adjust_g;
        !g#add_arc ((newi, afteri), Direct) +> adjust_g;
        !g#add_arc ((afteri, lasti), Direct) +> adjust_g;
        !g#add_arc ((newfakeelse, lasti), Direct) +> adjust_g;

        let finalthen = aux_statement (Some newfakethen, newauxinfo) st1 in
        attach_to_previous_node finalthen lasti;
        Some lasti

        
    | Selection  (Ast_c.If (e, st1, st2)), ii -> 
       (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
        *                 |                                      |
        *                 |->   newfakeelse -> ... -> finalelse -|
        * update: there is now also a link directly to lasti.
        *)
        let (iiheader, iielse, iifakeend) = 
          match ii with
          | [i1;i2;i3;i4;i5] -> [i1;i2;i3], i4, i5
          | _ -> raise Impossible
        in
        let newi = add_node_g (IfHeader (stmt, (e, iiheader))) lbl "if" in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode  lbl "[then]" in
        let newfakeelse = add_node_g FalseNode lbl "[else]" in
        let elsenode = add_node_g (Else iielse) lbl "else" in


        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), Direct) +> adjust_g;

        !g#add_arc ((newfakeelse, elsenode), Direct) +> adjust_g;

        let finalthen = aux_statement (Some newfakethen, auxinfo_label) st1 in
        let finalelse = aux_statement (Some elsenode, auxinfo_label) st2 in

        (match finalthen, finalelse with 
          | (None, None) -> None
          | _ -> 
              let lasti = add_node_g (EndStatement(Some iifakeend)) lbl "[endif]" in
              let afteri = add_node_g AfterNode lbl "[after]" in
              !g#add_arc ((newi, afteri),  Direct) +> adjust_g;
              !g#add_arc ((afteri, lasti), Direct) +> adjust_g;
              begin
                attach_to_previous_node finalthen lasti;
                attach_to_previous_node finalelse lasti;
                Some lasti
             end)
          
        
    | Selection  (Ast_c.IfCpp (st1s, st2s)), ii -> 
        let (ii,iifakeend) = 
          match ii with
          | [i1;i2;i3;i4] -> [i1;i2;i3], i4
          | [i1;i2;i3] -> [i1;i2], i3
          | _ -> raise Impossible
        in

        let newi = add_node_g (IfCpp (stmt, ((), ii))) lbl "ifcpp" in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode  lbl "[then]" in
        let newfakeelse = add_node_g FalseNode lbl "[else]" in

        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), Direct) +> adjust_g;

        let aux_statement_list (starti, newauxinfo) statxs =
        statxs +> List.fold_left (fun starti statement ->
          aux_statement (starti, newauxinfo) statement
        ) starti
        in


        let finalthen = aux_statement_list (Some newfakethen, auxinfo_label) st1s in
        let finalelse = aux_statement_list (Some newfakeelse, auxinfo_label) st2s in

        (match finalthen, finalelse with 
          | (None, None) -> None
          | _ -> 
              let lasti =  add_node_g (EndStatement (Some iifakeend)) lbl "[endifcpp]" in
              begin
                attach_to_previous_node finalthen lasti;
                attach_to_previous_node finalelse lasti;
                Some lasti
             end
        )
        

     (* ------------------------- *)        
    | Selection  (Ast_c.Switch (e, st)), ii -> 
        let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
        let ii = [i1;i2;i3] in


        let newswitchi = add_node_g (SwitchHeader (stmt, (e,ii))) lbl "switch" 
        in
        attach_to_previous_node starti newswitchi;

        let newendswitch = add_node_g (EndStatement (Some iifakeend)) lbl "[endswitch]" in

    
        (* The newswitchi is for the labels to know where to attach.
         * The newendswitch (endi) is for the 'break'. *)

        (* let finalthen = aux_statement (None, newauxinfo) st in *)

        (* Prepare var to be able to copy paste  *)
         let starti = None in
         (* let auxinfo = newauxinfo in *)
         let stmt = st in

         (* COPY PASTE of compound case.
          * Why copy paste ? why can't call directly compound case ? 
          * because we need to build a context_info that need some of the
          * information build inside the compound case: the nodei of {
          *)

         let finalthen = 
             match stmt with
         
             | Ast_c.Compound statxs, ii -> 
                 let (i1, i2) = 
                   match ii with 
                   | [i1; i2] -> (i1, i2) 
                   | _ -> raise Impossible
                 in

                 incr counter_for_braces;
                 let brace = !counter_for_braces in

                 let o_info  = "{" ^ i_to_s brace in
                 let c_info = "}" ^ i_to_s brace in

                 (* TODO, we should not allow to match a stmt that corresponds
                  * to a compound of a switch, so really SeqStart (stmt, ...)
                  * here ? 
                  *)
                 let newi = add_node_g (SeqStart (stmt,brace,i1)) lbl o_info in
                 let endnode = build_node (SeqEnd (brace, i2))    lbl c_info in

                 let newauxinfo = 
                  { auxinfo_label with braces = endnode:: auxinfo_label.braces}
                 in

                 (* new: cos of switch *)
                 let newauxinfo = { newauxinfo with 
                       ctx = SwitchInfo (newi, newendswitch, auxinfo.braces);
                       ctx_stack = newauxinfo.ctx::newauxinfo.ctx_stack
                   }
                 in
                 !g#add_arc ((newswitchi, newi), Direct) +> adjust_g; 
                 (* new: if have not a default case, then must add an edge 
                  * between start to end.
                  * todo? except if the case[range] coverthe whole spectrum 
                  *)

                 if not (statxs +> List.exists (function 
                   | (Labeled (Ast_c.Default _), _) -> true
                   | _ -> false
                    ))
                 then
                   !g#add_arc ((newswitchi, newendswitch), Direct) +> adjust_g;

                 attach_to_previous_node starti newi;
                 let starti = Some newi in
         
                 statxs +> List.fold_left (fun starti stat ->
                   aux_statement (starti, newauxinfo) stat
                 ) starti
         
         
                 (* braces: *)
                 +> fmap (fun starti -> 
                       let endi = !g#add_node endnode   +> adjust_g_i in
                       !g#add_arc ((starti, endi), Direct) +> adjust_g;
                       endi 
                         )
             | x -> raise Impossible
         in
         attach_to_previous_node finalthen newendswitch;


         (* what if has only returns inside. We must  try to see if the
          * newendswitch has been used via a 'break;'  or because no 
          * 'default:')
          *)
         (match finalthen with
         | Some finalthen -> 
             !g#add_arc ((finalthen, newendswitch), Direct) +> adjust_g;
             Some newendswitch
         | None -> 
             if (!g#predecessors newendswitch)#null
             then 
               begin
                 assert ((!g#successors newendswitch)#null);
                 !g#del_node newendswitch +> adjust_g;
                 None
               end
             else 
               Some newendswitch
         )

    | Labeled (Ast_c.Case  (_, _)), _
    | Labeled (Ast_c.CaseRange  (_, _, _)), _ -> 

        incr counter_for_switch;
        let switchrank = !counter_for_switch in
        let node, st = 
          match stmt with 
          | Labeled (Ast_c.Case  (e, st)), ii -> 
              (Case (stmt, (e, ii))),  st
          | Labeled (Ast_c.CaseRange  (e, e2, st)), ii -> 
              (CaseRange (stmt, ((e, e2), ii))), st
          | _ -> raise Impossible
        in

        let newi = add_node_g node  lbl "case:" in

        (match auxinfo.ctx with
        | SwitchInfo (startbrace, switchendi, _braces) -> 
            (* no need to attach to previous for the first case, cos would be
             * redundant. *)
            starti +> do_option (fun starti -> 
              if starti <> startbrace
              then attach_to_previous_node (Some starti) newi; 
              );

            let newcasenodei = 
              add_node_g (CaseNode switchrank) 
                 lbl ("[casenode] " ^ i_to_s switchrank) 
            in
            !g#add_arc ((startbrace, newcasenodei), Direct) +> adjust_g;
            !g#add_arc ((newcasenodei, newi), Direct) +> adjust_g;
        | _ -> raise (CaseNoSwitch (fst (List.hd ii)))
        );
        aux_statement (Some newi, auxinfo_label) st
        

    | Labeled (Ast_c.Default st), ii -> 
        incr counter_for_switch;
        let switchrank = !counter_for_switch in

        let newi = add_node_g (Default (stmt, ((),ii))) lbl "case default:" in
        attach_to_previous_node starti newi;

        (match auxinfo.ctx with
        | SwitchInfo (startbrace, switchendi, _braces) -> 
             let newcasenodei = 
               add_node_g (CaseNode switchrank) 
                 lbl ("[casenode] " ^ i_to_s switchrank) 
             in
             !g#add_arc ((startbrace, newcasenodei), Direct) +> adjust_g;
             !g#add_arc ((newcasenodei, newi), Direct) +> adjust_g;
        | _ -> raise (CaseNoSwitch (fst (List.hd ii)))
        );
        aux_statement (Some newi, auxinfo_label) st






     (* ------------------------- *)        
    | Iteration  (Ast_c.While (e, st)), ii -> 
       (* starti -> newi ---> newfakethen -> ... -> finalthen -
        *             |---|-----------------------------------|
        *                 |-> newfakelse 
        *)

        let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
        let ii = [i1;i2;i3] in

        let newi = add_node_g (WhileHeader (stmt, (e,ii))) lbl "while" in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode  lbl "[whiletrue]" in
        (* let newfakeelse = add_node_g FalseNode lbl "[endwhile]" in *)
        let newafter = add_node_g FallThroughNode lbl "[whilefall]" in
        let newfakeelse = add_node_g (EndStatement (Some iifakeend)) lbl "[endwhile]" in

        let newauxinfo = { auxinfo_label with
           ctx = LoopInfo (newi, newfakeelse,  auxinfo_label.braces);
           ctx_stack = auxinfo_label.ctx::auxinfo_label.ctx_stack
          }
        in

        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g;
        !g#add_arc ((newafter, newfakeelse), Direct) +> adjust_g;
        !g#add_arc ((newi, newafter), Direct) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen, newauxinfo) st in
        attach_to_previous_node finalthen newi;
        Some newfakeelse

        
    (* This time, may return None, for instance if goto in body of dowhile
     * (whereas While cant return None). But if return None, certainly 
     * some deadcode.
     *)
    | Iteration  (Ast_c.DoWhile (st, e)), ii -> 
       (* starti -> doi ---> ... ---> finalthen (opt) ---> whiletaili
        *             |--------- newfakethen ---------------|  |---> newfakelse
        *)

        let (iido, iiwhiletail, iifakeend) = 
          match ii with
          | [i1;i2;i3;i4;i5;i6] -> i1, [i2;i3;i4;i5], i6
          | _ -> raise Impossible
        in
        let doi = add_node_g (DoHeader (stmt, iido))  lbl "do" in
        attach_to_previous_node starti doi;
        let taili = add_node_g (DoWhileTail (e, iiwhiletail)) lbl "whiletail" 
        in


        let newfakethen = add_node_g TrueNode lbl "[dowhiletrue]" in
        (*let newfakeelse = add_node_g FalseNode lbl "[enddowhile]" in *)
        let newafter = add_node_g FallThroughNode lbl "[dowhilefall]" in
        let newfakeelse = add_node_g (EndStatement (Some iifakeend)) lbl "[enddowhile]" in

        let newauxinfo = { auxinfo_label with
           ctx = LoopInfo (taili, newfakeelse, auxinfo_label.braces);
           ctx_stack = auxinfo_label.ctx::auxinfo_label.ctx_stack
          }
        in

        !g#add_arc ((taili, newfakethen), Direct) +> adjust_g; 
        !g#add_arc ((newafter, newfakeelse), Direct) +> adjust_g;
        !g#add_arc ((taili, newafter), Direct) +> adjust_g;

        !g#add_arc ((newfakethen, doi), Direct) +> adjust_g; 

        let finalthen = aux_statement (Some doi, newauxinfo) st in 
        (match finalthen with
        | None -> 
            if (!g#predecessors taili)#null
            then raise (DeadCode (Some (ii +> List.hd +> fst)))
            else Some newfakeelse
        | Some finali -> 
            !g#add_arc ((finali, taili), Direct) +> adjust_g;
            Some newfakeelse
        )
            


    | Iteration  (Ast_c.For (e1opt, e2opt, e3opt, st)), ii -> 
        let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
        let ii = [i1;i2;i3] in

        let newi = 
          add_node_g (ForHeader (stmt, ((e1opt, e2opt, e3opt), ii))) lbl "for" 
        in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode  lbl "[fortrue]" in
        (*let newfakeelse = add_node_g FalseNode lbl "[endfor]" in*)
        let newafter = add_node_g FallThroughNode lbl "[forfall]" in
        let newfakeelse = add_node_g (EndStatement (Some iifakeend)) lbl "[endfor]" in

        let newauxinfo = { auxinfo_label with
             ctx = LoopInfo (newi, newfakeelse, auxinfo_label.braces); 
             ctx_stack = auxinfo_label.ctx::auxinfo_label.ctx_stack
          }
        in

        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g;
        !g#add_arc ((newafter, newfakeelse), Direct) +> adjust_g;
        !g#add_arc ((newi, newafter), Direct) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen, newauxinfo) st in
        attach_to_previous_node finalthen newi;
        Some newfakeelse


     (* ------------------------- *)        
    | Jump ((Ast_c.Continue|Ast_c.Break) as x),ii ->  
        (* flow_to_ast: *)
        let newi = 
          add_node_g 
            (match x with
            | Ast_c.Continue -> Continue (stmt, ((), ii))
            | Ast_c.Break    -> Break    (stmt, ((), ii))
            | _ -> raise Impossible
            )
            lbl "continue_break;"
        in
        attach_to_previous_node starti newi;

        (* let newi = some starti in *)

        (match auxinfo.ctx with
        | LoopInfo (loopstarti, loopendi, braces) -> 
            let desti = 
              (match x with 
              | Ast_c.Break -> loopendi 
              | Ast_c.Continue -> loopstarti 
              | x -> raise Impossible
              ) in
            let difference = List.length auxinfo.braces - List.length braces in
            assert (difference >= 0);
            let toend = take difference auxinfo.braces in
            let newi = insert_all_braces toend newi in
            !g#add_arc ((newi, desti), Direct) +> adjust_g;
            None

        | SwitchInfo (startbrace, loopendi, braces) -> 
            if x = Ast_c.Break then
              begin
                let difference = 
                  List.length auxinfo.braces - List.length braces
                in
                assert (difference >= 0);
                let toend = take difference auxinfo.braces in
                let newi = insert_all_braces toend newi in
                !g#add_arc ((newi, loopendi), Direct) +> adjust_g;
                None
              end
            else 
              (* old: raise (OnlyBreakInSwitch (fst (List.hd ii)))
               * in fact can have a continue, 
               *)
             if x = Ast_c.Continue then
               (try 
                 let (loopstarti, loopendi, braces) = 
                   auxinfo.ctx_stack +> find_some (function 
                     | LoopInfo (loopstarti, loopendi, braces) -> 
                         Some (loopstarti, loopendi, braces)
                     | _ -> None
                                                  ) in
                 let desti = loopstarti in
                 let difference = 
                   List.length auxinfo.braces - List.length braces in
                 assert (difference >= 0);
                 let toend = take difference auxinfo.braces in
                 let newi = insert_all_braces toend newi in
                 !g#add_arc ((newi, desti), Direct) +> adjust_g;
                 None
                 
                 with Not_found -> 
                   raise (OnlyBreakInSwitch (fst (List.hd ii)))
               )
             else raise Impossible
        | NoInfo -> raise (NoEnclosingLoop (fst (List.hd ii)))
        )        





    | Jump ((Ast_c.Return | Ast_c.ReturnExpr _) as kind), ii -> 
        (* flow_to_ast: *)
        let info = 
          match kind with
          | Ast_c.Return -> "return"
          | Ast_c.ReturnExpr _ -> "return ..."
          | _ -> raise Impossible
        in
        let newi = 
          add_node_g 
            (match kind with
            | Ast_c.Return ->       Return (stmt, ((),ii))
            | Ast_c.ReturnExpr e -> ReturnExpr (stmt, (e, ii))
            | _ -> raise Impossible
            )
            lbl info 
        in
        attach_to_previous_node starti newi;
        let newi = insert_all_braces auxinfo.braces newi in

        if auxinfo.ctx_bis
        then !g#add_arc ((newi, errorexiti), Direct) +> adjust_g
        else !g#add_arc ((newi, exiti), Direct) +> adjust_g
        ;
        None


    (* ------------------------- *)        
    | Ast_c.Decl decl, ii -> 
       let s = 
         (match decl with
         | (Ast_c.DeclList ([(Some ((s, _),_), typ, sto), _], _)) -> 
             "decl:" ^ s
         | _ -> "decl_novar_or_multivar"
         ) in
              
       let newi = add_node_g (Decl (decl)) lbl s in
       attach_to_previous_node starti newi;
       Some newi
        
    (* ------------------------- *)        
    | Ast_c.Asm, ii -> 
        let newi = add_node_g Asm lbl "asm;" in
        attach_to_previous_node starti newi;
        Some newi

  in
  (* todocheck: assert ? such as we have "consommer" tous les labels  *)

  let info = { 
    ctx = NoInfo; 
    ctx_stack = [];
    ctx_bis = false;
    labels = lbl_start; 
    braces = [] 
  } 
  in
  let lasti = aux_statement (Some enteri, info) topstatement in
  attach_to_previous_node lasti exiti;
  !g


(*****************************************************************************)
(* CFG checks *)
(*****************************************************************************)
(*
 * note: deadCode detection
 * What is dead code ? when there is no starti  to start from ? => make starti
 * an option too ?
 * Si on arrive sur un label: au moment d'un deadCode, on peut verifier les 
 * predecesseurs de ce label, auquel cas si y'en a, ca veut dire qu'en fait 
 * c'est pas du deadCode et que donc on peut se permettre de partir d'un starti
 * à None.
 * Mais si on a   xx; goto far:; near: yy; zz; far: goto near:. Bon ca doit
 * etre un cas tres tres rare, mais a cause de notre parcours, on va rejeter
 * ce programme car au moment d'arriver sur near:  on n'a pas encore de 
 * predecesseurs pour ce label.
 * De meme, meme le cas simple ou la derniere instruction c'est un return, 
 * alors ca va generer un DeadCode :(
 *  => Make a first pass where dont launch exn at all, create nodes, if starti
 *   is None then dont add    arc. 
 *     Make a second pass, just check that all nodes (except enter) have
 *      predecessors. (todo: if the pb is at a fake node, then try first
 *      successos that is non fake)
 *  => Make starti  an option too.
 *     So type is now  int option -> statement -> int option.
 * 
 * old: I think that DeadCode is too aggressive, what if  have both return in 
 *  else/then ? 
 *)

let deadcode_detection g = 
  (* phase 2, deadcode detection 
     old raise DeadCode: if lasti = None, but maybe not, in fact if have 2
     return in the then and else of an if ? 
     alt: but can assert that at least there exist a node to exiti,  just 
     check #pred of exiti *)

  g#nodes#iter (fun (k, node) -> 
    let pred = g#predecessors k in
    if pred#null then 
      (match unwrap node with
      | FunHeader _ -> ()
      | ErrorExit -> ()
      | Exit -> () (* if have in .c   loop: if(x) return; i++; goto loop *)
      (* old: | Enter -> () *)
      (*      | EndStatement _ -> pr2 "control_flow: deadcode sur fake node, pas grave"; *)
      | SeqEnd _ -> () (* todo?: certaines deviennent orphelins *)
      | x -> 
          (match Control_flow_c.extract_fullstatement node with
          | Some (st, ii::iis) -> raise (DeadCode (Some (fst ii)))
          | _ -> 
             pr2 "control_flow: orphelin nodes, maybe something wierd happened"
                )
      )
    )

(*
 * special_cfg_braces: 
 * The check are really specific to the way we have build our control_flow, 
 * with the { } in the graph so normally all those checks here are useless.
 *
 * evo: to better error reporting, to report earlier the message, 
 * pass the list of '{' (containing morover a brace_identifier) instead of 
 * just the depth.
 *)

let (check_control_flow: cflow -> unit) = fun g ->

  let nodes = g#nodes  in
  let starti = get_first_node g in

  let visited = ref (new oassocb []) in

  let print_trace_error xs =  pr2 "PB with flow:";  pr2 (Dumper.dump xs); in

  let rec dfs (nodei, (* Depth depth,*) startbraces,  trace)  = 
    let trace2 = nodei::trace in
    if !visited#haskey nodei 
    then 
      (* if loop back, just check that go back to a state where have same depth
         number *)
      let (*(Depth depth2)*) startbraces2 = !visited#find nodei in
      if  (*(depth = depth2)*) startbraces <> startbraces2
      then  
        begin 
          pr2 (sprintf "PB with flow: the node %d has not same braces count" 
                 nodei);  
          print_trace_error trace2  
        end
    else 
      let children = g#successors nodei in
      let _ = visited := !visited#add (nodei, (* Depth depth*) startbraces) in

      (* old: good, but detect a missing } too late, only at the end
      let newdepth = 
        (match fst (nodes#find nodei) with
        | StartBrace i -> Depth (depth + 1)
        | EndBrace i   -> Depth (depth - 1)
        | _ -> Depth depth
        ) 
      in
      *)
      let newdepth = 
        (match unwrap (nodes#find nodei),  startbraces with
        | SeqStart (_,i,_), xs  -> i::xs
        | SeqEnd (i,_), j::xs -> 
            if i = j 
            then xs
            else 
              begin 
                pr2 (sprintf ("PB with flow: not corresponding match between }%d and excpeted }%d at node %d") i j nodei); 
                print_trace_error trace2; 
                xs 
              end
        | SeqEnd (i,_), [] -> 
            pr2 (sprintf "PB with flow: too much } at }%d " i);
            print_trace_error trace2; 
            []
        | _, xs ->  xs
        ) 
      in

   
      if children#tolist = [] 
      then 
        if (* (depth = 0) *) startbraces <> []
        then print_trace_error trace2
      else 
        children#tolist +> List.iter (fun (nodei,_) -> 
          dfs (nodei, newdepth, trace2)
        )
    in

  dfs (starti, (* Depth 0*) [], [])
