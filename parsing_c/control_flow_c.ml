open Commonop open Common

(******************************************************************************)
(*
 note: deadCode detection
  What is dead code ? when there is no starti  to start from ? => make starti 
  an option too ?
  Si on arrive sur un label: au moment d'un deadCode, on peut verifier les 
  predecesseurs de ce label, auquel cas si y'en a, ca veut dire qu'en fait c'est
  pas du deadCode et que donc on peut se permettre de partir d'un starti à None.
  Mais si on a   xx; goto far:; near: yy; zz; far: goto near:. Bon ca doit etre 
  un cas tres tres rare, mais a cause de notre parcours, on va rejeter ce 
  programme car au moment d'arriver sur near:  on n'a pas encore de 
  predecesseurs pour ce label.
  De meme, meme le cas simple ou la derniere instruction c'est un return, alors 
  ca va generer un DeadCode :(
  => Make a first pass where dont launch exn at all, create nodes, if starti is 
     None then dont add    arc. 
     A second pass, just check that all nodes (except enter) have predecessors. 
      (todo: if the pb is at a fake node, then try first successos that is 
      non fake)
  => Make starti  an option too.
     So type is now  int option -> statement -> int option.

 old: I think that DeadCode is too aggressive, what if  have both return in 
  else/then ? 


 note: special_cfg_ast tag
  Because need go back from cfg to ast, have to introduce additionnal nodes 
  that normally are not needed by a normal compiler.

 note: special_cfg_braces tag
  Because julia wants the { and } in the control flow graph to make it easier 
  for the matcher, have to add some nodes in the graph.
  As for special_cfg_ast, normally not needed by a normal compiler.
  done: return,  break/continue (for while/for/dowhile/if),  
        break/continue  (for switch), 

 todo?: goto,  compute target level (but rare that different I think)
        ver1: just do init,  
        ver2: compute depth of label (easy, intercept compound in the visitor)

 DONE? add info in nodes, to later be able to pretty print back

 DONE 
  For switch, pass int ref (compteur) too ? (cos need know order of the case 
  if then later want to  go from CFG to (original) AST).


 todo: expression, linearize,  funcall (and launch exn  with StatementExpr)

 todo: To generate less exception with the breakInsideLoop, analyse correctly 
   the loop deguisé  comme list_for_each (qui sont actuellement retourné comme 
   des Tif par le lexer).
   Add a case ForMacro in ast_c (and in lexer/parser), and then do code that 
   imitates the code for the For.

 checktodo: after a switch, need check that all the st in the compound start 
 with a case: ?

 todo: can have code (and so nodes) in many places, in the size of an array, in 
  the init of initializer, but also in StatementExpr, ...

 todo?: steal code from CIL ? (but seems complicated ... again)


*)

open Ograph_extended
open Oassoc
open Oassocb

open Ast_c
open Visitor_c


(*----------------------------------------------------------------------------*)

type node = node1 * string (* to debug *)
and node1 = node2 * int list (* The labels. Trick used by ctl engine *)
and node2 = 
  | HeadFunc of definition

  | Enter 
  | Exit


(*  | NestedFunCall of expression   (* cos "fake" node *) (* TODO *) *)

  | Statement     of statement
  | Declaration   of declaration

  | Fake (* todo: est amené a disparaitre *)

  (* special_cfg_braces: *)
  (* The int is here to indicate to what { } they correspond. 
     Two pairwise { } share the same number. kind of "brace_identifier". 
     Used mostly for debugging or for checks.
     update: more importantly, used with CTL engine.
  *)
  | StartBrace of int * statement * Ast_c.info (* special_cfg_ast *)
  | EndBrace   of int * Ast_c.info

  | CaseNode of int (* to be able later to go back from a flow to an ast *)

  (* used by CTL *)
  | TrueNode
  | FalseNode
  | AfterNode
  | FallThroughNode

let unwrap ((node, labels), nodestr) = node
let rewrap ((_node, labels), nodestr) node = (node, labels), nodestr
let extract_labels ((node, labels), nodestr) = labels

type edge = Direct
(* old: | SpecialEdge,  with code later such as
     !g#add_arc ((newi, newfakeelse), SpecialEdge) +> adjust_g;  
   But not needed anymore, because have moved that info in the AfterNode.
*)


exception DeadCode of Common.parse_info option
exception CaseNoSwitch      of Common.parse_info
exception OnlyBreakInSwitch of Common.parse_info
exception NoEnclosingLoop   of Common.parse_info



(******************************************************************************)
(* special_cfg_braces: the nodei list is to handle current imbrication depth 
  (contain the must-close '}' ) *)
type additionnal_info =  additionnal_info2 * node list * int list
  and additionnal_info2 =
      | NoInfo 
      | LoopInfo   of nodei * nodei (* start, end *) *
                      node list    (* special_cfg_braces: *)
      | SwitchInfo of nodei * nodei (* start, end *) * 
                      node list    (* special_cfg_braces: *)

(* obsolete: type depthi = Depth of int *)

let get_labels (info, braces, labels) = labels
let get_braces (info, braces, labels) = braces
let add_label  (info, braces, labels) label = (info, braces, labels @ [label])

(*----------------------------------------------------------------------------*)
let (ast_to_control_flow: definition -> (node, edge) ograph_extended) = 
 fun funcdef ->
  let g = ref (new ograph_extended) in


  (* monad like, >>= *)
  let adjust_g_i (newg,newi) = begin  g := newg;   newi end in
  (* monad like, >> *)
  let adjust_g (newg)        = begin  g := newg;    end in

  let build_node node labels nodestr =
    let nodestr = 
      if !Flag_parsing_c.show_flow_labels
      then nodestr ^ ("[" ^ (labels +> List.map i_to_s +> join ",") ^ "]")
      else nodestr
    in
    ((node, labels), nodestr)
  in

  let add_node_g node labels nodestr = 
    !g#add_node (build_node node labels nodestr)  +> adjust_g_i
  in
    

  let attach_to_previous_node (starti: int option) (nodei: int) = 
    starti +> do_option (fun starti -> 
      !g#add_arc ((starti, nodei), Direct) +> adjust_g);
  in

  let label_list_empty = [] in



  let (funcs, functype, sto, compound, ((_,_,ii) as moreinfo)) = funcdef in
  let topstatement = Compound compound, ii in


  let headi = add_node_g (HeadFunc (funcs, functype, sto, [], moreinfo))
                         label_list_empty ("function " ^ funcs) in

(*
  let enteri = !g#add_node (Enter, "[enter]") +> adjust_g_i in
  let _ = !g#add_arc ((headi, enteri), Direct) +> adjust_g in
*)
  let enteri = headi in
  let exiti  = add_node_g Exit label_list_empty "[exit]" in
  


  (* alt: do via a todo list, so can do all in one pass (but more complex) *)
  (* todo: can also count the depth level and associate it to the node, for the 
     special_cfg_braces: *)
  let compute_labels statement = 

    (* map label to index number in graph *)
    let (h: (string, int) oassoc ref) = ref (new oassocb []) in

    begin
      statement +> visitor_statement_k { default_visitor_c with 
         kstatement = (fun (k, bigf) statement -> 
           match statement with
           | Labeled (Label (s, st)),ii -> 
              (* at this point I put a label_list_empty, but later
                 I will put the good labels. *)
              let newi = add_node_g (Statement (Labeled (Label (s, st)),ii)) 
                                     label_list_empty  (s ^ ":") in
               begin
                 (* label already exist ? todo: replace assert with a raise 
                    DuplicatedLabel *)
                 assert (not (!h#haskey s)); 
                 h := !h#add (s, newi);
                 k st;
               end
           | e -> k e
                 )};
      !h;
    end
    
  in

  let labels_assoc = compute_labels topstatement in



  let special_cfg_insert_all_braces xs starti = 
    xs  +> List.fold_left (fun acc e -> 
      (* Have to build a new node (clone), cos cant share it. This is now done
         by the caller. The clones are in xs *)
      (* old: !g#add_arc ((acc, e), Direct) +> adjust_g;  e  *)
      (* old: let node = !g#nodes#tolist +> List.find (fun (i, _) -> i = e) +> snd in *)
      let node = e in
      let newi = !g#add_node node +> adjust_g_i in

      !g#add_arc ((acc, newi), Direct) +> adjust_g;
      newi
      
      ) starti
  in
  let counter_for_braces = ref 0 in
  let counter_for_switch = ref 0 in
  let counter_for_labels = ref 0 in

  (* take start, return end 
      old: old code was returning an int, but goto has no end => aux_statement 
      should return   int option.
      old: old code was taking an int, but should also take int option.
     addon: to complete (break, continue (and enclosing loop),   
      switch (and associated case, casedefault)) we need to pass additionnal 
       info. The start/exit when enter in a loop,  to know the current 'for'.
     addon: to handle the special_cfg_braces, need again pass additionnal info.
  *)
  let rec (aux_statement: 
             (nodei option * additionnal_info) -> statement -> nodei option) = 
   fun (starti, auxinfo) statement ->

    incr counter_for_labels;
    let label_list = get_labels auxinfo @ [!counter_for_labels] in
    let auxinfo_label = add_label auxinfo !counter_for_labels in

    match statement with

    | Compound (declxs_statxs), ii -> 
        (* todo_cfg_to_ast *)
        let (i1, i2) = 
          match ii with 
          | [i1; i2] -> (i1, i2) 
          | _ -> raise Impossible
        in

        (* special_cfg_braces: *)
        incr counter_for_braces;
        let newi = 
          add_node_g 
            (StartBrace (!counter_for_braces, (Compound declxs_statxs, ii),i1))
            label_list ("{" ^ i_to_s !counter_for_braces) 
        in
        let endnode = build_node (EndBrace (!counter_for_braces, i2))
                               label_list  ("}" ^ i_to_s !counter_for_braces) 
        in
        let (oldinfo, braces, labels) = auxinfo_label in
        let newauxinfo = (oldinfo, endnode::braces, labels)
        in

        attach_to_previous_node starti newi;
        let starti = Some newi in


        declxs_statxs +> List.fold_left (fun (starti, auxinfo) st ->
          match st with
          | Right stat -> aux_statement (starti, auxinfo) stat,  auxinfo
          | Left decl -> 
              let s = 
                (match decl with
                | (DeclList ([(Some (s, _,_), typ, sto), _], _)) -> "decl:" ^ s
                | _ -> "decl_novar"
                ) in
              
              let newi = add_node_g (Declaration (decl)) label_list s in
              attach_to_previous_node starti newi;
              Some newi,  auxinfo
        ) (starti, newauxinfo)


        (* special_cfg_braces: *)
        +> (fun (starti, auxinfo) -> 
          (match starti with 
          | None -> None 
          | Some starti -> 
              (* subtil: not always return a Some *)
              let endi = !g#add_node endnode   +> adjust_g_i in
              !g#add_arc ((starti, endi), Direct) +> adjust_g;
              Some endi 
          )
           )


     (* ------------------------- *)        
    | Labeled (Label (s, st)), ii -> 
        (* todo_cfg_to_ast *)
        let ilabel = labels_assoc#find s in
        let node   = unwrap (!g#nodes#find ilabel) in
        let labelnode = build_node node label_list (s ^ ":") in
        !g#replace_node (ilabel, labelnode) +> adjust_g;
        attach_to_previous_node starti ilabel;
        aux_statement (Some ilabel, auxinfo_label) st


    | Jump (Goto s), ii -> 
       (* special_cfg_ast: *)
       let newi = 
         add_node_g (Statement (statement)) label_list ("goto " ^ s ^ ":") 
       in
       attach_to_previous_node starti newi;

       let ilabel = labels_assoc#find s in
       (* attach_to_previous_node starti ilabel *)
       (* todo: special_case: suppose that always goto to toplevel of function, 
          hence the Common.init *)
       (* todo?: can perhaps report when a goto is not a classic error_goto ? 
          that is when it does not jump to the toplevel of the function *)
       let newi = 
         special_cfg_insert_all_braces (Common.list_init (get_braces auxinfo)) 
                                       newi 
       in
       !g#add_arc ((newi, ilabel), Direct) +> adjust_g;
       None
        


        
     (* ------------------------- *)        
    | ExprStatement (None), ii -> 
        (* old: starti *)
        (* special_cfg_ast: *)
        let newi = 
          add_node_g (Statement (statement)) label_list ("emptyinstr;") 
        in
        attach_to_previous_node starti newi;
        Some newi


    | ExprStatement (Some e), ii -> 
        let s = 
          let (unwrap_e, typ, ii) = e in
          (match unwrap_e with
          | FunCall (( (Ident f), typ1, _),ii3) -> 
              f ^ "(...)"
          | Assignment (((Ident var), typ1, _), SimpleAssign, e) -> 
              var ^ " = ... ;"
          | Assignment (
              (RecordAccess ((Ident var, typ1, _), field), typ2, _),
               SimpleAssign, e) -> 
              var ^ "." ^ field ^ " = ... ;"
          | _ -> "statement"
          )
        in
        (* todo: may contain funcall, so have to "linearize" that expression *)
        let newi = add_node_g (Statement (statement)) label_list s in
        attach_to_previous_node starti newi;
        Some newi
        

     (* ------------------------- *)        
    | Selection  (If (e, st1, (ExprStatement (None), ii2))), ii -> 
       (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
                          |                                      |
                          |->   newfakeelse -> ... -> finalelse -|
          update: there is now also a link directly to lasti.
       *)
        let newi = 
          add_node_g 
            (Statement (Selection (If (e, st1, (ExprStatement None,ii2))), ii))
               label_list ("if")
        in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode label_list "[then]" in
        let newfakeelse = add_node_g FallThroughNode label_list "[fallthrough]" 
        in
        let afteri = add_node_g AfterNode label_list "[after]" in

        let lasti = add_node_g Fake label_list "[endif]" in

        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), Direct) +> adjust_g;
        !g#add_arc ((newi, afteri), Direct) +> adjust_g;
        !g#add_arc ((afteri, lasti), Direct) +> adjust_g;
        !g#add_arc ((newfakeelse, lasti), Direct) +> adjust_g;

        let finalthen = aux_statement (Some newfakethen, auxinfo_label) st1 in
        attach_to_previous_node finalthen lasti;
        Some lasti

        
    | Selection  (If (e, st1, st2)), ii -> 
       (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
                          |                                      |
                          |->   newfakeelse -> ... -> finalelse -|
          update: there is now also a link directly to lasti.
       *)
        let newi = 
          add_node_g (Statement (Selection (If (e, st1, st2)), ii))
                     label_list "if" 
        in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode label_list "[then]" in
        let newfakeelse = add_node_g FalseNode label_list "[else]" in


        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), Direct) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen, auxinfo_label) st1 in
        let finalelse = aux_statement (Some newfakeelse, auxinfo_label) st2 in
        (match finalthen, finalelse with 
          | (None, None) -> None
          | _ -> 
              let lasti = add_node_g Fake label_list "[endif]" in
              let afteri = add_node_g AfterNode label_list "[after]" in
              !g#add_arc ((newi, afteri), Direct) +> adjust_g;
              !g#add_arc ((afteri, lasti), Direct) +> adjust_g;
              begin
                attach_to_previous_node finalthen lasti;
                attach_to_previous_node finalelse lasti;
                Some lasti
             end
        )
        
        

     (* ------------------------- *)        
    | Selection  (Switch (e, st)), ii -> 
        let newswitchi = 
          add_node_g (Statement (Selection (Switch (e, st)),ii))
                     label_list "switch"
        in
        attach_to_previous_node starti newswitchi;

        let newendswitch = add_node_g Fake label_list "[endswitch]" in

    
        (* the newswitchi is for the labels to know where to attach, the 
           newendswitch (endi) is for the 'break' *)

        (* let finalthen = aux_statement (None, newauxinfo) st in *)

        (* Prepare var to be able to copy paste  *)
         let starti = None in
         (* let auxinfo = newauxinfo in *)
         let statement = st in
         (* COPY PASTE of compound case *)

         let finalthen = 
             match statement with
         
             | Compound (declxs_statxs), ii -> 
                 (* todo_cfg_to_ast *)
                 let (i1, i2) = 
                   match ii with 
                   | [i1; i2] -> (i1, i2) 
                   | _ -> raise Impossible
                 in

                 (* old: declxs_statxs +> map_filter 
                    (function Right stat -> Some stat | _ -> None) 
                    +> (fun statxs -> *)
         
                 (* special_cfg_braces: *)
                 incr counter_for_braces;
                 let newi = 
                   add_node_g 
                     (StartBrace 
                        (!counter_for_braces, (Compound declxs_statxs, ii), i1))
                     label_list ("{" ^ i_to_s !counter_for_braces)
                 in
                 let endnode = 
                   build_node (EndBrace (!counter_for_braces, i2))
                               label_list ("}" ^ i_to_s !counter_for_braces)
                 in
                 let (oldinfo, braces, labels) = auxinfo_label in
                 let newauxinfo = (oldinfo, endnode::braces, labels) in

                 (* new: cos of switch *)
                 let (oldinfo, braces, labels) = newauxinfo in
                 let newauxinfo = 
                   SwitchInfo (newi, newendswitch, get_braces auxinfo),
                   braces, labels
                 in
                 !g#add_arc ((newswitchi, newi), Direct) +> adjust_g; 

                 (* new: if have not a default case, then must add an edge 
                    between start to end *)
                 if (not (declxs_statxs +> List.exists (function 
                   | Right (Labeled (Default _), _) -> true
                   | _ -> false
                    )))
                 then
                   !g#add_arc ((newswitchi, newendswitch), Direct) +> adjust_g;


         
                 attach_to_previous_node starti newi;
                 let starti = Some newi in
         
         
                 declxs_statxs +> List.fold_left (fun (starti, auxinfo) st ->
                   match st with
                   | Right stat -> 
                       aux_statement (starti, auxinfo) stat,  auxinfo
                   | Left decl -> 
                       let s = 
                         (match decl with
                         | (DeclList ([(Some (s, _,_), typ, sto), _], _)) -> 
                             "decl:" ^ s
                         | _ -> "decl_novar"
                         ) in
                       
                       let newi = add_node_g (Declaration (decl)) label_list  s
                       in
                       attach_to_previous_node starti newi;
                       Some newi,  auxinfo
                 ) (starti, newauxinfo)
         
         
                 (* special_cfg_braces: *)
                 +> (fun (starti, auxinfo) -> 
                   (match starti with 
                   | None -> None 
                   | Some starti -> 
                       (* subtil: not always return a Some *)
                       let endi = !g#add_node endnode   +> adjust_g_i in
                       !g#add_arc ((starti, endi), Direct) +> adjust_g;
                       Some endi 
                   )
                    )
             | x -> error_cant_have x
         in



         attach_to_previous_node finalthen newendswitch;
         Some newendswitch


    | Labeled (Case  (e, st)), ii -> 
        incr counter_for_switch;
        let switchrank = !counter_for_switch in

        let newi = add_node_g (Statement (Labeled (Case (e, st)),ii))
                              label_list "case:"
        in

        (match fst3 auxinfo with
        | SwitchInfo (startbrace, switchendi, _braces) -> 
            (* no need to attach to previous for the first case, cos would be
               redundant *)
            (match starti with 
            | Some starti when  starti <> startbrace -> 
                attach_to_previous_node (Some starti) newi; 
            | _ -> ()
            );

            let newcasenodei = 
              add_node_g (CaseNode switchrank)
                         label_list ("[casenode] " ^ i_to_s switchrank) 
            in
            !g#add_arc ((startbrace, newcasenodei), Direct) +> adjust_g;
            !g#add_arc ((newcasenodei, newi), Direct) +> adjust_g;
        | _ -> raise (CaseNoSwitch (fst (List.hd ii)))
        );
        aux_statement (Some newi, auxinfo_label) st
        

    | Labeled (Default st), ii -> 
        incr counter_for_switch;
        let switchrank = !counter_for_switch in

        let newi = add_node_g (Statement (Labeled (Default st),ii))
                              label_list "case default:" in
        attach_to_previous_node starti newi;

        (match fst3 auxinfo with
        | SwitchInfo (startbrace, switchendi, _braces) -> 
             let newcasenodei = 
               add_node_g (CaseNode switchrank)
                          label_list ("[casenode] " ^ i_to_s switchrank) 
             in
             !g#add_arc ((startbrace, newcasenodei), Direct) +> adjust_g;
             !g#add_arc ((newcasenodei, newi), Direct) +> adjust_g;
        | _ -> raise (CaseNoSwitch (fst (List.hd ii)))
        );
        aux_statement (Some newi, auxinfo_label) st

    | Labeled (CaseRange  (e, e2, st)), ii -> 
        raise Todo





     (* ------------------------- *)        
    | Iteration  (While (e, st)), ii -> 
       (* starti -> newi ---> newfakethen -> ... -> finalthen -
                      |---|-----------------------------------|
                          |-> newfakelse 
       *)

        let newi = add_node_g (Statement (Iteration (While (e, st)), ii))
                              label_list "while"
        in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode label_list "[whiletrue]" in
        let newfakeelse = add_node_g FalseNode label_list "[endwhile]" in


        let (oldinfo, braces, labels) = auxinfo_label in
        let newauxinfo = LoopInfo (newi, newfakeelse, braces), 
                         braces, labels
        in

        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), Direct) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen, newauxinfo) st in
        attach_to_previous_node finalthen newi;
        Some newfakeelse

        
        
    | Iteration  (DoWhile (st, e)), ii -> 
       (* starti -> newi ---> newfakethen --> ... ---> finalthen --->   finali
                      |--------------------------------------------------|  |---> newfakelse 

       *)
        let newi = add_node_g (Statement (Iteration (DoWhile (st, e)),ii))
                              label_list "do" 
        in
        (* todo?: make a special node ? cos have to repeat the info, need reput 
           a Statement statement, a Fake node for the while (of dowhile) may not
           be enough. how found the corresponding condition ? peut etre a juste
           inverser les Fake et Statement, les mettre en fait dans newi et 
           finali respectively *)
        let finali = add_node_g Fake label_list "while (of dowhile)" in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode label_list "[dowhiletrue]" in
        let newfakeelse = add_node_g FalseNode label_list "[enddowhile]" in


        (* This time, may return None, for instance if goto in body of dowhile
           (whereas While cant return None).
           The code of while case (different from dowhile) is put in comment, 
           to illustrate the difference *)

        (* TODO, not used ????? *)
        (* let _newauxinfo = LoopInfo (finali, newfakeelse, snd auxinfo) in *)

        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g; 

        !g#add_arc (((*newi*)finali, newfakethen), Direct) +> adjust_g; 
        !g#add_arc (((*newi*)finali, newfakeelse), Direct) +> adjust_g;

        let finalthen = aux_statement (Some newfakethen, auxinfo_label) st in 

        (* code of while case (different from dowhile) 
           attach_to_previous_node finalthen newi;
           Some newfakeelse   *)
        (match finalthen with 
        | None -> None
        | Some finalthen -> 
            !g#add_arc ((finalthen, (*newi*)finali), Direct) +> adjust_g;
            Some newfakeelse
         )
        

    | Iteration  (For (e1opt, e2opt, e3opt, st)), ii -> 
        let newi = 
          add_node_g 
            (Statement (Iteration (For (e1opt, e2opt, e3opt, st)),ii))
            label_list "for"
        in
        attach_to_previous_node starti newi;
        let newfakethen = add_node_g TrueNode label_list "[fortrue]" in
        let newfakeelse = add_node_g FalseNode label_list "[endfor]" in


        let (oldinfo, braces, labels) = auxinfo_label in
        let newauxinfo = LoopInfo (newi, newfakeelse, braces), 
                         braces, labels 
        in

        !g#add_arc ((newi, newfakethen), Direct) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), Direct) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen, newauxinfo) st in
        attach_to_previous_node finalthen newi;
        Some newfakeelse


     (* ------------------------- *)        
    | Jump ((Continue|Break) as x),ii ->  
        (* todo_cfg_to_ast *)
        let newi = add_node_g (Statement (statement)) 
                              label_list "continue_break;"
        in
        attach_to_previous_node starti newi;

        (* let newi = some starti in *)

        (match fst3 auxinfo with
        | LoopInfo (loopstarti, loopendi, braces) -> 
            let desti = 
              (match x with 
              | Break -> loopendi 
              | Continue -> loopstarti 
              | x -> error_cant_have x
              ) in

            let difference = 
              List.length (get_braces auxinfo) - List.length braces 
            in
            assert (difference >= 0);
            let toend = take difference (get_braces auxinfo) in
            
            let newi = special_cfg_insert_all_braces toend newi in

            !g#add_arc ((newi, desti), Direct) +> adjust_g;
            None
        | SwitchInfo (startbrace, loopendi, braces) -> 
            if x = Break then
              begin
                let difference = 
                  List.length (get_braces auxinfo) -  List.length braces 
                in
                assert (difference >= 0);
                let toend = take difference (get_braces auxinfo) in
            
                let newi = special_cfg_insert_all_braces toend newi in

                !g#add_arc ((newi, loopendi), Direct) +> adjust_g;
                None
              end
            else raise (OnlyBreakInSwitch (fst (List.hd ii)))
        | NoInfo -> raise (NoEnclosingLoop (fst (List.hd ii)))
        )        





    | Jump (Return), ii -> 
        (* special_cfg_ast: *)
        let newi = add_node_g (Statement (statement)) label_list "return" in
        attach_to_previous_node starti newi;
        
        let newi = special_cfg_insert_all_braces (get_braces auxinfo) newi in
        
        !g#add_arc ((newi, exiti), Direct) +> adjust_g;
        None

    | Jump (ReturnExpr e), ii -> 
        let newi = add_node_g (Statement (statement)) label_list  "return ..."
        in
        attach_to_previous_node starti newi;

        let newi = special_cfg_insert_all_braces (get_braces auxinfo) newi in
        
        !g#add_arc ((newi, exiti), Direct) +> adjust_g;
        None
        (* old: attach_to_previous_node starti exiti *)

        
    (* ------------------------- *)        
    | Asm, ii -> failwith "asm code"


  in
  (* todocheck: assert ? such as we have "consommer" tous les labels  *)

  let lasti = aux_statement (Some enteri, (NoInfo, [], [])) topstatement in
  attach_to_previous_node lasti exiti;
  !g



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
      | Statement (st,ii::iis) -> raise (DeadCode (Some (fst ii)))

      | HeadFunc _ -> ()
      (* old: | Enter -> () *)

      | Fake -> pr2 "control_flow: deadcode sur fake node, pas grave";
      | EndBrace _ -> () (* todo?: certaines deviennent orphelins *)
      | x -> pr2 "control_flow: orphelin nodes, maybe something wierd happened"
      )
    );

(******************************************************************************)
(*
 statement, compound, 
 return,
 if, 

 switch

 for, while, dowhile
 break/continue
 goto, labels

 todo: maintain a todo list of labels.
 If two todos, then ambiguity (can disambiguate some case by analysing and 
 look if dependency, if one label  lead to another "naturally" (by a 
 sequence of Direct without jump, in a compound)).

*)

type returnkind = 
  | LastCurrentNode of nodei 
  | NoNextNode of nodei (* the node where we stopped *)


(*----------------------------------------------------------------------------*)
let get_next_node g nodei = 
    (match (g#successors nodei)#tolist with
    | [nexti, Direct] -> nexti,  unwrap (g#nodes#find nexti)
    | x -> error_cant_have x
    ) 

let get_next_nodes_ifthenelse_sorted g nodei = 
  (g#successors nodei)#tolist +> List.map (fun (nodei, Direct) -> 
    nodei, unwrap (g#nodes#find nodei), 
    (match unwrap (g#nodes#find nodei) with
    | TrueNode -> 0
    | FalseNode -> 1
    | FallThroughNode -> 2
    | AfterNode -> 3
    | _ -> failwith "wierd node at this place"
    )
    )                                     
    +> List.sort (fun (_, _, a) (_, _, b) -> compare a b)
    +> List.map (fun (x,y,z) -> (x,y))

let get_first_node g = 
    g#nodes#tolist +> List.find (fun (i, node) -> 
      match unwrap node with HeadFunc _ -> true | _ -> false
      ) +> fst

(* todo: return also a special node, when has no default case, there is a direct
   edge from startswitch to endswitch. *)
let get_next_nodes_switch_sorted g nodei = 
  (g#successors nodei)#tolist +> List.map (fun (nodei, Direct) -> 
    get_next_node g nodei +> fst, 
    (match unwrap (g#nodes#find nodei) with
    | CaseNode i -> i
    | _ -> raise Impossible
    ))
   +> List.sort (fun (_, a) (_, b) -> compare a b)
   +> List.map fst

(*----------------------------------------------------------------------------*)
(*
 obsolete?: a chaque fois que y'a un __xxx, faire un assert que ce _xxx est un 
  noInstr ou un compound vide.
*)
let (control_flow_to_ast: (node, edge) ograph_extended -> definition) = fun g ->
  
  let nodes = g#nodes  in
  let starti = get_first_node g in

  (* todo?: let visited = ref (new oassocb []) in *)


  let funcdef =  
    match unwrap (nodes#find starti) with 
    | HeadFunc funcdef -> funcdef  
    | _ -> raise Impossible
  in
  let (funcs, functype, sto, __compound, iifunc) = funcdef in


  (* ------------------------- *)        
  let rec (rebuild_compound_instr_list: nodei -> int -> (compound * returnkind))
   = fun starti level -> 
    match unwrap (nodes#find starti) with
    | EndBrace (level2,_) -> 
        if level = level2 
        then [], LastCurrentNode starti
        (* can raise Todo instead ? cos all opening { are correctly ended
           with a } *)
        else raise Impossible
    | Declaration decl -> 
        let nexti = get_next_node g starti +> fst in
        let (compound, return) = rebuild_compound_instr_list nexti level in
        Left decl::compound, return
        
    | x -> 
        let (st, return) = rebuild_statement starti in
        (match return with
        | NoNextNode _ -> [Right st], return
        | LastCurrentNode nodei -> 
            let nexti = get_next_node g nodei +> fst in
            let (compound, return) = rebuild_compound_instr_list nexti level in
            Right st::compound, return
        )
        
(*    | x -> error_cant_have x *)

  (* ------------------------- *)        
  and (rebuild_statement: nodei -> (statement * returnkind)) = fun starti -> 
    match unwrap (nodes#find starti) with

    | Statement ((ExprStatement (None), ii) as st) -> 
        st, LastCurrentNode starti
    | Statement ((ExprStatement (Some e), ii) as st) -> 
        st, LastCurrentNode starti

    (* ------------------------- *)        
    | Statement ((Jump (Return), ii) as st) -> 
        st, NoNextNode starti
    | Statement ((Jump (ReturnExpr e), ii) as st) -> 
        st, NoNextNode starti

          
    (* ------------------------- *)        
    | StartBrace (level,st,i1) -> 
        (match st with 
        | (Compound __st, _ii) -> 
            let (nexti, st) = get_next_node g starti in
            let (compound, return) = rebuild_compound_instr_list  nexti level in
            (* look for the info of the endbraces *)

            let i2 = 
              (match return with
              | LastCurrentNode lasti -> 
                  (match unwrap (nodes#find lasti) with
                  | EndBrace (level2,i2) -> i2
                  | _ -> raise Impossible
                  )
              | NoNextNode lasti -> 
                  pr2 "a return, have to go through graph";
                  let nexti = get_next_node g lasti +> fst in
                  (match unwrap (nodes#find nexti) with
                  | EndBrace (level2,i2) -> i2
                  | _ -> raise Impossible
                  )
                  
              ) in

            (Compound compound, [i1;i2]),  return
        | _ -> raise Impossible
        )

    (* ------------------------- *)        
    | Statement (Selection  (If (e, __st1, __st2)), ii) -> 

         (match get_next_nodes_ifthenelse_sorted g starti with
         | [(theni, TrueNode);  (elsei, FalseNode); (afteri, AfterNode)] -> 
               
           let (theni', _) = get_next_node g theni in
           let (elsei', _) = get_next_node g elsei in
   
           let (st1, return1) = rebuild_statement theni' in
           let (st2, return2) = rebuild_statement elsei' in
   
           (* assert next of return1 = next of return 2 *)
           (match return1, return2 with
           | LastCurrentNode return1, LastCurrentNode return2 -> 
               assert ((fst (get_next_node g return1)  =|= 
                        fst (get_next_node g return2)) && 
                       (fst (get_next_node g return1) =|= 
                        fst (get_next_node g afteri)));
               (Selection (If (e, st1, st2)),ii), 
                LastCurrentNode (get_next_node g return1 +> fst)
           | LastCurrentNode return, NoNextNode _ -> 
               (Selection (If (e, st1, st2)),ii), 
                LastCurrentNode (get_next_node g return +> fst)
           | NoNextNode _ , LastCurrentNode return  -> 
               (Selection (If (e, st1, st2)),ii), 
                LastCurrentNode (get_next_node g return +> fst)
           | NoNextNode i1 , NoNextNode i2  -> 
               (Selection (If (e, st1, st2)),ii), 
                NoNextNode i1 (* could be i2 *)
           )
   
         | [(theni, TrueNode); (falli, FallThroughNode); (afteri, AfterNode)] ->
             let (theni', _) = get_next_node g theni in
             let (st1, return1) = rebuild_statement theni' in
             let (st2) = (ExprStatement (None), []) in
             (match return1 with
             | LastCurrentNode return -> 
                 assert (fst (get_next_node g return) =|= 
                         fst (get_next_node g afteri));
                 (Selection (If (e, st1, st2)),ii), 
                  LastCurrentNode (get_next_node g afteri +> fst)
             | NoNextNode _ -> 
                 (Selection (If (e, st1, st2)),ii), 
                  LastCurrentNode (get_next_node g afteri +> fst)
             )
   
         | [(theni, TrueNode);  (elsei, FalseNode)] -> 
               
           let (theni', _) = get_next_node g theni in
           let (elsei', _) = get_next_node g elsei in
   
           let (st1, return1) = rebuild_statement theni' in
           let (st2, return2) = rebuild_statement elsei' in
   
           (* assert next of return1 = next of return 2 *)
           (match return1, return2 with
           | NoNextNode i1 , NoNextNode i2  -> 
               (Selection (If (e, st1, st2)),ii), 
               NoNextNode i1 (* could be i2 *)
           (* if no after node, that means that the two branches go wild *)
           | x -> raise Impossible 
           )
         | x -> raise Impossible
         )

    (* ------------------------- *)        
    | Statement (Selection  (Switch (e, _st)), ii) -> 
        let (st, return) = 
          (match get_next_node g starti with
          | (nexti,  (StartBrace (level, (Compound __st, _ii ), i1))) -> 
              let nodes_sorted = get_next_nodes_switch_sorted g nexti in
              
              let list_list_statement = 
                nodes_sorted +> map_filter (fun nodei -> 
                  (* todo: do only if the 'case:' in question have only 1 
                     predecessor *)
                  if ((g#predecessors nodei)#tolist +> List.length) >= 2
                  then None
                  else 
                    Some (rebuild_compound_instr_list nodei level)
                   ) in
              let compound = list_list_statement +> List.map fst +> List.concat
              in

              (* find the endswitch, it must be after the }level node *)
              let maybereturn = 
                  list_list_statement +> List.map snd +> map_filter (function
                    (* can be because a return, or a break, but if a break,
                       when after there should be a }level and after the 
                       endswitch. *)
                  | NoNextNode nodei -> 
                      (match unwrap (g#nodes#find nodei) with
                      | Statement (Jump (Break),_) -> 
                          let nexti = get_next_node g nodei +> fst in
                          (match unwrap (g#nodes#find nexti) with
                          | EndBrace (level2,i2) when level2 = level -> 
                              let nextii = get_next_node g nexti +> fst in
                              (match g#nodes#find nextii with
                              | (Fake, _), "[endswitch]" -> Some nextii
                              | _ -> raise Impossible
                              )
                          | _ -> raise Impossible
                          )
                      | _ -> None
                      )
  
                     (* there was no break or return, certainly the default 
                        case *)
                  | LastCurrentNode nodei -> 
                      failwith "certainly default case"
              ) in
               let return = 
                  (match maybereturn with 
                   | x::_ -> LastCurrentNode x
                   | [] -> NoNextNode (-1)
                  ) 
              in
              (Compound compound, ii), return
          | _ -> raise Impossible
          )
        in
        (Selection (Switch (e, st)), ii), return

(* alt: would like to return None, cos it will be handle elsewhere
  or can handle it, but when handle a case in the iter of the
  switch, check that have no 2 predecessors, which would mean
  that this case has certainly be already handled. *)      

    | Statement (Labeled (Case  (e, __st)), ii) -> 
        let (nexti, _) =  get_next_node g starti in
        let (st, return) = rebuild_statement nexti in
        (Labeled (Case (e, st)),ii),  return

    | Statement (Labeled (Default  ( __st)), ii) -> 
        let (nexti, _) =  get_next_node g starti in
        let (st, return) = rebuild_statement nexti in
        (Labeled (Default (st)),ii),  return

    | Statement ((Jump (Break),ii) as st) ->  
        st, NoNextNode starti
        


    (* ------------------------- *)        
    | Statement (Iteration  (For (e1opt, e2opt, e3opt, __st)), ii) -> 
        (match get_next_nodes_ifthenelse_sorted g starti with
         | [(theni, TrueNode);  (endfori, FalseNode)] -> 
               
           let (theni', _) = get_next_node g theni in
           let (st, return) = rebuild_statement theni' in
           (* check? if return is LastCurrentNode, it must be equal to endfori*)
           (Iteration  (For (e1opt, e2opt, e3opt, st)), ii), 
           LastCurrentNode endfori
         | _ -> raise Impossible           
        )

    | Statement (Iteration  (While (e, __st)), ii) -> 
        (match get_next_nodes_ifthenelse_sorted g starti with
         | [(theni, TrueNode);  (endfori, FalseNode)] -> 
               
           let (theni', _) = get_next_node g theni in
           let (st, return) = rebuild_statement theni' in
           (* check? if return is LastCurrentNode, it must be equal to endfori*)
           (Iteration  (While (e, st)), ii), LastCurrentNode endfori
         | _ -> raise Impossible           
        )
        

    | x -> 
        raise Todo
          

  in



  let (topcompound, returnkind) = 
    (match get_next_node g starti with
    | (nexti,  _) (* (Enter, s) *) -> 
        (* let (nextii, _) = get_next_node g nexti in *)
        let nextii = nexti in
        rebuild_statement nextii
  
    (* | x -> error_cant_have x *)
    ) 
  in
  (* todo?: assert stuff on returnkind ? normally lead to an exit node, 
     or nothing *)

  let topcompound2 = 
    match topcompound with 
    | (Compound st, ii) -> st  
    | x -> error_cant_have x 
  in

  (funcs, functype, sto, topcompound2, iifunc)
 

    





(******************************************************************************)

(*
  special_cfg_braces: 
   The check are really specific to the way we have build our control_flow, 
   with the { } in the graph so normally all those checks here are useless.
  evo: to better error reporting, to report earlier the message, 
   pass the list of '{' (containing morover a brace_identifier) instead of just
   the depth.

  obsolete? verifier que y'a que des noInstr d'accrocher au noeuds comme 
  if,while, mais aussi case, headfun, ...
*)

let (check_control_flow: (node, edge) ograph_extended -> unit) = fun g ->

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
      if  (*(depth = depth2)*) startbraces = startbraces2
      then ()
      else 
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
        | StartBrace (i,_,_), xs  -> i::xs
        | EndBrace (i,_), j::xs -> 
            if i = j 
            then xs
            else 
              begin 
                pr2 (sprintf ("PB with flow: not corresponding match between }%d and excpeted }%d at node %d") i j nodei); 
                print_trace_error trace2; 
                xs 
              end
        | EndBrace (i,_), [] -> 
            pr2 (sprintf "PB with flow: too much } at }%d " i);
            print_trace_error trace2; 
            []
        | _, xs ->  xs
        ) 
      in

   
      if children#tolist = [] 
      then 
        if (* (depth = 0) *) startbraces = []
        then ()
        else print_trace_error trace2
      else 
        children#tolist +> List.iter (fun (nodei,_) -> 
          dfs (nodei, newdepth, trace2))
    in

  dfs (starti, (* Depth 0*) [], [])

  
(******************************************************************************)

let test statement = 
 let g = ast_to_control_flow statement in
 check_control_flow g;
 print_ograph_extended g;
 assert (statement =*= statement +> ast_to_control_flow +> control_flow_to_ast);
  

