open Commonop open Common

(********************************************************************************)
(********************************************************************************)

(*------------------------------------------------------------------------------*)
(*
 note: deadCode detection
  what is dead code ? when there is no starti  to start from ? => make starti an option too ?
   si on arrive sur un label: au moment d'un deadCode, on peut verifier les predecesseurs de ce label,
   auquel cas si y'en a, ca veut dire qu'en fait c pas du deadCode et que donc on peut se permettre
   de partir d'un starti à None
  mais si on a   xx; goto far:; near: yy; zz; far: goto near:   , bon ca doit etre un cas tres tres rare, 
   mais a cause de notre parcours, on va rejeter ce programme car au moment d'arriver sur near:  on n'
   a pas encore de predecesseurs pour ce label.
  de meme, meme le cas simple ou la derniere instruction c'est un return, alors ca va generer un DeadCode :(
  => make a first pass where dont launch exn at all, create nodes, if starti is None then dont add    arc. 
          a second pass, just check that all nodes (except enter) have predecessors. 
            (todo: if the pb is at a fake node, then try first successos that is non fake)
  => make starti  an option too (so type is now  int option -> statement -> int option)
 old: I think that DeadCode is too aggressive, what if  have both return in else/then ? 


 note: special_cfg_ast tag
  because need go back from cfg to ast, have to introduce additionnal nodes that normally
   are not needed by a normal compiler.

 note: special_cfg_braces tag
  because julia wants the { and } in the control flow graph to make it easier for the matcher,
   have to add some nodes in the graph.
  as for special_cfg_ast, normally not needed by a normal compiler.
  done: return,  break/continue (for while/for/dowhile/if),  break/continue  (for switch), 
  todo?: goto,  compute target level (but rare that different I think)
        ver1: just do init,  
        ver2: compute depth of label (easy, intercept compound in the visitor)

 


 todo: expression, linearize,  funcall (and launch exn  with StatementExpr )

 todo: add info in nodes, to later be able to pretty print back
 todo:
  for switch, pass int ref (compteur) too ? (cos need know order of the case if then later want to 
  go from CFG to (original) AST

 todo: to generate less exception with the breakInsideLoop, analyse correctly the
   loop deguisé  comme list_for_each (qui sont actuellement retourné comme des Tif par le lexer)


 checktodo: after a switch, need check that all the st in the compound start with a case: ?

 todo: can have code (and so nodes) in many places, in the size of an array, in the init of initializer, 
  but also in StatementExpr, ...

 todo?: steal code from CIL ? (but seems complicated ... again)


*)
(*------------------------------------------------------------------------------*)

open Ograph_extended
open Oassoc
open Oassocb

open Ast_c
open Visitor_c


(*------------------------------------------------------------------------------*)
type node = node1 * string (* to debug *)
and node1 = 
  | HeadFunc of definition

  | Enter 
  | Exit

  | NestedFunCall of expression   (* cos "fake" node *)
  | Statement     of statement
  | Declaration   of declaration

  | Fake

  (* special_cfg_braces: *)
  (* the int is here to indicate to what { } they correspond. 
     two pairwise { } share the same number.
     kind of "brace_identifier". 
     used mostly for debugging or for checks.
  *)
  | StartBrace of int * statement (* special_cfg_ast *)
  | EndBrace   of int


type edge = 
  | Direct
  (* if, while, dowhile, for *)
  | ChoiceTrue
  | ChoiceFalse 
  (* todo: and with switch ? *)
  | SpecialEdge (* for julia *)


exception DeadCode of (Common.parse_info option)
exception CaseNoSwitch      of (Common.parse_info)
exception OnlyBreakInSwitch of (Common.parse_info)
exception NoEnclosingLoop   of (Common.parse_info)


type nodei = int



(*------------------------------------------------------------------------------*)
let get_next_node g nodei = 
    (match (g#successors nodei)#tolist with
    | [nexti, Direct] -> nexti,  g#nodes#find nexti
    | x -> error_cant_have x
    ) 

let get_next_2node g nodei = 
    (match (g#successors nodei)#tolist with
    | [nexti, ChoiceTrue; nexti2, ChoiceFalse] -> (nexti,   g#nodes#find nexti),  (nexti2, g#nodes#find nexti2)
    | [nexti, ChoiceFalse; nexti2, ChoiceTrue] -> (nexti2,  g#nodes#find nexti2), (nexti,  g#nodes#find nexti)
    | x -> error_cant_have x
    ) 

let get_first_node g () = 
    let starti = g#nodes#tolist +> List.find (fun (i, (node, nodes)) -> 
    match node with HeadFunc _ -> true | _ -> false
    ) +> fst 
    in
    starti 

(********************************************************************************)
(********************************************************************************)
type additionnal_info = 
    additionnal_info2 * 
    nodei list (* special_cfg_braces: current imbrication depth (contain the must-close '}' ) *)
  and additionnal_info2 =
  | NoInfo 
  | LoopInfo   of nodei * nodei (* start, end *) * nodei list         (* special_cfg_braces: *)
  | SwitchInfo of nodei * nodei (* start, end *) * nodei * nodei list (* special_cfg_braces: *)

(* type depthi = Depth of int*) (* obsolete: *)

(*------------------------------------------------------------------------------*)
let (ast_to_control_flow: definition -> (node, edge) ograph_extended) = fun funcdef ->
  let g = ref (new ograph_extended) in

  (* monad like, >>= *)
  let adjust_g_i (newg,newi) = begin  g := newg;   newi end in
  (* monad like, >> *)
  let adjust_g (newg)        = begin  g := newg;    end in

  let (funcs, functype, sto, compound, (_,_,ii)) = funcdef in
  let topstatement = Compound compound, ii in


  let headi = !g#add_node (HeadFunc funcdef, "function " ^ funcs) +> adjust_g_i in

  let enteri = !g#add_node (Enter, "[enter]") +> adjust_g_i in
  let exiti  = !g#add_node (Exit,  "[exit]")  +> adjust_g_i in
  
  let _ = !g#add_arc ((headi, enteri), Direct) +> adjust_g in


  (* alt: do via a todo list, so can do all in one pass (but more complex) *)
  (* todo: can also count the depth level and associate it to the node, for the special_cfg_braces: *)
  let compute_labels statement = 

    (* map label to index number in graph *)
    let (h: (string, int) oassoc ref) = ref (new oassocb []) in

    begin
      statement +> visitor_statement_k { default_visitor_c_continuation with 
         kstatement = (fun (k, bigf) statement -> 
           match statement with
           | Labeled (Label (s, st)),ii -> 
               let newi = !g#add_node (Statement statement, s ^ ":") +> adjust_g_i in
               begin
                 assert (not (!h#haskey s)); (* label already exist ? todo: replace assert with a raise DuplicatedLabel *)
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
      (* have to build a new node (clone), cos cant share it *)
      (* old:
      !g#add_arc ((acc, e), Direct) +> adjust_g;
      e
      *)
      let node = !g#nodes#tolist +> List.find (fun (i, _) -> i = e) +> snd in
      let newi = !g#add_node (node) +> adjust_g_i in

      !g#add_arc ((acc, newi), Direct) +> adjust_g;
      newi
      
      ) starti
  in
  let counter = ref 0 in



  (* take start, return end 
      old: old code was returning an int, but goto has no end => aux_statement should return   int option 
      old: old code was taking an int, but should also take int option
     addon: to complete (break, continue (and enclosing loop),   switch (and associated case, casedefault))
      need pass additionnal info:  the start/exit when enter in a loop,     so know the current 'for'
     addon: to handle the special_cfg_braces, need again pass additionnal info

  *)
  let rec (aux_statement: (nodei option * additionnal_info) -> statement -> nodei option) = fun (starti, auxinfo) statement ->

    let attach_to_previous_node starti nodei = 
      (match starti with None -> () | Some starti -> 
          !g#add_arc ((starti, nodei), Direct) +> adjust_g);
    in
      

    match statement with

    | Compound (declxs_statxs), ii -> 
        (* todo_cfg_to_ast *)
        (* old: declxs_statxs +> map_filter (function Right stat -> Some stat | _ -> None) +> (fun statxs -> *)

        (* special_cfg_braces: *)
        let _ = incr counter in 
        let newi = !g#add_node (StartBrace (!counter, statement), "{" ^ i_to_s !counter) +> adjust_g_i in
        let endi = !g#add_node (EndBrace !counter,   "}" ^ i_to_s !counter) +> adjust_g_i in
        let newauxinfo = (fst auxinfo, endi::snd auxinfo) in

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
              
              let newi = !g#add_node (Declaration (decl), s) +> adjust_g_i in
              (match starti with None -> () | Some starti -> 
                !g#add_arc ((starti, newi), Direct) +> adjust_g);
              Some newi,  auxinfo
        ) (starti, newauxinfo)


        (* special_cfg_braces: *)
        +> (fun (starti, auxinfo) -> 
          (match starti with 
          | None -> None 
          | Some starti -> 
              (* subtil: not always return a Some *)
              !g#add_arc ((starti, endi), Direct) +> adjust_g;
              Some endi 
          )
           )
        
    | Labeled (Label (s, st)), ii -> 
        (* todo_cfg_to_ast *)
        let ilabel = labels_assoc#find s in
        attach_to_previous_node starti ilabel;
        aux_statement (Some ilabel, auxinfo) st


    | Jump (Goto s), ii -> 
        (* special_cfg_ast: *)
        let newi = !g#add_node (Statement (statement), ("goto " ^ s ^ ":")) +> adjust_g_i in
        attach_to_previous_node starti newi;

        let ilabel = labels_assoc#find s in
        (*
        (match starti with None -> () | Some starti -> 
          !g#add_arc ((starti, ilabel), Direct) +> adjust_g);
        *)
        (* todo: special_case: suppose that always goto to toplevel of function, hence the Common.init *)
        (* todo?: can perhaps report when a goto is not a classic error_goto ? that is when 
             it does not jump to the toplevel of the function
        *)
        let newi = special_cfg_insert_all_braces (Common.list_init (snd auxinfo)) newi in

        !g#add_arc ((newi, ilabel), Direct) +> adjust_g;

        None
        


        
    | ExprStatement (None), ii -> 
        (* old: starti *)
        (* special_cfg_ast: *)
        let newi = !g#add_node (Statement (statement), ("emptyinstr;")) +> adjust_g_i in
        attach_to_previous_node starti newi;
        Some newi


    | ExprStatement (Some e), ii -> 
        let s = 
          (match e with
          | (FunCall ((Constant (Ident f), _),ii3),ii2) -> f ^ "(...)"
          | (Assignment ((Constant (Ident var), _), SimpleAssign, e), _) -> var ^ " = ... ;"
          | _ -> "statement"
          )
        in
        (* todo: may contain funcall, so have to "linearize" that expression *)
        let newi = !g#add_node (Statement (statement), s) +> adjust_g_i in
        attach_to_previous_node starti newi;
        Some newi
        
        
    | Selection  (If (e, st1, st2)), ii -> 
       (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
                          |                                      |
                          |->   newfakeelse -> ... -> finalelse -|
       *)
        let newi = !g#add_node (Statement (statement), "if") +> adjust_g_i in
        attach_to_previous_node starti newi;
        let newfakethen = !g#add_node (Fake, "[then]") +> adjust_g_i in
        let newfakeelse = !g#add_node (Fake, "[else]") +> adjust_g_i in
        let lasti = !g#add_node (Fake, "[endif]") +> adjust_g_i in

        !g#add_arc ((newi, lasti), SpecialEdge) +> adjust_g; (* for julia *)

        !g#add_arc ((newi, newfakethen), ChoiceTrue) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), ChoiceFalse) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen, auxinfo) st1 in
        let finalelse = aux_statement (Some newfakeelse, auxinfo) st2 in
        (match finalthen, finalelse with 
          | (None, None) -> None
          | _ -> 
              begin
                (match finalthen with None -> () | Some finalthen -> 
                  !g#add_arc ((finalthen, lasti), Direct) +> adjust_g);
                (match finalelse with None -> () | Some finalelse -> 
                  !g#add_arc ((finalelse, lasti), Direct) +> adjust_g);
                Some lasti
             end
        )
        
        

    | Selection  (Switch (e, st)), ii -> 
        let newi = !g#add_node (Statement (statement), "switch") +> adjust_g_i in
        attach_to_previous_node starti newi;

        let newfakeelse = !g#add_node (Fake, "[endswitch]") +> adjust_g_i in

        !g#add_arc ((newi, newfakeelse), SpecialEdge) +> adjust_g; (* for julia *)
    
        (* the newi is for the labels to know where to attach, the newfakeelse (endi) is for the 'break' *)
        (* let newauxinfo = SwitchInfo (newi, newfakeelse, snd auxinfo), snd auxinfo in *)
        let newi2 = newi in
       

        (* let finalthen = aux_statement (None, newauxinfo) st in *)

         (* copy paste oc compund case *)
         let starti = None in
         (* let auxinfo = newauxinfo in *)
         let statement = st in

         let finalthen = 
             match statement with
         
             | Compound (declxs_statxs), ii -> 
                 (* todo_cfg_to_ast *)
                 (* old: declxs_statxs +> map_filter (function Right stat -> Some stat | _ -> None) +> (fun statxs -> *)
         
                 (* special_cfg_braces: *)
                 let _ = incr counter in 
                 let newi = !g#add_node (StartBrace (!counter, statement), "{" ^ i_to_s !counter) +> adjust_g_i in
                 let endi = !g#add_node (EndBrace !counter,   "}" ^ i_to_s !counter) +> adjust_g_i in
                 let newauxinfo = (fst auxinfo, endi::snd auxinfo) in

                 (* new: cos of switch *)
                 let newauxinfo = SwitchInfo (newi2, newfakeelse, newi, snd auxinfo), snd newauxinfo in
         
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
                       
                       let newi = !g#add_node (Declaration (decl), s) +> adjust_g_i in
                       (match starti with None -> () | Some starti -> 
                         !g#add_arc ((starti, newi), Direct) +> adjust_g);
                       Some newi,  auxinfo
                 ) (starti, newauxinfo)
         
         
                 (* special_cfg_braces: *)
                 +> (fun (starti, auxinfo) -> 
                   (match starti with 
                   | None -> None 
                   | Some starti -> 
                       (* subtil: not always return a Some *)
                       !g#add_arc ((starti, endi), Direct) +> adjust_g;
                       Some endi 
                   )
                    )
             | x -> error_cant_have x
         in




        (match finalthen with None -> () | Some finalthen -> 
          !g#add_arc ((finalthen, newfakeelse), Direct) +> adjust_g);
        Some newfakeelse


    | Labeled (Case  (e, st)), ii -> 
        let newi = !g#add_node (Statement (statement), "case:") +> adjust_g_i in
        attach_to_previous_node starti newi;
        (match fst auxinfo with
        | SwitchInfo (switchstarti, switchendi, startbrace, braces) -> 
              (* !g#add_arc ((switchstarti, newi), Direct) +> adjust_g; *)
              !g#add_arc ((switchstarti, startbrace), Direct) +> adjust_g; 
              !g#add_arc ((startbrace, newi), Direct) +> adjust_g;
        | _ -> raise (CaseNoSwitch (fst (List.hd ii)))
        );
        aux_statement (Some newi, auxinfo) st
        

    | Labeled (Default st), ii -> 

        let newi = !g#add_node (Statement (statement), "case default:") +> adjust_g_i in
        attach_to_previous_node starti newi;
        (match fst auxinfo with
        | SwitchInfo (switchstarti, switchendi, startbrace, braces) -> 
              (* !g#add_arc ((switchstarti, newi), Direct) +> adjust_g; *)
              !g#add_arc ((switchstarti, startbrace), Direct) +> adjust_g; 
              !g#add_arc ((startbrace, newi), Direct) +> adjust_g;
        | _ -> raise (CaseNoSwitch (fst (List.hd ii)))
        );
        aux_statement (Some newi, auxinfo) st

(* todo:
    | Labeled (CaseRange  (e, e2, st)) -> 
*)

    | Iteration  (While (e, st)), ii -> 
       (* starti -> newi ---> newfakethen -> ... -> finalthen -
                      |---|-----------------------------------|
                          |-> newfakelse 
       *)

        let newi = !g#add_node (Statement (statement), "while") +> adjust_g_i in
        attach_to_previous_node starti newi;
        let newfakethen = !g#add_node (Fake, "[whiletrue]") +> adjust_g_i in
        let newfakeelse = !g#add_node (Fake, "[endwhile]") +> adjust_g_i in

(*        !g#add_arc ((newi, newfakeelse), SpecialEdge) +> adjust_g;  not needed, just follow ChoiceFalse *)

        let newauxinfo = LoopInfo (newi, newfakeelse, snd auxinfo), snd auxinfo in

        !g#add_arc ((newi, newfakethen), ChoiceTrue) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), ChoiceFalse) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen, newauxinfo) st in
        (match finalthen with None -> () | Some finalthen -> 
          !g#add_arc ((finalthen, newi), Direct) +> adjust_g);
        Some newfakeelse

        
        
    | Iteration  (DoWhile (st, e)), ii -> 
       (* starti -> newi ---> newfakethen --> ... ---> finalthen --->   finali
                      |--------------------------------------------------|  |---> newfakelse 

       *)
        let newi = !g#add_node (Statement (statement), "do") +> adjust_g_i in
        (* todo?: make a special node ? cos have to repeat the info, need reput a Statement statement 
            a Fake node for the while (of dowhile) may not be enough. how found the corresponding condition ?
           peut etre a juste inverser les Fake et Statement, les mettre en fait dans newi et finali respectively
        *)
        let finali = !g#add_node (Fake, "while (of dowhile)") +> adjust_g_i in
        attach_to_previous_node starti newi;
        let newfakethen = !g#add_node (Fake, "[dowhiletrue]") +> adjust_g_i in
        let newfakeelse = !g#add_node (Fake, "[enddowhile]") +> adjust_g_i in


        (* this time, may return None, for instance if goto in body of dowhile (whereas While cant return None) *)
        (* the code of while case (different from dowhile) is put in comment, to illustrate the difference *)

        (* TODO, not used ????? *)
        let newauxinfo = LoopInfo (finali, newfakeelse, snd auxinfo) in

        !g#add_arc ((newi, newfakethen), ChoiceTrue) +> adjust_g; 

        !g#add_arc (((*newi*)finali, newfakethen), ChoiceTrue) +> adjust_g; 
        !g#add_arc (((*newi*)finali, newfakeelse), ChoiceFalse) +> adjust_g;

        let finalthen = aux_statement (Some newfakethen, auxinfo) st in 

        (* code of while case (different from dowhile) 
        (match finalthen with None -> () | Some finalthen -> 
          !g#add_arc ((finalthen, newi), Direct) +> adjust_g);
        Some newfakeelse
        *)
        (match finalthen with 
        | None -> None
        | Some finalthen -> 
            !g#add_arc ((finalthen, (*newi*)finali), Direct) +> adjust_g;
            Some newfakeelse
         )
        

    | Iteration  (For (e1opt, e2opt, e3opt, st)), ii -> 
        let newi = !g#add_node (Statement (statement), "for") +> adjust_g_i in
        attach_to_previous_node starti newi;
        let newfakethen = !g#add_node (Fake, "[fortrue]") +> adjust_g_i in
        let newfakeelse = !g#add_node (Fake, "[endfor]") +> adjust_g_i in


        let newauxinfo = LoopInfo (newi, newfakeelse, snd auxinfo), snd auxinfo in

        !g#add_arc ((newi, newfakethen), ChoiceTrue) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), ChoiceFalse) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen, newauxinfo) st in
        (match finalthen with None -> () | Some finalthen -> 
          !g#add_arc ((finalthen, newi), Direct) +> adjust_g);
        Some newfakeelse


    | Jump ((Continue|Break) as x),ii ->  
        (* todo_cfg_to_ast *)
        let newi = !g#add_node (Statement (statement), ("continue_break;")) +> adjust_g_i in
        attach_to_previous_node starti newi;

        (* let newi = some starti in *)

        (match fst auxinfo with
        | LoopInfo (loopstarti, loopendi, braces) -> 
            let desti = (match x with Break -> loopendi | Continue -> loopstarti | x -> error_cant_have x) in

            let difference = List.length (snd auxinfo) - List.length braces in
            let _ = assert (difference >= 0) in
            let toend = take difference (snd auxinfo) in
            
            let newi = special_cfg_insert_all_braces toend newi in


            !g#add_arc ((newi, desti), Direct) +> adjust_g;
            None
        | SwitchInfo (loopstarti, loopendi, startbrace, braces) -> 
            if x = Break then
              begin

                let difference = List.length (snd auxinfo) - List.length braces in
                let _ = assert (difference >= 0) in
                let toend = take difference (snd auxinfo) in
            
                let newi = special_cfg_insert_all_braces toend newi in

                !g#add_arc ((newi, loopendi), Direct) +> adjust_g;
                None
              end
            else raise (OnlyBreakInSwitch (fst (List.hd ii)))
        | NoInfo -> raise (NoEnclosingLoop (fst (List.hd ii)))
        )        





    | Jump (Return), ii -> 
           (* special_cfg_ast: *)
           let newi = !g#add_node (Statement (statement), "return") +> adjust_g_i in
           attach_to_previous_node starti newi;

           let newi = special_cfg_insert_all_braces (snd auxinfo) newi in

           !g#add_arc ((newi, exiti), Direct) +> adjust_g;
           None

    | Jump (ReturnExpr e), ii -> 
           let newi = !g#add_node (Statement (statement), "return ...") +> adjust_g_i in
           attach_to_previous_node starti newi;

           let newi = special_cfg_insert_all_braces (snd auxinfo) newi in

           !g#add_arc ((newi, exiti), Direct) +> adjust_g;
           None

           (* old:
             (match starti with None -> () | Some starti -> 
            !g#add_arc ((starti, exiti), Direct) +> adjust_g);
           *)

        

        
(* todo:
    | Asm -> 
*)
    | x -> error_cant_have x

  in
  (* todocheck: assert ? such as we have "consommer" tous les labels  *)

  let lasti = aux_statement (Some enteri, (NoInfo, [])) topstatement in

  (match lasti with  | None -> ()  | Some lasti ->
     !g#add_arc ((lasti, exiti), Direct) +> adjust_g);

  (* phase 2, deadcode detection *)
  (* old raise DeadCode: if lasti = None, but maybe not, in fact if have 2 return in the then and else of an if ? *)
  (* alt: but can assert that at least there exist a node to exiti,  just check #pred of exiti *)

  !g#nodes#iter (fun (k, (node, s)) -> 
    let pred = !g#predecessors k in
    if pred#null then 
      (match node with
      | Statement (st,ii::iis) -> raise (DeadCode (Some (fst ii)))

      | HeadFunc _ -> ()
      (* old: | Enter -> () *)

      | Fake -> pr2 "deadcode sur fake node, pas grave";
      | x -> pr2 (Dumper.dump x);  
          (* raise (DeadCode None) *)
      )
    );

  !g

(********************************************************************************)
(********************************************************************************)

(*------------------------------------------------------------------------------*)
(*

 statement, compound, 
 return,
 if, 
 for, while, dowhile
 break/continue
 goto, labels
 switch

 todo: maintain a todo list of labels
 if two todos, then ambiguity (can disambiguate some case by analysing and look if 
   dependency, if one label  lead to another "naturally" (by a sequence of Direct without jump, in
   a compound)

*)

(*------------------------------------------------------------------------------*)
type returnkind = LastCurrentNode of nodei | NoNextNode


(*------------------------------------------------------------------------------*)
let (control_flow_to_ast: (node, edge) ograph_extended -> definition) = fun g ->
  
  let nodes = g#nodes  in
  let starti = get_first_node g () in

(*  let visited = ref (new oassocb []) in *)


  let funcdef =  match fst (nodes#find starti) with | HeadFunc funcdef -> funcdef  | _ -> raise Todo in
  let (funcs, functype, sto, __compound, iifunc) = funcdef in


  let rec (rebuild_compound: nodei -> (statement * returnkind)) = fun   starti ->
    match nodes#find starti with
    | (StartBrace (level, (Compound __st, ii)), s) -> 
        (match get_next_node g starti with
        | (nexti, st) -> 
            let (compound, return) = rebuild_compound_instr_list  nexti level in
            (Compound compound, ii),  return
        )
    | x -> error_cant_have x

  and (rebuild_compound_instr_list: nodei -> int -> (compound * returnkind)) = fun starti level -> 
    match nodes#find starti with
    | (EndBrace level2, s) -> 
        if level = level2 
        then [], LastCurrentNode starti
        else raise Todo
(*    | (Declaration decl, s) -> *)
    | x -> 
        let (st, return) = rebuild_statement starti in
        (match return with
        | NoNextNode -> [Right st], return
        | LastCurrentNode nodei -> 
            let nexti = get_next_node g nodei +> fst in
            let (compound, return) = rebuild_compound_instr_list nexti level in
            Right st::compound, return
        )
        
(*    | x -> error_cant_have x *)

  and (rebuild_statement: nodei -> (statement * returnkind)) = fun starti -> 
    match nodes#find starti with
    | (Statement ((ExprStatement (None), ii) as st), s) -> 
        st, LastCurrentNode starti
    | (Statement ((ExprStatement (Some e), ii) as st), s) -> 
        st, LastCurrentNode starti

    | (Statement ((Jump (Return), ii) as st), s) -> 
        st, NoNextNode
    | (Statement ((Jump (ReturnExpr e), ii) as st), s) -> 
        st, NoNextNode

          
    | (StartBrace (level,st), s) -> 
        rebuild_compound starti

    | (Statement (Selection  (If (e, __st1, __st2)), ii), s) -> 
        let ((theni, nodethen), (elsei, nodeelse)) = get_next_2node g starti in

        let (theni', fakethen) = get_next_node g theni in
        let (elsei', fakeelse) = get_next_node g elsei in

        let (st1, return1) = rebuild_statement theni' in
        let (st2, return2) = rebuild_statement elsei' in
        raise Todo


    | x -> error_cant_have x
          

  in




  let (topcompound, returnkind) = 
    (match get_next_node g starti with
    | (nexti,  (Enter, s)) -> 
        (match get_next_node g nexti with
        | (nextii, (StartBrace (level,st), _)) -> 
            rebuild_compound nextii
        | x -> error_cant_have x
        );
    | x -> error_cant_have x
    ) 
  in
  (* todo?: assert stuff on returnkind ? normally lead to an exit node, or nothing *)

  let topcompound2 = match topcompound with | (Compound st, ii) -> st  | x -> error_cant_have x in

  (funcs, functype, sto, topcompound2, iifunc)
 

    





(*******************************************************************************)
(*******************************************************************************)
(*------------------------------------------------------------------------------*)
let print_control_flow g = 
  with_open_outfile "/tmp/test.dot" (fun (pr,_) ->
    pr "digraph misc {\n" ;
    let nodes = g#nodes in
    nodes#iter (fun (k,(node, s)) -> 
      pr (sprintf "%d [label=\"%s   [%d]\"];" k s k); (* so can see if nodes without arcs were created *) (*  (Dumper.dump node)); *)
    );

    nodes#iter (fun (k,node) -> 
      let succ = g#successors k in
      succ#iter (fun (j,edge) ->
        pr (sprintf "%d -> %d;\n" k j);
      );
    );
    pr "}\n" ;
    );
  let status = Unix.system "dot /tmp/test.dot -Tps  -o /tmp/test.ps; gv /tmp/test.ps" in
  ()


(*------------------------------------------------------------------------------*)

(*
  special_cfg_braces: 
   the check are really specific to the way we have build our control_flow, with the { } in the graph 
   so normally all those checks here are useless 
  evo: to better error reporting, to report earlier the message, 
   pass the list of '{' (containing morover a brace_identifier) instead of just the depth
*)

let (check_control_flow: (node, edge) ograph_extended -> unit) = fun g ->

  let nodes = g#nodes  in
  let starti = get_first_node g () in


  let visited = ref (new oassocb []) in

  let print_trace_error xs =  pr2 "PB with flow:";  pr2 (Dumper.dump xs);  (* assert false *)  in

  let rec dfs (nodei, (* Depth depth,*) startbraces,  trace)  = 
    let trace2 = nodei::trace in
    if !visited#haskey nodei 
    then 
      (* if loop back, just check that go back to a state where have same depth number *)
      let (*(Depth depth2)*) startbraces2 = !visited#find nodei in
      if  (*(depth = depth2)*) startbraces = startbraces2
      then ()
      else 
        begin 
          pr2 (sprintf "PB with flow: the node %d has not same braces count" nodei);  
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
        (match fst (nodes#find nodei),  startbraces with
        | StartBrace (i,_), xs  -> i::xs
        | EndBrace i, j::xs -> 
            if i = j 
            then xs
            else 
              begin 
                pr2 (sprintf "PB with flow: not corresponding match between }%d and excpeted }%d at node %d" i j nodei); 
                print_trace_error trace2; 
                xs 
              end
        | EndBrace i, [] -> 
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
        children#tolist +> List.iter (fun (nodei,_) -> dfs (nodei, newdepth, trace2))
    in

  dfs (starti, (* Depth 0*) [], [])

  
(*------------------------------------------------------------------------------*)

let test statement = 
  let g = ast_to_control_flow statement in
  check_control_flow g;
  print_control_flow g;
  assert (statement = statement +> ast_to_control_flow +> control_flow_to_ast);
  pr2 "done";
  

