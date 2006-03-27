open Commonop open Common
(*
 deadCode detection
  what is dead code ? when there is no starti  to start from ? => make starti an option too ?
   si on arrive sur un label: au moment d'un deadCode, on peut verifier les predecesseurs de ce label,
   auquel cas si y'en a, ca veut dire qu'en fait c pas du deadCode et que dont on peut se permettre
   de partir d'un starti à None
  MAIS si on a   xx; goto far:; near: yy; zz; far: goto near:   , bon ca doit etre un cas tres tres rare, 
   mais a cause de notre parcours, on va rejeter ce programme car au moment d'arriver sur near:  on n'
   a pas encore de predecesseurs pour ce label.
  de meme, meme le cas simple ou la derniere instruction c'est un return, alors ca va generer un DeadCode :(
  => make a first pass where dont launch exn at all, create nodes, if starti is None then dont add    arc. 
          a second pass, just check that all nodes (except enter) have predecessors. 
            (todo: if the pb is at a fake node, then try first successos that is non fake)
  => make starti  an option too (so type is now  int option -> statement -> int option)
 old: I think that DeadCode is too aggressive, what if  have both return in else/then ? 
   

 todo: expression, linearize,  funcall (and launch exn  with StatementExpr )

 todo: complete (break, continue, ...)

 todo?: steal code from CIL ? (but seems complicated ... again)

*)

open Ograph_extended
open Oassoc
open Oassocb

open Ast_c
open Visitor_c


(*------------------------------------------------------------------------------*)
type node = node1 * string
and node1 = 
  | HeadFunc of definition
  | Enter 
  | Exit
  | NestedFunCall of expression   (* cos "fake" node *)
  | Statement of statement
  | Declaration of declaration
  | Fake

type edge = 
  | Direct
  (* if, while, for *)
  | ChoiceTrue
  | ChoiceFalse 
  (* todo: and with switch ? *)


exception DeadCode of (Common.parse_info option)

(*------------------------------------------------------------------------------*)
let (build_control_flow: definition -> (node, edge) ograph_extended) = fun funcdef ->
  let g = ref (new ograph_extended) in

  (* monad like, >>= *)
  let adjust_g_i (newg,newi) = 
    begin
      g := newg;
      newi
    end in
  (* monad like, >> *)
  let adjust_g (newg) = 
    begin
      g := newg;
    end in

  let (funcs, functype, sto, compound, (_,_,ii)) = funcdef in
  let topstatement = Compound compound, ii in


  let headi = !g#add_node (HeadFunc funcdef, "function " ^ funcs) +> adjust_g_i in

  let enteri = !g#add_node (Enter, "[enter]") +> adjust_g_i in
  let exiti  = !g#add_node (Exit,  "[exit]")  +> adjust_g_i in
  
  let _ = !g#add_arc ((headi, enteri), Direct) +> adjust_g in


  (* alt: do via a todo list, so can do all in one pass (but more complex) *)
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
                 assert (not (!h#haskey s)); (* label already exist ? *)
                 h := !h#add (s, newi);
                 k st;
               end
           | e -> k e
                 )};
      !h;
    end
    
  in

  let labels_assoc = compute_labels topstatement in

  (* take start, return end *)
  (*   old: old code was returning an int, but goto has no end => aux_statement should return   int option 
       old: old code was taking an int, but should also take int option
  *)
  let rec (aux_statement: int option -> statement -> int option) = fun starti statement ->
    match statement with
    | Compound (declxs_statxs), ii -> 
        (* old: declxs_statxs +> map_filter (function Right stat -> Some stat | _ -> None) +> (fun statxs -> *)
        declxs_statxs +> List.fold_left (fun starti st ->
          match st with
          | Right stat -> aux_statement starti stat 
          | Left decl -> 
              let s = 
                (match decl with
                | (DeclList ([(Some (s, _,_), typ, sto), _], _)) -> "decl:" ^ s
                | _ -> "decl_novar"
                ) in
              
              let newi = !g#add_node (Declaration (decl), s) +> adjust_g_i in
              (match starti with None -> () | Some starti -> 
                !g#add_arc ((starti, newi), Direct) +> adjust_g);
              Some newi
        ) starti
        
    | Labeled (Label (s, st)), ii -> 
        let ilabel = labels_assoc#find s in
        (match starti with None -> () | Some starti -> 
          !g#add_arc ((starti, ilabel), Direct) +> adjust_g);
        aux_statement (Some ilabel) st
        
(* todo:
    | Labeled (Case  (e, st)) -> 
    | Labeled (CaseRange  (e, e2, st)) -> 
    | Labeled (Default st) -> 
*)
    | ExprStatement (None), ii -> starti
    | ExprStatement (Some e), ii -> 
        let s = 
          (match e with
          | (FunCall ((Constant (Ident f), _),ii3),ii2) -> f ^ "(...)"
          | _ -> "statement"
          )
        in
        (* todo: may contain funcall, so have to "linearize" that expression *)
        let newi = !g#add_node (Statement (statement), s) +> adjust_g_i in
        (match starti with None -> () | Some starti -> 
         !g#add_arc ((starti, newi), Direct) +> adjust_g);
        Some newi
        
        
    | Selection  (If (e, st1, st2)), ii -> 
       (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
                          |                                      |
                          |->   newfakeelse -> ... -> finalelse -|
       *)
        let newi = !g#add_node (Statement (statement), "if") +> adjust_g_i in
        (match starti with None -> () | Some starti -> 
         !g#add_arc ((starti, newi), Direct) +> adjust_g);
        let newfakethen = !g#add_node (Fake, "[then]") +> adjust_g_i in
        let newfakeelse = !g#add_node (Fake, "[else]") +> adjust_g_i in
        let lasti = !g#add_node (Fake, "[endif]") +> adjust_g_i in
        !g#add_arc ((newi, newfakethen), ChoiceTrue) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), ChoiceFalse) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen) st1 in
        let finalelse = aux_statement (Some newfakeelse) st2 in
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
        
        

(* todo:    | Selection  (Switch (e, st)) -> *)

    | Iteration  (While (e, st)), ii -> 
       (* starti -> newi ---> newfakethen -> ... -> finalthen -
                      |---|-----------------------------------|
                          |-> newfakelse 
       *)

        let newi = !g#add_node (Statement (statement), "while") +> adjust_g_i in
        (match starti with None -> () | Some starti -> 
          !g#add_arc ((starti, newi), Direct) +> adjust_g);
        let newfakethen = !g#add_node (Fake, "[whiletrue]") +> adjust_g_i in
        let newfakeelse = !g#add_node (Fake, "[endwhile]") +> adjust_g_i in
        !g#add_arc ((newi, newfakethen), ChoiceTrue) +> adjust_g;
        !g#add_arc ((newi, newfakeelse), ChoiceFalse) +> adjust_g;
        let finalthen = aux_statement (Some newfakethen) st in
        (match finalthen with None -> () | Some finalthen -> 
          !g#add_arc ((finalthen, newi), Direct) +> adjust_g);
        Some newfakeelse

        
        
(* todo:
    | Iteration  (DoWhile (st, e)) -> (* this time, may return None, for instance if goto in body of dowhile (whereas While cant return None) *)
    | Iteration  (For (e1opt, e2opt, e3opt, st)) -> 
*)
    | Jump (Goto s), ii -> 
        (* could build a, additional node, to see it in dot but ... *)
        let ilabel = labels_assoc#find s in
        (match starti with None -> () | Some starti -> 
          !g#add_arc ((starti, ilabel), Direct) +> adjust_g);
        None
        

    | Jump (Return), ii -> 
        (* could build an additional node, to see it in dot but ... *)
          (match starti with None -> () | Some starti -> 
             !g#add_arc ((starti, exiti), Direct) +> adjust_g);
           None

    | Jump (ReturnExpr e), ii -> 
           let newi = !g#add_node (Statement (statement), "return ...") +> adjust_g_i in
           (match starti with None -> () | Some starti -> 
             !g#add_arc ((starti, newi), Direct) +> adjust_g);
           !g#add_arc ((newi, exiti), Direct) +> adjust_g;
           None
(* todo:
    | Jump (Continue|Break) -> 
    | Asm -> 
*)
    | _ -> raise Todo

  in

  (* todocheck: assert ? such as we have "consommer" tous les labels
  *)

  let lasti = aux_statement (Some enteri) topstatement in

 (* old raise DeadCode:, but maybe not, in fact if have 2 return in the then and else of an if ? *)
 (* alt: but can assert that at least there exist a node to exiti,  just check #pred of exiti *)
  (match lasti with  | None -> ()  | Some lasti ->
     !g#add_arc ((lasti, exiti), Direct) +> adjust_g);

  (* phase 2, deadcode detection *)
  !g#nodes#iter (fun (k, (node, s)) -> 
    let pred = !g#predecessors k in
    if pred#null then 
      (match node with
      | Statement (st,ii::iis) -> raise (DeadCode (Some ii))

      | HeadFunc _ -> ()
      (* old: | Enter -> () *)

      | Fake -> pr2 "deadcode sur fake node, pas grave";
      | x -> pr2 (Dumper.dump x);  raise (DeadCode None)
      )
    );

  !g



(*------------------------------------------------------------------------------*)
let print_control_flow g = 
  with_open_outfile "/tmp/test.dot" (fun (pr,_) ->
    pr "digraph misc {\n" ;
    let nodes = g#nodes in
    nodes#iter (fun (k,(node, s)) -> 
      pr (sprintf "%d [label=\"%s\"];" k s); (* so can see if nodes without arcs were created *) (*  (Dumper.dump node)); *)
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

let test statement = 
  let g = build_control_flow statement in
  print_control_flow g;
  pr2 "done";
  

