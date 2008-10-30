open Common

open Ast_c

(*****************************************************************************)
(* 
 * There is more information in the CFG we build that in the CFG usually built
 * in a compiler. This is because:
 *
 *  - We need later to go back from flow to original ast, because we are 
 *    doing a refactoring tool, so different context. So we have to add
 *    some nodes for '{' or '}' or goto that normally disapear in a CFG.
 *    We must keep those entities, in the same way that we must keep the parens
 *    (ParenExpr, ParenType) in the Ast_c during parsing.
 *
 *    Moreover, the coccier can mention in his semantic patch those entities,
 *    so we must keep those entities in the CFG.
 *    
 *    We also have to add some extra nodes to make the process that goes from 
 *    flow to ast deterministic with for instance the CaseNode, or easier 
 *    with for instance the Fake node.
 *
 *  - The coccinelle engine later transforms some nodes, and we need to rebuild
 *    the ast from a statement now defined and altered in different nodes. 
 *    So we can't just put all the parsing info (Ast_c.il) in the top node of
 *    a statement. We have to split those Ast_c.il in different nodes, to 
 *    later reconstruct a full Ast_c.il from different nodes. This is why
 *    we need the Else node, ...
 * 
 *    Note that at the same time, we also need to store the fullstatement 
 *    in the top node, because the CTL engine need to get that information
 *    when dealing with MetaStatement (statement S; in a Semantic Patch).
 *    
 *    
 *  - The CTL engine needs more information than just the CFG, and we use
 *    tricks to encode those informations in the nodes:
 *
 *       - We have some TrueNode, FalseNode to know in what branch we are.
 *         Normally we could achieve this by putting this information in the
 *         edges, but CTL engine know nothing about edges, it must do
 *         everything with only nodes information.
 *
 *       - We need to mark each braces with an identifier so that the CTL
 *         can know if one specific '}' correspond to a specific '{'.
 *
 *       - We add some labels to each node to handle the MetaRuleElem and 
 *         MetaStatement. It allows to groups nodes that belong to the same
 *         statement. Normally CFG are there to abstract from this, but in
 *         Coccinelle we need sometimes the CFG view, and sometimes the Ast
 *         view and the labels allow that.
 *
 *       - We even add nodes. We add '}', not only to be able to go back to AST
 *         but also because of the CTL engine. So one '}' may in fact be 
 *         represented by multiple nodes, one in each CFG path.
 * 
 *       - need After, 
 *       - need FallThrough.
 *       - Need know if ErrorExit, 
 *
 * choice: Julia proposed that the flow is in fact just
 * a view through the Ast, which means just Ocaml ref, so that when we
 * modify some nodes, in fact it modifies the ast. But I prefer do it 
 * the functionnal way.
 * 
 * The node2 type should be as close as possible to Ast_cocci.rule_elem to
 * facilitate the job of cocci_vs_c.
 * 
 *)

(*****************************************************************************)


(* ---------------------------------------------------------------------- *)
(* The string is for debugging. Used by Ograph_extended.print_graph. 
 * The int list are Labels. Trick used for CTL engine. Must not 
 * transform that in a triple or record because print_graph would
 * not work.
 *)
type node = node1 * string  
  and node1 = node2 * nodeinfo 
    and nodeinfo = {
      labels: int list;
      bclabels: int list; (* parent of a break or continue node *)
      is_loop: bool;
      is_fake: bool;
    }
    and node2 =

  (* ------------------------ *)
  (* For CTL to work, we need that some nodes loop over itself. We
   * need that every nodes have a successor. Julia also want to go back
   * indefinitely. So must tag some nodes as the beginning and end of
   * the graph so that some fix_ctl function can easily find those
   * nodes.
   * 
   * If have a function, then no need for EndNode; Exit and ErrorExit
   * will play that role.
   * 
   * When everything we analyze was a function there was no pb. We used
   * FunHeader as a Topnode and Exit for EndNode but now that we also
   * analyse #define body, so we need those nodes.
   *)
   | TopNode 
   | EndNode 

   (* ------------------------ *)
   | FunHeader of definition (* but empty body *)

   | Decl   of declaration

   (* ------------------------ *)
   (* flow_to_ast: cocci: Need the { and } in the control flow graph also
    * because the coccier can express patterns containing such { }.
    *
    * ctl: to make possible the forall (AX, A[...]), have to add more than
    * one node sometimes for the same '}' (one in each CFG path) in the graph.
    *
    * ctl: Morover, the int in the type is here to indicate to what { } 
    * they correspond. Two pairwise { } share the same number. kind of 
    * "brace_identifier". Used for debugging or for checks and more importantly,
    * needed by CTL engine.
    *
    * Because of those nodes, there is no equivalent for Compound.
    * 
    * There was a problem with SeqEnd. Some info can be tagged on it 
    * but there is multiple SeqEnd that correspond to the same '}' even
    * if they are in different nodes. Solved by using shared ref
    * and allow the "already-tagged" token.
    *)
  | SeqStart of statement * int * info
  | SeqEnd   of int * info


  | ExprStatement of statement * (expression option) wrap


  | IfHeader  of statement * expression wrap
  | Else of info
  | WhileHeader of statement * expression wrap
  | DoHeader of statement * info
  | DoWhileTail of expression wrap
  | ForHeader of statement * 
                 (exprStatement wrap * exprStatement wrap * exprStatement wrap)
                 wrap
  | SwitchHeader of statement * expression wrap
  | MacroIterHeader of statement * (string * argument wrap2 list) wrap

  (* Used to mark the end of if, while, dowhile, for, switch. Later we
   * will be able to "tag" some cocci code on this node.
   * 
   * This is because in
   * 
   * - S + foo();
   * 
   * the S can be anything, including an if, and this is internally
   * translated in a series of MetaRuleElem, and the last element is a
   * EndStatement, and we must tag foo() to this EndStatement.
   * Otherwise, without this last common node, we would tag foo() to 2
   * nodes :( So having a unique node makes it correct, and in
   * flow_to_ast we must propagate back this + foo() to the last token
   * of an if (maybe a '}', maybe a ';') 
   * 
   * The problem is that this stuff should be in transformation.ml,  
   * but need information available in flow_to_ast, but we dont want
   * to polluate both files.
   * 
   * So the choices are 
   * 
   * - soluce julia1, extend Ast_c by adding a fake token to the if
   * 
   * - extend Ast with a Skip, and add this next to EndStatement node,
   * and do special case in flow_to_ast to start from this node
   * (not to get_next EndStatement, but from EndStatement directly)
   * and so add a case when have directly a EndStatement node an extract
   * the statement from it.
   * 
   * - remonter dans le graphe pour accrocher le foo() non plus au 
   * EndStatement (qui n'a pas d'equivalent niveau token dans l'ast_c), 
   * mais au dernier token de la branche Else (ou Then si y'a pas de else).
   * 
   * I first did solution 2 and then when we decided to use ref,
   * I use julia'as solution. Have virtual-placeholders, the fakeInfo
   * for the if, while, and put this shared ref in the EndStatement.
   *)
  | EndStatement of info option (* fake_info *)

  | Return     of statement * unit wrap
  | ReturnExpr of statement * expression wrap

  (* ------------------------ *)
  | IfdefHeader of ifdef_directive
  | IfdefElse of ifdef_directive
  | IfdefEndif of ifdef_directive


  (* ------------------------ *)
  | DefineHeader of string wrap * define_kind

  | DefineExpr of expression 
  | DefineType of fullType
  | DefineDoWhileZeroHeader of unit wrap

  | Include of includ

  (* obsolete? *)
  | MacroTop of string * argument wrap2 list * il 

  (* ------------------------ *)
  | Case  of statement * expression wrap
  | Default of statement * unit wrap

  | Continue of statement * unit wrap
  | Break    of statement * unit wrap

  (* no counter part in cocci *)
  | CaseRange of statement * (expression * expression) wrap
  | Label of statement * string wrap
  | Goto of statement * string wrap


  | Asm of statement * asmbody wrap
  | MacroStmt of statement * unit wrap

  (* ------------------------ *)
  (* some control nodes *)
  | Enter 
  | Exit


  (* Redundant nodes, often to mark the end of an if/switch.
   * That makes it easier to do later the flow_to_ast. 
   * update: no more used for the end. see Endstatement. Just used
   * to mark the start of the function, as required by julia.
   * Maybe would be better to use instead a Enter2.
   *)
  | Fake 

  (* flow_to_ast: In this case, I need to know the  order between the children
   * of the switch in the graph. 
   *)
  | CaseNode of int 

  (* ------------------------ *)
  (* for ctl:  *)
  | TrueNode
  | FalseNode
  | InLoopNode (* almost equivalent to TrueNode but just for loops *)

  | AfterNode
  | FallThroughNode

  | ErrorExit

type edge = Direct (* Normal | Shadow *)

type cflow = (node, edge) Ograph_extended.ograph_mutable


(* ------------------------------------------------------------------------ *)
let unwrap ((node, info), nodestr) = node
let rewrap ((_node, info), nodestr) node = (node, info), nodestr
let extract_labels ((node, info), nodestr) = info.labels
let extract_bclabels ((node, info), nodestr) = info.bclabels
let extract_is_loop ((node, info), nodestr) = info.is_loop 
let extract_is_fake ((node, info), nodestr) = info.is_fake

let mk_any_node is_fake node labels bclabels nodestr =
  let nodestr = 
    if !Flag_parsing_c.show_flow_labels
    then nodestr ^ ("[" ^ (labels +> List.map i_to_s +> join ",") ^ "]")
    else nodestr
  in
  ((node, {labels = labels;is_loop=false;bclabels=bclabels;is_fake=is_fake}),
   nodestr)

let mk_node = mk_any_node false
let mk_fake_node = mk_any_node true (* for duplicated braces *)

(* ------------------------------------------------------------------------ *)
let first_node g = 
  g#nodes#tolist +> List.find (fun (i, node) -> 
    match unwrap node with TopNode -> true | _ -> false
    ) +> fst

let find_node f g = 
  g#nodes#tolist +> List.find (fun (nodei, node) -> 
    f (unwrap node)) 
     +> fst


(* remove an intermediate node and redirect the connexion  *)
let remove_one_node nodei g = 
  let preds = (g#predecessors nodei)#tolist in
  let succs = (g#successors nodei)#tolist in
  assert (not (null preds));

  preds +> List.iter (fun (predi, Direct) -> 
    g#del_arc ((predi, nodei), Direct);
  );
  succs +> List.iter (fun (succi, Direct) -> 
    g#del_arc ((nodei, succi), Direct);
  );
  
  g#del_node nodei;
  
  (* connect in-nodes to out-nodes *)
  preds +> List.iter (fun (pred, Direct) -> 
    succs +> List.iter (fun (succ, Direct) -> 
      g#add_arc ((pred, succ), Direct);
    );
  )



(* ------------------------------------------------------------------------ *)

let extract_fullstatement node = 
  match unwrap node with
  | Decl decl -> 
      (* new policy. no more considered as a statement *)
      (* old: Some (Ast_c.Decl decl, []) *)
      None 
  | MacroStmt (st, _) -> Some st
  | MacroIterHeader (st, _) -> Some st

  | Include _ 
  | DefineHeader _ | DefineType _ | DefineExpr  _ | DefineDoWhileZeroHeader _
  | MacroTop _
      -> None

  | IfdefHeader _ | IfdefElse _ | IfdefEndif _ 
      -> None

  | SeqStart (st,_,_) 
  | ExprStatement (st, _)
  | IfHeader  (st, _) 
  | WhileHeader (st, _)
  | DoHeader (st, _)
  | ForHeader (st, _)
  | SwitchHeader (st, _)
  | Return     (st, _)
  | ReturnExpr (st, _)
  (* no counter part in cocci *)
  | Label (st, _)
  | Case  (st,_)
  | CaseRange (st, _)
  | Default   (st, _)
  | Goto (st, _)
  | Continue (st, _)
  | Break    (st, _)
  | Asm (st,_)
      -> Some st

  | TopNode|EndNode
  | FunHeader _
  | SeqEnd  _ 
  | Else _ 
  | EndStatement _
  | DoWhileTail _
  | Enter 
  | Exit
  | Fake
  | CaseNode _
  | TrueNode
  | FalseNode
  | InLoopNode
  | AfterNode
  | FallThroughNode
  | ErrorExit
    -> None
