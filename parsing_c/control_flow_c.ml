open Commonop open Common

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
 *    Morover, the coccier can mention in his semantic patch those entities,
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
 *  a view through the Ast, which means just Ocaml ref, so that when we
 *  modify some nodes, in fact it modifies the ast. But I prefer do it 
 *  the functionnal way.
 * 
 *)

(*****************************************************************************)
open Ast_c


(* The string is for debugging. Used by Ograph_extended.print_graph. 
 * The int list are Labels. Trick used for CTL engine. 
 *)
type node = node1 * string  
 and node1 = node2 * int list 
 and node2 = 
  | FunHeader of (string * functionType * storage) wrap

  | Decl   of declaration

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
  (* no counter part in cocci *)
  | Label of statement * string wrap
  | Case  of statement * expression wrap
  | CaseRange of statement * (expression * expression) wrap
  | Default of statement * unit wrap

  | Goto of statement * string wrap
  | Continue of statement * unit wrap
  | Break    of statement * unit wrap

  | Asm
  | IfCpp of statement * unit wrap

  | CPPInclude of string wrap
  | CPPDefine of (string * string) wrap

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

  | AfterNode
  | FallThroughNode

  | ErrorExit

type edge = Direct

type cflow = (node, edge) Ograph_extended.ograph_extended


(* ------------------------------------------------------------------------ *)
let unwrap ((node, labels), nodestr) = node
let rewrap ((_node, labels), nodestr) node = (node, labels), nodestr
let extract_labels ((node, labels), nodestr) = labels


(* ------------------------------------------------------------------------ *)
let get_first_node g = 
  g#nodes#tolist +> List.find (fun (i, node) -> 
    match unwrap node with FunHeader _ -> true | _ -> false
    ) +> fst

(* ------------------------------------------------------------------------ *)

let extract_fullstatement node = 
  match unwrap node with
  | Decl decl -> 
      (* new policy. no more considered as a statement *)
      (* old: Some (Ast_c.Decl decl, []) *)
      None 
  | Asm -> Some (Ast_c.Asm, [])
  | IfCpp _ -> None (* other ? *)
  | CPPInclude _ | CPPDefine _ -> None

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
      -> Some st

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
  | AfterNode
  | FallThroughNode
  | ErrorExit
    -> None
