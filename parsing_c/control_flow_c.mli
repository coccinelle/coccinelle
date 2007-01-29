open Ast_c

type node = node1 * string (* For debugging. Used by print_graph *)
 and node1 = node2 * int list (* Labels. Trick used for CTL engine *)
 and node2 =
  | FunHeader     of (string * functionType * storage) wrap
  | Decl          of declaration

  | SeqStart      of statement * int * info
  | SeqEnd        of int * info

  | ExprStatement of statement * (expression option) wrap

  | IfHeader      of statement * expression wrap
  | Else          of info
  | WhileHeader   of statement * expression wrap
  | DoHeader      of statement * info
  | DoWhileTail   of expression wrap
  | ForHeader     of statement * 
                 (exprStatement wrap * exprStatement wrap * exprStatement wrap)
                 wrap
  | SwitchHeader  of statement * expression wrap

  | EndStatement  of info option

  | Return        of statement * unit wrap
  | ReturnExpr    of statement * expression wrap

  (* ------------------------ *)
  (* no counter part in cocci *)
  | Label     of statement * string wrap
  | Case      of statement * expression wrap
  | CaseRange of statement * (expression * expression) wrap
  | Default   of statement * unit wrap

  | Goto      of statement * string wrap
  | Continue  of statement * unit wrap
  | Break     of statement * unit wrap

  | Asm

  | IfCpp of statement * unit wrap

  | CPPInclude of string wrap
  | CPPDefine  of (string * string) wrap

  (* ------------------------ *)
  (* some control nodes *)
  | Enter 
  | Exit
  | Fake
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

val unwrap : node -> node2
val rewrap : node -> node2 -> node
val extract_labels : node -> int list
val extract_fullstatement : node -> Ast_c.statement option

val get_first_node : cflow -> Ograph_extended.nodei

