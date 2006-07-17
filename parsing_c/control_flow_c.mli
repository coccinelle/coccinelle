
type node = node1 * string (* to debug *)
and node1 = node2 * int list (* The labels. Trick used by ctl engine. *)
and node2 =
    HeadFunc of Ast_c.definition
  | Enter
  | Exit
  | Statement of Ast_c.statement
  | Declaration of Ast_c.declaration
  | Fake
  | StartBrace of int * Ast_c.statement * Ast_c.info
  | EndBrace of int * Ast_c.info
  | CaseNode of int
  | TrueNode
  | FalseNode
  | AfterNode
  | FallThroughNode



type edge = Direct

exception DeadCode          of Common.parse_info option
exception CaseNoSwitch      of Common.parse_info
exception OnlyBreakInSwitch of Common.parse_info
exception NoEnclosingLoop   of Common.parse_info

val unwrap : node -> node2
val rewrap : node -> node2 -> node
val extract_labels : node -> int list

val ast_to_control_flow :
  Ast_c.definition -> (node, edge) Ograph_extended.ograph_extended

val control_flow_to_ast :
  (node, edge) Ograph_extended.ograph_extended -> Ast_c.definition

val deadcode_detection : (node, edge) Ograph_extended.ograph_extended -> unit

val check_control_flow : (node, edge) Ograph_extended.ograph_extended -> unit

val test : Ast_c.definition -> unit
