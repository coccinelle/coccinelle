
type node = node1 * string
and node1 =
    HeadFunc of Ast_c.definition
  | Enter
  | Exit
  | NestedFunCall of Ast_c.expression
  | Statement of Ast_c.statement
  | Declaration of Ast_c.declaration
  | Fake
  | StartBrace of int * Ast_c.statement
  | EndBrace of int
  | TrueNode
  | FalseNode
  | AfterNode

type edge = Direct

exception DeadCode of Common.parse_info option
exception CaseNoSwitch of Common.parse_info
exception OnlyBreakInSwitch of Common.parse_info
exception NoEnclosingLoop of Common.parse_info

val ast_to_control_flow :
  Ast_c.definition -> (node, edge) Ograph_extended.ograph_extended

val control_flow_to_ast :
  (node, edge) Ograph_extended.ograph_extended -> Ast_c.definition

val check_control_flow : (node, edge) Ograph_extended.ograph_extended -> unit

val test : Ast_c.definition -> unit
