
exception DeadCode          of Common.parse_info option
exception CaseNoSwitch      of Common.parse_info
exception OnlyBreakInSwitch of Common.parse_info
exception NoEnclosingLoop   of Common.parse_info

val ast_to_control_flow : Ast_c.definition -> Control_flow_c.cflow

val deadcode_detection : Control_flow_c.cflow -> unit
val check_control_flow : Control_flow_c.cflow -> unit

