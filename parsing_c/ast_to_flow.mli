
val ast_to_control_flow : Ast_c.definition -> Control_flow_c.cflow

val simple_cfg : Control_flow_c.node2 -> string -> Control_flow_c.cflow

val deadcode_detection : Control_flow_c.cflow -> unit
val check_control_flow : Control_flow_c.cflow -> unit


type error = 
  | DeadCode          of Common.parse_info option
  | CaseNoSwitch      of Common.parse_info
  | OnlyBreakInSwitch of Common.parse_info
  | NoEnclosingLoop   of Common.parse_info
  | GotoCantFindLabel of string * Common.parse_info
  | DuplicatedLabel of string
  | NestedFunc

exception Error of error

val report_error : error -> unit
