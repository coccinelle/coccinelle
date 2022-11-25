type 'a outer = Outer of 'a | Inner of 'a
val ast_to_control_flow :
    Ast_c.toplevel -> (Ast_c.toplevel outer * Control_flow_c.cflow option) list

val deadcode_detection : Control_flow_c.cflow -> unit
val check_control_flow : Control_flow_c.cflow -> unit

val annotate_loop_nodes : Control_flow_c.cflow -> Control_flow_c.cflow

type error =
  | DeadCode          of Common.parse_info option
  | CaseNoSwitch      of Common.parse_info
  | OnlyBreakInSwitch of Common.parse_info
  | WeirdSwitch       of Common.parse_info
  | NoEnclosingLoop   of Common.parse_info
  | GotoCantFindLabel of string * Common.parse_info
  | NoExit of Common.parse_info
  | DuplicatedLabel of string
  | ComputedGoto
  | Define of Common.parse_info

exception Error of error

val report_error : error -> unit

val realstring : bool ref
