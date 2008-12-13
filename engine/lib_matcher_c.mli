
(* a protocol is for the moment represented as 2 rule_elem, a positive 
 * pattern (e.g. spin_lock_irq()) and negative one (e.g. spin_unlock_irq())
 *)
type protocol_match = 
  | MatchPos of Ograph_extended.nodei
  | MatchNeg of Ograph_extended.nodei
  | NoMatch 
  | MatchProblem of string


val find_nodes_satisfying_pattern: 
  Control_flow_c.cflow -> Ast_cocci.rule_elem -> Ograph_extended.nodei list
val find_nodes_containing_expr: 
  Control_flow_c.cflow -> Ast_c.expression -> Ograph_extended.nodei list


val find_nodes_upward_satisfying_protocol: 
  Ograph_extended.nodei -> Control_flow_c.cflow -> 
  Ast_cocci.rule_elem * Ast_cocci.rule_elem -> 
  protocol_match
