type mvar = Ast_cocci.meta_name
type predicate =
    InLoop
  | TrueBranch
  | EscTrueBranch
  | FalseBranch
  | After
  | GotoAfter
  | FallThrough
  | LoopFallThrough
  | Return
  | FunHeader
  | UnsafeBrace
  | Top
  | Exit
  | PreExit
  | ErrorExit
  | Goto
  | Paren of Ast_cocci.meta_name
  | Match of Ast_cocci.rule_elem
  | Label of Ast_cocci.meta_name
  | BCLabel of Ast_cocci.meta_name
  | PrefixLabel of Ast_cocci.meta_name
  | BindGood of Ast_cocci.meta_name
  | BindBad of Ast_cocci.meta_name
  | FakeBrace
type ctlcocci = (predicate, Ast_cocci.meta_name) Wrapper_ctl.wrapped_ctl
type metavars_binding = Ast_c.metavars_binding
type metavar_binding_kind2 =
    NormalMetaVal of Ast_c.metavar_binding_kind
  | ParenVal of Ast_cocci.meta_name
  | LabelVal of labelval
  | GoodVal
  | BadVal
and labelval = Absolute of int list | Prefix of int list
and metavars_binding2 = (mvar, metavar_binding_kind2) Common.assoc
type label_ctlcocci =
    predicate ->
    (Control_flow_c.G.key *
     (predicate * (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution))
    list
type quicklabel_ctlcocci = predicate -> bool
type model =
    Control_flow_c.cflow * label_ctlcocci * quicklabel_ctlcocci *
    Control_flow_c.G.key list
type transformation_info =
    (Control_flow_c.G.key * metavars_binding * Ast_cocci.rule_elem) list
type numbered_transformation_info =
    (int list *
     (Control_flow_c.G.key * metavars_binding * Ast_cocci.rule_elem))
    list
val equal_binding : 'a list -> 'a list -> bool
