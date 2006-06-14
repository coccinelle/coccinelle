type mvar = string
type metavar_binding_kind2 =
    NormalMetaVar of Ast_c.metavar_binding_kind
  | ParenVar of string
module ENV :
  sig
    type value = metavar_binding_kind2
    type mvar = string
    val eq_mvar : 'a -> 'a -> bool
    val eq_val : 'a -> 'a -> bool
    val merge_val : 'a -> 'b -> 'a
  end
module CFG :
  sig
    type node = int
    type cfg =
        (Control_flow_c.node, Control_flow_c.edge)
        Ograph_extended.ograph_extended
    val predecessors :
      < predecessors : 'a -> < tolist : ('b * 'c) list; .. >; .. > ->
      'a -> 'b list
  end
type ('a, 'b) generic_ctl =
  ('a, 'b) Ctl_engine.CTL_ENGINE(ENV)(CFG).generic_ctl =
    False
  | True
  | Pred of 'a * 'b modif
  | Not of ('a, 'b) generic_ctl
  | Exists of 'b * ('a, 'b) generic_ctl
  | And of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
  | Or of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
  | Implies of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
  | AF of ('a, 'b) generic_ctl
  | AX of ('a, 'b) generic_ctl
  | AG of ('a, 'b) generic_ctl
  | AU of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
  | EF of ('a, 'b) generic_ctl
  | EX of ('a, 'b) generic_ctl
  | EG of ('a, 'b) generic_ctl
  | EU of ('a, 'b) generic_ctl * ('a, 'b) generic_ctl
and 'a modif =
  'a Ctl_engine.CTL_ENGINE(ENV)(CFG).modif =
    Modif of 'a
  | UnModif of 'a
  | Control
type ('a, 'b) generic_subst =
  ('a, 'b) Ctl_engine.CTL_ENGINE(ENV)(CFG).generic_subst =
    Subst of 'a * 'b
  | NegSubst of 'a * 'b
type ('a, 'b) generic_substitution = ('a, 'b) generic_subst list
type ('a, 'b, 'c) generic_witness =
  ('a, 'b, 'c) Ctl_engine.CTL_ENGINE(ENV)(CFG).generic_witness =
    Wit of 'a * 'b * 'c * ('a, 'b, 'c) generic_witness list
  | NegWit of 'a * 'b * 'c * ('a, 'b, 'c) generic_witness list
val sat :
  CFG.cfg *
  ('a ->
   (CFG.node * (ENV.mvar, ENV.value) generic_subst list *
    (CFG.node, (ENV.mvar, ENV.value) generic_subst list, 'b list)
    generic_witness list)
   list) *
  CFG.node list ->
  ('a, ENV.mvar) generic_ctl ->
  (CFG.node * (ENV.mvar, ENV.value) generic_subst list *
   (CFG.node, (ENV.mvar, ENV.value) generic_subst list, 'b list)
   generic_witness list)
  list
