type strict = STRICT | NONSTRICT
type keep_binding = bool
type ('pred, 'mvar, 'anno) generic_ctl =
    False
  | True
  | Pred of 'pred
  | Not of ('pred, 'mvar, 'anno) generic_ctl
  | Exists of keep_binding * 'mvar * ('pred, 'mvar, 'anno) generic_ctl
  | And of strict * ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | AndAny of direction * strict * ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | HackForStmt of direction * strict * ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | Or of ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | Implies of ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | AF of direction * strict * ('pred, 'mvar, 'anno) generic_ctl
  | AX of direction * strict * ('pred, 'mvar, 'anno) generic_ctl
  | AG of direction * strict * ('pred, 'mvar, 'anno) generic_ctl
  | AW of direction * strict * ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | AU of direction * strict * ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | EF of direction * ('pred, 'mvar, 'anno) generic_ctl
  | EX of direction * ('pred, 'mvar, 'anno) generic_ctl
  | EG of direction * ('pred, 'mvar, 'anno) generic_ctl
  | EU of direction * ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | Let of string * ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | LetR of direction * string * ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | Ref of string
  | SeqOr of ('pred, 'mvar, 'anno) generic_ctl *
      ('pred, 'mvar, 'anno) generic_ctl
  | Uncheck of ('pred, 'mvar, 'anno) generic_ctl
  | InnerAnd of ('pred, 'mvar, 'anno) generic_ctl
  | XX of ('pred, 'mvar, 'anno) generic_ctl
and direction = FORWARD | BACKWARD
val unwrap : 'a * 'b -> 'a
val rewrap : 'a * 'b -> 'c -> 'c * 'b
val get_line : 'a * 'b -> 'b
type ('mvar, 'value) generic_subst =
    Subst of 'mvar * 'value
  | NegSubst of 'mvar * 'value
type ('mvar, 'value) generic_substitution =
    ('mvar, 'value) generic_subst list
type ('state, 'subst, 'anno) generic_witnesstree =
    Wit of 'state * 'subst * 'anno *
      ('state, 'subst, 'anno) generic_witnesstree list
  | NegWit of ('state, 'subst, 'anno) generic_witnesstree
type 'a modif = Modif of 'a | UnModif of 'a | Control
