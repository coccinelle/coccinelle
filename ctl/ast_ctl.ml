
(* ---------------------------------------------------------------------- *)
(* Types                                                                  *)
(* ---------------------------------------------------------------------- *)

(* CTL parameterised on basic predicates and metavar's*)
type ('pred,'mvar,'anno) generic_ctl = 
  | False
  | True
  | Pred of 'pred
  | Not of (('pred,'mvar,'anno) generic_ctl * 'anno)
  | Exists of 'mvar * (('pred,'mvar,'anno) generic_ctl * 'anno)
  | And of (('pred,'mvar,'anno) generic_ctl * 'anno) * 
      (('pred,'mvar,'anno) generic_ctl * 'anno)
  | Or  of (('pred,'mvar,'anno) generic_ctl * 'anno) * 
      (('pred,'mvar,'anno) generic_ctl * 'anno)
  | Implies of (('pred,'mvar,'anno) generic_ctl * 'anno) * 
      (('pred,'mvar,'anno) generic_ctl * 'anno)
  | AF of (('pred,'mvar,'anno) generic_ctl * 'anno)
  | AX of (('pred,'mvar,'anno) generic_ctl * 'anno)
  | AG of (('pred,'mvar,'anno) generic_ctl * 'anno)
  | AU of (('pred,'mvar,'anno) generic_ctl * 'anno) * 
      (('pred,'mvar,'anno) generic_ctl * 'anno)
  | EF of (('pred,'mvar,'anno) generic_ctl * 'anno)
  | EX of (('pred,'mvar,'anno) generic_ctl * 'anno)
  | EG of (('pred,'mvar,'anno) generic_ctl * 'anno)
  | EU of (('pred,'mvar,'anno) generic_ctl * 'anno) * 
      (('pred,'mvar,'anno) generic_ctl * 'anno)
  | Let of string * 
      (('pred,'mvar,'anno) generic_ctl * 'anno) * 
      (('pred,'mvar,'anno) generic_ctl * 'anno)
  | Ref of string

(* NOTE: No explicit representation of the bottom subst., i.e., FALSE *)
type ('mvar,'value) generic_subst = 
  | Subst of 'mvar * 'value
  | NegSubst of 'mvar * 'value
type ('mvar,'value) generic_substitution = ('mvar,'value) generic_subst list;;

type ('state,'subst,'anno) generic_witness =
  | Wit of 'state * 'subst * 'anno * ('state,'subst,'anno) generic_witness list
  | NegWit of 'state * 'subst * 'anno * ('state,'subst,'anno)generic_witness list
type ('state,'subst,'anno) generic_witnesstree = 
    ('state,'subst,'anno) generic_witness list;;

(* ---------------------------------------------------------------------- *)

type 'a modif = Modif of 'a | UnModif of 'a | Control

