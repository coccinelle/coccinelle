
(* ---------------------------------------------------------------------- *)
(* Types                                                                  *)
(* ---------------------------------------------------------------------- *)

(* CTL parameterised on basic predicates and metavar's*)
type ('pred,'mvar,'anno) generic_ctl =
    ('pred,'mvar,'anno) generic_ctl_bis * 'anno
 and ('pred,'mvar,'anno) generic_ctl_bis = 
  | False
  | True
  | Pred of 'pred
  | Not of (('pred,'mvar,'anno) generic_ctl)
  | Exists of 'mvar * (('pred,'mvar,'anno) generic_ctl)
  | And of (('pred,'mvar,'anno) generic_ctl) * 
      (('pred,'mvar,'anno) generic_ctl)
  | Or  of (('pred,'mvar,'anno) generic_ctl) * 
      (('pred,'mvar,'anno) generic_ctl)
  | Implies of (('pred,'mvar,'anno) generic_ctl) * 
      (('pred,'mvar,'anno) generic_ctl)
  | AF of direction *
	(('pred,'mvar,'anno) generic_ctl) * (('pred,'mvar,'anno) generic_ctl)
  | AX of direction * (('pred,'mvar,'anno) generic_ctl)
  | AG of direction * (('pred,'mvar,'anno) generic_ctl)
  | AU of direction *
	(* versions with exists v *)
	(('pred,'mvar,'anno) generic_ctl) * (('pred,'mvar,'anno) generic_ctl) *
	(* versions without exists v *)
	(('pred,'mvar,'anno) generic_ctl) * (('pred,'mvar,'anno) generic_ctl)
  | EF of direction * (('pred,'mvar,'anno) generic_ctl)
  | EX of direction * (('pred,'mvar,'anno) generic_ctl)
  | EG of direction * (('pred,'mvar,'anno) generic_ctl)
  | EU of direction * (('pred,'mvar,'anno) generic_ctl) * 
      (('pred,'mvar,'anno) generic_ctl)
  | Let of string * 
      (('pred,'mvar,'anno) generic_ctl) * 
      (('pred,'mvar,'anno) generic_ctl)
  | Ref of string

and direction = FORWARD (* the normal way *) | BACKWARD (* toward the start *)

let unwrap (ctl,_) = ctl
let rewrap (_,model) ctl = (ctl,model)
let get_line (_,l) = l


(* NOTE: No explicit representation of the bottom subst., i.e., FALSE *)
type ('mvar,'value) generic_subst = 
  | Subst of 'mvar * 'value
  | NegSubst of 'mvar * 'value
type ('mvar,'value) generic_substitution = ('mvar,'value) generic_subst list

type ('state,'subst,'anno) generic_witnesstree =
    Wit of
      'state * 'subst * 'anno * ('state,'subst,'anno) generic_witnesstree list
  | NegWit of
      'state * 'subst * 'anno * ('state,'subst,'anno) generic_witnesstree list

(* ---------------------------------------------------------------------- *)

type 'a modif = Modif of 'a | UnModif of 'a | Control

(* ---------------------------------------------------------------------- *)
