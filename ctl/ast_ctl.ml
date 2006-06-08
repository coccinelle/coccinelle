(* **********************************************************************
 *
 * Abstract Syntax for CTL-FVex
 *
 * **********************************************************************)

(* -------------------------------------------------------------------- *)
(* Generic Types to be instantiated                                     *)
(* -------------------------------------------------------------------- *)


(* CTL parameterised on basic predicates and metavar's*)
type ('pred,'mvar) generic_ctl = 
  | False
  | True
  | Pred of 'pred * 'mvar modif
  | Not of ('pred,'mvar) generic_ctl
  | Exists of 'mvar * ('pred,'mvar) generic_ctl		(* !!! *)
  | And of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl
  | Or  of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl
  | Implies of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl
  | AF of ('pred,'mvar) generic_ctl
  | AX of ('pred,'mvar) generic_ctl
  | AG of ('pred,'mvar) generic_ctl
  | AU of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl
  | EF of ('pred,'mvar) generic_ctl
  | EX of ('pred,'mvar) generic_ctl
  | EG of ('pred,'mvar) generic_ctl
  | EU of ('pred,'mvar) generic_ctl * ('pred,'mvar) generic_ctl

and 'mvar modif = Modif of 'mvar | UnModif of 'mvar | Control



(* NOTE: No explicit representation of the bottom subst., i.e., FALSE *)
type ('mvar,'value) subst = 
  | Subst of 'mvar * 'value
  | NegSubst of 'mvar * 'value

type ('mvar,'value) generic_substitution = ('mvar,'value) subst list;;

type ('state,'subst,'anno) generic_witness =
  | Wit of 'state * 'subst * 'anno * ('state,'subst,'anno) generic_witness list
  | NegWit of 'state * 'subst * 'anno * ('state,'subst,'anno)generic_witness list




type ('pred, 'mvar, 'valu, 'hole, 'a, 'b) model = 
    (* the control flow *)
    ('a, 'b) Ograph_extended.ograph_extended *                    

    (* the labelling function *)
    ('pred -> (Ograph_extended.nodei * 
               ('mvar ,'valu) generic_substitution * 
               (Ograph_extended.nodei, ('mvar, 'valu) generic_substitution, 'hole list) generic_witness list)
              list) * 

    (* the set of states *)
    Ograph_extended.nodei list                                                    



