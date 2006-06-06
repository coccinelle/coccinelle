(* **********************************************************************
 *
 * Abstract Syntax for CTL-FVex
 *
 * $Id$
 *
 * **********************************************************************)

(* -------------------------------------------------------------------- *)
(* Generic Types to be instantiated                                     *)
(* -------------------------------------------------------------------- *)


(* CTL parameterised on basic predicates and metavar's*)
type ('a,'b)generic_ctl = 
    False
  | True
  | Pred of 'a * 'b modif
  | Not of ('a,'b)generic_ctl
  | Exists of 'b * ('a,'b)generic_ctl		(* !!! *)
  | And of ('a,'b)generic_ctl * ('a,'b)generic_ctl
  | Or of ('a,'b)generic_ctl * ('a,'b)generic_ctl
  | Implies of ('a,'b)generic_ctl * ('a,'b)generic_ctl
  | AF of ('a,'b)generic_ctl
  | AX of ('a,'b)generic_ctl
  | AG of ('a,'b)generic_ctl
  | AU of ('a,'b)generic_ctl * ('a,'b)generic_ctl
  | EF of ('a,'b)generic_ctl
  | EX of ('a,'b)generic_ctl
  | EG of ('a,'b)generic_ctl
  | EU of ('a,'b)generic_ctl * ('a,'b)generic_ctl

and 'b modif = Modif of 'b | UnModif of 'b | Control

(* Subst's parameterised on (meta-)var's and result *)
type ('a,'b)generic_substitution = ('a * 'b) list list;;



(* -------------------------------------------------------------------- *)
(* Substitutions and instantiated CTL                                   *)
(* -------------------------------------------------------------------- *)

type metavar = string;;			     (* FIX ME *)

(* Substitutions map metavar's to *)
type substitution = (metavar,string)generic_substitution;; (* FIX ME *)

type ctl = (string,metavar)generic_ctl;;     (* FIX ME *)

