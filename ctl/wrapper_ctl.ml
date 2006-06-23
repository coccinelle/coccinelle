(* **********************************************************************
 *
 * Wrapping for FUNCTORS and MODULES
 * 
 *
 * $Id$
 *
 * **********************************************************************)

type ('pred, 'mvar) wrapped_ctl = 
    ('pred * 'mvar Ast_ctl.modif,  'mvar) Ast_ctl.generic_ctl

type ('value, 'pred) wrapped_binding = 
  | ClassicVar of 'value
  | PredVar of 'pred Ast_ctl.modif

type ('pred,'state,'mvar,'value,'wit) labelfunc =
    'pred -> ('state * ('mvar, 'value) Ast_ctl.generic_substitution * 'wit) list

type ('pred,'state,'mvar,'value,'wit) wrapped_labelfunc =
    ('pred * 'mvar Ast_ctl.modif,
     'state,
     'mvar,
     ('value,'pred) wrapped_binding,
     'wit) labelfunc


(* ********************************************************************** *)
(* Module type: PREDICATE                                                 *)
(* ********************************************************************** *)
module type PREDICATE =
sig
  type predicate
end


(* This module must convert the labelling function passed as parameter, by
   using convert_label. Then create a SUBST2 module handling the wrapped_binding.
   Then it can instantiates the generic CTL_ENGINE module. Call sat.
   And then process the witness tree to remove all that is not revelevant for
   the transformation phase.
*)
module CTL_ENGINE_BIS =
  functor (SUB : Ctl_engine.SUBST) ->
    functor (G : Ctl_engine.GRAPH) ->
      functor(P : PREDICATE) ->
struct

  open Ast_ctl

  type predicate = P.predicate

  module WRAPPER_ENV =
  struct
    type mvar = SUB.mvar
    type value = (SUB.value,predicate) wrapped_binding
    let eq_mvar = SUB.eq_mvar
    let eq_val wv1 wv2 = 
      match (wv1,wv2) with
	| (ClassicVar(v1),ClassicVar(v2)) -> SUB.eq_val v1 v2
	| (PredVar(v1),PredVar(v2))       -> v1 = v2   (* ok? *)
	| _                               -> false
    let merge_val wv1 wv2 = 
      match (wv1,wv2) with
	| (ClassicVar(v1),ClassicVar(v2)) -> ClassicVar(SUB.merge_val v1 v2)
	| _                               -> wv1       (* ok? *)
  end
    
  module WRAPPER_ENGINE = Ctl_engine.CTL_ENGINE (WRAPPER_ENV) (G)

  (* Wrap a label function *)
  let (wrap_label: ('pred,'state,'mvar,'value,'wit) labelfunc -> 
	('pred,'state,'mvar,'value,'wit) wrapped_labelfunc)
      = fun oldlabelfunc ->  fun (p, predvar) ->
	let penv = 
	  match predvar with
	    | Ast_ctl.Modif(x)   -> [Ast_ctl.Subst(x,PredVar(Ast_ctl.Modif(p)))]
	    | Ast_ctl.UnModif(x) -> [Ast_ctl.Subst(x,PredVar(Ast_ctl.UnModif(p)))]
	    | Ast_ctl.Control    -> [] in
	let conv_sub sub =
	  match sub with
	    | Ast_ctl.Subst(x,v)    -> Ast_ctl.Subst(x,ClassicVar(v))
	    | Ast_ctl.NegSubst(x,v) -> Ast_ctl.NegSubst(x,ClassicVar(v)) in
	let conv_trip (s,env,wit) = (s,penv @ (List.map conv_sub env),wit) 
	in
          List.map conv_trip (oldlabelfunc p)

(*  let (satbis :
         G.cfg *
         (predicate -> (G.node * (SUB.mvar, SUB.value) Ast_ctl.generic_substitution * 'a list) list) *
         G.node list -> 
        (predicate * SUB.mvar Ast_ctl.modif, SUB.mvar) Ast_ctl.generic_ctl -> 
        (G.node * (SUB.mvar * SUB.value) list  * predicate) list) = 
*)
  let satbis =
    fun ((grp,lab,states) as m) -> fun phi -> 
      let res = WRAPPER_ENGINE.sat (grp,wrap_label lab,states) phi in
	res
end

             
