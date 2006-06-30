(* **********************************************************************
 *
 * Wrapping for FUNCTORS and MODULES
 * 
 *
 * $Id$
 *
 * **********************************************************************)

type info = int

type ('pred, 'mvar) wrapped_ctl = 
    ('pred * 'mvar Ast_ctl.modif,  'mvar, info) Ast_ctl.generic_ctl

type ('value, 'pred) wrapped_binding = 
  | ClassicVar of 'value
  | PredVar of 'pred Ast_ctl.modif

type ('pred,'state,'mvar,'value) labelfunc =
    'pred -> ('state * ('mvar, 'value) Ast_ctl.generic_substitution) list

type ('pred,'state,'mvar,'value,'wit) wrapped_labelfunc =
    ('pred * 'mvar Ast_ctl.modif) -> 
      ('state * 
	 ('mvar, ('value,'pred) wrapped_binding) Ast_ctl.generic_substitution
	 * 'wit) list


(* ********************************************************************** *)
(* Module: PREDICATE (predicates for CTL formulae)                        *)
(* ********************************************************************** *)

module type PREDICATE =
sig
  type predicate
end



(* ********************************************************************** *)
(* Module type: CTL_ENGINE_BIS (wrapper for CTL_ENGINE)                   *)
(* ********************************************************************** *)

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
	| (PredVar(v1),PredVar(v2))       -> v1 = v2   (* FIX ME: ok? *)
	| _                               -> false
    let merge_val wv1 wv2 = 
      match (wv1,wv2) with
	| (ClassicVar(v1),ClassicVar(v2)) -> ClassicVar(SUB.merge_val v1 v2)
	| _                               -> wv1       (* FIX ME: ok? *)


    let print_mvar x = SUB.print_mvar x
    let print_value x = 
      match x with
      | ClassicVar v -> SUB.print_value v
      | PredVar v -> Format.print_string "<predvar>"
  end

  (* Instantiate a wrapped version of CTL_ENGINE *)
  module WRAPPER_ENGINE = Ctl_engine.CTL_ENGINE (WRAPPER_ENV) (G)

  (* Wrap a label function *)
  let (wrap_label: ('pred,'state,'mvar,'value) labelfunc -> 
	('pred,'state,'mvar,'value,'wit) wrapped_labelfunc)
      = fun oldlabelfunc ->  fun (p, predvar) ->
	let top_wit = [] in
	let penv = 
	  match predvar with
	    | Modif(x)   -> [Subst(x,PredVar(Modif(p)))]
	    | UnModif(x) -> [Subst(x,PredVar(UnModif(p)))]
	    | Control    -> [] in
	let conv_sub sub =
	  match sub with
	    | Subst(x,v)    -> Subst(x,ClassicVar(v))
	    | NegSubst(x,v) -> NegSubst(x,ClassicVar(v)) in
	let conv_trip (s,env) = (s,penv @ (List.map conv_sub env),top_wit) 
	in
          List.map conv_trip (oldlabelfunc p)

  (* Collects, unwraps, and filters witness trees *)
  (* NOTE: only makes sense specifically for coccinelle generated CTL *)
  (* FIX ME: what about negative witnesses and negative substitutions *)
  exception NEGATIVE_WITNESS
  let unwrap_wits acc wits =
    let mkth th =
      List.concat (
	List.map (function Subst(x,ClassicVar(v)) -> [(x,v)] | _ -> []) th) in
    let rec loop neg acc wits =
      match wits with
	| []  -> []
(*
	| (Wit(s,[Subst(x,ClassicVar(v))],anno,wits')::rest) -> 
	    (loop neg ((x,v)::acc) wits') @ (loop neg acc rest)
*)
	| (Wit(s,[Subst(x,PredVar(Modif(v)))],anno,wits')::rest) -> 
	    (s,acc,v) :: (loop neg acc rest)
(*
	| (Wit(s,[sub],anno,wits')::rest) ->
	    (loop neg acc wits') @ (loop neg acc rest)
	| (Wit(s,[],anno,wits')::rest) -> 
	    (loop neg acc wits') @ (loop neg acc rest)
*)
	| (Wit(s,th,anno,wits')::rest) ->
	      (loop neg ((mkth th) @ acc) wits') @ (loop neg acc rest)
	| (NegWit(s,th,anno,wits')::rest) -> 
	    if neg then
	      (loop (not neg) ((mkth th) @ acc) wits') @ (loop neg acc rest)
	    else
	      raise NEGATIVE_WITNESS
    in
      try (loop false acc wits) with NEGATIVE_WITNESS -> []
  ;;

  (* The wrapper for sat from the CTL_ENGINE *)
  let satbis_noclean (grp,lab,states) phi =
    WRAPPER_ENGINE.sat (grp,wrap_label lab,states) phi
      
  (* Returns the "cleaned up" result from satbis_noclean *)
  let (satbis :
         G.cfg *
	 (predicate,G.node,SUB.mvar,SUB.value) labelfunc *
         G.node list -> 
	(predicate,SUB.mvar) wrapped_ctl ->
        (G.node * (SUB.mvar * SUB.value) list * predicate) list) = 
    fun m phi ->
      let noclean = (satbis_noclean m phi) in
	Common.uniq (
	  List.concat (List.map (fun (_,_,w) -> unwrap_wits [] w) noclean))
	  
  (* ------------------ Partial matches ------------------ *)
  let collect_predvar_bindings res =
    let wits = List.map (fun (_,_,w) -> w) res in
    let rec loop wits =
      List.fold_left Common.union_set []
	(List.map
	   (function
	       Wit(s,th,_,wits) | NegWit(s,th,_,wits) ->
		 Common.union_set
		   (List.fold_left
		      (function rest ->
			function
			    Subst(x,PredVar(_)) -> x :: rest
			  | NegSubst(x,PredVar(_)) ->
			      failwith "unexpected negsubst on predvar"
			  | _ -> rest)
		      [] th)
		   (loop wits))
	   wits) in
    loop wits

  let check_conjunction phipsi res_phi res_psi res_phipsi =
    let phi_code = collect_predvar_bindings res_phi in
    let psi_code = collect_predvar_bindings res_psi in
    let all_code = collect_predvar_bindings res_phipsi in
    let check str = function
	[] -> ()
      |	l ->
	  Printf.printf "Warning: conjunction derived from SP line %d drops code on the\nfollowing lines, which was matched by the %s side of the conjunction:\n"
	    (Ast_ctl.get_line phipsi) str;
	  List.iter
	    (function x -> SUB.print_value x; Format.print_flush())
	    l in
    check "left" (Common.minus_set all_code phi_code);
    check "right" (Common.minus_set all_code psi_code)

(* END OF MODULE: CTL_ENGINE_BIS *)
end
