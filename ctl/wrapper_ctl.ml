(*
 * Copyright 2012, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./wrapper_ctl.ml"
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
  | ClassicVal of 'value
  | PredVal of 'pred Ast_ctl.modif

type ('pred,'state,'mvar,'value) labelfunc =
    'pred ->
      ('state * ('pred * ('mvar, 'value) Ast_ctl.generic_substitution)) list

(* pad: what is 'wit ? *)
type ('pred,'state,'mvar,'value,'wit) wrapped_labelfunc =
  ('pred * 'mvar Ast_ctl.modif) ->
      ('state *
       ('mvar,('value,'pred) wrapped_binding) Ast_ctl.generic_substitution *
       'wit
      ) list

(* ********************************************************************** *)
(* Module type: CTL_ENGINE_BIS (wrapper for CTL_ENGINE)                   *)
(* ********************************************************************** *)

(* This module must convert the labelling function passed as parameter, by
   using convert_label. Then create a SUBST2 module handling the
   wrapped_binding.  Then it can instantiates the generic CTL_ENGINE
   module. Call sat.  And then process the witness tree to remove all that
   is not revelevant for the transformation phase.
*)

module CTL_ENGINE_BIS =
  functor (SUB : Ctl_engine.SUBST) ->
    functor (G : Ctl_engine.GRAPH) ->
      functor(P : Ctl_engine.PREDICATE) ->
struct

  exception TODO_CTL of string  (* implementation still not quite done so... *)
  exception NEVER_CTL of string		  (* Some things should never happen *)

  module A = Ast_ctl

  type predicate = P.t
  module WRAPPER_ENV =
  struct
    type mvar = SUB.mvar
    type value = (SUB.value,predicate) wrapped_binding
    let eq_mvar = SUB.eq_mvar
    let eq_val wv1 wv2 =
      match (wv1,wv2) with
	| (ClassicVal(v1),ClassicVal(v2)) -> SUB.eq_val v1 v2
	| (PredVal(v1),PredVal(v2))       -> v1 = v2   (* FIX ME: ok? *)
	| _                               -> false
    let merge_val wv1 wv2 =
      match (wv1,wv2) with
	| (ClassicVal(v1),ClassicVal(v2)) -> ClassicVal(SUB.merge_val v1 v2)
	| _                               -> wv1       (* FIX ME: ok? *)


    let print_mvar x = SUB.print_mvar x
    let print_value x =
      match x with
	ClassicVal v -> SUB.print_value v
      | PredVal(A.Modif v) -> P.print_predicate v
      | PredVal(A.UnModif v) -> P.print_predicate v
      |	PredVal(A.Control) -> Format.print_string "no value"
  end

  module WRAPPER_PRED =
    struct
      type t = P.t * SUB.mvar Ast_ctl.modif
      let print_predicate (pred, modif) =
        begin
          P.print_predicate pred;
	  (match modif with
	    Ast_ctl.Modif x | Ast_ctl.UnModif x ->
	      Format.print_string " with <modifTODO>"
	  | Ast_ctl.Control -> ())
        end
    end

  (* Instantiate a wrapped version of CTL_ENGINE *)
  module WRAPPER_ENGINE =
    Ctl_engine.CTL_ENGINE (WRAPPER_ENV) (G) (WRAPPER_PRED)

  (* Wrap a label function *)
  let (wrap_label: ('pred,'state,'mvar,'value) labelfunc ->
	('pred,'state,'mvar,'value,'wit) wrapped_labelfunc) =
    fun oldlabelfunc ->
      fun (p, predvar) ->

	let penv p'  =
	  match predvar with
	  | A.Modif(x)   -> [A.Subst(x,PredVal(A.Modif(p')))]
	  | A.UnModif(x) -> [A.Subst(x,PredVal(A.UnModif(p')))]
	  | A.Control    -> [] in

	let conv_sub sub =
	  match sub with
	  | A.Subst(x,v)    -> A.Subst(x,ClassicVal(v))
	  | A.NegSubst(x,v) -> A.NegSubst(x,ClassicVal(v)) in

	let conv_trip (s,(p',env)) =
          (s,penv p' @ (List.map conv_sub env),[](*pad: ?*))
        in
        List.map conv_trip (oldlabelfunc p)

  (* ---------------------------------------------------------------- *)

  (* FIX ME: what about negative witnesses and negative substitutions *)
  let unwrap_wits modifonly wits =
    let mkth th =
      Common.map_filter
	(function A.Subst(x,ClassicVal(v)) -> Some (x,v) | _ -> None)
	th in
    let rec loop neg acc = function
	A.Wit(st,[A.Subst(x,PredVal(A.Modif(v)))],anno,wit) ->
	  (match wit with
	    [] -> [(st,acc,v)]
	  | _ -> raise (NEVER_CTL "predvar tree should have no children"))
      | A.Wit(st,[A.Subst(x,PredVal(A.UnModif(v)))],anno,wit)
	when not modifonly or !Flag.track_iso_usage ->
	  (match wit with
	    [] -> [(st,acc,v)]
	  | _ -> raise (NEVER_CTL "predvar tree should have no children"))
      | A.Wit(st,th,anno,wit) ->
	  List.concat (List.map (loop neg ((mkth th) @ acc)) wit)
      | A.NegWit(_) -> [] (* why not failure? *) in
    List.concat (List.map (function wit -> loop false [] wit) wits)
  ;;

(*
  (* a match can return many trees, but within each tree, there has to be
     at most one value for each variable that is in the used_after list *)
  let collect_used_after used_after envs =
    let print_var var = SUB.print_mvar var; Format.print_flush() in
    List.concat
      (List.map
	 (function used_after_var ->
	   let vl =
	     List.fold_left
	       (function rest ->
		 function env ->
		   try
		     let vl = List.assoc used_after_var env in
		     match rest with
		       None -> Some vl
		     | Some old_vl when SUB.eq_val vl old_vl -> rest
		     | Some old_vl -> print_var used_after_var;
			 Format.print_newline();
			 SUB.print_value old_vl;
			 Format.print_newline();
			 SUB.print_value vl;
			 Format.print_newline();
			 failwith "incompatible values"
		   with Not_found -> rest)
	       None envs in
	   match vl with
	     None -> []
	   | Some vl -> [(used_after_var, vl)])
	 used_after)
*)

  (* a match can return many trees, but within each tree, there has to be
     at most one value for each variable that is in the used_after list *)
  (* actually, this should always be the case, because these variables
  should be quantified at the top level.  so the more complicated
  definition above should not be needed. *)
  let collect_used_after used_after envs =
    List.concat
      (List.map
	 (function used_after_var ->
	   let vl =
	     List.fold_left
	       (function rest ->
		 function env ->
		   try
		     let vl = List.assoc used_after_var env in
		     if List.exists (function x -> SUB.eq_val x vl) rest
		     then rest
		     else vl::rest
		   with Not_found -> rest)
	       [] envs in
	   List.map (function x -> (used_after_var, x)) vl)
	 used_after)

  (* ----------------------------------------------------- *)

  (* The wrapper for sat from the CTL_ENGINE *)
  let satbis_noclean (grp,lab,states) (phi,reqopt) :
      ('pred,'anno) WRAPPER_ENGINE.triples =
    WRAPPER_ENGINE.sat (grp,wrap_label lab,states) phi reqopt

  (* Returns the "cleaned up" result from satbis_noclean *)
  let (satbis :
         G.cfg *
	 (predicate,G.node,SUB.mvar,SUB.value) labelfunc *
         G.node list ->
	   ((predicate,SUB.mvar) wrapped_ctl *
	      (WRAPPER_PRED.t list list)) ->
	     (WRAPPER_ENV.mvar list * (SUB.mvar * SUB.value) list) ->
	       ((WRAPPER_PRED.t, 'a) WRAPPER_ENGINE.triples *
		  ((G.node * (SUB.mvar * SUB.value) list * predicate)
		     list list *
		     bool *
		     (WRAPPER_ENV.mvar * SUB.value) list list))) =
    fun m phi (used_after, binding) ->
      let noclean = satbis_noclean m phi in
      let witness_trees = List.map (fun (_,_,w) -> w) noclean in
      let res = List.map (unwrap_wits true) witness_trees in
      let new_bindings =
	List.map
	  (function bindings_per_witness_tree ->
	    (List.map (function (_,env,_) -> env) bindings_per_witness_tree))
	  (List.map (unwrap_wits false) witness_trees) in
      (noclean,
       (res,not(noclean = []),
	   (* throw in the old binding.  By construction it doesn't conflict
           with any of the new things, and it is useful if there are no new
	   things. *)
	(List.map (collect_used_after used_after) new_bindings)))

let print_bench _ = WRAPPER_ENGINE.print_bench()

(* END OF MODULE: CTL_ENGINE_BIS *)
end
