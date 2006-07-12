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
    'pred -> ('state * ('mvar, 'value) Ast_ctl.generic_substitution) list

type ('pred,'state,'mvar,'value,'wit) wrapped_labelfunc =
    ('pred * 'mvar Ast_ctl.modif) -> 
      ('state * 
       ('mvar, ('value,'pred) wrapped_binding) Ast_ctl.generic_substitution *
       'wit)
	list




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
  module WRAPPER_ENGINE = Ctl_engine.CTL_ENGINE (WRAPPER_ENV) (G) (WRAPPER_PRED)

  (* Wrap a label function *)
  let (wrap_label: ('pred,'state,'mvar,'value) labelfunc -> 
	('pred,'state,'mvar,'value,'wit) wrapped_labelfunc)
      = fun oldlabelfunc ->  fun (p, predvar) ->
	let penv = 
	  match predvar with
	    | A.Modif(x)   -> [A.Subst(x,PredVal(A.Modif(p)))]
	    | A.UnModif(x) -> [A.Subst(x,PredVal(A.UnModif(p)))]
	    | A.Control    -> [] in
	let conv_sub sub =
	  match sub with
	    | A.Subst(x,v)    -> A.Subst(x,ClassicVal(v))
	    | A.NegSubst(x,v) -> A.NegSubst(x,ClassicVal(v)) in
	let conv_trip (s,env) = (s,penv @ (List.map conv_sub env),A.TopWit) in
        List.map conv_trip (oldlabelfunc p)

  (* Collects, unwraps, and filters witness trees *)
  (* NOTE: only makes sense specifically for coccinelle generated CTL *)
  (* FIX ME: what about negative witnesses and negative substitutions *)

  let rec dnf = function
    A.AndWits(c1,c2) ->
      let c1res = dnf c1 in
      let c2res = dnf c2 in
      List.fold_left
	(function rest ->
	  (function cur1 ->
	    List.fold_left
	      (function rest ->
		(function cur2 ->
		  let x = A.AndWits(cur1,cur2) in
		  if List.mem x rest then rest else x::rest))
	      rest c2res))
	[] c1res
  | A.OrWits(c1,c2) -> Common.union_set (dnf c1) (dnf c2)
  | A.Wit(st,th,anno,wit) ->
      List.map (function wit -> A.Wit(st,th,anno,wit)) (dnf wit)
  | A.NegWit(wit) -> (* already pushed in as far as possible *)
      List.map (function wit -> A.NegWit wit) (dnf wit)
  | A.TopWit -> [A.TopWit]

  exception NEGATIVE_WITNESS
  let unwrap_wits wit =
    let mkth th =
      Common.map_filter
	(function A.Subst(x,ClassicVal(v)) -> Some (x,v) | _ -> None)
	th in
    let wits = dnf wit in
    let rec no_negwits = function
	A.AndWits(c1,c2) | A.OrWits(c1,c2) -> no_negwits c1 && no_negwits c2
      | A.Wit(st,th,anno,wit) -> no_negwits wit
      | A.NegWit(_) -> false
      | A.TopWit -> true in
    let rec loop neg acc = function
	A.AndWits(c1,c2) -> (loop neg acc c1) @ (loop neg acc c2)
      | A.OrWits(_,_) -> raise (NEVER_CTL "or is not possible")
      | A.Wit(st,[A.Subst(x,PredVal(A.Modif(v)))],anno,wit) ->
	  (match wit with
	    A.TopWit -> [(st,acc,v)]
	  | _ -> raise (NEVER_CTL "predvar tree should have no children"))
      | A.Wit(st,th,anno,wit) -> loop neg ((mkth th) @ acc) wit
      | A.NegWit(wit) ->
	  if no_negwits wit
	  then raise NEGATIVE_WITNESS
	  else raise (TODO_CTL "nested negative witnesses")
      | A.TopWit -> [] in
    List.concat
      (List.map
	 (function wit -> try loop false [] wit with NEGATIVE_WITNESS -> [])
	 wits)
  ;;

	  
  (* ------------------ Partial matches ------------------ *)
  (* Limitation: this only gives information about terms with PredVals, which
     can be optimized to only those with modifs *)
  let collect_predvar_bindings res =
    let wits = List.map (fun (_,_,w) -> w) res in
    let rec loop = function
	A.AndWits(c1,c2) | A.OrWits(c1,c2) -> (loop c1) @ (loop c2)
      | A.Wit(st,th,anno,wit) ->
	  (Common.map_filter
	    (function A.Subst(_,(PredVal(_) as x)) -> Some (st,x) | _ -> None)
	    th) @
	  (loop wit)
      | A.NegWit(wit) -> loop wit
      | A.TopWit -> [] in
    List.fold_left Common.union_set [] (List.map loop wits)

  let check_conjunction phipsi res_phi res_psi res_phipsi =
    let phi_code = collect_predvar_bindings res_phi in
    let psi_code = collect_predvar_bindings res_psi in
    let all_code = collect_predvar_bindings res_phipsi in
    let check str = function
	[] -> ()
      |	l ->
	  Printf.printf "Warning: The conjunction derived from SP line %d:\n"
	    (Ast_ctl.get_line phipsi);
	  Printf.printf
	    "drops code matched on the %s side at the following nodes\naccording to the corresponding predicates\n" str;
	  List.iter
	    (function (n,x) ->
	      G.print_node n; Format.print_flush(); Printf.printf ": ";
	      WRAPPER_ENV.print_value x; Format.print_flush();
	      Printf.printf "\n")
	    l in
    check "left" (Common.minus_set phi_code all_code);
    check "right" (Common.minus_set psi_code all_code)

  (* ----------------------------------------------------- *)

  (* The wrapper for sat from the CTL_ENGINE *)
  let satbis_noclean (grp,lab,states) phi :
      ('pred,'anno) WRAPPER_ENGINE.triples =
    WRAPPER_ENGINE.sat (grp,wrap_label lab,states) phi check_conjunction
      
  (* Returns the "cleaned up" result from satbis_noclean *)
  let (satbis :
         G.cfg *
	 (predicate,G.node,SUB.mvar,SUB.value) labelfunc *
         G.node list -> 
	(predicate,SUB.mvar) wrapped_ctl ->
        (G.node * (SUB.mvar * SUB.value) list * predicate) list) = 
    fun m phi ->
      let noclean = (satbis_noclean m phi) in
      flush stdout;
	Common.uniq (
	  List.concat (List.map (fun (_,_,w) -> unwrap_wits w) noclean))

(* END OF MODULE: CTL_ENGINE_BIS *)
end
