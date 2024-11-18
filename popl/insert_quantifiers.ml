(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

module Ast = Ast_cocci
module Past = Ast_popl

(* --------------------------------------------------------------------- *)

let rec fvs_sequence = function
    Past.Seq(elem,seq) ->
      Common.union_set (fvs_element elem) (fvs_sequence seq)
  | Past.Empty -> []
  | Past.SExists(var,seq) -> failwith "not possible"

and fvs_element = function
    Past.Term(term) -> Ast.get_fvs term
  | Past.Or(seq1,seq2) ->
      Common.union_set (fvs_sequence seq1) (fvs_sequence seq2)
  | Past.DInfo(dots,seq_bef,seq_aft) ->
      List.fold_left
	(function prev ->
	  function cur ->
	    Common.union_set (fvs_element cur) prev)
      (fvs_dots dots) seq_bef
  | Past.EExists(var,seq) -> failwith "not possible"

and fvs_dots = function
    Past.Dots -> []
  | Past.Nest(seq) -> fvs_sequence seq
  | Past.When(dots,seq) -> Common.union_set (fvs_dots dots) (fvs_sequence seq)
  | Past.DExists(var,dots) -> failwith "not possible"

(* --------------------------------------------------------------------- *)

let rec quant_sequence bound = function
    Past.Seq(elem,seq) ->
      let fe = fvs_element elem in
      let fs = fvs_sequence seq in
      let inter = Common.inter_set fe fs in
      let free = Common.minus_set inter bound in
      let new_bound = free @ bound in
      List.fold_right (function cur -> function rest -> Past.SExists(cur,rest))
	free (Past.Seq(quant_element new_bound elem,
		       quant_sequence new_bound seq))
  | Past.Empty -> Past.Empty
  | Past.SExists(var,seq) -> failwith "not possible"

and quant_element bound = function
    Past.Term(term) as x ->
      let free = Common.minus_set (fvs_element x) bound in
      List.fold_right (function cur -> function rest -> Past.EExists(cur,rest))
	free x
  | Past.Or(seq1,seq2) ->
      Past.Or(quant_sequence bound seq1,quant_sequence bound seq2)
  | Past.DInfo(dots,seq_bef,seq_aft) ->
      Past.DInfo(quant_dots bound dots,seq_bef,
		 List.map (quant_element bound) seq_aft)
  | Past.EExists(var,seq) -> failwith "not possible"

and quant_dots bound = function
    Past.Dots -> Past.Dots
  | Past.Nest(seq) -> Past.Nest(quant_sequence bound seq)
  | Past.When(dots,seq) ->
      let fd = fvs_dots dots in
      let fs = fvs_sequence seq in
      let inter = Common.inter_set fd fs in
      let free = Common.minus_set inter bound in
      let new_bound = free @ bound in
      List.fold_right (function cur -> function rest -> Past.DExists(cur,rest))
	free (Past.When(quant_dots new_bound dots,
			quant_sequence new_bound seq))
  | Past.DExists(var,dots) -> failwith "not possible"

(* --------------------------------------------------------------------- *)

let insert_quantifiers x = quant_sequence [] x
