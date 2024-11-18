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

and fvs_term = function
    Past.Atomic(term) -> Ast.get_fvs term
  | Past.IfThen(test,thn,(afvs,_,_,_)) ->
      Common.union_set afvs
	(Common.union_set (fvs_term test) (fvs_term thn))
  | Past.TExists(var,term) -> failwith "not possible"

and fvs_element = function
    Past.Term(term,_) -> fvs_term term
  | Past.Or(seq1,seq2) ->
      Common.union_set (fvs_sequence seq1) (fvs_sequence seq2)
  | Past.DInfo(dots) -> fvs_dots dots
  | Past.EExists(var,seq) -> failwith "not possible"

and fvs_dots = function
    Past.Dots -> []
  | Past.Nest(seq) -> fvs_sequence seq
  | Past.When(dots,seq) -> Common.union_set (fvs_dots dots) (fvs_sequence seq)

(* --------------------------------------------------------------------- *)

let inter_set l1 l2 = List.filter (function l1e -> List.mem l1e l2) l1

let minus_set l1 l2 = List.filter (function l1e -> not (List.mem l1e l2)) l1

let rec quant_sequence bound = function
    Past.Seq(elem,seq) ->
      let fe = fvs_element elem in
      let fs = fvs_sequence seq in
      let inter = inter_set fe fs in
      let free = minus_set inter bound in
      let new_bound = free @ bound in
      List.fold_right (function cur -> function rest -> Past.SExists(cur,rest))
	free (Past.Seq(quant_element new_bound elem,
		       quant_sequence new_bound seq))
  | Past.Empty -> Past.Empty
  | Past.SExists(var,seq) -> failwith "not possible"

and quant_term bound = function
    (Past.Atomic(term)) as x ->
      let free = minus_set (Ast.get_fvs term) bound in
      List.fold_right (function cur -> function rest -> Past.TExists(cur,rest))
	free x
  | Past.IfThen(test,thn,((afvs,_,_,_) as aft)) ->
      let fts = fvs_term test in
      let fth = fvs_term thn in
      let inter = inter_set fts fth in
      let free = minus_set inter bound in
      let new_bound = free @ bound in
      List.fold_right (function cur -> function rest -> Past.TExists(cur,rest))
	free (Past.IfThen(quant_term new_bound test,
			  quant_term new_bound thn,
			  aft))
  | Past.TExists(var,term) -> failwith "not possible"

and quant_element bound = function
    Past.Term(term,ba) ->
      Past.Term(quant_term bound term,dots_bef_aft bound ba)
  | Past.Or(seq1,seq2) ->
      Past.Or(quant_sequence bound seq1,quant_sequence bound seq2)
  | Past.DInfo(dots) ->
      Past.DInfo(quant_dots bound dots)
  | Past.EExists(var,seq) -> failwith "not possible"

and dots_bef_aft bound = function
    Past.AddingBetweenDots (brace_term,n) ->
      Past.AddingBetweenDots (quant_term bound brace_term,n)
  | Past.DroppingBetweenDots (brace_term,n) ->
      Past.DroppingBetweenDots (quant_term bound brace_term,n)
  | Past.NoDots -> Past.NoDots

and quant_dots bound = function
    Past.Dots -> Past.Dots
  | Past.Nest(seq) -> Past.Nest(quant_sequence bound seq)
  | Past.When(dots,seq) ->
      Past.When(quant_dots bound dots, quant_sequence bound seq)

(* --------------------------------------------------------------------- *)

let insert_quantifiers x = quant_sequence [] x
