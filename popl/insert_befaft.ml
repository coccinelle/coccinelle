(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

module Past = Ast_popl

(* --------------------------------------------------------------------- *)

let rec get_before a = function
    Past.Seq(elem,seq) ->
      let (elem,ea) = get_before_element a elem in
      let (seq,sla) = get_before ea seq in
      (Past.Seq(elem,seq),sla)
  | Past.Empty -> (Past.Empty,a)
  | Past.SExists(var,seq) -> failwith "not possible"

and get_before_element a = function
    Past.Term(term) as s -> (s,[s])
  | Past.Or(seq1,seq2) ->
      let (seq1,seq1a) = get_before a seq1 in
      let (seq2,seq2a) = get_before a seq2 in
      (Past.Or(seq1,seq2),Common.union_set seq1a seq2a)
  | Past.DInfo(dots,_,seq_aft) ->
      let dots = get_before_dots a dots in
      (Past.DInfo(dots,a,seq_aft),a)
  | Past.EExists(var,seq) -> failwith "not possible"

and get_before_dots a = function
    Past.Dots -> Past.Dots
  | Past.Nest(seq) ->
      let (seq,_) = get_before a seq in
      Past.Nest(seq)
  | Past.When(dots,seq) ->
      let dots = get_before_dots a dots in
      let (seq,_) = get_before [] seq in
      Past.When(dots,seq)
  | Past.DExists(var,dots) -> failwith "not possible"

(* --------------------------------------------------------------------- *)

let rec get_after a = function
    Past.Seq(elem,seq) ->
      let (seq,sla) = get_after a seq in
      let (elem,ea) = get_after_element sla elem in
      (Past.Seq(elem,seq),ea)
  | Past.Empty -> (Past.Empty,a)
  | Past.SExists(var,seq) -> failwith "not possible"

and get_after_element a = function
    Past.Term(term) as s -> (s,[s])
  | Past.Or(seq1,seq2) ->
      let (seq1,seq1a) = get_after a seq1 in
      let (seq2,seq2a) = get_after a seq2 in
      (Past.Or(seq1,seq2),Common.union_set seq1a seq2a)
  | Past.DInfo(dots,seq_bef,_) ->
      let dots = get_after_dots a dots in
      (Past.DInfo(dots,seq_bef,a),a)
  | Past.EExists(var,seq) -> failwith "not possible"

and get_after_dots a = function
    Past.Dots -> Past.Dots
  | Past.Nest(seq) ->
      let (seq,_) = get_after (Common.union_set (get_first [] seq) a) seq in
      Past.Nest(seq)
  | Past.When(dots,seq) ->
      let dots = get_after_dots a dots in
      let (seq,_) = get_after [] seq in
      Past.When(dots,seq)
  | Past.DExists(var,dots) -> failwith "not possible"

(* --------------------------------------------------------------------- *)
(* like get_after, but just returns the a component; doesn't modify the term *)

and get_first a = function
    Past.Seq(elem,seq) ->
      let sla = get_first a seq in
      let ea  = get_first_element sla elem in
      ea
  | Past.Empty -> a
  | Past.SExists(var,seq) -> failwith "not possible"

and get_first_element a = function
    Past.Term(term) as s -> [s]
  | Past.Or(seq1,seq2) ->
      Common.union_set (get_first a seq1) (get_first a seq2)
  | Past.DInfo(dots,_,_) -> a
  | Past.EExists(var,seq) -> failwith "not possible"

(* --------------------------------------------------------------------- *)
(* Entry point *)

let insert_befaft sl =
  let (sl,_) = get_before [] sl in
  let (sl,_) = get_after [] sl in
  sl
