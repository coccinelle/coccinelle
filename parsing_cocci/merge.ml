(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* given parsed minus code and a stream of + code, figure out where to put
the + code in the mcode of the minus code *)

(* Need to be able to find the nearest inhabited line rather than just
adding 1 or subtracting 1 to the actual line number.  This is an issue for
plus.ml as well.  This problem is dealt with by the logical line field,
which is not incremented for blank lines. *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Step 1: convert minus/context code to an ordered stream of tokens *)

type position =
    Minus of Ast.info * Ast.anything list list ref
  | Context of Ast.info * Ast.anything Ast.befaft ref
  | Bad of Ast.info

let mcode = function
    (_,_,Ast.MINUS(info,plus_stream)) -> [Minus (info,plus_stream)]
  | (_,_,Ast.CONTEXT(info,plus_stream)) -> [Context (info,plus_stream)]
  | _ -> failwith "not possible 1"

let bad_mcode = function
    (_,_,Ast.MINUS(info,plus_stream)) -> Bad(info)
  | (_,_,Ast.CONTEXT(info,plus_stream)) -> Bad(info)
  | _ -> failwith "not possible 2"

let make_bad l =
  List.map
    (function
	Minus(info,plus_stream) -> Bad(info)
      |	Context(info,plus_stream) -> Bad(info)
      |	x -> x)
    l

(* --------------------------------------------------------------------- *)
(* combiner info *)

let bind x y = x @ y
let option_default = []

(* --------------------------------------------------------------------- *)

let get_option f = function
    Some x -> f x
  | None -> option_default

let ident recursor k i = k i (* nothing special to do *)

let expression recursor k e =
  match Ast0.unwrap e with
    Ast0.Edots(dots,whencode) | Ast0.Ecircles(dots,whencode)
  | Ast0.Estars(dots,whencode) ->
      (bad_mcode dots) ::
      (get_option (function x -> make_bad(recursor.V0.combiner_expression x))
	 whencode)
  | _ -> k e

let donothing recursor k ft = k ft

(* needs a case for things to which new code cannot be attached *)
let parameterTypeDef recursor k p =
  match Ast0.unwrap p with
    Ast0.Pdots(dots) -> [bad_mcode dots]
  | Ast0.Pcircles(dots) -> [bad_mcode dots]
  | _ -> k p

let statement recursor k s =
  match Ast0.unwrap s with
    Ast0.Dots(d,whencode) | Ast0.Circles(d,whencode)
  | Ast0.Stars(d,whencode) ->
      (bad_mcode d) ::
      (get_option
	 (function x ->
	   make_bad(recursor.V0.combiner_statement_dots x))
	 whencode)
  | _ -> k s

let top_level recursor k t =
  match Ast0.unwrap t with
    Ast0.FILEINFO(old_file,new_file) ->
      [bad_mcode old_file;bad_mcode new_file]
  | Ast0.ERRORWORDS(exps) ->
      make_bad (List.concat (List.map recursor.V0.combiner_expression exps))
  | _ -> k t

let recursor =
  V0.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing
    ident expression donothing donothing parameterTypeDef donothing
    statement top_level

let rule code = List.concat (List.map recursor.V0.combiner_top_level code)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Step 2: merge the plus stream with the minus/context tokens *)

(* Mcode *)

let get_start l =
  let (_,_,_,start,_) = List.hd (List.hd l) in
  start

let get_finish l =
  let (_,_,_,_,finish) = List.hd (List.rev (List.hd (List.rev l))) in
  finish

let get_real_start l =
  let (_,start,_,_,_) = List.hd (List.hd l) in
  start

let get_real_finish l =
  let (_,_,finish,_,_) = List.hd (List.rev (List.hd (List.rev l))) in
  finish

let get_minus_next_line mline = function
    [] -> mline + 1
  | Bad(info)::xs -> info.Ast.logical_line
  | Minus(info,_)::xs -> info.Ast.logical_line
  | Context(info,_)::xs -> info.Ast.logical_line

let drop_lines l = List.map (List.map (function (x,_,_,_,_) -> x)) l

let rec merge minus_stream plus_stream =
  match (minus_stream,plus_stream) with
    (_,[]) -> ()
  | ([],plus::plus_stream) ->
      failwith
	(Printf.sprintf
	   "minus stream ran out before plus stream\n(plus code begins on line %d)\n"
	   (get_real_start plus))
  | (Bad(info)::minus_stream,plus::plus_stream) ->
      let pfinish = get_finish plus in
      if info.Ast.logical_line > pfinish
      then
	failwith
	  (Printf.sprintf
	     "plus code starting on line %d has no minus or context code to attach to\n"
	     (get_real_start plus))
      else merge minus_stream (plus::plus_stream)
  | (((Minus(info,cell)::minus_stream) as all_minus),plus::plus_stream) ->
      let mline = info.Ast.logical_line in
      let mnext_line = get_minus_next_line mline minus_stream in
      let pstart = get_start plus in
      let pfinish = get_finish plus in
      if pstart < mline && pfinish > mline
      then (cell := (drop_lines plus) @ !cell; merge minus_stream plus_stream)
      else if pfinish + 1 = mline
      then (cell := (drop_lines plus) @ !cell; merge all_minus plus_stream)
      else if not(mline = mnext_line) && (pstart - 1 = mline)
      then (cell := !cell @ (drop_lines plus); merge minus_stream plus_stream)
      else if pfinish < mline
      then
	Printf.printf "failed to merge + code between lines %d and %d"
	  (get_real_start plus) (get_real_finish plus)
      else merge minus_stream (plus::plus_stream)
  | (((Context(info,cell)::minus_stream) as all_minus),plus::plus_stream) ->
      let mline = info.Ast.logical_line in
      let mnext_line = get_minus_next_line mline minus_stream in
      let pstart = get_start plus in
      let pfinish = get_finish plus in
      if pfinish + 1 = mline
      then (cell := Ast.BEFORE (drop_lines plus); merge all_minus plus_stream)
      else if not(mline = mnext_line) && (pstart - 1 = mline)
      then
	begin
	  (match !cell with
	    Ast.BEFORE x -> cell := Ast.BEFOREAFTER (x,drop_lines plus)
	  | _ -> cell := Ast.AFTER (drop_lines plus));
	  merge minus_stream plus_stream
	end
      else if pfinish < mline
      then
	Printf.printf "failed to merge + code between lines %d and %d"
	  (get_real_start plus) (get_real_finish plus)
      else merge minus_stream (plus::plus_stream)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Entry point *)

let do_merge minus plus_stream =
  let minus_tokens = rule minus in
  merge minus_tokens plus_stream
