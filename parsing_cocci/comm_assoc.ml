(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
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


(* searches for E op ..., for any commutative and associative binary
operator.  When this satisfies the isomorphism conditions (ie all minus, or
context for the op and ...), then this is converted to Nested(E,op).
Nested is not used before this phase. *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0

let comm_assoc =
  [Ast.Arith(Ast.Plus);Ast.Arith(Ast.Mul);Ast.Arith(Ast.And);Ast.Arith(Ast.Or);
    Ast.Logical(Ast.AndLog);Ast.Logical(Ast.OrLog)]

let is_minus e =
  match Ast0.get_mcodekind e with Ast0.MINUS(cell) -> true | _ -> false

let is_context e =
  !Flag.sgrep_mode2 or (* everything is context for sgrep *)
  (match Ast0.get_mcodekind e with
    Ast0.CONTEXT(cell) -> true
  | _ -> false)

let nopos mc =
  match Ast0.get_pos mc with Ast0.MetaPos _ -> false | Ast0.NoMetaPos -> true

let process_binops rule_name =
  let donothing r k e = k e in
  let mcode x = x in
  let expr r k e1 =
    let e = k e1 in
    match Ast0.unwrap e with
      Ast0.Binary(left,op,right)
      when List.mem (Ast0.unwrap_mcode op) comm_assoc ->
	(match Ast0.unwrap right with
	  Ast0.Edots(d,None) ->
	    if (is_minus e || (is_context e && is_context right))
		&& nopos op && nopos d
	    (* keep dots to record required modif *)
	    then Ast0.rewrap e (Ast0.Nested(left,op,right))
	    else
	      (Printf.printf
		 "%s: position variables or mixed modifs interfere with comm_assoc iso" rule_name;
	       Unparse_ast0.expression e1;
	       Format.print_newline();
	       e)
	| Ast0.Edots(d,_) -> 
	    (Printf.printf
	       "%s: whencode interferes with comm_assoc iso" rule_name;
	     Unparse_ast0.expression e1;
	     Format.print_newline();
	     e)
	| _ -> e)
    | _ -> e in
  V0.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode
    donothing donothing donothing donothing donothing donothing
    donothing expr donothing donothing donothing donothing donothing
    donothing donothing

let comm_assoc rule rule_name dropped_isos =
  if List.mem "comm_assoc" dropped_isos
  then rule
  else List.map (process_binops rule_name).V0.rebuilder_top_level rule
