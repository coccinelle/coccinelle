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


open Ograph_extended

(*****************************************************************************)
(* the different ctl formula related types *)
(*****************************************************************************)
type mvar = Ast_cocci.meta_name

type predicate =
    InLoop | TrueBranch | FalseBranch
  | After (* pointer to the code after an if or while *)
  | FallThrough
  | Return (* any exit from the current function *)
  | FunHeader | Top | Exit | ErrorExit | Goto
  | Paren of Ast_cocci.meta_name
  | Match of Ast_cocci.rule_elem
  | Label of Ast_cocci.meta_name
  | BCLabel of Ast_cocci.meta_name (* parent of break or continue *)
  | PrefixLabel of Ast_cocci.meta_name
  | BindGood of Ast_cocci.meta_name (* used to implement \+ *)
  | BindBad  of Ast_cocci.meta_name
  | FakeBrace

type ctlcocci = (predicate, Ast_cocci.meta_name) Wrapper_ctl.wrapped_ctl


(*****************************************************************************)
(* the different binding types *)
(*****************************************************************************)
type metavars_binding = Ast_c.metavars_binding

(* used in ctlcocci_integration *)
type metavar_binding_kind2 = 
  | NormalMetaVal of Ast_c.metavar_binding_kind
  | ParenVal of Ast_cocci.meta_name
  | LabelVal of int list
  | GoodVal | BadVal (* used to implement \+ *)

and metavars_binding2 = (mvar, metavar_binding_kind2) Common.assoc



(*****************************************************************************)
(* the CTL model related types *)
(*****************************************************************************)
type label_ctlcocci = 
 predicate -> 
 (nodei * 
 (predicate * (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution)) 
 list
 
type model = Control_flow_c.cflow * label_ctlcocci * nodei list

type transformation_info = 
 (nodei * metavars_binding * Ast_cocci.rule_elem) list


(*****************************************************************************)
(* comparing binding *)
(*****************************************************************************)

let equal_binding xs ys = 
  List.sort compare xs = List.sort compare ys
