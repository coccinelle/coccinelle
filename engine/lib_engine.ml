(*
 * Copyright 2012-2015, Inria
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


# 0 "./lib_engine.ml"
open Ograph_extended

(*****************************************************************************)
(* the different ctl formula related types *)
(*****************************************************************************)
type mvar = Ast_cocci.meta_name

type predicate =
    InLoop | TrueBranch | EscTrueBranch | FalseBranch
  | After (* pointer to the code after a block, if, or while *)
  | FallThrough | LoopFallThrough
  | Return (* any exit from the current function *)
  | FunHeader | UnsafeBrace | Top | Exit | ErrorExit | Goto
  | Paren of Ast_cocci.meta_name
  | Match of Ast_cocci.rule_elem
  | Label of Ast_cocci.meta_name
  | BCLabel of Ast_cocci.meta_name (* parent of break or continue *)
  | PrefixLabel of Ast_cocci.meta_name
  | BindGood of Ast_cocci.meta_name (* used to implement \+ *)
  | BindBad  of Ast_cocci.meta_name
  | FakeBrace

(* coccionly: *)
type ctlcocci = (predicate, Ast_cocci.meta_name) Wrapper_ctl.wrapped_ctl


(*****************************************************************************)
(* the different binding types *)
(*****************************************************************************)
type metavars_binding = Ast_c.metavars_binding

(* used in ctlcocci_integration *)
type metavar_binding_kind2 =
  | NormalMetaVal of Ast_c.metavar_binding_kind
  | ParenVal of Ast_cocci.meta_name
  | LabelVal of labelval
  | GoodVal | BadVal (* used to implement \+ *)

and labelval = Absolute of int list | Prefix of int list

and metavars_binding2 = (mvar, metavar_binding_kind2) Common.assoc



(*****************************************************************************)
(* the CTL model related types *)
(*****************************************************************************)
(* coccionly: *)
type label_ctlcocci =
 predicate ->
 (nodei *
 (predicate * (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution))
 list

type quicklabel_ctlcocci = predicate -> bool

type model = Control_flow_c.cflow * label_ctlcocci * quicklabel_ctlcocci *
      nodei list

type transformation_info =
 (nodei * metavars_binding * Ast_cocci.rule_elem) list

type numbered_transformation_info =
 (int list * (nodei * metavars_binding * Ast_cocci.rule_elem)) list


(*****************************************************************************)
(* comparing binding *)
(*****************************************************************************)

let equal_binding xs ys =
  List.sort compare xs = List.sort compare ys
