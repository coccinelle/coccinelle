(*
 * Copyright 2012-2014, INRIA
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


# 0 "./pycocci_aux.ml"
open Ast_c
open Common

let caller s f a =
  let str = ref ([] : string list) in
  let pr_elem info =
    let comments_before =
      List.map Token_c.str_of_token (Ast_c.get_comments_before info) in
    let comments_after =
      List.map Token_c.str_of_token (Ast_c.get_comments_after info) in
    (* constructed backwards *)
    str := (List.rev comments_after) @ (Ast_c.str_of_info info) ::
	   (List.rev comments_before) @ !str in
  let pr_sp _ = () in
  f ~pr_elem ~pr_space:pr_sp a;
  String.concat s (List.rev !str)

let call_pretty f a = caller " " f a
let call_pretty0 f a = caller "" f a

let exprrep = call_pretty Pretty_print_c.pp_expression_gen

let commalistrep list_printer elem_printer comma_printer x =
  (call_pretty list_printer x,
   List.map
     (function x ->
       call_pretty elem_printer (comma_printer x) (* drop commas *))
     x)

let exprlistrep =
  commalistrep Pretty_print_c.pp_arg_list_gen Pretty_print_c.pp_arg_gen
    Ast_c.unwrap

let paramlistrep =
  commalistrep Pretty_print_c.pp_param_list_gen Pretty_print_c.pp_param_gen
    Ast_c.unwrap

let initlistrep =
  commalistrep Pretty_print_c.pp_init_list_gen Pretty_print_c.pp_init_gen
    Ast_c.unwrap

let fieldlistrep =
  commalistrep Pretty_print_c.pp_field_list_gen Pretty_print_c.pp_field_gen
    (function x -> x)

let stringrep = function
  Ast_c.MetaIdVal        (s,_) -> s
| Ast_c.MetaFuncVal      s -> s
| Ast_c.MetaLocalFuncVal s -> s
| Ast_c.MetaExprVal      (expr,_) -> exprrep expr
| Ast_c.MetaExprListVal  expr_list ->
    call_pretty Pretty_print_c.pp_arg_list_gen expr_list
| Ast_c.MetaTypeVal      typ -> call_pretty Pretty_print_c.pp_type_gen typ
| Ast_c.MetaInitVal      ini -> call_pretty Pretty_print_c.pp_init_gen ini
| Ast_c.MetaInitListVal  ini -> call_pretty Pretty_print_c.pp_init_list_gen ini
| Ast_c.MetaDeclVal      decl ->
    call_pretty Pretty_print_c.pp_decl_gen decl
| Ast_c.MetaFieldVal      field ->
    call_pretty Pretty_print_c.pp_field_gen field
| Ast_c.MetaFieldListVal      field ->
    call_pretty Pretty_print_c.pp_field_list_gen field
| Ast_c.MetaStmtVal      statement ->
    call_pretty Pretty_print_c.pp_statement_gen statement
| Ast_c.MetaParamVal     param ->
    call_pretty Pretty_print_c.pp_param_gen param
| Ast_c.MetaParamListVal params ->
    call_pretty Pretty_print_c.pp_param_list_gen params
| Ast_c.MetaFragListVal frags ->
    call_pretty0 Pretty_print_c.pp_string_fragment_list_gen frags
| Ast_c.MetaFmtVal fmt ->
    call_pretty0 Pretty_print_c.pp_string_format_gen fmt
| Ast_c.MetaListlenVal n -> string_of_int n
| Ast_c.MetaPosVal (pos1, pos2) ->
    let print_pos = function
	Ast_cocci.Real x -> string_of_int x
      | Ast_cocci.Virt(x,off) -> Printf.sprintf "%d+%d" x off in
    Common.sprintf ("pos(%s,%s)") (print_pos pos1) (print_pos pos2)
| Ast_c.MetaPosValList positions -> "TODO: <<postvallist>>"

