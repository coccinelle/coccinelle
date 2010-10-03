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


open Ast_c
open Common
open Pycaml

let rec exprrep expr = match expr with
  Ast_c.Ident s -> s
| Ast_c.Constant c -> constantrep c
| Ast_c.FunCall (e,args) -> "TODO: FunCall"
| Ast_c.CondExpr (e1,e2,e3) -> "TODO: CondExpr"
| Ast_c.Sequence (e1,e2) -> "TODO: Sequence"
| Ast_c.Assignment (e1,op,e2) -> "TODO: Assignment"
| Ast_c.Postfix (e,op) -> "TODO: Postfix"
| Ast_c.Infix (e,op) -> "TODO: Infix"
| Ast_c.Unary (e,op) -> "TODO: Unary"
| Ast_c.Binary (e1,op,e2) -> "TODO: Binary"
| Ast_c.ArrayAccess (e1,e2) -> "TODO: ArrayAccess"
| Ast_c.RecordAccess (e1,s) -> "TODO: RecordAccess"
| Ast_c.RecordPtAccess (e,s) -> "TODO: RecordPtAccess"
| Ast_c.SizeOfExpr e -> "TODO: SizeOfExpr"
| Ast_c.SizeOfType t -> "TODO: SizeOfType"
| Ast_c.Cast (t,e) -> "TODO: Cast"
| Ast_c.StatementExpr c -> "TODO: StatementExpr"
| Ast_c.Constructor (t,i) -> "TODO: Constructor"
| Ast_c.ParenExpr e -> "TODO: ParenExpr"
and constantrep c = match c with
  Ast_c.String (s,isWchar) -> s 
| Ast_c.MultiString -> "TODO: MultiString"
| Ast_c.Char (s,isWchar) -> s
| Ast_c.Int s -> s 
| Ast_c.Float (s,t) -> s

let call_pretty f a =
  let str = ref ([] : string list) in
  let pr_elem info = str := (Ast_c.str_of_info info) :: !str in
  let pr_sp _ = () in
  f pr_elem pr_sp a;
  String.concat " " (List.rev !str)

let stringrep mvb = match mvb with
  Ast_c.MetaIdVal        s -> s
| Ast_c.MetaFuncVal      s -> s
| Ast_c.MetaLocalFuncVal s -> s
| Ast_c.MetaExprVal      ((expr,_),[il]) -> (exprrep expr)
| Ast_c.MetaExprVal	 e -> "TODO: <<MetaExprVal>>"
| Ast_c.MetaExprListVal  expr_list -> "TODO: <<exprlist>>"
| Ast_c.MetaTypeVal      typ -> call_pretty Pretty_print_c.pp_type_gen typ
| Ast_c.MetaStmtVal      statement -> "TODO: stmt"
| Ast_c.MetaParamVal     params -> "TODO: <<param>>"
| Ast_c.MetaParamListVal params -> "TODO: <<paramlist>>"
| Ast_c.MetaListlenVal n -> string_of_int n
| Ast_c.MetaPosVal (pos1, pos2) ->
    let print_pos = function
	Ast_cocci.Real x -> string_of_int x
      | Ast_cocci.Virt(x,off) -> Printf.sprintf "%d+%d" x off in
    Common.sprintf ("pos(%s,%s)") (print_pos pos1) (print_pos pos2)
| Ast_c.MetaPosValList positions -> "TODO: <<postvallist>>"

