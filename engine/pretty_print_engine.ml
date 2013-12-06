(*
 * Copyright 2012, INRIA
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


# 0 "./pretty_print_engine.ml"
open Common.Infix

open Lib_engine


let pp = Common.pp

let pp_meta (_,x) = pp x

let rec pp_binding_kind = function
  | Ast_c.MetaIdVal        (s,_) -> pp ("id " ^ s)
  | Ast_c.MetaFuncVal      s -> pp ("func " ^ s)
  | Ast_c.MetaLocalFuncVal s -> pp ("localfunc " ^ s)
  | Ast_c.MetaExprVal      (expr,_) -> Pretty_print_c.pp_expression_simple expr
  | Ast_c.MetaExprListVal  expr_list -> pp "<<exprlist>>"
  | Ast_c.MetaInitVal      ini ->
      Pretty_print_c.pp_init_simple ini
  | Ast_c.MetaInitListVal      ini -> pp "<<initlist>>"
  | Ast_c.MetaTypeVal      typ ->
      Pretty_print_c.pp_type_simple typ
  | Ast_c.MetaDeclVal      decl ->
      Pretty_print_c.pp_decl_simple decl
  | Ast_c.MetaFieldVal      decl ->
      Pretty_print_c.pp_field_simple decl
  | Ast_c.MetaFieldListVal      decls ->
      List.iter Pretty_print_c.pp_field_simple decls
  | Ast_c.MetaStmtVal      statement ->
      Pretty_print_c.pp_statement_simple statement
  | Ast_c.MetaFmtVal fmt -> Pretty_print_c.pp_string_format_simple fmt
  | Ast_c.MetaFragListVal frags ->
      frags +> (List.iter Pretty_print_c.pp_string_fragment_simple)
  | Ast_c.MetaParamVal     params -> pp "<<param>>"
  | Ast_c.MetaParamListVal params -> pp "<<paramlist>>"
  | Ast_c.MetaListlenVal n -> pp (string_of_int n)
  | Ast_c.MetaPosVal (pos1, pos2) ->
      let print_pos = function
	  Ast_cocci.Real x -> string_of_int x
	| Ast_cocci.Virt(x,off) -> Printf.sprintf "%d+%d" x off in
      pp (Common.sprintf ("pos(%s,%s)") (print_pos pos1) (print_pos pos2))
  | Ast_c.MetaPosValList l ->
      pp (Common.sprintf ("poss[%s]")
	    (String.concat ", "
	       (List.map
		  (function (fl,ce,(minl,minc),(maxl,maxc)) ->
		    Printf.sprintf "(%s,%s,(%d,%d),(%d,%d))"
		      fl ce minl minc maxl maxc)
		  l)))

and pp_binding subst =
  begin
    pp "[";
    Common.print_between (fun () -> pp ";"; Format.print_cut() )
      (fun ((r,s), kind) ->
	pp r; pp "."; pp s; pp " --> "; pp_binding_kind kind)
      subst;
    pp "]";
  end


let pp_binding_kind2 = function
  | ParenVal s -> pp "pv("; pp_meta s; pp ")"
  | NormalMetaVal x -> pp_binding_kind x
  | LabelVal (Absolute xs) ->
      begin
        pp "labelval";
        pp "(";
        Common.print_between (fun () -> pp ",") Format.print_int xs;
        pp ")";
      end
  | LabelVal (Prefix xs) ->
      begin
        pp "prefixlabelval";
        pp "(";
        Common.print_between (fun () -> pp ",") Format.print_int xs;
        pp ")";
      end
  | GoodVal -> pp "goodval"
  | BadVal ->  pp "badval"


let rec pp_predicate = function
  | InLoop -> pp "InLoop"
  | TrueBranch -> pp "TrueBranch"
  | FalseBranch -> pp "FalseBranch"
  | After -> pp "After"
  | FallThrough -> pp "FallThrough"
  | LoopFallThrough -> pp "LoopFallThrough"
  | Return -> pp "Return"
  | UnsafeBrace -> pp "UnsafeBrace"
  | FunHeader -> pp "FunHeader"
  | Top -> pp "Top"
  | ErrorExit -> pp "ErrorExit"
  | Exit -> pp "Exit"
  | Goto -> pp "Goto"
  | Paren s -> pp "Paren("; pp_meta s; pp ")"
  | Match (re) -> Pretty_print_cocci.print_rule_elem re
  | Label s -> pp "Label("; pp_meta s; pp ")"
  | BCLabel s -> pp "BreakContinueLabel("; pp_meta s; pp ")"
  | PrefixLabel s -> pp "PrefixLabel("; pp_meta s; pp ")"
  | BindGood s -> pp "BindGood("; pp_meta s; pp ")"
  | BindBad s ->  pp "BindBad(";  pp_meta s; pp ")"
  | FakeBrace -> pp "FakeBrace"

and pp_binding2 subst =
  begin
    pp "[";
    Common.print_between (fun () -> pp ";";Format.print_cut(); )
      (fun (s, kind) -> pp s; pp " --> "; pp_binding_kind2 kind)
      subst;
    pp "]";
  end

and pp_binding2_ctlsubst subst =
  begin
    pp "[";
    Common.print_between (fun () -> pp ";"; Format.print_cut(); )
      (function
          Ast_ctl.Subst (s, kind) ->
	    pp_meta s; pp " --> ";  pp_binding_kind2 kind;
       | Ast_ctl.NegSubst (s, kind) ->
	   pp_meta s; pp " -/-> "; pp_binding_kind2 kind;
      )
      subst;
    pp "]";
  end

let predicate_to_string pred =
  Common.format_to_string (function _ -> pp_predicate pred)


let pp_pred_smodif = fun (pred, smodif) ->
  begin
    pp_predicate pred;
(*
  (match smodif with
  |  Ast_ctl.Modif x | Ast_ctl.UnModif x -> pp " with <modifTODO>"
  | Ast_ctl.Control -> ()
  )
*)
  end


let pp_ctlcocci show_plus inline_let_def ctl =
  begin
    if show_plus
    then begin
      Pretty_print_cocci.print_plus_flag := true;
      Pretty_print_cocci.print_minus_flag := true;
    end
    else begin
      Pretty_print_cocci.print_plus_flag := false;
      Pretty_print_cocci.print_minus_flag := false;
    end;
    Common.pp_do_in_box (fun () ->
      Pretty_print_ctl.pp_ctl (pp_pred_smodif,(fun s -> pp_meta s))
        inline_let_def ctl;
      );
  end


