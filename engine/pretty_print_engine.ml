(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common.Infix

open Lib_engine


let pp = Common.pp

let pp_meta (_,x) = pp x

let rec pp_binding_kind = function
  | Ast_c.MetaIdVal        s -> pp ("id " ^ s)
  | Ast_c.MetaFuncVal      s -> pp ("func " ^ s)
  | Ast_c.MetaLocalFuncVal s -> pp ("localfunc " ^ s)
  | Ast_c.MetaExprVal    (expr,_,_) -> Pretty_print_c.pp_expression_simple expr
  | Ast_c.MetaAssignOpVal op        ->
      pp "meta assign op ";
      Pretty_print_c.pp_assignOp_simple op
  | Ast_c.MetaBinaryOpVal op        ->
      pp "meta binary op ";
      Pretty_print_c.pp_binaryOp_simple op
  | Ast_c.MetaExprListVal  expr_list ->
      Pretty_print_c.pp_arg_list_simple expr_list
  | Ast_c.MetaInitVal      ini ->
      Pretty_print_c.pp_init_simple ini
  | Ast_c.MetaInitListVal      ini -> pp "<<initlist>>"
  | Ast_c.MetaTypeVal      typ ->
      Pretty_print_c.pp_type_simple typ
  | Ast_c.MetaDeclVal      (decl,_) ->
      Pretty_print_c.pp_decl_simple decl
  | Ast_c.MetaFieldVal      decl ->
      Pretty_print_c.pp_field_simple decl
  | Ast_c.MetaFieldListVal      decls ->
      List.iter Pretty_print_c.pp_field_simple decls
  | Ast_c.MetaStmtVal      (statement,_,_) ->
      Pretty_print_c.pp_statement_simple statement
  | Ast_c.MetaStmtListVal      (statxs,_) ->
      Pretty_print_c.pp_statement_seq_list_simple statxs
  | Ast_c.MetaFmtVal fmt -> Pretty_print_c.pp_string_format_simple fmt
  | Ast_c.MetaAttrArgVal arg -> Pretty_print_c.pp_attr_arg_simple arg
  | Ast_c.MetaFragListVal frags ->
      frags +> (List.iter Pretty_print_c.pp_string_fragment_simple)
  | Ast_c.MetaParamVal     params -> pp "<<param>>"
  | Ast_c.MetaParamListVal params -> pp "<<paramlist>>"
  | Ast_c.MetaDParamListVal params -> pp "<<define_paramlist>>"
  | Ast_c.MetaListlenVal n -> pp (string_of_int n)
  | Ast_c.MetaPosVal (pos1, pos2) ->
      let print_pos = function
	  Ast_cocci.Real x -> string_of_int x
	| Ast_cocci.Virt(x,off) -> Printf.sprintf "%d+%d" x off in
      pp (Printf.sprintf ("pos(%s,%s)") (print_pos pos1) (print_pos pos2))
  | Ast_c.MetaPosValList l ->
      pp (Printf.sprintf ("poss[%s]")
	    (String.concat ", "
	       (List.map
		  (function
		      (fl,ce,None,(minl,minc),(maxl,maxc)) ->
			Printf.sprintf "(%s,%s,unknown,(%d,%d),(%d,%d))"
			  fl ce minl minc maxl maxc
		    | (fl,ce,Some((ceminl,ceminc),(cemaxl,cemaxc)),
		       (minl,minc),(maxl,maxc)) ->
			Printf.sprintf
			   "(%s,%s,((%d,%d),(%d,%d)),(%d,%d),(%d,%d))"
			   fl ce ceminl ceminc cemaxl cemaxc
			   minl minc maxl maxc)
		  l)))
  | Ast_c.MetaComValList params -> pp "<<comlist>>"
  | Ast_c.MetaNoVal -> pp "no value"

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


let pp_predicate = function
  | InLoop -> pp "InLoop"
  | TrueBranch -> pp "TrueBranch"
  | EscTrueBranch -> pp "EscTrueBranch"
  | FalseBranch -> pp "FalseBranch"
  | After -> pp "After"
  | GotoAfter -> pp "GotoAfter"
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
