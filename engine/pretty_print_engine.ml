open Commonop

open Lib_engine


let pp = Common.pp 

let pp_meta (_,x) = pp x

let pr_elem info = 
  let s = Ast_c.str_of_info info in
  pp s


let rec pp_binding_kind = function
  | Ast_c.MetaIdVal        s -> pp ("id " ^ s)
  | Ast_c.MetaFuncVal      s -> pp ("func " ^ s)
  | Ast_c.MetaLocalFuncVal s -> pp ("localfunc " ^ s)
  | Ast_c.MetaExprVal      expr -> 
      Pretty_print_c.pp_expression_simple expr
  | Ast_c.MetaExprListVal  expr_list -> pp "<<exprlist>>"
  | Ast_c.MetaTypeVal      typ -> 
      Pretty_print_c.pp_type_gen pr_elem typ
  | Ast_c.MetaStmtVal      statement -> 
      Pretty_print_c.pp_statement_simple statement
  | Ast_c.MetaParamVal     params -> pp "<<param>>"
  | Ast_c.MetaParamListVal params -> pp "<<paramlist>>"
  | Ast_c.MetaConstVal cst -> 
      Pretty_print_c.pp_cst_gen pr_elem cst

and pp_binding subst = 
  begin
    pp "[";
    Common.print_between (fun () -> pp ";"; Format.print_cut() ) 
      (fun ((_,s), kind) -> pp s; pp " --> "; pp_binding_kind kind)
      subst;
    pp "]";
  end


let pp_binding_kind2 = function
  | ParenVal s -> pp "pv("; pp_meta s; pp ")"
  | NormalMetaVal x -> pp_binding_kind x
  | LabelVal xs -> 
      begin
        pp "labelval";
        pp "(";
        Common.print_between (fun () -> pp ",") Format.print_int xs;
        pp ")";
      end

  
let rec pp_predicate = function 
  | TrueBranch -> pp "TrueBranch"
  | FalseBranch -> pp "FalseBranch"
  | After -> pp "After"
  | FallThrough -> pp "FallThrough"
  | Return -> pp "Return"
  | ErrorExit -> pp "ErrorExit"
  | Exit -> pp "Exit"
  | Paren s -> pp "Paren("; pp_meta s; pp ")"
  | Match re -> Pretty_print_cocci.print_rule_elem re
  | Label s -> pp "Label("; pp_meta s; pp ")"
  | PrefixLabel s -> pp "PrefixLabel("; pp_meta s; pp ")"

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


