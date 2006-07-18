open Common open Commonop

open Lib_engine


let pp = Common.pp 

let pp_binding_kind2 = function
  | ParenVal s -> pp ("parenvar(" ^ s ^ ")")
  | NormalMetaVal x -> Pretty_print_c.pp_binding_kind x
  | LabelVal xs -> 
      begin
        pp "labelval";
        pp "(";
        Common.print_between (fun () -> pp ",") Format.print_int xs;
        pp ")";
      end

  
let pp_predicate = function 
  | TrueBranch -> pp "TrueBranch"
  | FalseBranch -> pp "FalseBranch"
  | After -> pp "After"
  | FallThrough -> pp "FallThrough"
  | Return -> pp "Return"
  | Exit -> pp "Exit"
  | Paren s -> pp "Paren("; pp s; pp ")"
  | Match re -> Pretty_print_cocci.rule_elem "" re
  | Label s -> pp "Label("; pp s; pp ")"
  | PrefixLabel s -> pp "PrefixLabel("; pp s; pp ")"

and pp_binding2 subst = 
  begin
    pp "[";
    Common.print_between (fun () -> pp ";" ) 
      (fun (s, kind) -> pp s; pp " --> "; pp_binding_kind2 kind)
      subst;
    pp "]";
  end

and pp_binding2_ctlsubst subst = 
  begin
    pp "[";
    Common.print_between (fun () -> pp ";" ) 
      (function
       | Ast_ctl.Subst (s, kind) ->    pp s; pp " --> ";  pp_binding_kind2 kind;
       | Ast_ctl.NegSubst (s, kind) -> pp s; pp " -/-> "; pp_binding_kind2 kind;
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


let pp_ctlcocci_no_mcodekind inline_let_def ctl = 
  begin
    Pretty_print_cocci.print_plus_flag := false;
    Pretty_print_cocci.print_minus_flag := false;
    Common.pp_init (fun () -> 
      Pretty_print_ctl.pp_ctl (pp_pred_smodif,(fun s -> pp s)) 
        inline_let_def ctl;
      );
  end


let pp_transformation_info trans_info = 
  trans_info +> List.iter (fun (i, subst, re) -> 
    pp ("transform state:" ^ (i_to_s i));
    Format.force_newline();
    pp "  with rule_elem:";
    Pretty_print_cocci.rule_elem "" re;
    Format.force_newline();
    pp "  with binding:";
    Pretty_print_c.pp_binding subst;
    Format.force_newline();
    );
  
