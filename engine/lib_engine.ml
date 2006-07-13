type mvar = string

type predicate =
  | TrueBranch | FalseBranch
  | After (* pointer to the code after an if or while *)
  | Return (* any exit from the current function *)
  | Paren of string
  | Match of Ast_cocci.rule_elem
  | Label of string
  | PrefixLabel of string

type ctlcocci = (predicate, string) Wrapper_ctl.wrapped_ctl


(* used in ctlcocci_integration *)
type metavar_binding_kind2 = 
  | NormalMetaVal of Ast_c.metavar_binding_kind
  | ParenVal of string
  | LabelVal of int list

and metavars_binding2 = (mvar, metavar_binding_kind2) Common.assoc

type label_ctlcocci = 
 predicate -> 
   (Ograph_extended.nodei * 
    (mvar, metavar_binding_kind2) Ast_ctl.generic_substitution)
   list


type transformation_info = 
    (Ograph_extended.nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list


(* ------------------------------------------------------------------------ *)

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
  | Return -> pp "Return"
  | Paren s -> pp "Paren("; pp s; pp ")"
  | Match re -> Unparse_cocci.rule_elem "" re
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


let pp_ctlcocci_no_mcodekind ctl = 
  begin
    Unparse_cocci.print_plus_flag := false;
    Unparse_cocci.print_minus_flag := false;
    Common.pp_init (fun () -> 
      Pretty_print_ctl.pp_ctl (pp_pred_smodif,(fun s -> pp s)) ctl;
      );
  end
