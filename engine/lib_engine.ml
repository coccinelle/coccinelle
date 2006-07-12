type mvar = string

type metavar_binding_kind2 = 
  | NormalMetaVar of Ast_c.metavar_binding_kind
  | ParenVar of string
  | LabelValue of int list

and metavars_binding2 = (mvar, metavar_binding_kind2) Common.assoc

type predicate =
  | TrueBranch | FalseBranch
  | After (* pointer to the code after an if or while *)
  | Return (* any exit from the current function *)
  | Paren of string
  | Match of Ast_cocci.rule_elem
  | Label of string
  | PrefixLabel of string

type transformation_info = 
    (Ograph_extended.nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list

(* ------------------------------------------------------------------------ *)

let pp = Format.print_string 

let pp_binding_kind2 = function
  | ParenVar s -> pp ("parenvar(" ^ s ^ ")")
  | NormalMetaVar x -> Pretty_print_c.pp_binding_kind x
  
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
    Common.print_between (fun () -> Format.print_string ";" ) 
      (fun (s, kind) -> 
        pp s;
        pp " --> ";
        pp_binding_kind2 kind)
      subst;
    pp "]";
  end

and pp_binding2_ctlsubst subst = 
  begin
    pp "[";
    Common.print_between (fun () -> Format.print_string ";" ) 
      (function
        | Ast_ctl.Subst (s, kind) -> 
            pp s;
            pp " --> ";
            pp_binding_kind2 kind;
            
        | Ast_ctl.NegSubst (s, kind) -> 
            pp s;
            pp " -/-> ";
            pp_binding_kind2 kind;
      )
      subst;
    pp "]";
  end

let predicate_to_string pred =
  Common.format_to_string (function _ -> pp_predicate pred)
