open Common open Commonop

(* It is not the same thing than Unparse_c. Unparse_c correctly insert spaces, 
   comments, and so on, but that means that Unparse_c cannot pretty print an 
   expression independently, and that Unparse_c need the original file to be 
   able to unparse. So for debugging purpose Unparse_c is not practical, 
   hence this module. *)

open Ast_c

let pr_elem ((info,(mcode,env))) = 
  let s = info.str in
  Format.print_string s


let rec pp_expression x =
  match x with
  | Constant (String s),        typ, is     -> is +> List.iter pr_elem
  | Ident (c),         typ,[i]     -> pr_elem i
  | Constant (c),         typ,[i]     -> pr_elem i
  | FunCall  (e, es),     typ,[i1;i2] -> 
        pp_expression e; pr_elem i1; 
        es +> List.iter (fun (e, opt) -> 
          (match opt with
          | [] -> ()
          | [i] -> pr_elem i
          | x -> error_cant_have x
          );
          (match e with
          | Left e -> pp_expression e
          | Right (returnType, (sto, iisto)) -> raise Todo
          );
            );
        pr_elem i2;
  | Binary   (e1, op, e2),    typ,[i] -> 
      pp_expression e1;   pr_elem i; pp_expression e2
  | _ -> raise Todo

and pp_statement x = 
  match x with
  | ExprStatement (None), [i] -> pr_elem i;
  | ExprStatement (None), [] -> ()
  | ExprStatement (Some e), [i] -> pp_expression e; pr_elem i
  | ExprStatement (Some e), [] -> 
     (* the last ExprStatement of a for does not have a trailing ';' 
        hence the [] for ii  *)
      pp_expression e; 
  | _ -> raise Todo






let rec pp_binding_kind = function
  | MetaIdVal        s -> Format.print_string ("id " ^ s)
  | MetaFuncVal      s -> Format.print_string ("func " ^ s)
  | MetaLocalFuncVal s -> Format.print_string ("localfunc " ^ s)
  | MetaExprVal      expr -> pp_expression expr
  | MetaExprListVal  expr_list -> raise Todo
  | MetaTypeVal      typ -> raise Todo
  | MetaStmtVal      statement -> pp_statement statement
  | MetaParamVal     params -> raise Todo
  | MetaParamListVal params -> raise Todo

and pp_binding subst = 
  begin
    Format.print_string "[";
    Common.print_between (fun () -> Format.print_string ";" ) 
      (fun (s, kind) -> 
        Format.print_string s;
        Format.print_string " --> ";
        pp_binding_kind kind)
      subst;
    Format.print_string "]";
  end
