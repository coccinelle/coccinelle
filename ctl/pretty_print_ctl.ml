open Common open Commonop
open Format

open Ast_ctl

let char_and = "&"
let char_or  = "v" 
let char_not = "!" 

(*
let char_and = "/\\"
let char_or = "\\/"
let char_not = "-|"
*)

(* Need introduce the Val constructor, or use -rectype. *)
type ('a, 'b, 'c) environment = (string, ('a, 'b, 'c) binding_val) Common.assoc 
  and ('a, 'b, 'c) binding_val = 
    Val of ('a, 'b, 'c) generic_ctl * ('a, 'b, 'c) environment

let rec (pp_ctl: 
   ('pred -> unit) * ('mvar -> unit) -> bool -> 
   ('pred, 'mvar, 'info) generic_ctl -> 
   unit) =  
 fun (pp_pred, pp_mvar) inline_let_def ctl -> 

   


  let rec pp_aux env = fun ctl ->
  match Ast_ctl.unwrap ctl with
  | False              -> pp "False"
  | True               -> pp "True"
  | Pred(p)            -> pp_pred p
  | Not(phi)           -> pp char_not; pp_do_in_box (fun () -> pp_aux env phi)
  | Exists(v,phi)      ->  
      pp "(";
      pp ("Ex ");
      pp_mvar v;
      pp " . "; 
      print_cut();
      pp_do_in_box (fun () -> pp_aux env phi); 
      pp ")"
  | And(phi1,phi2)     ->  pp_2args env char_and phi1 phi2; 
  | Or(phi1,phi2)      ->  pp_2args env char_or phi1 phi2; 
  | Implies(phi1,phi2) ->   pp_2args env "=>" phi1 phi2;
  | AF(phi1)             -> pp "AF("; pp_arg env phi1; pp ")"
  | AX(phi1)             -> pp "AX("; pp_arg env phi1; pp ")"
  | AG(phi1)             -> pp "AG("; pp_arg env phi1; pp ")"
  | EF(phi1)             -> pp "EF("; pp_arg env phi1; pp ")"
  | EX(phi1)	         -> pp "EX("; pp_arg env phi1; pp ")"
  | EG(phi1)		 -> pp "EG("; pp_arg env phi1; pp ")"
  | AU(phi1,phi2)        -> pp "A[";pp_2args_bis env "U" phi1 phi2; pp "]" 
  | EU(phi1,phi2)	 -> pp "E[";pp_2args_bis env "U" phi1 phi2; pp "]" 
  | Let (x,phi1,phi2)  -> 
      let env' = (x, (Val (phi1,env)))::env in

      if not inline_let_def
      then
        begin
          pp ("Let"^" "^x);
	  pp " = ";
          pp_do_in_box (fun () -> pp_aux env phi1);
          print_space ();
          pp "in"; 
          print_space ();
        end;
      
      let pp_do_in_zero_box f = Format.open_box 0; f(); Format.close_box () in
      pp_do_in_zero_box (fun () -> pp_aux env' phi2);
  | Ref(s)             -> 
      if inline_let_def
      then
        let Val (phi1,env') = List.assoc s env in
        pp_aux env' phi1

      else 
       (* pp "Ref(";  *)
       pp s 
       (* pp ")" *)

 and pp_2args env sym phi1 phi2 = 
   begin
     pp "(";
     pp_do_in_box (fun () -> pp_aux env phi1); 
     print_space();
     pp sym;
     print_space ();
     pp_do_in_box (fun () -> pp_aux env phi2);
     pp ")";
   end
 and pp_2args_bis env sym phi1 phi2 = 
   begin
     pp_do_in_box (fun () -> pp_aux env phi1); 
     print_space();
     pp sym;
     print_space();
     pp_do_in_box (fun () -> pp_aux env phi2);
  end
     
 and pp_arg env phi = 
   begin
     pp_do_in_box (fun () -> pp_aux env phi);
   end

 in
 begin
   pp_init (fun () ->  pp_aux [] ctl;)
 end



