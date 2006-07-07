open Common open Commonop
open Format

open Ast_ctl

let pp = Format.print_string
let box f = open_box 1; f(); close_box ()

let rec (pp_ctl: ('pred -> unit) * ('mvar -> unit) -> ('pred, 'mvar, 'info) generic_ctl -> unit) = 
fun (pp_pred, pp_mvar) ctl -> 
 let rec pp_aux = fun ctl ->
  match Ast_ctl.unwrap ctl with
  | False              -> pp "False"
  | True               -> pp "True"
  | Pred(p)            -> pp_pred p
  | Not(phi)           -> pp "-|("; box (fun () -> pp_aux phi); pp ")"
  | Exists(v,phi)      ->  
      pp "(";
      pp ("Ex");
      pp_mvar v;
      pp "."; 
      print_cut();
      box (fun () -> pp_aux phi); 
      pp ")"
  | And(phi1,phi2)     ->  pp_2args "/\\" phi1 phi2; 
  | Or(phi1,phi2)      ->  pp_2args "\\/" phi1 phi2; 
  | Implies(phi1,phi2) ->   pp_2args "=>" phi1 phi2;
  | AF(phi1)             -> pp "AF("; pp_arg phi1; pp ")"
  | AX(phi1)             -> pp "AX("; pp_arg phi1; pp ")"
  | AG(phi1)             -> pp "AG("; pp_arg phi1; pp ")"
  | EF(phi1)             -> pp "EF("; pp_arg phi1; pp ")"
  | EX(phi1)	         -> pp "EX("; pp_arg phi1; pp ")"
  | EG(phi1)		 -> pp "EG("; pp_arg phi1; pp ")"
  | AU(phi1,phi2)        -> pp "A[";pp_2args_bis "U" phi1 phi2; pp "]" 
  | EU(phi1,phi2)	 -> pp "E[";pp_2args_bis "U" phi1 phi2; pp "]" 
  | Let (x,phi1,phi2)  -> 
      pp ("Let"^" "^x); 
      print_space ();
      pp "="; 
      print_space ();
      box (fun () -> pp_aux phi1);
      print_space ();
      pp "in"; 
      print_space ();
      box (fun () -> pp_aux phi2);
  | Ref(s)             -> 
       (* pp "Ref(";  *)
       pp s; 
       (* pp ")" *)

 and pp_2args sym phi1 phi2 = 
   begin
     pp "(";
     box (fun () -> pp_aux phi1); 
     print_cut();
     pp sym;
     print_cut ();
     box (fun () -> pp_aux phi2);
     pp ")";
   end
 and pp_2args_bis sym phi1 phi2 = 
   begin
     box (fun () -> pp_aux phi1); 
     print_cut();
     print_space();
     pp sym;
     print_space();
     print_cut ();
     box (fun () -> pp_aux phi2);
  end
     
 and pp_arg phi = 
   begin
     box (fun () -> pp_aux phi);
   end

 in
 pp_aux ctl



