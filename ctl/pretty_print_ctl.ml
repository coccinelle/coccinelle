open Common open Commonop
open Format

open Ast_ctl

(* todo?: a txt_to_latex, that use Format to compute the good space but
 * then generate latex to better output.
 *)

let char_and  = "&"
let char_and_any  = "&+"
let char_or   = "v"
let char_seqor   = "|"
let char_not  = "!"
let char_back = "^"

(*
let char_and = "/\\"
let char_or = "\\/"
let char_not = "-|"
*)

(* need introduce the Val constructor, or use -rectype. *)
type ('a,'b,'c) environment = (string, ('a,'b,'c) binding_val) Common.assoc 
and ('a, 'b, 'c) binding_val = 
    Val of ('a,'b,'c) generic_ctl * ('a,'b,'c) environment

let rec (pp_ctl: 
   ('pred -> unit) * ('mvar -> unit) -> bool -> 
   ('pred, 'mvar, 'info) generic_ctl -> 
   unit) =  
 fun (pp_pred, pp_mvar) inline_let_def ctl -> 

   let rec pp_aux env = fun ctl ->
     match Ast_ctl.unwrap ctl with
       False              -> pp "False"
     | True               -> pp "True"
     | Pred(p)            -> pp_pred p
     | Not(phi)           ->
	 pp char_not; Common.pp_do_in_box (fun () -> pp_aux env phi)
     | Exists(v,phi)      ->  
	 pp "(";
	 pp ("Ex ");
	 pp_mvar v;
	 pp " . "; 
	 print_cut();
	 Common.pp_do_in_box (fun () -> pp_aux env phi); 
	 pp ")";
     | AndAny(dir,phi1,phi2) ->
	 pp_2args env (char_and_any^(pp_dirc dir)) phi1 phi2; 
     | And(phi1,phi2)        -> pp_2args env char_and phi1 phi2; 
     | Or(phi1,phi2)         -> pp_2args env char_or phi1 phi2; 
     | SeqOr(phi1,phi2)      -> pp_2args env char_seqor phi1 phi2; 
     | Implies(phi1,phi2)    -> pp_2args env "=>" phi1 phi2;
     | AF(dir,phi1)     -> pp "AF"; pp_dir dir;  pp_arg_paren env phi1; 
     | AX(dir,phi1)     -> pp "AX"; pp_dir dir;  pp_arg_paren env phi1; 
     | AG(dir,phi1)     -> pp "AG"; pp_dir dir;  pp_arg_paren env phi1;
     | EF(dir,phi1)     -> pp "EF"; pp_dir dir;  pp_arg_paren env phi1;
     | EX(dir,phi1)     -> pp "EX"; pp_dir dir;  pp_arg_paren env phi1; 
     | EG(dir,phi1)     -> pp "EG"; pp_dir dir;  pp_arg_paren env phi1; 
     | AW(dir,phi1,phi2) ->
	 pp "A"; pp_dir dir; pp "["; 
           pp_2args_bis env "W" phi1 phi2; 
         pp "]" 
     | AU(dir,phi1,phi2) ->
	 pp "A"; pp_dir dir; pp "["; 
           pp_2args_bis env "U" phi1 phi2; 
         pp "]" 
     | EU(dir,phi1,phi2)  ->
	 pp "E"; pp_dir dir; pp "["; 
           pp_2args_bis env "U" phi1 phi2; 
         pp "]" 
     | Let (x,phi1,phi2)  -> 
	 let env' = (x, (Val (phi1,env)))::env in
	 
	 if not inline_let_def
	 then
           begin
             pp ("Let"^" "^x);
	     pp " = ";
	     print_cut();
             Common.pp_do_in_box (fun () -> pp_aux env phi1);
             print_space ();
             pp "in"; 
             print_space ();
           end;
	 pp_do_in_zero_box (fun () -> pp_aux env' phi2);
     | LetR (dir,x,phi1,phi2)  -> 
	 let env' = (x, (Val (phi1,env)))::env in
	 
	 if not inline_let_def
	 then
           begin
             pp ("LetR"^" "^x); pp_dir dir;
	     pp " = ";
	     print_cut();
             Common.pp_do_in_box (fun () -> pp_aux env phi1);
             print_space ();
             pp "in"; 
             print_space ();
           end;
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
     | Uncheck(phi1) ->
	 pp "Uncheck"; pp_arg_paren env phi1; 

     | Dots _ -> failwith "should not occur"
     | PDots _ -> failwith "should not occur"

   and pp_dir = function
       FORWARD -> ()
     | BACKWARD -> pp char_back
	     
   and pp_dirc = function
       FORWARD -> ""
     | BACKWARD -> char_back
	     
   and pp_2args env sym phi1 phi2 = 
     begin
       pp "(";
       Common.pp_do_in_box (fun () -> pp_aux env phi1); 
       print_space();
       pp sym;
       print_space ();
       Common.pp_do_in_box (fun () -> pp_aux env phi2);
       pp ")";
     end
   and pp_2args_bis env sym phi1 phi2 = 
     begin
       Common.pp_do_in_box (fun () -> pp_aux env phi1); 
       print_space();
       pp sym;
       print_space();
       Common.pp_do_in_box (fun () -> pp_aux env phi2);
     end
       
   and pp_arg_paren env phi =  Common.pp_do_in_box (fun () -> 
     pp "(";
     pp_aux env phi;
     pp ")";
   )      
   in
   Common.pp_do_in_box (fun () ->  pp_aux [] ctl;)
