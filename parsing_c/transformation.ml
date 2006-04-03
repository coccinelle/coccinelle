open Common open Commonop

open Ast_c
open Visitor_c

let test_simple_trans1 = fun xs -> 

  let trans = { default_visitor_c_continuation_s with
      kexpr_s = (fun (k, bigf) e -> 
        let rec aux = function
            (* subtil:  dont do  oldexpr_s e for Cast,  otherwise it does not recurse, => have to introduce aux function *)
          | (FunCall ((Constant (Ident s), [(ii1,())]), xs), ii2) -> 
               let xs' = xs +> List.map (fun (e,ii) -> 
                 match e with
                 | Left e -> Left (aux e), ii
                 | Right _ -> raise Todo
                                        ) in
               (FunCall ((Constant (Ident (String.uppercase s)), [{ii1 with str = String.uppercase ii1.str},()]), xs'), ii2)
          | (Constant (Int i), [(ii,())]) -> 
               let vi = s_to_i i in
               let vi = vi+100 in
                (Constant (Int (i_to_s vi)), [{ii with str = i_to_s vi},()])
          | x -> k x
        in
        aux e
          );
              } 
  in


  xs +> List.map (function
    | Declaration ((DeclList (((Some (s, _, iis)  , _, _),[])::xs, _)) as decl)  -> 
          Declaration (visitor_decl_k_s  trans decl)
    | Definition ((s, _, _, _, _) as def)                    -> 
          Definition (visitor_def_k_s    trans  def)
    | x -> error_cant_have x

  )
