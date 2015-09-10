open Common

(* When in a semantic patch there is f(X) ... f(X) we want to force
 * the two X to be equal in the concrete code, but we would like that
 * there be equal modulo some isomorphisms, so that the following
 * concrete code also match: f(a && b) g(); f(b && a)

 * Maybe would be easier to transform ast_c in ast_cocci and call the
 * iso engine of julia. *)

open Ast_c

let rec (iso_e_e: expression -> expression -> bool) = fun a b ->
  raise Todo
    (*
      let rec (=~=) a b =
      match (a, b) with
      | (Ident a, typa, iia), (Ident b, typb, iib) -> a = b
      | (Constant a, typa, iia), (Constant b, typb, iib) -> a = b
      | (FunCall  (ea, eas), typa, iia), (FunCall  (eb, ebs), typb, iib)        ->
      ea =~= eb &&
      List.length eas = List.length ebs &&
      List.for_all (fun (a, b) ->
      match (a, b) with
      | (Left ea, iia), (Left eb, iib) -> ea =~= eb
      | _ -> raise Todo
      )
      (zip eas ebs)
      | (Binary (ea1,Logical AndLog,ea2),typa, iia), (Binary (eb1,Logical AndLog, eb2), typb, iib) ->
      (ea1 =~= eb1  && ea2 =~= eb2)
      ||
      (ea1 =~= eb2  && ea2 =~= eb1)

      | _ -> raise Todo
      in
      a =~= b
    *)

and (iso_st_st: statement -> statement -> bool) = fun a b ->
  raise Todo
and (iso_t_t: fullType -> fullType -> bool) = fun a b ->
  raise Todo


(*
let _ = assert (iso_e_e
  (cexpression_of_string "a&&b")
  (cexpression_of_string "b&&a")
*)
