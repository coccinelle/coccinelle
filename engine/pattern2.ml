open Common open Commonop

module XMATCH = struct

  (***************************************************************************)
  (* Combinators *) 
  (***************************************************************************)
  (*
   * version0: 
   *   type ('a, 'b) matcher = 'a -> 'b -> bool
   *
   * version1: same but with a global variable holding the current binding
   *  BUT bug
   *   - can have multiple possibilities
   *   - globals sux
   *   - sometimes have to undo, cos if start match, then it binds, 
   *     and if later it does not match, then must undo the first binds.
   *     ex: when match parameters, can  try to match, but then we found far 
   *     later that the last argument of a function does not match
   *      => have to uando the binding !!!
   *      (can handle that too with a global, by saving the 
   *      global, ... but sux)
   *   => better not use global
   * 
   * version2: 
   *    type ('a, 'b) matcher = binding -> 'a -> 'b -> binding list
   *
   * Empty list mean failure (let matchfailure = []).
   * To be able to have pretty code, have to use partial application 
   * powa, and so the type is in fact
   *
   * version3:
   *    type ('a, 'b) matcher =  'a -> 'b -> binding -> binding list
   *
   * Then by defining the correct combinators, can have quite pretty code (that 
   * looks like the clean code of version0).
   * 
   * opti: return a lazy list of possible matchs ?
   * 
   *)
  type tin = Lib_engine.metavars_binding
 (* in fact dont care about 'b *)
  type 'b tout = ('b * Lib_engine.metavars_binding) list 

 (* was >&&> *)
  let (>>=) m1 m2 = fun binding ->
    let xs = m1 binding in
    let xxs = xs +> List.map (fun (x, binding) -> m2 x binding) in
    List.flatten xxs

  let (>||>) m1 m2 = fun binding ->
    m1 binding ++  m2 binding

  (* An exclusive or (xor). *)
  (*
    let (>|+|>) m1 m2 = fun binding -> 
    let xs = m1 binding in
    if null xs
    then m2 binding
    else xs
    
  *)

  let return res = fun binding -> 
    [res, binding]
  let fail = fun binding -> 
    []

  (***************************************************************************)
  (* Tokens *) 
  (***************************************************************************)

  let tokenf_one ia ib = fun binding -> 
    [ib, binding]

  (***************************************************************************)
  (* Environment *) 
  (***************************************************************************)

  (* pre: if have declared a new metavar that hide another one, then
   * must be passed with a binding that deleted this metavar *)
  let check_add_metavars_binding inherited = fun (k, valu) binding -> 
    (match Common.optionise (fun () -> binding +> List.assoc k) with
    | Some (valu') ->
        if
          (match valu, valu' with
          | Ast_c.MetaIdVal a, Ast_c.MetaIdVal b -> a =$= b
          | Ast_c.MetaFuncVal a, Ast_c.MetaFuncVal b -> a =$= b
          | Ast_c.MetaLocalFuncVal a, Ast_c.MetaLocalFuncVal b -> 
              (* do something more ? *)
              a =$= b

          (* al_expr before comparing !!! and accept when they match.
           * Note that here we have Astc._expression, so it is a match
           * modulo isomorphism (there is no metavariable involved here,
           * just isomorphisms). => TODO call isomorphism_c_c instead of
           * =*=. Maybe would be easier to transform ast_c in ast_cocci
           * and call the iso engine of julia. *)
          | Ast_c.MetaExprVal a, Ast_c.MetaExprVal b -> 
              Abstract_line_c.al_expr a =*= Abstract_line_c.al_expr b
          | Ast_c.MetaStmtVal a, Ast_c.MetaStmtVal b -> 
              Abstract_line_c.al_statement a =*= Abstract_line_c.al_statement b
          | Ast_c.MetaTypeVal a, Ast_c.MetaTypeVal b -> 
              Abstract_line_c.al_type a =*= Abstract_line_c.al_type b

          | Ast_c.MetaExprListVal a, Ast_c.MetaExprListVal b -> 
              failwith "not handling MetaExprListVal"
          | Ast_c.MetaParamVal a, Ast_c.MetaParamVal b -> 
              failwith "not handling MetaParamVal"
          | Ast_c.MetaParamListVal a, Ast_c.MetaParamListVal b -> 
              failwith "not handling MetaParamListVal"
          | Ast_c.MetaTextVal a, Ast_c.MetaTextVal b -> a =$= b
          | _ -> raise Impossible
          ) 
        then [binding]
        else []

    | None -> 
        if inherited then []
        else 
          let valu' = 
            (match valu with
            | Ast_c.MetaIdVal a        -> Ast_c.MetaIdVal a
            | Ast_c.MetaFuncVal a      -> Ast_c.MetaFuncVal a
            | Ast_c.MetaLocalFuncVal a -> Ast_c.MetaLocalFuncVal a (* more ? *)
            | Ast_c.MetaExprVal a -> Ast_c.MetaExprVal (Abstract_line_c.al_expr a)
            | Ast_c.MetaStmtVal a -> Ast_c.MetaStmtVal (Abstract_line_c.al_statement a)
            | Ast_c.MetaTypeVal a -> Ast_c.MetaTypeVal (Abstract_line_c.al_type a)
            | Ast_c.MetaExprListVal a ->  failwith "not handling MetaExprListVal"
            | Ast_c.MetaParamVal a ->     failwith "not handling MetaParamVal"
            | Ast_c.MetaParamListVal a -> failwith "not handling MetaParamListVal"
            | Ast_c.MetaTextVal s -> Ast_c.MetaTextVal s
            ) 
          in
          [binding +> Common.insert_assoc (k, valu')]
    )


  let envf inherited = fun (k, valu) binding -> 
    check_add_metavars_binding inherited (k, valu) binding
      +> List.map (fun x -> valu, x)


  (***************************************************************************)
  (* Misc *) 
  (***************************************************************************)

  type 'b tdistr = 'b -> 'b
  let distrf distrop mck x = fun binding -> 
    [x, binding]
  let distrf_e x = x
  let distrf_type x = x
  let distrf_node x = x
  let distrf_args x = x

end



module MATCH  = Cocci_vs_c.COCCI_VS_C (XMATCH)


let match_re_node2 a b binding = 
  MATCH.rule_elem_node a b binding +> List.map snd

(* subtil, 3 args, otherwise profile nothing *)
let match_re_node a b c = 
  Common.profile_code "Pattern2.match_re_node" (fun () -> match_re_node2 a b c)


