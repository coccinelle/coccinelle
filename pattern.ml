open Common open Commonop

let (=$=) = fun s1 s2 -> s1 = s2

module A = Ast_cocci
module B = Ast_c
module F = Control_flow_c

(*
let unmcode = function
  | e, A.MINUS ( _, _) -> e
  | e, A.CONTEXT ( _, _) -> e
  | e, A.PLUS _ -> internal_error "cant have PLUS in Final ast_cocci"
*)

let undots = function
  | A.DOTS e -> e
  | A.CIRCLES e -> e
  | A.STARS e -> e

(******************************************************************************)

type metavar_bindings = {
  mutable metaId: (string,  string) assoc;
  mutable metaFunc: (string, string) assoc;
  } 
let empty_metavar_binding = {
  metaId = [];
  metaFunc = [];
} 

(* todo?: differentiate more the type, so forbid mistake such as adding a binding for Metafunc in Metaid  *)
type metavar_binding = 
  | MetaId of (string * string)
  | MetaFunc of (string * string)


(* semiglobal for metavar binding (logic vars) *)
let _sg_metavars_binding = ref empty_metavar_binding


let check_add anassoc k valu = 
  (match optionise (fun () -> (anassoc +> List.find (function (k', _) -> k' = k))) with
      | Some (k', valu') ->
          assert (k = k');
          (valu = valu',  anassoc)
      | None -> 
          (true, anassoc +> insert_assoc (k, valu))
  )
          

let check_add_metavars_binding = function
  | MetaId (s1, s2) -> 
      let (good, binding) = check_add (!_sg_metavars_binding.metaId) s1 s2 in
      !_sg_metavars_binding.metaId <- binding;
      good
          
  | MetaFunc (s1, s2) -> 
      let (good, binding) = check_add (!_sg_metavars_binding.metaFunc) s1 s2 in
      !_sg_metavars_binding.metaFunc <- binding;
      good
  

let with_metaalvars_binding binding f = 
  let oldbinding = !_sg_metavars_binding in
  let _ = _sg_metavars_binding := binding  in
  let res = f _sg_metavars_binding in
  let _ = _sg_metavars_binding := oldbinding in
  res
  

(******************************************************************************)

let rec (match_re_node: Ast_cocci.rule_elem -> Control_flow_c.node -> bool) = fun re (node, s) -> 
  match re, node with
  | _, F.Enter -> false
  | _, F.Exit -> false
  | _, F.Fake -> false
  | _, F.NestedFunCall _ -> raise Todo

  | re, F.Statement st ->      match_re_st   re st
  | re, F.Declaration decl ->  match_re_decl re decl

  | re, F.HeadFunc funcdef -> 
      let (idb, typb, stob, statb, _) = funcdef in
      (match re, typb with
      | A.FunDecl (stoa, ida, _, paramsa, _),   (retb, paramsb, isvaargs, _) -> 
          (
           match ida with
           | (A.Id (ida, mcode)) when ida =$= idb ->   true
           | (A.MetaFunc (ida, mcode)) -> check_add_metavars_binding (MetaFunc (ida, idb))
           | _ -> false
          ) && 
           match_params paramsa paramsb
      | _, _ -> false
      )
      




and match_re_st = fun re st -> 
  match re, st with
  | A.ExprStatement (ep, _),         (B.ExprStatement (Some ec) , ii) -> 
      match_e_e ep ec
  | _, _ -> false




and match_re_decl = fun re decl -> 
  match re, decl with
  | A.Decl (A.UnInit (typa, sa, _)), (B.DeclList (xs, _)) -> 
      xs +> List.exists (function 
        | (Some (sb, iniopt,_), typb, sto), _ ->
            match_t_t  typa typb &&
            match_ident sa sb (* the order is important *)
        | (None, typ, sto), _ -> false
       )
(*
  | Decl (Init (typa, sa, _, expa, _)),  
*)
  | _, _ -> false






and match_e_e = fun ep ec -> 
  match ep, ec with
  | A.Assignment (ea1, (opa, _), ea2),   (B.Assignment (eb1, opb, eb2), ii) -> 
      match_e_e ea1 eb1 &&
      match_e_e ea2 eb2 &&
      equal_assignOp opa  opb

  (* cas general = a MetaIdExpr can match everything ? *)

  | A.Ident ida,                (B.Constant (B.Ident idb) , ii) ->
      match_ident ida idb

  | A.FunCall (ea1, _, eas, _), (B.FunCall (eb1, ebs), ii) -> 
      let eas = undots eas in
      let ebs = ebs +> List.map fst +> List.map (function
        | Left e -> e
        | Right typ -> raise Todo
        ) in

      match_e_e ea1 eb1  &&

      zip eas ebs +> List.for_all (fun (ea, eb) -> match_e_e ea eb)

  | _, _ -> false
      



and match_t_t typa typb = 
  match typa, typb with
  | A.StructUnionName ((sa,_), (sua, _)),    (qu, (B.StructUnionName ((sb,_), sub), _)) -> 
      equal_structUnion sua sub &&
      sa =$= sb
  | A.Pointer (typa, _),            (qu, (B.Pointer typb, _)) -> 
      match_t_t typa typb

  | A.BaseType ((basea, mcode), signopt),   (qu, (B.BaseType baseb, _)) -> 
      (match basea, baseb with
      | A.CharType,  B.IntType B.CChar -> true
      | A.IntType,   B.IntType (B.Si (sign, B.CInt)) -> true
      | x, y -> 
          let _ = 1+2 in
          false
      )
  | A.TypeName (sa, _),  (qu, (B.TypeName sb, _)) when sa =$= sb -> true
  | _, _ -> false


and match_params pas pbs = 
  (* todo: if contain metavar ? => recurse on two list and consomme *)
  let pas' = pas +> undots +> List.filter (function A.Param (x,y,z) -> true | _ -> false) in
  List.length pas' = List.length pbs &&
  zip pas' pbs +> List.for_all (function
    | A.Param (ida, qua, typa), ((hasreg, idb, typb, _), ii) -> 
        match_t_t typa typb &&
        match_ident ida idb  (* the order is important *)
    | x -> error_cant_have x
    )



and match_ident ida idb = 
  match ida with
  | (A.Id (ida, _)) when ida =$= idb ->   true
  | (A.MetaId (ida, _)) -> check_add_metavars_binding (MetaId (ida, idb))
  | _ -> false




(******************************************************************************)
(* normally Ast_cocci  could reuse some types of Ast_c, so those functions
   should be useless *)
(******************************************************************************)

and equal_assignOp a b = 
  match a, b with
  | A.SimpleAssign, B.SimpleAssign -> true
  | A.OpAssign a,   B.OpAssign b -> 
      equal_arithOp a b
  | _ -> false


and equal_arithOp a b = 
  match a, b with
  | A.Plus     , B.Plus     -> true
  | A.Minus    , B.Minus    -> true
  | A.Mul      , B.Mul      -> true
  | A.Div      , B.Div      -> true
  | A.Mod      , B.Mod      -> true
  | A.DecLeft  , B.DecLeft  -> true
  | A.DecRight , B.DecRight -> true
  | A.And      , B.And      -> true
  | A.Or       , B.Or       -> true
  | A.Xor      , B.Xor      -> true
  | _          , _          -> false

and equal_structUnion a b = 
  match a, b with
  | A.Struct, B.Struct -> true
  | A.Union,  B.Union -> true
  | _, _ -> false
