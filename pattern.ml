open Common open Commonop

module A = Ast_cocci
module B = Ast_c
module F = Control_flow_c

(******************************************************************************)

(******************************************************************************)

let ((=$=): string -> string -> bool) = fun s1 s2 -> s1 = s2

let undots = function
  | A.DOTS    e -> e
  | A.CIRCLES e -> e
  | A.STARS   e -> e

type sequence_processing_style = Ordered | Unordered

(******************************************************************************)

type metavars_binding = {
  mutable metaId: (string,  string) assoc;
  mutable metaFunc: (string, string) assoc;
  mutable metaExpr: (string, Ast_c.expression) assoc;
  } 

let empty_metavars_binding = {
  metaId = [];
  metaFunc = [];
  metaExpr = [];
} 


(*
version0: (Ast_cocci.rule_elem -> Control_flow_c.node -> bool)
  type ('a, 'b) matcher = 'a -> 'b -> bool
version1: same but with a global variable holding the current binding
 BUT bug
  - can have multiple possibilities
  - globals sux
  - sometimes have to undo, cos if start match, then it binds, and if later
     it does not match, then must undo the first binds
    ex: when patch parameters, can  try to match, but then we found far later that the last argument 
       of a function does not match
        => have to undo the binding !!!
     (can handle that too with a global, by saving the global, ... but sux)
   => better not use global
version2: (binding -> Ast_cocci.rule_elem -> Control_flow_c.node -> binding list)
  type ('a, 'b) matcher = binding -> 'a -> 'b -> binding list
 empty list mean failure
 let matchfailure = []
 in fact to be able to have prettier code, have to use partial application powa, and so the type is in fact
  type ('a, 'b) matcher =  'a -> 'b -> binding -> binding list
 then by defining the correct combinators, can have quite pretty code (that looks like the 
   clean code of the first version

opti: return a lazy list of possible matchs ?
*)

type ('a, 'b) matcher = 'a -> 'b -> metavars_binding -> metavars_binding list

(* monad like stuff *)

let (>&&>) m1 m2 = fun binding ->
  let xs = m1 binding in
  let xxs = xs +> List.map (fun binding -> m2 binding) in
  List.flatten xxs

let (>||>) m1 m2 = fun binding ->
  m1 binding ++  m2 binding

let return res = fun binding -> 
  match res with
  | false -> []
  | true -> [binding]

(* other combinator for choice ?
   there is 2 or ? Or and Xor ? (the Disj for instance seems to be a Xor)
    in fact it is a Xor I think everytime
*)




(******************************************************************************)

let check_add k valu (===) anassoc = 
  (match optionise (fun () -> (anassoc +> List.find (function (k', _) -> k' = k))) with
      | Some (k', valu') ->
          assert (k = k');
          (valu === valu',  anassoc)
      | None -> 
          (true, anassoc +> insert_assoc (k, valu))
  )
          

(* old: semiglobal for metavar binding (logic vars) *)
(* old:
let _sg_metavars_binding = ref empty_metavar_binding

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
*)

(* todo?: differentiate more the type, so forbid mistake such as adding a binding for Metafunc in Metaid  *)
type metavar_binding = 
  | MetaId   of (string * string)
  | MetaFunc of (string * string)
  | MetaExpr of (string * Ast_c.expression)

let _MatchFailure = []
let _GoodMatch binding = [binding]

let check_add_metavars_binding = fun addon binding -> 
  match addon with
  | MetaId (s1, s2) -> 
      let (good, newbinding) = check_add s1 s2 (=$=) binding.metaId in
      if good 
      then _GoodMatch {binding with metaId = newbinding}
      else _MatchFailure
          
  | MetaFunc (s1, s2) -> 
      let (good, newbinding) = check_add s1 s2 (=$=) binding.metaFunc in
      if good 
      then _GoodMatch {binding with metaFunc = newbinding}
      else _MatchFailure

(* todo, aa_expr  before comparing !!! *)
  | MetaExpr (s1, s2) -> 
      let (good, newbinding) = check_add s1 s2 (=) binding.metaExpr in
      if good 
      then _GoodMatch {binding with metaExpr = newbinding}
      else _MatchFailure

(******************************************************************************)

let rec (match_re_node: (Ast_cocci.rule_elem, Control_flow_c.node) matcher) = fun re (node, s) -> 
  match re, node with
  | _, F.Enter -> return false
  | _, F.Exit -> return false
  | _, F.Fake -> return false
  | _, F.NestedFunCall _ -> raise Todo

  | re, F.Statement st ->     match_re_st   re st
  | re, F.Declaration decl -> match_re_decl re decl

  | re, F.HeadFunc funcdef -> 
      let (idb, typb, stob, statb, _) = funcdef in
      (match re, typb with
      | A.FunDecl (stoa, ida, _, paramsa, _),   (retb, paramsb, isvaargs, _) -> 
          ( match ida with
           | (A.Id (ida, mcode)) when ida =$= idb ->   return true
           | (A.MetaFunc (ida, mcode)) -> check_add_metavars_binding (MetaFunc (ida, idb))
           | _ -> return false
          ) 
           >&&>
           (match_params paramsa paramsb)
          

      | _, _ -> return false
      )
      




and (match_re_st: (Ast_cocci.rule_elem, Ast_c.statement) matcher)  = fun re st -> 
  match re, st with
  | A.ExprStatement (ep, _),         (B.ExprStatement (Some ec) , ii) -> 
      match_e_e ep ec
  | _, _ -> return false




and (match_re_decl: (Ast_cocci.rule_elem, Ast_c.declaration) matcher) = fun re decl -> 
  match re, decl with
  | A.Decl (A.UnInit (typa, sa, _)), (B.DeclList (xs, _)) -> 
      xs +> List.fold_left (fun acc var -> 
        acc >||>
        (match var with
        | (Some (sb, iniopt,_), typb, sto), _ ->
            match_t_t  typa typb >&&>
            match_ident sa sb (* the order is still important ? *)
        | (None, typ, sto), _ -> return false
        )
       ) (return false)
(*
  | Decl (Init (typa, sa, _, expa, _)),  
*)
  | _, _ -> return false






and (match_e_e: (Ast_cocci.expression, Ast_c.expression) matcher) = fun ep ec -> 
  match ep, ec with
  | A.Assignment (ea1, (opa, _), ea2),   (B.Assignment (eb1, opb, eb2), ii) -> 
      if equal_assignOp opa  opb
      then (match_e_e ea1 eb1 >&&>  match_e_e ea2 eb2) 
      else return false
            

  (* cas general = a MetaExpr can match everything *)
  | A.MetaExpr ((ida,_), opttypa),  expb -> 
      check_add_metavars_binding (MetaExpr (ida, expb))

  | A.Ident ida,                (B.Constant (B.Ident idb) , ii) ->
      match_ident ida idb

  | A.Constant (A.String sa, _),  (B.Constant (B.String (sb, _)), ii) when sa =$= sb -> return true
  | A.Constant (A.Char sa, _),    (B.Constant (B.Char (sb, _)), ii) when sa =$= sb -> return true
  | A.Constant (A.Int sa, _),     (B.Constant (B.Int (sb)), ii) when sa =$= sb -> return true
  | A.Constant (A.Float sa, _),   (B.Constant (B.Float (sb, ftyp)), ii) when sa =$= sb -> return true

  | A.FunCall (ea1, _, eas, _), (B.FunCall (eb1, ebs), ii) -> 

      match_e_e ea1 eb1  >&&> (

      let eas' = undots eas +> List.filter (function A.EComma _ -> false | _ -> true) in
      let ebs' = ebs +> List.map fst +> List.map (function
        | Left e -> e
        | Right typ -> raise Todo
        ) in
      match_arguments (match eas with A.DOTS _ -> Ordered | A.CIRCLES _ -> Unordered | A.STARS _ -> raise Todo)
        eas' ebs'
     )

  | A.EComma _, _ -> raise Impossible (* can have EComma only in arg lists *)
  | _, _ -> return false


and (match_arguments: sequence_processing_style -> (Ast_cocci.expression list, Ast_c.expression list) matcher) = fun seqstyle eas ebs ->
(* old:
      if List.length eas = List.length ebs
      then
        (zip eas ebs +> List.fold_left (fun acc (ea, eb) -> acc >&&> match_e_e ea eb) (return true))
      else return false
*)
  match seqstyle with
  | Ordered -> 
      (match eas, ebs with
      | [], [] -> return true
      | [], y::ys -> return false
      | x::xs, ys -> 
          (match x, ys with
          | A.Edots (_, optexpr), ys -> 
              let yys = Common.tails ys in
              yys +> List.fold_left (fun acc ys -> 
                acc >||>  match_arguments seqstyle xs ys
                  ) (return false)

          | A.Ecircles (_,_), ys -> raise Impossible (* in Ordered mode *)
          | A.Estars (_,_), ys -> raise Impossible (* in Ordered mode *)
          | x, y::ys -> 
              match_e_e x y >&&> 
              match_arguments seqstyle xs ys
          | x, [] -> return false
          )
      )
  | Unordered -> raise Todo



and (match_t_t: (Ast_cocci.fullType, Ast_c.fullType) matcher) = fun   typa typb -> 
  match typa, typb with
  | A.StructUnionName ((sa,_), (sua, _)),    (qu, (B.StructUnionName ((sb,_), sub), _)) -> 
     if equal_structUnion sua sub &&  sa =$= sb
     then return true
     else return false
  | A.Pointer (typa, _),            (qu, (B.Pointer typb, _)) -> 
      match_t_t typa typb

  | A.BaseType ((basea, mcode), signopt),   (qu, (B.BaseType baseb, _)) -> 
      (match basea, baseb with
      | A.CharType,  B.IntType B.CChar -> return true
      | A.IntType,   B.IntType (B.Si (sign, B.CInt)) -> return true
      | x, y -> return false
      )
  | A.TypeName (sa, _),  (qu, (B.TypeName sb, _)) when sa =$= sb -> return true
  | _, _ -> return false


and (match_params: (Ast_cocci.parameter_list, ((Ast_c.parameterTypeDef * Ast_c.il) list)) matcher) = fun pas pbs ->
  (* todo: if contain metavar ? => recurse on two list and consomme *)
  let pas' = pas +> undots +> List.filter (function A.Param (x,y,z) -> true | _ -> false) in
  if (List.length pas' = List.length pbs) 
  then
  (zip pas' pbs +> List.fold_left (fun acc param -> 
   match param with
    | A.Param (ida, qua, typa), ((hasreg, idb, typb, _), ii) -> 
        acc >&&>
        match_t_t typa typb >&&>
        match_ident ida idb  (* the order is still important ? *)
    | x -> error_cant_have x
    ) (return true)
  )
  else return false



and (match_ident: (Ast_cocci.ident, string) matcher) = fun ida idb -> 
  match ida with
  | (A.Id (ida, _)) when ida =$= idb -> return true
  | (A.MetaId (ida, _)) ->  check_add_metavars_binding (MetaId (ida, idb))
  | _ -> return false




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
