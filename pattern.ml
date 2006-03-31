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
  metaId: (string,  string) assoc;
  metaFunc: (string, string) assoc;
  metaExpr: (string, Ast_c.expression) assoc;
  metaExprList: (string, Ast_c.expression list) assoc;
  metaType: (string, Ast_c.fullType) assoc;
  metaStmt: (string, Ast_c.statement) assoc;
  } 

let empty_metavars_binding = {
  metaId = [];
  metaFunc = [];
  metaExpr = [];
  metaExprList = [];
  metaType = [];
  metaStmt = [];
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
 empty list mean failure (let matchfailure = [])
 to be able to have pretty code, have to use partial application powa, and so the type is in fact
  type ('a, 'b) matcher =  'a -> 'b -> binding -> binding list
 then by defining the correct combinators, can have quite pretty code (that looks like the 
   clean code of version0)

opti: return a lazy list of possible matchs ?
*)

type ('a, 'b) matcher = 'a -> 'b -> metavars_binding -> metavars_binding list

(* monad like stuff
   src: the papers on parser combinators in haskell (cf a pearl by meijer in ICFP I believe)
 *)

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
  | MetaExprList of (string * Ast_c.expression list)
  | MetaType of (string * Ast_c.fullType)
  | MetaStmt of (string * Ast_c.statement)

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

(* todo, aa_expr  before comparing !!! 
   and maybe accept when they match ?
   note that here we have Astc._expression, so it is a match modulo isomorphism
   (there is not metavariable involved here,  just isomorphisms)
*)
  | MetaExpr (s1, s2) -> 
      let (good, newbinding) = check_add s1 s2 (=) binding.metaExpr in
      if good 
      then _GoodMatch {binding with metaExpr = newbinding}
      else _MatchFailure
  | MetaExprList (s1, s2) -> 
      let (good, newbinding) = check_add s1 s2 (=) binding.metaExprList in
      if good 
      then _GoodMatch {binding with metaExprList = newbinding}
      else _MatchFailure

(* todo, aa_type  before comparing !!! 
   and maybe accept when they match modulo iso ? not just =
*)
  | MetaType (s1, s2) -> 
      let (good, newbinding) = check_add s1 s2 (=) binding.metaType in
      if good 
      then _GoodMatch {binding with metaType = newbinding}
      else _MatchFailure

(* todo, aa_stmt  before comparing !!! 
   and maybe accept when they match modulo iso ? not just =
*)
  | MetaStmt (s1, s2) -> 
      let (good, newbinding) = check_add s1 s2 (=) binding.metaStmt in
      if good 
      then _GoodMatch {binding with metaStmt = newbinding}
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
            (* todo: a MetaId can not match a func ? 
               todo: MetaFuncLocal ? (in fact it should be MetaFuncLocal here, and question is does MetaFunc match a func decl ? 
               todo: as usual, handle the Opt/Unique/Multi
            *)
           | _ -> return false
          ) 
           >&&>
           (* todo: stoa vs stob *)
           (* todo: isvaargs ? retb ? *)
           (match_params paramsa paramsb)
          

      | _, _ -> return false
      )
      




and (match_re_st: (Ast_cocci.rule_elem, Ast_c.statement) matcher)  = fun re st -> 
  match re, st with

  (* cas general: a Meta can match everything *)
  | A.MetaStmt ((ida,_)),  stb -> 
      check_add_metavars_binding (MetaStmt (ida, stb))


  | A.ExprStatement (ep, _),         (B.ExprStatement (Some ec) , ii) -> 
      match_e_e ep ec


  (* I have just to manage the node itself, so just the head of the if/while/for,  not its body *)
  | A.IfHeader (_,_, ea, _), (B.Selection  (B.If (eb, st1b, st2b)), ii) -> 
      match_e_e ea eb

  | A.WhileHeader (_, _, ea, _), (B.Iteration  (B.While (eb, stb)), ii) -> 
      match_e_e ea eb

  | A.ForHeader (_, _, ea1opt, _, ea2opt, _, ea3opt, _), (B.Iteration  (B.For ((eb1opt,_), (eb2opt,_), (eb3opt,_), stb)), ii) -> 
      match_opt match_e_e ea1opt eb1opt >&&>
      match_opt match_e_e ea2opt eb2opt >&&>
      match_opt match_e_e ea3opt eb3opt >&&>
      return true
      
      

      



  (* todo: Else, Do WhileTail, *)

  (* todo: Return, ReturnExpr => have such node in Ast ? *)



  (* not me?: SeqStart SeqEnd ? *)

  (* not me?: MetaStmList ? *)

  (* not me?: Disj *)
  

  (* Nest ? *)
  (* Exp ? *)


  (* not me: Dots/Circles/Stars *)
  (* todo: Opt/Unique/Multi *)
  | _, _ -> return false




and (match_re_decl: (Ast_cocci.rule_elem, Ast_c.declaration) matcher) = fun re decl -> 
  match re, decl with
  | A.Decl (A.UnInit (typa, sa, _)), (B.DeclList (xs, _)) -> 
      xs +> List.fold_left (fun acc var -> 
        acc >||>
        (match var with
        | (Some (sb, iniopt,_), typb, sto), _ ->
         (* isomorphisms handled here, good?  cos allow an initializer (iniopt) where a SP does not mention one *)
         (* todo, use sto? lack of sto in Ast_cocci *)
            match_t_t  typa typb >&&>
            match_ident sa sb
        | (None, typ, sto), _ -> return false
        )
       ) (return false)
(* todo:
  | Decl (Init (typa, sa, _, expa, _)),  
*)

  (* todo: Opt/Unique/Multi *)
  | _, _ -> return false






and (match_e_e: (Ast_cocci.expression, Ast_c.expression) matcher) = fun ep ec -> 
  match ep, ec with
  (* cas general: a MetaExpr can match everything *)
  | A.MetaExpr ((ida,_), opttypa),  expb -> 
      (* todo: use type *)
      check_add_metavars_binding (MetaExpr (ida, expb))

  (* todo: MetaConst *)

  | A.Ident ida,                (B.Constant (B.Ident idb) , ii) ->
      match_ident ida idb

 (* todo: handle some isomorphisms in int/float ? can have different format 1l can match a 1 *)
 (* todo: normally string can contain some metavar too, so should recurse on the string *)
  | A.Constant (A.String sa, _),  (B.Constant (B.String (sb, _)), ii)    when sa =$= sb -> return true
  | A.Constant (A.Char sa, _),    (B.Constant (B.Char   (sb, _)), ii)    when sa =$= sb -> return true
  | A.Constant (A.Int sa, _),     (B.Constant (B.Int    (sb)), ii)       when sa =$= sb -> return true
  | A.Constant (A.Float sa, _),   (B.Constant (B.Float  (sb, ftyp)), ii) when sa =$= sb -> return true

  | A.FunCall (ea1, _, eas, _), (B.FunCall (eb1, ebs), ii) -> 
     (* todo: do special case to allow IdMetaFunc, cos doing the recursive call will be too late,
          match_ident will not have the info whether it was a function
        todo: but how detect when do x.field = f;  how know that f is a Func ?
          by having computed some information before the matching
     *)

      match_e_e ea1 eb1  >&&> (

      (* for the pattern phase, no need the EComma *)
      let eas' = undots eas +> List.filter (function A.EComma _ -> false | _ -> true) in
      let ebs' = ebs +> List.map fst +> List.map (function
        | Left e -> e
        | Right typ -> raise Todo
        ) in
      match_arguments (match eas with A.DOTS _ -> Ordered | A.CIRCLES _ -> Unordered | A.STARS _ -> raise Todo)
        eas' ebs'
     )

  | A.Assignment (ea1, (opa, _), ea2),   (B.Assignment (eb1, opb, eb2), ii) -> 
      return (equal_assignOp opa  opb) >&&>
      (match_e_e ea1 eb1 >&&>  match_e_e ea2 eb2) 


  | A.CondExpr (ea1, _, ea2opt, _, ea3), (B.CondExpr (eb1, eb2, eb3), ii) -> 

      match_e_e ea1 eb1 >&&>
      (match ea2opt, eb2 with
      | None, (B.NoExpr, ii) -> return true
      | Some ea2, _ -> match_e_e ea2 eb2
      | _,_ -> return false
      ) >&&>
      match_e_e ea3 eb3
   
  (* todo?: handle some isomorphisms here ? *)

  | A.Postfix (ea, (opa,_)), (B.Postfix (eb, opb), ii) -> 
      return (equal_fixOp opa opb) >&&>
      match_e_e ea eb

  | A.Infix (ea, (opa,_)), (B.Infix (eb, opb), ii) -> 
      return (equal_fixOp opa opb) >&&>
      match_e_e ea eb

  | A.Unary (ea, (opa,_)), (B.Unary (eb, opb), ii) -> 
      return (equal_unaryOp opa opb) >&&>
      match_e_e ea eb

  | A.Binary (ea1, (opa,_), ea2), (B.Binary (eb1, opb, eb2), ii) -> 
      return (equal_binaryOp opa opb) >&&>
      match_e_e ea1 eb1 >&&> 
      match_e_e ea2 eb2

        
  (* todo?: handle some isomorphisms here ?  (with pointers = Unary Deref) *)

  | A.ArrayAccess (ea1, _, ea2, _), (B.ArrayAccess (eb1, eb2), ii) -> 
      match_e_e ea1 eb1 >&&>
      match_e_e ea2 eb2


  (* todo?: handle some isomorphisms here ? *)

  | A.RecordAccess (ea, _, ida), (B.RecordAccess (eb, idb), ii) ->
      match_e_e ea eb >&&>
      match_ident ida idb

  | A.RecordPtAccess (ea, _, ida), (B.RecordPtAccess (eb, idb), ii) ->
      match_e_e ea eb >&&>
      match_ident ida idb

  (* todo?: handle some isomorphisms here ? *)

  | A.Cast (_, typa, _, ea), (B.Cast (typb, eb), ii) ->
      match_t_t typa typb >&&>
      match_e_e ea eb

  (* todo? iso ? allow all the combinations ? *)

  | A.Paren (_, ea, _), (B.ParenExpr (eb), ii) -> 
      match_e_e ea eb



  | A.MetaExprList _, _   -> raise Impossible (* only in arg lists *)

  | A.EComma _, _   -> raise Impossible (* can have EComma only in arg lists *)

  (* todo: in fact can also have the Edots family inside nest, as in if(<... x ... y ...>) *)
  | A.Edots _, _    -> raise Impossible (* can have EComma only in arg lists *)
  | A.Ecircles _, _ -> raise Impossible (* can have EComma only in arg lists *)
  | A.Estars _, _   -> raise Impossible (* can have EComma only in arg lists *)


  | A.DisjExpr eas, eb -> 
      eas +> List.fold_left (fun acc ea -> acc >||>  match_e_e ea eb) (return false)

  (* todo: Nest *)      

  (* todo: Opt/Unique/Multi *)

  | _, _ -> return false


and (match_arguments: sequence_processing_style -> (Ast_cocci.expression list, Ast_c.expression list) matcher) = fun seqstyle eas ebs ->
(* old:
      if List.length eas = List.length ebs
      then
        (zip eas ebs +> List.fold_left (fun acc (ea, eb) -> acc >&&> match_e_e ea eb) (return true))
      else return false
*)
(* todo: MetaExprList *)
  match seqstyle with
  | Ordered -> 
      (match eas, ebs with
      | [], [] -> return true
      | [], y::ys -> return false
      | x::xs, ys -> 
          (match x, ys with
          | A.Edots (_, optexpr), ys -> 
              (* todo: if optexpr, then a WHEN and so may have to filter yys *)
              let yys = Common.tails ys in (* '...' can take more or less the beginnings of the arguments *)
              yys +> List.fold_left (fun acc ys -> 
                acc >||>  match_arguments seqstyle xs ys
                  ) (return false)

          | A.Ecircles (_,_), ys -> raise Impossible (* in Ordered mode *)
          | A.Estars (_,_), ys   -> raise Impossible (* in Ordered mode *)

          | A.MetaExprList (ida,_), ys -> 
              let startendxs = (Common.zip (Common.inits ys) (Common.tails ys)) in
              startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
                acc >||> (
                check_add_metavars_binding (MetaExprList (ida, startxs)) >&&>
                match_arguments seqstyle xs endxs
             )) (return false)

              

          | x, y::ys -> 
              match_e_e x y >&&> 
              match_arguments seqstyle xs ys
          | x, [] -> return false
          )
      )
  | Unordered -> raise Todo



and (match_t_t: (Ast_cocci.fullType, Ast_c.fullType) matcher) = fun   typa typb -> 
  match typa, typb with

  (* cas general *)
  | A.MetaType (ida,_),  typb -> 
      check_add_metavars_binding (MetaType (ida, typb))

  | A.BaseType ((basea, mcode), signaopt),   (qu, (B.BaseType baseb, _)) -> 
      let match_sign signa signb = 
        (match signa, signb with
         (* iso on sign, if not mentionned then free.  tochange? *)
        | None, _ -> return true
        | Some (a,_), b -> return (equal_sign a b)
        ) in


      (* handle some iso on type ? (cf complex C rule for possible implicit casting) *)
      (match basea, baseb with
      | A.VoidType,  B.Void -> assert (signaopt = None); return true
      | A.CharType,  B.IntType B.CChar -> 
          (* todo?: also match signed CChar2 ? *)
          return true
      | A.ShortType, B.IntType (B.Si (signb, B.CShort)) -> match_sign signaopt signb
      | A.IntType,   B.IntType (B.Si (signb, B.CInt))   -> match_sign signaopt signb
      | A.LongType,  B.IntType (B.Si (signb, B.CLong))  -> match_sign signaopt signb
      | A.FloatType, B.FloatType (B.CFloat) -> 
          assert (signaopt = None); (* no sign on float in C *)
          return true
      | A.DoubleType, B.FloatType (B.CDouble) -> 
          assert (signaopt = None); (* no sign on float in C *)
          return true
          
          
      | x, y -> return false
      )

  (* todo? iso with array *)
  | A.Pointer (typa, _),            (qu, (B.Pointer typb, _)) -> 
      match_t_t typa typb

  | A.Array (typa, _, eaopt, _), (qu, (B.Array (ebopt, typb), _)) -> 
      match_t_t typa typb >&&>
      (match eaopt, ebopt with
       (* todo: handle the iso on optionnal size specifification ? *)
      | None, None -> return true
      | Some ea, Some eb -> match_e_e ea eb
      | _, _ -> return false
      )


  | A.StructUnionName ((sa,_), (sua, _)),    (qu, (B.StructUnionName ((sb,_), sub), _)) -> 
     (* todo: could also match a Struct that has provided a name *)
     return (equal_structUnion sua sub &&  sa =$= sb)

  | A.TypeName (sa, _),  (qu, (B.TypeName sb, _)) when sa =$= sb -> return true


  (* todo: Opt/Unique/Multi *)
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
        match_ident ida idb  
    | x -> error_cant_have x
    ) (return true)
  )
  else return false



and (match_ident: (Ast_cocci.ident, string) matcher) = fun ida idb -> 
  match ida with
  | (A.Id (ida, _)) when ida =$= idb -> return true
  | (A.MetaId (ida, _)) ->  check_add_metavars_binding (MetaId (ida, idb))

  (* todo: and other cases ? too late ? or need more info on idb !! its type ? *)

  (* todo: Opt/Unique/Multi *)
  | _ -> return false




and match_opt f eaopt ebopt =
      (match eaopt, ebopt with
      | None, None -> return true
      | Some ea, Some eb -> f ea eb
      | _, _ -> return false
      )

(******************************************************************************)
(* normally Ast_cocci  should reuse some types of Ast_c, 
   so those functions  should not exists *)
(******************************************************************************)

and equal_unaryOp a b = 
  match a, b with
  | A.GetRef   , B.GetRef  -> true
  | A.DeRef    , B.DeRef   -> true
  | A.UnPlus   , B.UnPlus  -> true
  | A.UnMinus  , B.UnMinus -> true
  | A.Tilde    , B.Tilde   -> true
  | A.Not      , B.Not     -> true
  | _, _ -> false


and equal_assignOp a b = 
  match a, b with
  | A.SimpleAssign, B.SimpleAssign -> true
  | A.OpAssign a,   B.OpAssign b -> 
      equal_arithOp a b
  | _ -> false


and equal_fixOp a b = 
  match a, b with
  | A.Dec, B.Dec -> true
  | A.Inc, B.Inc -> true
  | _ -> false

and equal_binaryOp a b = 
  match a, b with
  | A.Arith a,    B.Arith b ->   equal_arithOp a b
  | A.Logical a,  B.Logical b -> equal_logicalOp a b
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

and equal_logicalOp a b = 
  match a, b with
  | A.Inf    , B.Inf    -> true
  | A.Sup    , B.Sup    -> true
  | A.InfEq  , B.InfEq  -> true
  | A.SupEq  , B.SupEq  -> true
  | A.Eq     , B.Eq     -> true
  | A.NotEq  , B.NotEq  -> true
  | A.AndLog , B.AndLog -> true
  | A.OrLog  , B.OrLog  -> true
  | _          , _          -> false
  


and equal_structUnion a b = 
  match a, b with
  | A.Struct, B.Struct -> true
  | A.Union,  B.Union -> true
  | _, _ -> false


and equal_sign a b = 
  match a, b with
  | A.Signed, B.Signed -> true
  | A.Unsigned,  B.UnSigned -> true
  | _, _ -> false
