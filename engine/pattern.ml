open Common open Commonop

module A = Ast_cocci
module B = Ast_c
module F = Control_flow_c

(*****************************************************************************)
type sequence_processing_style = Ordered | Unordered

(* todo? put in semantic_c.ml *)
type semantic_info_ident = 
  | Function 
  | LocalFunction (* entails Function *)
  | DontKnow

let term ((s,_,_) : 'a Ast_cocci.mcode) = s


(*****************************************************************************)

(* 0x0 is equivalent to 0,  value format isomorphism *)
let equal_c_int s1 s2 = 
  try 
    int_of_string s1 = int_of_string s2
  with Failure("int_of_string") -> 
    s1 = s2


(* Normally Ast_cocci should reuse some types of Ast_c, so those
 * functions should not exist.
 * 
 * update: but now Ast_c depends on Ast_cocci, so can't make too
 * Ast_cocci depends on Ast_c, so have to stay with those equal_xxx
 * functions. *)

let equal_unaryOp a b = 
  match a, b with
  | A.GetRef   , B.GetRef  -> true
  | A.DeRef    , B.DeRef   -> true
  | A.UnPlus   , B.UnPlus  -> true
  | A.UnMinus  , B.UnMinus -> true
  | A.Tilde    , B.Tilde   -> true
  | A.Not      , B.Not     -> true
  | _, _ -> false


let equal_arithOp a b = 
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

let equal_logicalOp a b = 
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
  


let equal_assignOp a b = 
  match a, b with
  | A.SimpleAssign, B.SimpleAssign -> true
  | A.OpAssign a,   B.OpAssign b -> equal_arithOp a b
  | _ -> false


let equal_fixOp a b = 
  match a, b with
  | A.Dec, B.Dec -> true
  | A.Inc, B.Inc -> true
  | _ -> false

let equal_binaryOp a b = 
  match a, b with
  | A.Arith a,    B.Arith b ->   equal_arithOp a b
  | A.Logical a,  B.Logical b -> equal_logicalOp a b
  | _ -> false


let equal_structUnion a b = 
  match a, b with
  | A.Struct, B.Struct -> true
  | A.Union,  B.Union -> true
  | _, _ -> false


let equal_sign a b = 
  match a, b with
  | A.Signed,    B.Signed   -> true
  | A.Unsigned,  B.UnSigned -> true
  | _, _ -> false

let equal_storage a b = 
  match a, b with
  | A.Static   , B.Sto B.Static
  | A.Auto     , B.Sto B.Auto
  | A.Register , B.Sto B.Register
  | A.Extern   , B.Sto B.Extern 
      -> true
  | _ -> false




(*****************************************************************************)
(* Binding combinators *)
(*****************************************************************************)
(*
 * version0: 
 *   (Ast_cocci.rule_elem -> Control_flow_c.node -> bool)
 *   type ('a, 'b) matcher = 'a -> 'b -> bool
 * 
 * version1: same but with a global variable holding the current binding
 *  BUT bug
 *   - can have multiple possibilities
 *   - globals sux
 *   - sometimes have to undo, cos if start match, then it binds, and if later
 *      it does not match, then must undo the first binds.
 *     ex: when match parameters, can  try to match, but then we found far 
 *     later that the last argument of a function does not match
 *      => have to undo the binding !!!
 *      (can handle that too with a global, by saving the global, ... but sux)
 *   => better not use global
 * 
 * version2: 
 *  (binding -> Ast_cocci.rule_elem -> Control_flow_c.node -> binding list)
 *   type ('a, 'b) matcher = binding -> 'a -> 'b -> binding list
 *  Empty list mean failure (let matchfailure = []).
 *  To be able to have pretty code, have to use partial application powa, and 
 *  so the type is in fact
 *    type ('a, 'b) matcher =  'a -> 'b -> binding -> binding list
 *  Then by defining the correct combinators, can have quite pretty code (that 
 *   looks like the clean code of version0).
 * 
 * opti: return a lazy list of possible matchs ?
 *)

type ('a, 'b) matcher = 
    'a -> 'b -> Lib_engine.metavars_binding -> Lib_engine.metavars_binding list

(* monad like stuff
 * src: papers on parser combinators in haskell (cf a pearl by meijer in ICFP)
 *)

let (>&&>) m1 m2 = fun binding ->
  let xs = m1 binding in
  let xxs = xs +> List.map (fun binding -> m2 binding) in
  List.flatten xxs


let (>||>) m1 m2 = fun binding ->
  if true then 
    m1 binding ++ m2 binding 
  else 
    let xs = m1 binding in
    if null xs
    then m2 binding
    else xs

(* An exclusive or (xor). *)
(*
let (>|+|>) m1 m2 = fun binding -> 
  let xs = m1 binding in
  if null xs
  then m2 binding
  else xs
*)


let return res = fun binding -> 
  match res with
  | false -> []
  | true -> [binding]



let match_opt f eaopt ebopt =
  match eaopt, ebopt with
  | None, None -> return true
  | Some ea, Some eb -> f ea eb
  | _, _ -> return false


(*****************************************************************************)
(* Metavariable and environments handling *)
(*****************************************************************************)
let _MatchFailure = []
let _GoodMatch binding = [binding]

(* all referenced inherited variables have to be bound.  This would be
naturally checked for the minus or context ones in the matching process, but
have to check the plus ones as well.  The result of get_inherited contains
all of these, but the potential redundant checking for the minus and context
ones is probably not a big deal.  If it's a problem, could fix free_vars to
distinguish between + variables and the other ones. *)
let all_bound l =
  let is_bound inhvar binding =
    match Common.optionise (fun () -> binding +> List.assoc inhvar) with
      Some _ -> _GoodMatch binding
    | None -> _MatchFailure in
  List.fold_left
    (function prev -> function inhvar -> is_bound inhvar >&&> prev)
    (return true) l

(* pre: if have declared a new metavar that hide another one, then must be 
   passed with a binding that deleted this metavar *)
let check_add_metavars_binding keep inherited = fun (k, valu) binding -> 
  if not (keep = A.Unitary)
  then
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
           * and call the iso engine of julia.
        *)
          | Ast_c.MetaExprVal a, Ast_c.MetaExprVal b -> 
              Lib_parsing_c.al_expr a =*= Lib_parsing_c.al_expr b
          | Ast_c.MetaStmtVal a, Ast_c.MetaStmtVal b -> 
              Lib_parsing_c.al_statement a =*= Lib_parsing_c.al_statement b
          | Ast_c.MetaTypeVal a, Ast_c.MetaTypeVal b -> 
              Lib_parsing_c.al_type a =*= Lib_parsing_c.al_type b
		
          | Ast_c.MetaExprListVal a, Ast_c.MetaExprListVal b -> 
              failwith "not handling MetaExprListVal"
          | Ast_c.MetaParamVal a, Ast_c.MetaParamVal b -> 
              failwith "not handling MetaParamVal"
          | Ast_c.MetaParamListVal a, Ast_c.MetaParamListVal b -> 
              failwith "not handling MetaParamListVal"


          | Ast_c.MetaTextVal a, Ast_c.MetaTextVal b -> a =$= b

          | _ -> raise Impossible
		) 
	then _GoodMatch binding
	else _MatchFailure
	    
    | None -> 
	if inherited then _MatchFailure
	else 
          let valu' = 
            (match valu with
            | Ast_c.MetaIdVal a        -> Ast_c.MetaIdVal a
            | Ast_c.MetaFuncVal a      -> Ast_c.MetaFuncVal a
            | Ast_c.MetaLocalFuncVal a -> Ast_c.MetaLocalFuncVal a (* more ? *)
            | Ast_c.MetaExprVal a ->
		Ast_c.MetaExprVal (Lib_parsing_c.al_expr a)
            | Ast_c.MetaStmtVal a ->
		Ast_c.MetaStmtVal (Lib_parsing_c.al_statement a)
            | Ast_c.MetaTypeVal a ->
		Ast_c.MetaTypeVal (Lib_parsing_c.al_type a)
            | Ast_c.MetaExprListVal a ->
		failwith "not handling MetaExprListVal"
            | Ast_c.MetaParamVal a ->
		failwith "not handling MetaParamVal"
            | Ast_c.MetaParamListVal a ->
		failwith "not handling MetaParamListVal"
            | Ast_c.MetaTextVal s -> Ast_c.MetaTextVal s
            )
	  in _GoodMatch (binding +> Common.insert_assoc (k, valu')))
  else _GoodMatch binding
      
(*****************************************************************************)
(* The pattern functions, "Cocci vs C" *) 
(*****************************************************************************)
      
let rec (match_e_e: (Ast_cocci.expression,Ast_c.expression) matcher) = 
  fun ep ec ->
    all_bound (A.get_inherited ep) >&&>
  match A.unwrap ep, ec with
  
  (* cas general: a MetaExpr can match everything *)
  | A.MetaExpr (ida,keep,opttypa,inherited), (((expr, opttypb), ii) as expb) ->
      (match opttypa, opttypb with
      | None, _ -> return true
      | Some (tas : Type_cocci.typeC list), Some tb -> 
          tas +>
	  List.fold_left
	    (fun acc ta -> acc >||>  return (Types.compatible_type ta tb)) 
            (return false)
      | Some _, None -> 
          pr2 ("I don't have any type information. Certainly a pb in " ^
                    "type_annoter_c.ml");
          return false
      ) >&&>
      check_add_metavars_binding keep inherited
	(term ida,Ast_c.MetaExprVal(expb))


  (* old: | A.Edots _, _ -> raise Impossible
     In fact now can also have the Edots inside normal expression, 
     not just in arg lists.
     in 'x[...];'  
     less: in if(<... x ... y ...>) *)
  | A.Edots (_, None), _    -> return true
  | A.Edots (_, Some expr), _    -> failwith "not handling when on Edots"


  | A.MetaConst _, _ -> failwith "not handling MetaConst"
  | A.MetaErr _, _ -> failwith "not handling MetaErr"

  | A.Ident ida,                (((B.Ident idb) , typ), ii) ->
      match_ident DontKnow ida idb

 (* todo: handle some isomorphisms in int/float ? can have different format :
  *   1l can match a 1.
  * TODO: normally string can contain some metavar too, so should recurse on 
  *  the string 
  *)
  | A.Constant (A.String sa,_,_),  ((B.Constant (B.String (sb, _)), typ),ii)  
    when sa =$= sb -> return true
  | A.Constant (A.Char sa,_,_),    ((B.Constant (B.Char   (sb, _)), typ),ii)
    when sa =$= sb -> return true
  | A.Constant (A.Int sa,_,_),     ((B.Constant (B.Int    (sb)), typ),ii)
    when equal_c_int sa sb -> return true
  | A.Constant (A.Float sa,_,_),   ((B.Constant (B.Float  (sb, ftyp)), typ),ii)
    when sa =$= sb -> return true

  | A.FunCall (ea1, _, eas, _), ((B.FunCall (eb1, ebs), typ),ii) -> 
     (* todo: do special case to allow IdMetaFunc, cos doing the recursive call
        will be too late, match_ident will not have the info whether it  was a 
        function.
        todo: but how detect when do x.field = f;  how know that f is a Func ?
        By having computed some information before the matching! *)
      
     (* Allow match with FunCall containing types. Now ast_cocci
      * allow type in parameter, and morover ast_cocci allow f(...) and
      * those ... could match type. 
      *)
      match_e_e ea1 eb1  >&&> (

      (* for the pattern phase, no need the EComma *)
      let eas' =
	A.undots eas +>
	List.filter (function x -> 
          match A.unwrap x with A.EComma _ -> false | _ -> true) 
      in
      match_arguments 
        (match A.unwrap eas with 
        | A.DOTS _ -> Ordered 
        | A.CIRCLES _ -> Unordered 
        | A.STARS _ -> failwith "not handling stars"
        )
        eas' ebs
     )

  | A.Assignment (ea1, opa, ea2),   ((B.Assignment (eb1, opb, eb2), typ),ii) ->
      return (equal_assignOp (term opa)  opb) >&&>
      (match_e_e ea1 eb1 >&&>  match_e_e ea2 eb2) 


  | A.CondExpr (ea1,_,ea2opt,_,ea3), ((B.CondExpr (eb1, eb2opt, eb3), typ),ii) 
    ->
      match_e_e ea1 eb1 >&&>
      match_opt match_e_e ea2opt eb2opt >&&>
      match_e_e ea3 eb3
   
  (* todo?: handle some isomorphisms here ? *)

  | A.Postfix (ea, opa), ((B.Postfix (eb, opb), typ),ii) -> 
      return (equal_fixOp (term opa) opb) >&&>
      match_e_e ea eb

  | A.Infix (ea, opa), ((B.Infix (eb, opb), typ),ii) -> 
      return (equal_fixOp (term opa) opb) >&&>
      match_e_e ea eb

  | A.Unary (ea, opa), ((B.Unary (eb, opb), typ),ii) -> 
      return (equal_unaryOp (term opa) opb) >&&>
      match_e_e ea eb

  | A.Binary (ea1, opa, ea2), ((B.Binary (eb1, opb, eb2), typ),ii) -> 
      return (equal_binaryOp (term opa) opb) >&&>
      match_e_e ea1 eb1 >&&> 
      match_e_e ea2 eb2

        
  (* todo?: handle some isomorphisms here ?  (with pointers = Unary Deref) *)

  | A.ArrayAccess (ea1, _, ea2, _), ((B.ArrayAccess (eb1, eb2), typ),ii) -> 
      match_e_e ea1 eb1 >&&>
      match_e_e ea2 eb2


  (* todo?: handle some isomorphisms here ? *)

  | A.RecordAccess (ea, _, ida), ((B.RecordAccess (eb, idb), typ),ii) ->
      match_e_e ea eb >&&>
      match_ident DontKnow ida idb

  | A.RecordPtAccess (ea, _, ida), ((B.RecordPtAccess (eb, idb), typ),ii) ->
      match_e_e ea eb >&&>
      match_ident DontKnow ida idb

  (* todo?: handle some isomorphisms here ? *)
  (* todo?: do some iso-by-absence on cast ? 
   *    by trying | ea, B.Case (typb, eb) -> match_e_e ea eb ?
   *)
  | A.Cast (_, typa, _, ea), ((B.Cast (typb, eb), typ),ii) ->
      match_ft_ft typa typb >&&>
      match_e_e ea eb

  | A.SizeOfExpr (_, ea), ((B.SizeOfExpr (eb), typ),ii) ->
      match_e_e ea eb

  | A.SizeOfType (_, _, typa, _), ((B.SizeOfType (typb), typ),ii) ->
      match_ft_ft typa typb

  (* todo? iso ? allow all the combinations ? *)
  | A.Paren (_, ea, _), ((B.ParenExpr (eb), typ),ii) -> 
      match_e_e ea eb

  | A.NestExpr _, _ -> failwith "not my job to handle NestExpr"


  | A.MetaExprList _, _   -> raise Impossible (* only in arg lists *)

  (* only in arg lists *)
  | A.TypeExp _, _ 
  | A.EComma _, _  
  | A.Ecircles _, _
  | A.Estars _, _   -> 
      raise Impossible


  | A.DisjExpr eas, eb -> 
      eas +> List.fold_left (fun acc ea -> acc >||>  match_e_e ea eb) 
        (return false)


  | A.MultiExp _, _ | A.UniqueExp _,_ | A.OptExp _,_ -> 
      failwith "not handling Opt/Unique/Multi on expr"


  (* have not a counter part in coccinelle, for the moment *)
  | _, ((B.Sequence _,_),_) 

  | _, ((B.StatementExpr _,_),_) 
  | _, ((B.Constructor,_),_) 
    -> return false

  | _, _ -> return false

(* ------------------------------------------------------------------------- *)
and (match_ident: semantic_info_ident -> (Ast_cocci.ident, string) matcher) = 
 fun seminfo_idb ida idb -> 
    all_bound (A.get_inherited ida) >&&>
 match A.unwrap ida with
 | A.Id ida -> return ((term ida) =$= idb)
 | A.MetaId(ida,keep,inherited) ->
     check_add_metavars_binding keep inherited
       (term ida, Ast_c.MetaIdVal (idb))

 | A.MetaFunc (ida,keep,inherited) -> 
     (match seminfo_idb with
     | LocalFunction | Function -> 
	 check_add_metavars_binding keep inherited
	   (term ida,(Ast_c.MetaFuncVal idb))
     | DontKnow -> 
         failwith "MetaFunc and MetaLocalFunc, need semantic info about id"
     )

 | A.MetaLocalFunc (ida,keep,inherited) -> 
     (match seminfo_idb with
     | LocalFunction -> 
	 check_add_metavars_binding keep inherited
           (term ida, (Ast_c.MetaLocalFuncVal idb))
     | Function -> return false
     | DontKnow -> 
         failwith "MetaFunc and MetaLocalFunc, need semantic info about id"
     )

 | A.OptIdent _ | A.UniqueIdent _ | A.MultiIdent _ -> 
     failwith "not handling Opt/Unique/Multi for ident"


  
(*-------------------------------------------------------------------------- *)
and (match_arguments: sequence_processing_style -> 
     (Ast_cocci.expression list, Ast_c.argument Ast_c.wrap2 list) matcher) = 
 fun seqstyle eas ebs ->
  match seqstyle with
  | Ordered -> 

      (match eas, ebs with
      | [], [] -> return true
      | [], y::ys -> return false
      | x::xs, ys -> 
	  all_bound (A.get_inherited x) >&&>
          (match A.unwrap x, ys with
          | A.Edots (_, optexpr), ys -> 
              (* todo: if optexpr, then a WHEN and so may have to filter yys *)
              (* '...' can take more or less the beginnings of the arguments *)
              let yys = Common.tails ys in 
              yys +> List.fold_left (fun acc ys -> 
                acc >||>  match_arguments seqstyle xs ys
                  ) (return false)

          | A.MetaExprList(ida,keep,inherited), ys -> 
              let startendxs = Common.zip (Common.inits ys) (Common.tails ys)
              in
              startendxs +>
	      List.fold_left
		(fun acc (startxs, endxs) -> 
                  acc >||>
		  (check_add_metavars_binding keep inherited
                     (term ida, Ast_c.MetaExprListVal (startxs))
		     >&&>
                   match_arguments seqstyle xs endxs))
		(return false)


          | A.Ecircles (_,_), ys -> raise Impossible (* in Ordered mode *)
          | A.Estars (_,_), ys   -> raise Impossible (* in Ordered mode *)
           (* filtered by the caller, in the case for FunCall *)
          | A.EComma (_), ys -> raise Impossible 

          | unwrapx, y::ys -> 
              match_argument x y >&&> 
              match_arguments seqstyle xs ys
              
          | x, [] -> return false
          )
      )
  | Unordered -> failwith "not handling ooo"

and match_argument = fun arga argb -> 
    all_bound (A.get_inherited arga) >&&>
  match A.unwrap arga, argb with
  | A.TypeExp tya,  (Right (B.ArgType (tyb, sto_iisto)),ii) ->
      match_ft_ft tya tyb

  | A.TypeExp tya,  _ -> return false
  | _, (Right (B.ArgType (tyb, sto_iisto)),ii) -> return false

  | unwrapx, (Left y,ii) ->  match_e_e arga y
  | unwrapx, (Right (B.ArgAction y),ii) -> return false


(*-------------------------------------------------------------------------- *)
and (match_params: 
       sequence_processing_style -> 
         (Ast_cocci.parameterTypeDef list, 
          ((Ast_c.parameterType * Ast_c.il) list)) 
           matcher) = 
 fun seqstyle pas pbs ->
 (* todo: if contain metavar ? => recurse on two list and consomme *)

  match seqstyle with
  | Ordered -> 
      (match pas, pbs with
      | [], [] -> return true
      | [], y::ys -> return false
      | x::xs, ys -> 
	  all_bound (A.get_inherited x) >&&>
          (match A.unwrap x, ys with
          | A.Pdots (_), ys -> 

              (* '...' can take more or less the beginnings of the arguments *)
              let yys = Common.tails ys in 
              yys +> List.fold_left (fun acc ys -> 
                acc >||>  match_params seqstyle xs ys
                  ) (return false)


          | A.MetaParamList(ida,keep,inherited), ys -> 
              let startendxs = (Common.zip (Common.inits ys) (Common.tails ys))
              in
              startendxs +>
	      List.fold_left
		(fun acc (startxs, endxs) -> 
                  acc >||> (
                  check_add_metavars_binding keep inherited
		    (term ida, Ast_c.MetaParamListVal (startxs)) >&&>
                  match_params seqstyle xs endxs))
		(return false)


          | A.Pcircles (_), ys -> raise Impossible (* in Ordered mode *)

          (* filtered by the caller, in the case for FunDecl *)
          | A.PComma (_), ys -> raise Impossible 

          | A.MetaParam (ida,keep,inherited), (y,_)::ys -> 
             (* todo: use quaopt, hasreg ? *)
	      check_add_metavars_binding keep inherited
                (term ida, Ast_c.MetaParamVal (y)) >&&>
              match_params seqstyle xs ys

          | A.Param (typa, ida), (((hasreg, idb, typb), _), _)::ys -> 
              (match (ida, idb) with
		(None,Some _) -> return true
              | (Some ida,Some idb) -> 
                  (* todo: use quaopt, hasreg ? *)
                  (match_ft_ft typa typb >&&>
                   match_ident DontKnow ida idb
                  ) >&&> 
                  match_params seqstyle xs ys
              | (_,None) -> 
                  assert (null ys);
                  assert (
                    match typb with 
                    | (_qua, (B.BaseType B.Void,_)) -> true
                    | _ -> false
                          );
   
                  return (ida = None)
              )

          | x, [] -> return false

          | A.VoidParam _, _ -> failwith "handling VoidParam"
          | (A.OptParam _ | A.UniqueParam _), _ -> 
              failwith "handling Opt/Unique/Multi for Param"
                                
          )
      )

  | Unordered -> failwith "handling ooo"



(* ------------------------------------------------------------------------- *)
and (match_re_decl: (Ast_cocci.declaration, Ast_c.declaration) matcher) = 
 fun decla (B.DeclList (xs, _)) -> 
   xs +> List.fold_left (fun acc var -> acc >||> match_re_onedecl decla var)
     (return false)

and match_storage stoa stob =
  (* "iso-by-absence" for storage. *)
  match stoa with 
  | None -> return true
  | Some x -> return (equal_storage (term x) (fst stob))

and match_re_onedecl = fun decla declb -> 
  all_bound (A.get_inherited decla) >&&>
  match A.unwrap decla, declb with
  (* Un Metadecl est introduit dans l'asttoctl pour sauter au dessus
     de toutes les declarations qui sont au debut d'un fonction et
     commencer le reste du match au premier statement. Alors, ca matche
     n'importe quelle declaration. On n'a pas besoin d'ajouter
     quoi que ce soit dans l'environnement. C'est une sorte de DDots *)
  | A.MetaDecl(ida,keep,_inherited), _ -> return true

  (* could handle iso here but handled in standard.iso *)
  | A.UnInit (stoa, typa, sa, _), ((Some ((sb, None),_), typb, stob), _) ->

      match_storage stoa stob >&&> match_ft_ft typa typb >&&>
      match_ident DontKnow sa sb
  | A.Init (stoa,typa,sa,_,inia,_),((Some ((sb,Some inib),_),typb,stob), _) ->

      match_storage stoa stob >&&> 
      match_ft_ft typa typb >&&>
      match_ident DontKnow sa sb >&&>
      match_initialiser inia inib

  | A.TyDecl (typa, _), _ ->

      (* accept only '((None, typb, sto), _)' or do iso-by-absence here ?
         allow typedecl and var ? *)
      failwith "fill something in for a declaration that is just a type"

  | _, ((None, typb, sto), _) -> return false

      
  | A.DisjDecl xs, _ -> 
      xs +> List.fold_left (fun acc decla -> 
        acc >||> match_re_onedecl decla declb
        ) (return false)
  | A.OptDecl _, _ | A.UniqueDecl _, _ | A.MultiDecl _, _ -> 
      failwith "not handling Opt/Unique/Multi Decl"
  | _, _ -> return false


(* ------------------------------------------------------------------------- *)
and (match_initialiser: (Ast_cocci.initialiser, Ast_c.initialiser) matcher) = 
  fun inia inib -> 
    all_bound (A.get_inherited inia) >&&>
    match (A.unwrap inia,inib) with
    | (A.InitExpr expa, (B.InitExpr expb, _ii)) -> match_e_e expa expb
    | (A.InitList (i1, ias, i2, []), (B.InitList ibs, _ii)) -> 
        match_initialisers ias (Ast_c.split_comma ibs)
    | (A.InitList (i1, ias, i2, whencode),(B.InitList ibs, _ii)) ->
        failwith "TODO: not handling whencode in initialisers"
    | (A.InitGccDotName (i1, ida, i2, inia), (B.InitGcc (idb, inib), _ii)) -> 
        match_ident DontKnow ida idb >&&> 
        match_initialiser inia inib
    | (A.InitGccName (ida, i1, inia), (B.InitGcc (idb, inib), _ii)) -> 
        match_ident DontKnow ida idb >&&> 
        match_initialiser inia inib

    | (A.InitGccIndex (i1,ea,i2,i3,inia), (B.InitGccIndex (eb, inib), ii)) -> 
        match_e_e ea eb >&&>
        match_initialiser inia inib

    | (A.InitGccRange (i1,e1a,i2,e2a,i3,i4,inia), 
      (B.InitGccRange (e1b, e2b, inib), ii)) -> 
        match_e_e e1a e1b >&&>
        match_e_e e2a e2b >&&>
        match_initialiser inia inib

    | A.IComma _, _ -> failwith "not updated for IComma"

    | A.MultiIni _, _ | A.UniqueIni _,_ | A.OptIni _,_ -> 
      failwith "not handling Opt/Unique/Multi on initialisers"
          
    | _, _ -> return false

and match_initialisers = fun ias ibs -> 
  match ias, ibs with
  | [], ys -> return true
  | x::xs, ys -> 
      let permut = Common.uncons_permut ys in
      permut +> List.fold_left (fun acc ((e, _pos), rest) -> 
        acc >||> 
          (
            (match e with 
            | Left y -> match_initialiser x y 
            | Right y -> return false 
            ) >&&> match_initialisers xs rest
          )
      ) (return false)
  




  
(* ------------------------------------------------------------------------- *)

and (match_ft_ft: (Ast_cocci.fullType, Ast_c.fullType) matcher) =
  fun typa typb ->
    all_bound (A.get_inherited typa) >&&>
    match (A.unwrap typa,typb) with
      (A.Type(cv,ty1),((qu,il),ty2)) ->
	(* Drop out the const/volatile part that has been matched.
         * This is because a SP can contain  const T v; in which case
         * later in match_t_t when we encounter a T, we must not add in
         * the environment the whole type.
         *)
	let new_il todrop = List.filter (fun (pi,_) -> 
          not(pi.Common.str = todrop)) 
        in

        if qu.B.const && qu.B.volatile 
        then pr2 "warning: the type is both const & volatile but cocci does not handle that";

	(match cv with
          (* "iso-by-absence" *)
	  None -> match_t_t ty1 typb
	| Some(A.Const,_,_) ->
	    if qu.B.const
	    then
	      match_t_t ty1
		(({qu with B.const = false},new_il "const" il),ty2)
	    else return false
	| Some(A.Volatile,_,_) ->
	    if qu.B.volatile
	    then
	      match_t_t ty1
		(({qu with B.volatile = false},new_il "volatile" il),ty2)
	    else return false)
    | A.DisjType typas, typb -> 
	typas +>
	List.fold_left (fun acc typa -> acc >||>  match_ft_ft typa typb) 
          (return false)
    | (A.OptType(ty),typb) ->
	pr2 "warning: ignoring ? arity on type";
	match_ft_ft ty typb
    | (A.UniqueType(ty),typb) ->
	pr2 "warning: ignoring ! arity on type";
	match_ft_ft ty typb
    | (A.MultiType(ty),typb) ->
	pr2 "warning: ignoring + arity on type";
	match_ft_ft ty typb

(*
 * Why not (Ast_cocci.typeC, Ast_c.typeC) matcher ?
 * because when there is MetaType, we want that T record the whole type, 
 * including the qualifier, and so this type (and the new_il function in
 * preceding function).
*)
and (match_t_t: (Ast_cocci.typeC, Ast_c.fullType) matcher) =
  fun typa typb ->
    all_bound (A.get_inherited typa) >&&> 
    match A.unwrap typa, typb with

      (* cas general *)
    | A.MetaType(ida,keep,inherited),  typb -> 
	check_add_metavars_binding keep inherited
	  (term ida, B.MetaTypeVal typb)

    | A.BaseType (basea, signaopt),   (qu, (B.BaseType baseb, iib)) -> 

        (* todo: iso on sign, if not mentioned then free.  tochange? 
         * but that require to know if signed int because explicit
         * signed int,  or because implicit signed int.
         *)

        let compute_signb (baseb, iib) = 
          let iibs = iib +> List.map (fun (ii,mc) -> ii.Common.str) in
          match baseb, iibs with
          | B.Void, ["void"] -> None

          | B.FloatType (B.CFloat), ["float"] -> None
          | B.FloatType (B.CDouble), ["double"] -> None
          | B.FloatType (B.CLongDouble), ["long";"double"] -> None

          | B.IntType (B.CChar), ["char"] -> None


          | B.IntType (B.Si (sign, base)), xs -> 
              (match sign, base, xs with
              | B.Signed, B.CChar2,   ["signed";"char"] -> Some B.Signed
              | B.UnSigned, B.CChar2,   ["unsigned";"char"] -> Some B.UnSigned

              | B.Signed, B.CShort, ["short"] -> None
              | B.Signed, B.CShort, ["signed";"short"] -> Some B.Signed
              | B.UnSigned, B.CShort, ["unsigned";"short"] -> Some B.UnSigned

              | B.Signed, B.CInt, ["int"] -> None
              | B.Signed, B.CInt, ["signed";"int"] -> Some B.Signed
              | B.UnSigned, B.CInt, ["unsigned";"int"] -> Some B.UnSigned
              | B.UnSigned, B.CInt, ["unsigned";] -> Some B.UnSigned (*fix*)

              | B.Signed, B.CLong, ["long"] -> None
              | B.Signed, B.CLong, ["signed";"long"] -> Some B.Signed
              | B.UnSigned, B.CLong, ["unsigned";"long"] -> Some B.UnSigned

              | B.Signed, B.CLongLong, ["long";"long"] -> None
              | B.Signed, B.CLongLong, ["signed";"long";"long"] -> 
                  Some B.Signed
              | B.UnSigned, B.CLongLong, ["unsigned";"long";"long"] -> 
                  Some B.UnSigned
              | _ -> failwith "strange type1, maybe because of weird order"
              )

          | _ -> failwith "strange type2, maybe because of weird order"
        in
        let signbopt = compute_signb (baseb, iib) in

	let match_sign signa signb = 
          match signa, signb with
          | None, None -> return true
          | Some a, Some b -> return (equal_sign (term a) b)
          | _, _ -> return false
        in
	
	
      (* handle some iso on type ? (cf complex C rule for possible implicit
	 casting) *)
	(match term basea, baseb with
	| A.VoidType,  B.Void -> assert (signaopt = None); return true
	| A.CharType,  B.IntType B.CChar when signaopt = None -> 
            return true
        | A.CharType,  B.IntType (B.Si (_, B.CChar2)) when signaopt <> None -> 
            match_sign signaopt signbopt


	| A.ShortType, B.IntType (B.Si (_, B.CShort)) ->
	    match_sign signaopt signbopt
	| A.IntType,   B.IntType (B.Si (_, B.CInt))   ->
	    match_sign signaopt signbopt
	| A.LongType,  B.IntType (B.Si (_, B.CLong))  ->
	    match_sign signaopt signbopt

	| A.FloatType, B.FloatType (B.CFloat) -> 
            assert (signaopt = None); (* no sign on float in C *)
            return true
	| A.DoubleType, B.FloatType (B.CDouble) -> 
            assert (signaopt = None); (* no sign on float in C *)
            return true

        | _, B.IntType (B.Si (_, B.CLongLong)) 
        | _, B.FloatType B.CLongDouble 
           -> pr2 "warning: long long or long double not handled by ast_cocci";
              return false
	| x, y -> return false
        )

    | A.ImplicitInt (signa),   _ -> 
	failwith "implicitInt pattern not supported"
	  
  (* todo? iso with array *)
    | A.Pointer (typa, _),            (qu, (B.Pointer typb, _)) -> 
	match_ft_ft typa typb

    | A.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2), _ ->
	failwith "TODO: matching for function pointer"
	  
    | A.Array (typa, _, eaopt, _), (qu, (B.Array (ebopt, typb), _)) -> 
	match_ft_ft typa typb >&&>
        match_opt match_e_e  eaopt ebopt
       (* todo: handle the iso on optionnal size specifification ? *)
	  
    | A.StructUnionName (sua, sa), (qu, (B.StructUnionName (sub, sb), _)) -> 
     (* todo: could also match a Struct that has provided a name *)
	if equal_structUnion (term sua) sub
	then match_ident DontKnow sa sb
	else return false

    | A.StructUnionDef (sua, sa, lb, decls, rb), _ -> 
	failwith "to be filled in"

   (* todo? handle isomorphisms ? because Unsigned Int can be match on a 
      uint in the C code. But some CEs consists in renaming some types,
      so we don't want apply isomorphisms every time. *) 
    | A.TypeName sa,  (qu, (B.TypeName sb, _)) ->
	return ((term sa) =$= sb)
    | (_,_) -> return false (* incompatible constructors *)




(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let (match_re_node2: (Ast_cocci.rule_elem, Control_flow_c.node) matcher) = 
 fun re node -> 
   all_bound (A.get_inherited re) >&&>
  match A.unwrap re, F.unwrap node with

  (* note: the order of the clauses is important. *)

  | _, F.Enter | _, F.Exit | _, F.ErrorExit -> return false

  (* the metaRuleElem contains just '-' information. We dont need to add
   * stuff in the environment. If we need stuff in environment, because
   * there is a + S somewhere, then this will be done via MetaStmt, not
   * via MetaRuleElem. 
   * Can match TrueNode/FalseNode/... so must be placed before those cases.
   *)
  | A.MetaRuleElem _, _ -> return true

  | _, F.Fake
  | _, F.EndStatement _
  | _, F.CaseNode _ 
  | _, F.TrueNode | _, F.FalseNode | _, F.AfterNode | _, F.FallThroughNode 
    -> return false

  (* cas general: a Meta can match everything *)
  | A.MetaStmt (ida,keep,_,inherited),  _unwrap_node -> 
     (* match only "header"-statement *)
     (match Control_flow_c.extract_fullstatement node with
     | Some stb -> 
         check_add_metavars_binding keep inherited
	   (term ida, Ast_c.MetaStmtVal stb)
     | None -> return false
     )

  (* not me?: *)
  | A.MetaStmtList _, _ -> failwith "not handling MetaStmtList"

  | A.Exp expr, nodeb -> 
      (* julia's code *)
      (*
      (function binding ->
        let globals = ref [] in
        let bigf = { Visitor_c.default_visitor_c with Visitor_c.kexpr = 
            (fun (k, bigf) e ->
	      match match_e_e expr e binding with
		[] -> (* failed *) k e
	      |	b -> globals := b @ !globals);
              }
        in
       *)
        let globals = ref [] in
        let bigf = { Visitor_c.default_visitor_c with 
          Visitor_c.kexpr = (fun (k, bigf) expr -> 
            push2 expr globals;  k expr 
          );
        } 
        in
        Visitor_c.vk_node bigf node;
        !globals +> List.fold_left (fun acc e -> acc >||> match_e_e expr e) 
           (return false)
       
  | A.Ty ty, nodeb -> 
      let globals = ref [] in
      let bigf = { Visitor_c.default_visitor_c with 
        Visitor_c.ktype = (fun (k, bigf) ty -> 
          push2 ty globals;  k ty 
        );
      } 
      in
      Visitor_c.vk_node bigf node;
      !globals +> List.fold_left (fun acc t -> acc >||> match_ft_ft ty t) 
        (return false)
       

  | A.FunHeader (_,_,stoa, tya, ida, _, paramsa, _), 
    F.FunHeader ((idb, (retb, (paramsb, (isvaargs,_))), stob), _) -> 
      (* todo: isvaargs ? *)

      match_ident LocalFunction ida idb >&&>

      (* "iso-by-absence" for storage, and return type. *)
      (match_storage stoa stob) >&&> 
      (match tya with 
       | None -> return true
       | Some tya -> match_ft_ft tya retb
      ) >&&>
      (
       (* for the pattern phase, no need the EComma *)
       let paramsa' = A.undots paramsa +> List.filter(function x -> 
         match A.unwrap x with A.PComma _ -> false | _ -> true)
       in
       match_params
        (match A.unwrap paramsa with 
        | A.DOTS _ -> Ordered 
        | A.CIRCLES _ -> Unordered 
        | A.STARS _ -> failwith "not yet handling stars (interprocedural stuff)"
        )
         paramsa' paramsb
      )

  | A.Decl (_,decla), F.Decl declb -> match_re_decl decla declb

  | A.SeqStart _, F.SeqStart _ -> return true
  | A.SeqEnd _,   F.SeqEnd   _ -> return true

  | A.ExprStatement (ea, _), F.ExprStatement (_, (Some eb, ii)) -> 
      match_e_e ea eb

  | A.IfHeader (_,_, ea, _), F.IfHeader (_, (eb,ii)) -> match_e_e ea eb
  | A.Else _,                F.Else _                -> return true
  | A.WhileHeader (_, _, ea, _), F.WhileHeader (_, (eb,ii)) -> match_e_e ea eb
  | A.DoHeader _,             F.DoHeader _          -> return true
  | A.WhileTail (_,_,ea,_,_), F.DoWhileTail (eb,ii) -> match_e_e ea eb

  | A.ForHeader (_, _, ea1opt, _, ea2opt, _, ea3opt, _), 
    F.ForHeader (_, (((eb1opt,_), (eb2opt,_), (eb3opt,_)), ii)) -> 
      match_opt match_e_e ea1opt eb1opt >&&>
      match_opt match_e_e ea2opt eb2opt >&&>
      match_opt match_e_e ea3opt eb3opt >&&>
      return true

  | A.SwitchHeader(_,_,ea,_), F.SwitchHeader _ ->
      failwith "switch not supported"

  | A.Goto,                  F.Goto (_, (_,ii))       -> return true
  | A.Break _,               F.Break (_, ((),ii))      -> return true
  | A.Continue _,            F.Continue (_, ((),ii))   -> return true
  | A.Return _,              F.Return (_, ((),ii))     -> return true
  | A.ReturnExpr (_, ea, _), F.ReturnExpr (_, (eb,ii)) -> match_e_e ea eb

  | A.Include(incl,filea), F.CPPInclude (fileb, _) -> 
      return ((term filea) =$= fileb)

  | A.Define(define,ida,bodya), F.CPPDefine ((idb, bodyb), _)  ->
      match_ident DontKnow ida idb >&&> 
      all_bound (A.get_inherited ida) >&&>
      (match A.unwrap bodya with
      | A.DMetaId (idbody, keep) -> 
          let inherited = false (* TODO ? *) in
          check_add_metavars_binding keep inherited
            (term idbody, Ast_c.MetaTextVal (bodyb))

      | A.Ddots (dots) -> return true
      )

  | A.Default(_,_), F.Default _ -> failwith "switch not supported"
  | A.Case(_,ea,_), F.Case _ -> failwith "switch not supported"

  | _, F.ExprStatement (_, (None, ii)) -> return false (* happen ? *)

  (* have not a counter part in coccinelle, for the moment *)
  (* todo?: print a warning at least ? *)
  | _, F.Label _
  | _, F.CaseRange _ 
  | _, F.Asm
  | _, F.IfCpp _
    -> return false

  | _, _ -> return false



(* subtil, 3 args, otherwise profile nothing *)
let match_re_node a b c = 
  Common.profile_code "Pattern.match_re_node" (fun () -> match_re_node2 a b c)
