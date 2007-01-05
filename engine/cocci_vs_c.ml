open Common open Commonop

module A = Ast_cocci
module B = Ast_c
module F = Control_flow_c

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type sequence = Ordered | Unordered

(* todo? put in semantic_c.ml *)
type info_ident = 
  | Function 
  | LocalFunction (* entails Function *)
  | DontKnow

let term ((s,_,_) : 'a Ast_cocci.mcode) = s
let mcodekind (_,i,mc) = mc
let wrap_mcode (_,i,mc) = ("fake", i, mc)


let mcode_contain_plus = function
  | Ast_cocci.CONTEXT (Ast_cocci.NOTHING) -> false
  | Ast_cocci.CONTEXT _ -> true
  | Ast_cocci.MINUS ([]) -> false
  | Ast_cocci.MINUS (x::xs) -> true
  | Ast_cocci.PLUS -> raise Impossible

let mcode_simple_minus = function
  | Ast_cocci.MINUS ([]) -> true
  | _ -> false


let seqstyle eas =      
   match A.unwrap eas with 
   | A.DOTS _ -> Ordered 
   | A.CIRCLES _ -> Unordered 
   | A.STARS _ -> failwith "not handling stars"
         
  

(* Normally Ast_cocci should reuse some types of Ast_c, so those
 * functions should not exist.
 * 
 * update: but now Ast_c depends on Ast_cocci, so can't make too
 * Ast_cocci depends on Ast_c, so have to stay with those equal_xxx
 * functions. 
 *)

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
(* combinators *)
(*****************************************************************************)
(* monad like stuff
 * src: papers on parser combinators in haskell (cf a pearl by meijer in ICFP)
 *)

module type PARAM = 
  sig 
    type tin
    type 'a tout
    val (>>=): 
            (tin -> 'c tout)  -> ('c -> (tin -> 'b tout)) -> (tin -> 'b tout)
    val return : 'b -> tin -> 'b tout
    val fail : tin -> 'b tout

    val (>||>) : 
      (tin -> 'a tout) ->
      (tin -> 'a tout) -> 
      (tin -> 'a tout)

    val tokenf_one : 'a A.mcode -> Ast_c.info -> tin -> Ast_c.info tout

    val envf : 
      A.inherited -> string * Ast_c.metavar_binding_kind ->
      tin -> Ast_c.metavar_binding_kind tout

    type 'b tdistr
    val distrf : 
      'a tdistr ->  A.mcodekind -> 'a -> tin -> 'a tout
    val distrf_e : Ast_c.expression tdistr
    val distrf_type : Ast_c.fullType tdistr
    val distrf_node : Control_flow_c.node2 tdistr
    val distrf_args : (Ast_c.argument, Ast_c.il) either list tdistr
  end




module COCCI_VS_C =
  functor (X : PARAM) -> 
struct

type ('a, 'b) matcher = 'a -> 'b  -> X.tin -> 'b X.tout

let (>>=) = X.(>>=)
let return = X.return
let fail = X.fail

let (>||>) = X.(>||>)

let tokenf_one = X.tokenf_one
let envf = X.envf

let distrf = X.distrf
let distrf_e = X.distrf_e
let distrf_node = X.distrf_node
let distrf_type = X.distrf_type
let distrf_args = X.distrf_args



(* should be raise Impossible when called from transformation.ml *)
let fail2 = fail

let (option: ('a,'b) matcher -> ('a option,'b option) matcher)= fun f t1 t2 ->
  match (t1,t2) with
  | (Some t1, Some t2) -> 
      f t1 t2 >>= (fun (x : 'b) -> 
        return (Some x)
      )
  | (None, None) -> return None
  | _ -> fail




(*****************************************************************************)
(* Tokens *) 
(*****************************************************************************)
let tokenf xs ys = 
  assert (List.length xs = List.length ys);
  List.fold_right (fun  (ia, ib) acc -> 
    (* assert s1 = s2 ? no more cos now have some "fake" string,
     * and also because now s1:'a, no more s1:string
     *)
    acc >>= (fun xs -> 
    tokenf_one ia ib >>= (fun i' -> 
        return (i'::xs)
      ))) (Common.zip xs ys) (return [])

   
let tokenf_wrap xs ys e = 
  tokenf xs ys >>= (fun ii' ->  return (e, ii'))


(*****************************************************************************)
(* "Cocci vs C" *) 
(*****************************************************************************)

(* toc: 
 *  - expression
 *  - ident
 *  - arguments
 *  - parameters
 *  - declaration
 *  - type       
 *  - node
 *)
       

let rec (expression: (Ast_cocci.expression, Ast_c.expression) matcher) =
 fun ea eb -> 
  match A.unwrap ea, eb with
  
  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr (ida,true,opttypa,inherited), (((expr, opttypb), ii) as expb) ->
      let match_type = 
        match opttypa, opttypb with
        | None, _ -> true
        | Some tas, Some tb -> 
            tas +> List.exists (fun ta ->  Types.compatible_type ta tb)
        | Some _, None -> 
            failwith ("I have not the type information. Certainly a pb in " ^
                         "annotate_typer.ml")
      in

      (* get binding, assert =*=,  distribute info in ida *)
      envf inherited (term ida, Ast_c.MetaExprVal expb) >>= (fun v -> 
        match v with
        | Ast_c.MetaExprVal expa -> 
            if (Abstract_line_c.al_expr expa =*= Abstract_line_c.al_expr expb)
               && match_type
            then distrf distrf_e (mcodekind ida) expb
            else fail
        | _ -> raise Impossible
      )

  | A.MetaExpr (ida,false,opttypa,inherited), _ ->
      failwith "should not appear in transformed code"

  (* old: | A.Edots _, _ -> raise Impossible. In fact now can also have
   * the Edots inside normal expression, not just in arg lists. in
   * 'x[...];' less: in if(<... x ... y ...>) *)
  | A.Edots (mcode, None), expb    -> 
      distrf distrf_e (mcodekind mcode) expb

  | A.Edots (_, Some expr), _    -> failwith "not handling when on Edots"


  | A.MetaConst _, _ -> failwith "not handling MetaConst"
  | A.MetaErr _, _ -> failwith "not handling MetaErr"
      
  | A.Ident ida,                ((B.Ident idb, typ),ii) ->
      ident DontKnow ida (idb, ii) >>= (fun (idb', ii') -> 
        return ((B.Ident idb', typ),ii')
      )


 (* todo?: handle some isomorphisms in int/float ? can have different
  * format : 1l can match a 1.
  * 
  * todo: normally string can contain some metavar too, so should
  * recurse on the string *)
  | A.Constant ((A.Int ia,_,_) as i1), ((B.Constant (B.Int ib) , typ),ii)
      when ia =$= ib ->  
      (B.Constant (B.Int ib), typ) +> tokenf_wrap [i1] ii 

  | A.Constant ((A.Char ia,_,_) as i1), ((B.Constant (B.Char (ib,t)), typ),ii)
      when ia =$= ib ->  
      (B.Constant (B.Char (ib, t)), typ) +> tokenf_wrap [i1] ii

  | A.Constant ((A.String ia,_,_)as i1),((B.Constant (B.String (ib,t)),typ),ii)
      when ia =$= ib ->  
      (B.Constant (B.String (ib, t)), typ) +> tokenf_wrap [i1] ii

  | A.Constant ((A.Float ia,_,_) as i1),((B.Constant (B.Float (ib,t)),typ),ii)
      when ia =$= ib ->  
      (B.Constant (B.Float (ib,t)), typ) +> tokenf_wrap [i1] ii


  | A.FunCall (ea, i2, eas, i3),  ((B.FunCall (eb, ebs), typ),ii) -> 
      (* todo: do special case to allow IdMetaFunc, cos doing the
       * recursive call will be too late, match_ident will not have the
       * info whether it was a function. todo: but how detect when do
       * x.field = f; how know that f is a Func ? By having computed
       * some information before the matching!
       * 
       * Allow match with FunCall containing types. Now ast_cocci allow
       * type in parameter, and morover ast_cocci allow f(...) and those
       * ... could match type. *)

      expression ea eb >>= (fun e' -> 
      arguments (seqstyle eas) (A.undots eas) ebs >>= (fun arg' -> 
          (B.FunCall (e', arg'),typ) +> tokenf_wrap [i2;i3] ii
        ))


  | A.Assignment (ea1, opa, ea2), ((B.Assignment (eb1, opb, eb2), typ),ii) -> 
      if equal_assignOp (term opa) opb 
      then
        expression ea1 eb1 >>= (fun e1' -> 
        expression ea2 eb2 >>= (fun e2' -> 
            (B.Assignment (e1', opb, e2'), typ) +> tokenf_wrap [opa] ii
        ))
      else fail

  | A.CondExpr (ea1,i1,ea2opt,i2,ea3),((B.CondExpr (eb1,eb2opt,eb3),typ),ii) ->
      expression ea1 eb1 >>= (fun e1' -> 
      option expression ea2opt eb2opt >>= (fun e2opt' -> 
      expression ea3 eb3 >>= (fun e3' -> 
            (B.CondExpr (e1', e2opt', e3'),typ) +> tokenf_wrap [i1;i2] ii
          )))

  (* todo?: handle some isomorphisms here ? *)
  | A.Postfix (ea, opa), ((B.Postfix (eb, opb), typ),ii) -> 
      if equal_fixOp (term opa) opb
      then
        expression ea eb >>= (fun e' -> 
          (B.Postfix (e', opb), typ) +> tokenf_wrap [opa] ii
        )
      else fail
        
        
  | A.Infix (ea, opa), ((B.Infix (eb, opb), typ),ii) -> 
      if equal_fixOp (term opa) opb
      then
        expression ea eb >>= (fun e' -> 
          (B.Infix (e', opb), typ) +> tokenf_wrap [opa] ii
        )
      else fail

  | A.Unary (ea, opa), ((B.Unary (eb, opb), typ),ii) -> 
      if equal_unaryOp (term opa) opb
      then 
        expression ea eb >>= (fun e' -> 
          (B.Unary (e', opb), typ) +> tokenf_wrap [opa] ii
        )
      else fail


  | A.Binary (ea1, opa, ea2), ((B.Binary (eb1, opb, eb2), typ),ii) -> 
      if equal_binaryOp (term opa) opb
      then 
        expression ea1 eb1 >>= (fun e1' -> 
        expression ea2 eb2 >>= (fun e2' -> 
            (B.Binary (e1', opb, e2'), typ) +> tokenf_wrap [opa] ii
          ))
      else fail


  (* todo?: handle some isomorphisms here ?  (with pointers = Unary Deref) *)
  | A.ArrayAccess (ea1, i1, ea2, i2), ((B.ArrayAccess (eb1, eb2), typ),ii) -> 
      expression ea1 eb1 >>= (fun e1' -> 
      expression ea2 eb2 >>= (fun e2' -> 
          (B.ArrayAccess (e1', e2'),typ) +> tokenf_wrap [i1;i2] ii
        ))

  (* todo?: handle some isomorphisms here ? *)
  | A.RecordAccess (ea, dot, ida), ((B.RecordAccess (eb, idb), typ),ii) ->
      (match ii with
      | [i1;i2] -> 
          ident DontKnow ida (idb, [i2]) >>= (fun (idb', i2') -> 
          tokenf [dot] [i1] >>= (fun i1' -> 
          expression ea eb >>= (fun e' -> 
                return 
                  ((B.RecordAccess (e', idb'), typ), i1' ++ i2')
              )))
      | _ -> raise Impossible
      )


  | A.RecordPtAccess (ea,fleche,ida),((B.RecordPtAccess (eb, idb), typ), ii) ->
      (match ii with
      | [i1;i2] -> 
          ident DontKnow ida (idb, [i2]) >>= (fun (idb', i2') -> 
          tokenf [fleche] [i1] >>= (fun  i1' -> 
          expression ea eb >>= (fun e' -> 
                return 
                  ((B.RecordPtAccess (e',idb'),typ), i1' ++ i2')
              )))
      | _ -> raise Impossible
      )
  (* todo?: handle some isomorphisms here ? 
   * todo?: do some iso-by-absence on cast ? 
   *    by trying | ea, B.Case (typb, eb) -> match_e_e ea eb ?
   *)

  | A.Cast (i1, typa, i2, ea), ((B.Cast (typb, eb), typ),ii) -> 
      fullType typa typb >>= (fun t' -> 
      expression ea eb >>= (fun e' -> 
          (B.Cast (t', e'),typ) +> tokenf_wrap [i1;i2]  ii
        ))

  | A.SizeOfExpr (i1, ea), ((B.SizeOfExpr (eb), typ),ii) -> 
      expression ea eb >>= (fun e' -> 
        (B.SizeOfExpr (e'), typ) +> tokenf_wrap [i1]  ii
      )

  | A.SizeOfType (i1, i2, typa, i3), ((B.SizeOfType typb, typ),ii) -> 
      fullType typa typb >>= (fun t' -> 
        (B.SizeOfType (t'),typ) +> tokenf_wrap [i1;i2;i3]  ii
      )


  (* todo? iso ? allow all the combinations ? *)
  | A.Paren (i1, ea, i2), ((B.ParenExpr (eb), typ),ii) -> 
      expression ea eb >>= (fun e' -> 
        (B.ParenExpr (e'), typ) +> tokenf_wrap [i1;i2] ii
      )

  | A.NestExpr _, _ -> failwith "not my job to handle NestExpr"
  
  (* only in arg lists *)
  | A.MetaExprList _, _   
  | A.TypeExp _, _ 
  | A.EComma _, _  
  | A.Ecircles _, _ 
  | A.Estars _, _   
      -> raise Impossible


  | A.DisjExpr eas, eb -> 
      eas +> List.fold_left (fun acc ea -> 
        acc >||> (expression ea eb)
      ) fail

  | A.MultiExp _, _ | A.UniqueExp _,_ | A.OptExp _,_ -> 
      failwith "not handling Opt/Unique/Multi on expr"


 (* Because of Exp cant put a raise Impossible; have to put a raise NoMatch; *)

 (* have not a counter part in coccinelle, for the moment *) 
  | _, ((B.Sequence _,_),_) 

  | _, ((B.StatementExpr _,_),_) 
  | _, ((B.Constructor,_),_) 
  | _, ((B.MacroCall _,_),_) 
    -> fail

  | _, _ -> fail



(* ------------------------------------------------------------------------- *)
and (ident: info_ident -> (Ast_cocci.ident, string Ast_c.wrap) matcher) = 
 fun infoidb ida (idb, ii) -> 
  match A.unwrap ida with
  | A.Id sa -> 
      if (term sa) =$= idb
      then idb +> tokenf_wrap [sa] ii
      else fail

  | A.MetaId(ida,true,inherited) -> 
      (* get binding, assert =*=,  distribute info in i1 *)
      envf inherited (term ida, Ast_c.MetaIdVal (idb)) >>= (fun v -> 
        match v with
        | Ast_c.MetaIdVal sa -> 
            if (sa =$= idb) 
            then idb +> tokenf_wrap [ida] ii
            else fail
        | _ -> raise Impossible
      )
  | A.MetaFunc(ida,true,inherited) -> 
      (match infoidb with 
      | LocalFunction | Function -> 
          envf inherited (term ida, Ast_c.MetaFuncVal idb) >>= (fun v -> 
            match v with
            | Ast_c.MetaFuncVal sa -> 
                if(sa =$= idb) 
                then idb +> tokenf_wrap [ida] ii
                else fail
            | _ -> raise Impossible
          )
      | DontKnow -> 
          failwith
	    "MetaFunc and MetaLocalFunc, need more semantic info about id")
      
  | A.MetaLocalFunc(ida,true,inherited) -> 
      (match infoidb with
      | LocalFunction -> 
          envf inherited (term ida, Ast_c.MetaLocalFuncVal idb) >>= (fun v -> 
            match v with
            | Ast_c.MetaLocalFuncVal sa -> 
                if (sa =$= idb) 
                then idb +> tokenf_wrap [ida] ii
                else fail
            | _ -> raise Impossible
          )
           
           
      | Function -> fail
      | DontKnow -> 
          failwith
	    "MetaFunc and MetaLocalFunc, need more semantic info about id")
       
  | A.MetaId(ida,false,inherited) | A.MetaFunc(ida,false,inherited)
  | A.MetaLocalFunc(ida,false,inherited) ->
      failwith "should not appear in transformed code"
 
  | A.OptIdent _ | A.UniqueIdent _ | A.MultiIdent _ -> 
      failwith "not handling Opt/Unique/Multi for ident"


(* ------------------------------------------------------------------------- *)
and (arguments: sequence -> 
      (Ast_cocci.expression list, Ast_c.argument Ast_c.wrap2 list) matcher) = 
 fun seqstyle eas ebs ->
  (* in fact it gives the  unwrapped and the wrapped version *)
  match seqstyle with
  | Unordered -> failwith "not handling ooo"
  | Ordered -> 
      arguments_bis eas (Ast_c.split_comma ebs) >>= (fun ebs' -> 
        return (Ast_c.unsplit_comma ebs')
      )
(* because '...' can match nothing, need to take care when have 
 * ', ...'   or '...,'  as in  f(..., X, Y, ...). It must match
 * f(1,2) for instance.
 *)
      
and arguments_bis = fun eas ebs -> 
  match eas, ebs with
  | [], [] -> return []
  | [], y::ys -> fail
  | x::xs, ys -> 
      (match A.unwrap x, ys with
      | A.Edots (mcode, optexpr), ys -> 
          (* todo: if optexpr, then a WHEN and so may have to filter yys *)
          if optexpr <> None then failwith "not handling when in argument";

          (* '...' can take more or less the beginnings of the arguments *)
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in

          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (
              distrf distrf_args (mcodekind mcode) startxs >>=(fun startxs'->
                  arguments_bis xs endxs >>= (fun endxs' -> 
                    return (startxs' ++ endxs')
                  ))
              )
            ) fail 
      | A.EComma i1, Right ii::ys -> 
          tokenf [i1] ii >>= (fun ii' -> 
          arguments_bis xs ys >>= (fun ys' -> 
            return (Right ii'::ys')
          ))
      | A.EComma i1, _ -> fail
      | A.MetaExprList (ida, true, inherited), ys -> 
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            let startxs' = Ast_c.unsplit_comma startxs in
            acc >||> (
              envf inherited (term ida, Ast_c.MetaExprListVal startxs') >>= 
                (fun v -> 
                  match v with
                  | Ast_c.MetaExprListVal startxs'' -> 
                      (* TODO
                      if (Abstract_line_c.al_expr expa =*= 
                          Abstract_line_c.al_expr expb)
                      *)
                     distrf distrf_args (mcodekind ida) startxs
                  | _ -> raise Impossible
                ) >>= (fun startxs' -> 
                  arguments_bis xs endxs >>= (fun endxs' -> 
                    return (startxs' ++ endxs')
                  )
                )
            )
          ) fail 

      | A.MetaExprList (ida, false, inherited), ys -> 
	  failwith "should not appear in transformed code"

      | unwrapx, (Left y)::ys -> 
          argument x y >>= (fun y' -> 
          arguments_bis xs ys >>= (fun ys' -> 
            return (Left y'::ys')
          ))
      | unwrapx, (Right y)::ys -> 
          raise Impossible

      | unwrapx, [] -> fail

      )
            
      





and argument arga argb = 
   match A.unwrap arga, argb with
  | A.TypeExp tya,  Right (B.ArgType (tyb, (sto, iisto))) ->
      if sto <> (B.NoSto, false)
      then failwith "the argument have a storage and ast_cocci does not have"
      else 
        fullType tya tyb >>= (fun t' -> 
          return (Right (B.ArgType (t', (sto, iisto))))
        )
  | A.TypeExp tya,  _                                  -> fail
  | _,              Right (B.ArgType (tyb, sto_iisto)) -> fail
  | _, Left y -> 
      expression arga y >>= (fun e' -> 
        return (Left e')
      )
  | _, Right (B.ArgAction y) -> fail


(* ------------------------------------------------------------------------- *)
and (parameters: sequence -> 
      (Ast_cocci.parameterTypeDef list, Ast_c.parameterType Ast_c.wrap2 list)
        matcher) = 
 fun seqstyle pas pbs ->
   raise Todo

and (parameter: (Ast_cocci.parameterTypeDef, (Ast_c.parameterType)) matcher) = 
 fun pa pb  -> 
   raise Todo


(* ------------------------------------------------------------------------- *)
and (declaration: (Ast_cocci.declaration, Ast_c.declaration) matcher) =
 fun decla declb -> 
   raise Todo

(* facto with code for FunHeader ? *)
and storage stoa stob =
  raise Todo

and onedecl = fun decla declb -> 
   raise Todo


and (fullType: (Ast_cocci.fullType, Ast_c.fullType) matcher) = 
 fun typa typb -> 
   raise Todo


and (fullTypebis: (Ast_cocci.typeC, Ast_c.fullType) matcher) = 
 fun typa typb -> 
  match A.unwrap typa, typb with

  (* cas general *)
  | A.MetaType(ida,keep, inherited),  typb -> 
      (* todo: keep ? pass to env *)
      envf inherited (term ida, B.MetaTypeVal typb) >>= (fun v ->  
        match v with
        | B.MetaTypeVal typa  -> 
          if (Abstract_line_c.al_type typa =*= Abstract_line_c.al_type typb)
          then distrf distrf_type (mcodekind ida) typb
          else fail
        | _ -> raise Impossible
      )
  | unwrap, (qub, typb) -> 
      typeC typa typb >>= (fun typ' -> 
        return (qub, typ')
      )
      


(* todo: toput in ast_c.ml
let split_signb_baseb_ii (baseb, ii) = 
  let iis = ii +> List.map (fun (ii,mc) -> ii.Common.str, (ii,mc)) in
  match baseb, iis with
  
  | B.Void, ["void",i1] -> None, [i1]
      
  | B.FloatType (B.CFloat),["float",i1] -> None, [i1]
  | B.FloatType (B.CDouble),["double",i1] -> None, [i1]
  | B.FloatType (B.CLongDouble),["long",i1;"double",i2] -> None,[i1;i2]
      
  | B.IntType (B.CChar), ["char",i1] -> None, [i1]


  | B.IntType (B.Si (sign, base)), xs -> 
      (match sign, base, xs with
      | B.Signed, B.CChar2,   ["signed",i1;"char",i2] -> 
          Some (B.Signed, i1), [i2]
      | B.UnSigned, B.CChar2,   ["unsigned",i1;"char",i2] -> 
          Some (B.UnSigned, i1), [i2]

      | B.Signed, B.CShort, ["short",i1] -> None, [i1]
      | B.Signed, B.CShort, ["signed",i1;"short",i2] -> 
          Some (B.Signed, i1), [i2]
      | B.UnSigned, B.CShort, ["unsigned",i1;"short",i2] -> 
          Some (B.UnSigned, i1), [i2]

      | B.Signed, B.CInt, ["int",i1] -> None, [i1]
      | B.Signed, B.CInt, ["signed",i1;"int",i2] -> 
          Some (B.Signed, i1), [i2]
      | B.UnSigned, B.CInt, ["unsigned",i1;"int",i2] -> 
          Some (B.UnSigned, i1), [i2]

      | B.UnSigned, B.CInt, ["unsigned",i1;] -> 
          Some (B.UnSigned, i1), []

      | B.Signed, B.CLong, ["long",i1] -> None, [i1]
      | B.Signed, B.CLong, ["signed",i1;"long",i2] -> 
          Some (B.Signed, i1), [i2]
      | B.UnSigned, B.CLong, ["unsigned",i1;"long",i2] -> 
          Some (B.UnSigned, i1), [i2]

      | B.Signed, B.CLongLong, ["long",i1;"long",i2] -> None, [i1;i2]
      | B.Signed, B.CLongLong, ["signed",i1;"long",i2;"long",i3] -> 
          Some (B.Signed, i1), [i2;i3]
      | B.UnSigned, B.CLongLong, ["unsigned",i1;"long",i2;"long",i3] -> 
          Some (B.UnSigned, i1), [i2;i3]
      | _ -> failwith "strange type1, maybe because of weird order"
      )
  | _ -> failwith "strange type2, maybe because of weird order"

      *)
and (typeC: (Ast_cocci.typeC, Ast_c.typeC) matcher) = 
 fun typa typb -> 
  match A.unwrap typa, typb with
(*
  | A.BaseType (basea, signaopt), (B.BaseType baseb, ii) -> 
       (* In ii there is a list, sometimes of length 1 or 2 or 3.
        * And even if in baseb we have a Signed Int, that does not mean
        * that ii is of length 2, cos Signed is the default, so if in signa
        * we have Signed explicitely ? we cant "accrocher" this mcode to 
        * something :( So for the moment when there is signed in cocci,
        * we force that there is a signed in c too (done in pattern.ml).
        *)
        let signbopt, iibaseb = split_signb_baseb_ii (baseb, ii) in

        
	(match term basea, baseb with
        |  A.VoidType,  B.Void -> 
            assert (signaopt = None); 
            let ii' = tag_symbols [basea] ii binding in
            (B.BaseType B.Void, ii')
	| A.CharType,  B.IntType B.CChar when signaopt = None -> 
            let ii' = tag_symbols [basea] ii binding in
            (B.BaseType (B.IntType B.CChar), ii')


        | A.CharType,  B.IntType (B.Si (sign, B.CChar2)) when signaopt <> None
          -> 
            let ii' = 
              transform_sign signaopt signbopt ++ 
              tag_symbols [basea] iibaseb  binding 
            in
            B.BaseType (B.IntType (B.Si (sign, B.CChar2))), ii'

	| A.ShortType, B.IntType (B.Si (signb, B.CShort)) ->
            let ii' = 
              transform_sign signaopt signbopt ++ 
              tag_symbols [basea] iibaseb  binding 
            in
            B.BaseType (B.IntType (B.Si (signb, B.CShort))), ii'
	| A.IntType,   B.IntType (B.Si (signb, B.CInt))   ->
            let ii' = 
              transform_sign signaopt signbopt ++ 
              tag_symbols [basea] iibaseb  binding 
            in
            B.BaseType (B.IntType (B.Si (signb, B.CInt))), ii'
	| A.LongType,  B.IntType (B.Si (signb, B.CLong))  ->
            let ii' = 
              transform_sign signaopt signbopt ++ 
              tag_symbols [basea] iibaseb  binding 
            in
            B.BaseType (B.IntType (B.Si (signb, B.CLong))), ii'

	| A.FloatType, B.FloatType (B.CFloat) -> 
            raise Todo
	| A.DoubleType, B.FloatType (B.CDouble) -> 
            raise Todo

        | _, B.IntType (B.Si (_, B.CLongLong)) 
        | _, B.FloatType B.CLongDouble 
           -> raise NoMatch
              

        | _ -> raise NoMatch
            

        )
*)

    | A.Pointer (typa, imult),            (B.Pointer typb, ii) -> 
        fullType typa typb >>= (fun typ' -> 
          (B.Pointer typb) +> tokenf_wrap [imult] ii
        )
        
    | A.Array (typa, _, eaopt, _), (B.Array (ebopt, typb), _) -> 
        raise Todo

    | A.StructUnionName(sua, sa), (B.StructUnionName (sb, sub), ii) -> 
        (* sa is now an ident, not an mcode, old: ... && (term sa) =$= sb *)
        (match ii with
        | [i1;i2] -> 
            if equal_structUnion  (term sua) sub 
            then
              ident DontKnow sa (sb, [i2]) >>= (fun (sb', i2') -> 
              tokenf [wrap_mcode sua] [i1] >>= (fun i1' -> 
                return (B.StructUnionName (sb', sub), i1' ++ i2')
              ))
            else fail
      | _ -> raise Impossible
        )

        

    | A.StructUnionDef(sua, sa, lb, decls, rb), _ -> 
	failwith "to be filled in"

    | A.TypeName sa,  (B.TypeName sb, ii) ->
        if (term sa) =$= sb
        then (B.TypeName sb) +> tokenf_wrap  [sa] ii
        else fail
          
    | _, _ -> fail


and transform_sign signa signb = 
  match signa, signb with
  | None, None -> return []
  | Some signa,  Some (signb, ib) -> 
      if equal_sign (term signa) signb
      then tokenf_one signa ib >>= (fun i' -> return [i'])
      else fail
  | _, _ -> fail


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let (rule_elem_node: (Ast_cocci.rule_elem, Control_flow_c.node) matcher) = 
 fun re node -> 
  let rewrap x = 
    x >>= (fun x' -> return (F.rewrap node x'))
  in
  rewrap (
  match A.unwrap re, F.unwrap node with

  (* note: the order of the clauses is important. *)

  | _, F.Enter | _, F.Exit | _, F.ErrorExit -> fail2

  (* the metaRuleElem contains just '-' information. We dont need to add
   * stuff in the environment. If we need stuff in environment, because
   * there is a + S somewhere, then this will be done via MetaStatement, not
   * via MetaRuleElem. 
   * Can match TrueNode/FalseNode/... so must be placed before those cases.
   *)
  | A.MetaRuleElem(mcode,true,inherited), unwrap_node -> 
     (match unwrap_node with
     | F.CaseNode _
     | F.TrueNode | F.FalseNode | F.AfterNode | F.FallThroughNode
       -> 
         if mcode_contain_plus (mcodekind mcode)
         then failwith "try add stuff on fake node";
         (* minusize or contextize a fake node is ok *)
         return unwrap_node

     | F.EndStatement None -> 
         if mcode_contain_plus (mcodekind mcode)
         then
           let fake_info = Common.fake_parse_info, Ast_c.emptyAnnot in
           let fake_info = Ast_c.al_info fake_info in
           distrf distrf_node (mcodekind mcode) 
             (F.EndStatement (Some fake_info)) 
         else return unwrap_node
         
     | F.EndStatement (Some _) -> raise Impossible (* really ? *)

     | F.FunHeader _ -> failwith "a MetaRuleElem can't transform a headfunc"
     | n -> distrf distrf_node (mcodekind mcode) n
     )

  | A.MetaRuleElem(mcode,false,inherited), unwrap_node -> 
      failwith "shouldn't appear in transformed code"


  (* rene cant have found that a state containing a fake/exit/... should be 
   * transformed 
   *)
  | _, F.EndStatement _ | _, F.CaseNode _
  | _, F.TrueNode | _, F.FalseNode | _, F.AfterNode | _, F.FallThroughNode
    -> fail2

  (* really ? diff between pattern.ml and transformation.ml *)
  | _, F.Fake -> fail2

  (* cas general: a Meta can match everything *)
  (* really ? diff between pattern.ml and transformation.ml *)
  (* failwith "I cant have been called. I can only transform MetaRuleElem." *)
  | A.MetaStmt (ida,true,_metainfo,inherited),  unwrap_node -> 
     (* match only "header"-statement *)
     (match Control_flow_c.extract_fullstatement node with
     | Some stb -> 
         envf inherited (term ida, Ast_c.MetaStmtVal stb) >>= (fun v -> 
           (* do the match v with ... ? *)
           return unwrap_node
         )
     | None -> fail
     )

  | A.MetaStmt (ida,false,_metainfo,inherited),  unwrap_node -> 
      failwith "shouldn't appear in transformed code"



  (* not me?: *)
  | A.MetaStmtList _, _ -> 
      failwith "not handling MetaStmtList"

  (* It is important to put this case before the one that follows, cos
     want to transform a switch, even if cocci does not have a switch
     statement, because we may have put an Exp, and so have to
     transform the expressions inside the switch. *)

  | A.Exp exp, nodeb -> 
     (* Now keep fullstatement inside the control flow node, 
      * so that can then get in a MetaStmtVar the fullstatement to later
      * pp back when the S is in a +. But that means that 
      * Exp will match an Ifnode even if there is no such exp
      * inside the condition of the Ifnode (because the exp may
      * be deeper, in the then branch). So have to not visit
      * all inside a node anymore.
      * 
      * update: j'ai choisi d'accrocher au noeud du CFG à la
      * fois le fullstatement et le partialstatement et appeler le 
      * visiteur que sur le partialstatement.
      *)
      raise Todo

(*

      let bigf = { Visitor_c.default_visitor_c_s with Visitor_c.kexpr_s = 
             (fun (k,_) e -> 
               try transform_e_e exp e   binding 
               with NoMatch -> k e
             )
          }
      in
      let visitor_e = Visitor_c.visitor_expr_k_s bigf in

      (match nodeb with
      | F.Decl declb -> F.Decl (declb +> Visitor_c.visitor_decl_k_s bigf)
      | F.ExprStatement (st, (eopt, ii)) ->  
          F.ExprStatement (st, (eopt +> map_option visitor_e, ii))

      | F.IfHeader (st, (e,ii))     -> F.IfHeader     (st, (visitor_e e, ii))
      | F.SwitchHeader (st, (e,ii)) -> F.SwitchHeader (st, (visitor_e e, ii))
      | F.WhileHeader (st, (e,ii))  -> F.WhileHeader  (st, (visitor_e e, ii))
      | F.DoWhileTail (e,ii)  -> F.DoWhileTail (visitor_e e, ii)

      | F.ForHeader (st, (((e1opt,i1), (e2opt,i2), (e3opt,i3)), ii)) -> 
          F.ForHeader (st,
                       (((e1opt +> Common.map_option visitor_e, i1),
                         (e2opt +> Common.map_option visitor_e, i2),
                         (e3opt +> Common.map_option visitor_e, i3)),
                        ii))
            
      | F.ReturnExpr (st, (e,ii)) -> F.ReturnExpr (st, (visitor_e e, ii))
            
      | F.Case  (st, (e,ii)) -> F.Case (st, (visitor_e e, ii))
      | F.CaseRange (st, ((e1, e2),ii)) -> 
          F.CaseRange (st, ((visitor_e e1, visitor_e e2), ii))

      (* called on transforming a node that does not contain any expr *)
      | _ -> raise Impossible 
      )

*)

  | A.FunHeader (allminus, stoa, tya, ida, oparen, paramsa, cparen),
    F.FunHeader ((idb, (retb, (paramsb, (isvaargs, iidotsb))), stob), ii) -> 
     (match ii with
     | iidb::ioparenb::icparenb::iistob -> 
        if isvaargs 
        then failwith "not handling variable length arguments func";
        let iidotsb' = iidotsb in (* todo *)

        ident LocalFunction ida (idb, [iidb])        >>= (fun (idb', iidb') -> 
        tokenf [oparen;cparen] [ioparenb;icparenb]   >>= (fun iiparensb' -> 
        (* "iso-by-absence" for storage, and return type. *)
        (match tya with
        | None -> 
            if allminus 
            then distrf distrf_type (Ast_cocci.MINUS []) retb       
            else return retb
        | Some tya -> fullType tya retb
        ) >>= (fun retb' -> 
        parameters (seqstyle paramsa) (A.undots paramsa) paramsb 
          >>= (fun paramsb' -> 
        (let stob' = stob in
        let (iistob') = iistob in
        return 
          (F.FunHeader 
              ((idb', (retb', (paramsb', (isvaargs, iidotsb'))), stob'), 
              (iidb'++iiparensb'++iistob'))
          )
        )
          ))))
     | _ -> raise Impossible
     )
      

  | A.Decl decla, F.Decl declb -> 
      declaration decla declb >>= (fun decl' -> 
        return (F.Decl (decl'))
      )

  | A.SeqStart mcode, F.SeqStart (st, level, i1) -> 
      tokenf_one mcode i1 >>= (fun i' -> 
        return (F.SeqStart (st, level, i'))
      )

  | A.SeqEnd mcode, F.SeqEnd (level, i1) -> 
      tokenf_one mcode i1 >>= (fun i' -> 
        return (F.SeqEnd (level, i'))
      )


  | A.ExprStatement (ea, i1), F.ExprStatement (st, (Some eb, ii)) -> 
      expression ea eb >>= (fun e' -> 
      tokenf [i1] ii >>= (fun i' -> 
        return (F.ExprStatement (st, (Some (e'),i' )))
      ))

  | A.IfHeader (i1,i2, ea, i3), F.IfHeader (st, (eb,ii)) -> 
      expression ea eb >>= (fun e' -> 
      tokenf [i1;i2;i3] ii >>= (fun i' -> 
        return (F.IfHeader (st, (e',i')))
      ))

  | A.Else ia, F.Else ib -> 
      tokenf_one ia ib >>= (fun i' -> 
        return (F.Else (i'))
      )
  | A.WhileHeader (i1, i2, ea, i3), F.WhileHeader (st, (eb, ii)) -> 
      expression ea eb >>= (fun e' -> 
      tokenf [i1;i2;i3] ii >>= (fun i' -> 
        return (F.WhileHeader (st, (e', i')))
      ))
  | A.DoHeader ia, F.DoHeader (st, ib) -> 
      tokenf_one ia ib >>= (fun i' -> 
        return (F.DoHeader (st, i'))
      )
  | A.WhileTail (i1,i2,ea,i3,i4), F.DoWhileTail (eb, ii) -> 
      expression ea eb >>= (fun e' -> 
      tokenf [i1;i2;i3;i4] ii >>= (fun i' -> 
        return (F.DoWhileTail (e', i'))
      ))
  | A.ForHeader (i1, i2, ea1opt, i3, ea2opt, i4, ea3opt, i5), 
    F.ForHeader (st, (((eb1opt,ib1), (eb2opt,ib2), (eb3opt,ib3)), ii))
    -> 
      let transform (ea, ia) (eb, ib) = 
        option expression ea eb >>= (fun eopt' -> 
          eopt' +> tokenf_wrap ia ib
        )
      in
      tokenf [i1;i2;i5] ii >>= (fun i' -> 
      transform (ea1opt, [i3]) (eb1opt, ib1) >>= (fun e1' ->  
      transform (ea2opt, [i4]) (eb2opt, ib2) >>= (fun e2' -> 
      transform (ea3opt, []) (eb3opt, ib3) >>= (fun e3' -> 
        return (F.ForHeader (st, ((e1', e2', e3'),i')))
      ))))


  | A.Break (i1, i2), F.Break (st, ((),ii)) -> 
      tokenf [i1;i2] ii >>= (fun i' -> 
        return (F.Break (st, ((), i')))
      )
  | A.Continue (i1, i2), F.Continue (st, ((),ii)) -> 
      tokenf [i1;i2] ii >>= (fun i' -> 
        return (F.Continue (st, ((), i')))
      )
  | A.Return (i1, i2), F.Return (st, ((),ii)) -> 
      tokenf [i1;i2] ii >>= (fun i' -> 
        return (F.Return (st, ((), i')))
      )
  | A.ReturnExpr (i1, ea, i2), F.ReturnExpr (st, (eb, ii)) -> 
      tokenf [i1;i2] ii >>= (fun i' -> 
      expression ea eb >>= (fun e' -> 
        return (F.ReturnExpr (st, (e', i')))
      ))

  | _, F.ExprStatement (_, (None, ii)) -> fail (* happen ? *)

  (* have not a counter part in coccinelle, for the moment *)
  (* todo?: print a warning at least ? *)
  | _, F.SwitchHeader _ 
  | _, F.Label _
  | _, F.Case _  | _, F.CaseRange _  | _, F.Default _
  | _, F.Goto _ 
  | _, F.Asm
  | _, F.IfCpp _
    -> fail2

  | _, _ -> fail 
  )
end


