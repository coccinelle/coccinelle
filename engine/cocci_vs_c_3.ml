open Common open Commonop

module A = Ast_cocci
module B = Ast_c

module F = Control_flow_c

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type sequence = Ordered | Unordered

let seqstyle eas =      
   match A.unwrap eas with 
   | A.DOTS _ -> Ordered 
   | A.CIRCLES _ -> Unordered 
   | A.STARS _ -> failwith "not handling stars"

let (redots : 'a A.dots -> 'a list -> 'a A.dots)=fun eas easundots ->
  A.rewrap eas ( 
    match A.unwrap eas with 
    | A.DOTS _ -> A.DOTS easundots
    | A.CIRCLES _ -> A.CIRCLES easundots
    | A.STARS _ -> A.STARS easundots
  )
        
(* todo? put in semantic_c.ml *)
type info_ident = 
  | Function 
  | LocalFunction (* entails Function *)
  | DontKnow


let term      (s,i,mc) = s
let mcodekind (s,i,mc) = mc

let tuple_of_list1 = function [a] -> a | _ -> failwith "tuple_of_list1"
let tuple_of_list2 = function [a;b] -> a,b | _ -> failwith "tuple_of_list2"
let tuple_of_list3 = function [a;b;c] -> a,b,c | _ -> failwith "tuple_of_list3"
let tuple_of_list4 = function [a;b;c;d] -> a,b,c,d | _ -> failwith "tuple_of_list4"
let tuple_of_list5 = function [a;b;c;d;e] -> a,b,c,d,e | _ -> failwith "tuple_of_list5"

let mcode_contain_plus = function
  | Ast_cocci.CONTEXT (_,Ast_cocci.NOTHING) -> false
  | Ast_cocci.CONTEXT _ -> true
  | Ast_cocci.MINUS (_,[]) -> false
  | Ast_cocci.MINUS (_,x::xs) -> true
  | Ast_cocci.PLUS -> raise Impossible

let mcode_simple_minus = function
  | Ast_cocci.MINUS (_,[]) -> true
  | _ -> false


(*---------------------------------------------------------------------------*)
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

(*---------------------------------------------------------------------------*)
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


(*****************************************************************************)
(* Functor parameter combinators *)
(*****************************************************************************)
(* monad like stuff
 * src: papers on parser combinators in haskell (cf a pearl by meijer in ICFP)
 * 
 * version0: was not tagging the SP, so just tag the C
 *  val (>>=): 
 *   (tin -> 'c tout)  -> ('c -> (tin -> 'b tout)) -> (tin -> 'b tout)
 *   val return : 'b -> tin -> 'b tout
 *   val fail : tin -> 'b tout
 * 
 * version1: now also tag the SP so return a ('a * 'b)
 *)

module type PARAM = 
  sig 
    type tin
    type 'x tout
    val (>>=): 
      (tin -> ('a * 'b) tout)  -> 
      ('a -> 'b -> (tin -> ('c * 'd) tout)) -> 
      (tin -> ('c * 'd) tout)

    val return : ('a * 'b) -> tin -> ('a *'b) tout
    val fail : tin -> ('a * 'b) tout

    val (>||>) : 
      (tin -> 'x tout) ->
      (tin -> 'x tout) -> 
      (tin -> 'x tout)

    val (>|+|>) : 
      (tin -> 'x tout) ->
      (tin -> 'x tout) -> 
      (tin -> 'x tout)

    type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout

    val tokenf : ('a A.mcode, B.info) matcher

    val distrf_e : (string A.mcode, B.expression) matcher
    val distrf_args : 
      (string A.mcode, (Ast_c.argument, Ast_c.il) either list) matcher
    val distrf_type : (string A.mcode, Ast_c.fullType) matcher
    (*
    val distrf_node : Control_flow_c.node2 tdistr
    *)

    val cocciExp : 
      (A.expression, B.expression) matcher -> (A.expression, F.node) matcher

    (* val cocciTy *)

    val envf : 
      bool (*keep*) -> A.inherited -> 
      string * Ast_c.metavar_binding_kind ->
      tin -> 
      (string * Ast_c.metavar_binding_kind) tout

  end

(*****************************************************************************)
(* Functor code, "Cocci vs C" *)
(*****************************************************************************)

module COCCI_VS_C =
  functor (X : PARAM) -> 
struct

type ('a, 'b) matcher = 'a -> 'b  -> X.tin -> ('a * 'b) X.tout

let (>>=) = X.(>>=)
let return = X.return
let fail = X.fail

let (>||>) = X.(>||>)
let (>|+|>) = X.(>|+|>)

let tokenf = X.tokenf

(* should be raise Impossible when called from transformation.ml *)
let fail2 = fail


let (option: ('a,'b) matcher -> ('a option,'b option) matcher)= fun f t1 t2 ->
  match (t1,t2) with
  | (Some t1, Some t2) -> 
      f t1 t2 >>= (fun t1 t2 -> 
        return (Some t1, Some t2)
      )
  | (None, None) -> return (None, None)
  | _ -> fail

(*---------------------------------------------------------------------------*)
(* toc: 
 *  - expression
 *  - ident
 *  - arguments
 *  - parameters
 *  - declaration
 *  - initializers
 *  - type       
 *  - node
 *)

(*---------------------------------------------------------------------------*)
let rec (expression: (Ast_cocci.expression, Ast_c.expression) matcher) =
 fun ea eb -> 
  let wa x = A.rewrap ea x  in
  match A.unwrap ea, eb with
  
  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr (ida,keep,opttypa,inherited), (((expr, opttypb), ii) as expb) ->
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
      X.envf keep inherited (term ida, Ast_c.MetaExprVal expb) >>= (fun _s v ->
        match v with
        | Ast_c.MetaExprVal expa -> 
            if (Lib_parsing_c.al_expr expa =*= Lib_parsing_c.al_expr expb) && 
               match_type
            then 
              X.distrf_e ida expb >>= (fun ida expb -> 
                return (
                  A.MetaExpr (ida,keep,opttypa,inherited)+> A.rewrap ea,
                  expb
                ))
            else fail
        | _ -> raise Impossible
      )

  (* old: | A.Edots _, _ -> raise Impossible. In fact now can also have
   * the Edots inside normal expression, not just in arg lists. in
   * 'x[...];' less: in if(<... x ... y ...>) *)
  | A.Edots (mcode, None), expb    -> 
      X.distrf_e mcode expb >>= (fun mcode expb -> 
        return (
          A.Edots (mcode, None) +> A.rewrap ea , 
          expb
        ))


  | A.Edots (_, Some expr), _    -> failwith "not handling when on Edots"

  | A.MetaConst _, _ -> failwith "not handling MetaConst"
  | A.MetaErr _, _ -> failwith "not handling MetaErr"


  | A.Ident ida,   ((B.Ident idb, typ),ii) ->
      let ib1 = tuple_of_list1 ii in
      ident DontKnow ida (idb, ib1) >>= (fun ida (idb, ib1) -> 
        return (
          ((A.Ident ida)) +> wa, 
          ((B.Ident idb, typ),[ib1])
        ))
          

  (* todo?: handle some isomorphisms in int/float ? can have different
   * format : 1l can match a 1.
   * 
   * todo: normally string can contain some metavar too, so should
   * recurse on the string 
   *)
  | A.Constant (ia1), ((B.Constant (ib) , typ),ii) -> 
      (match term ia1, ib with 
      | A.Int x, B.Int y 
      | A.Char x, B.Char (y,_) (* todo: use kind ? *)
      | A.Float x, B.Float (y,_) (* todo: use floatType ? *)
          -> 
          if x =$= y 
          then 
            let ib1 = tuple_of_list1 ii in 
            tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
              return ( 
                ((A.Constant ia1)) +> wa, 
                ((B.Constant (ib), typ),[ib1])
              ))
          else fail
      | A.String sa, B.String (sb,_kind) -> 
          (match ii with
          | [ib1] -> 
            tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
              return ( 
                ((A.Constant ia1)) +> wa, 
                ((B.Constant (ib), typ),[ib1])
              ))
          | _ -> fail (* multi string, not handled *)
          )
      | _, _ -> fail
      )


  | A.FunCall (ea, ia1, eas, ia2),  ((B.FunCall (eb, ebs), typ),ii) -> 
      (* todo: do special case to allow IdMetaFunc, cos doing the
       * recursive call will be too late, match_ident will not have the
       * info whether it was a function. todo: but how detect when do
       * x.field = f; how know that f is a Func ? By having computed
       * some information before the matching!
       * 
       * Allow match with FunCall containing types. Now ast_cocci allow
       * type in parameter, and morover ast_cocci allow f(...) and those
       * ... could match type. 
       *)
      let (ib1, ib2) = tuple_of_list2 ii in
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
      arguments (seqstyle eas) (A.undots eas) ebs >>= (fun easundots ebs -> 
        let eas = redots eas easundots in
        return (
          ((A.FunCall (ea, ia1, eas, ia2)) +> wa,
          ((B.FunCall (eb, ebs),typ), [ib1;ib2])
        ))))))




  | A.Assignment (ea1, opa, ea2), ((B.Assignment (eb1, opb, eb2), typ),ii) -> 
      let (opbi) = tuple_of_list1 ii in
      if equal_assignOp (term opa) opb 
      then
        expression ea1 eb1 >>= (fun ea1 eb1 -> 
        expression ea2 eb2 >>= (fun ea2 eb2 -> 
        tokenf opa opbi >>= (fun opa opbi -> 
          return (
            ((A.Assignment (ea1, opa, ea2))) +> wa,
            ((B.Assignment (eb1, opb, eb2), typ), [opbi])
        ))))
      else fail

  | A.CondExpr(ea1,ia1,ea2opt,ia2,ea3),((B.CondExpr(eb1,eb2opt,eb3),typ),ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      expression ea1 eb1 >>= (fun ea1 eb1 -> 
      option expression ea2opt eb2opt >>= (fun  ea2opt eb2opt -> 
      expression ea3 eb3 >>= (fun ea3 eb3 -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        return (
          ((A.CondExpr(ea1,ia1,ea2opt,ia2,ea3))) +> wa,
          ((B.CondExpr (eb1, eb2opt, eb3),typ), [ib1;ib2])
        ))))))

  (* todo?: handle some isomorphisms here ? *)
  | A.Postfix (ea, opa), ((B.Postfix (eb, opb), typ),ii) -> 
      let opbi = tuple_of_list1 ii in
      if equal_fixOp (term opa) opb
      then
        expression ea eb >>= (fun ea eb -> 
        tokenf opa opbi >>= (fun opa opbi -> 
          return (
            ((A.Postfix (ea, opa))) +> wa,
            ((B.Postfix (eb, opb), typ),[opbi])
        )))
      else fail
        
        
  | A.Infix (ea, opa), ((B.Infix (eb, opb), typ),ii) -> 
      let opbi = tuple_of_list1 ii in
      if equal_fixOp (term opa) opb
      then
        expression ea eb >>= (fun ea eb -> 
        tokenf opa opbi >>= (fun opa opbi -> 
          return (
            ((A.Infix (ea, opa))) +> wa,
            ((B.Infix (eb, opb), typ),[opbi])
        )))
      else fail

  | A.Unary (ea, opa), ((B.Unary (eb, opb), typ),ii) -> 
      let opbi = tuple_of_list1 ii in
      if equal_unaryOp (term opa) opb
      then
        expression ea eb >>= (fun ea eb -> 
        tokenf opa opbi >>= (fun opa opbi -> 
          return (
            ((A.Unary (ea, opa))) +> wa,
            ((B.Unary (eb, opb), typ),[opbi])
        )))
      else fail



  | A.Binary (ea1, opa, ea2), ((B.Binary (eb1, opb, eb2), typ),ii) -> 
      let opbi = tuple_of_list1 ii in
      if equal_binaryOp (term opa) opb
      then 
        expression ea1 eb1 >>= (fun ea1 eb1 -> 
        expression ea2 eb2 >>= (fun ea2 eb2 -> 
        tokenf opa opbi >>= (fun opa opbi -> 
          return (
            ((A.Binary (ea1, opa, ea2))) +> wa,
            ((B.Binary (eb1, opb, eb2), typ),[opbi]
          )))))
      else fail


  (* todo?: handle some isomorphisms here ?  (with pointers = Unary Deref) *)
  | A.ArrayAccess (ea1, ia1, ea2, ia2),((B.ArrayAccess (eb1, eb2), typ),ii) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      expression ea1 eb1 >>= (fun ea1 eb1 -> 
      expression ea2 eb2 >>= (fun ea2 eb2 -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        return (
          ((A.ArrayAccess (ea1, ia1, ea2, ia2))) +> wa,
          ((B.ArrayAccess (eb1, eb2),typ), [ib1;ib2])
        )))))

  (* todo?: handle some isomorphisms here ? *)
  | A.RecordAccess (ea, ia1, ida), ((B.RecordAccess (eb, idb), typ),ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      ident DontKnow ida (idb, ib2) >>= (fun ida (idb, ib2) -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      expression ea eb >>= (fun ea eb -> 
        return (
          ((A.RecordAccess (ea, ia1, ida))) +> wa,
          ((B.RecordAccess (eb, idb), typ), [ib1;ib2])
        ))))



  | A.RecordPtAccess (ea,ia1,ida),((B.RecordPtAccess (eb, idb), typ), ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      ident DontKnow ida (idb, ib2) >>= (fun ida (idb, ib2) -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      expression ea eb >>= (fun ea eb -> 
        return (
          ((A.RecordPtAccess (ea, ia1, ida))) +> wa,
          ((B.RecordPtAccess (eb, idb), typ), [ib1;ib2])
        ))))


  (* todo?: handle some isomorphisms here ? 
   * todo?: do some iso-by-absence on cast ? 
   *    by trying | ea, B.Case (typb, eb) -> match_e_e ea eb ?
   *)

  | A.Cast (ia1, typa, ia2, ea), ((B.Cast (typb, eb), typ),ii) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      fullType typa typb >>= (fun typa typb -> 
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        return (
          ((A.Cast (ia1, typa, ia2, ea))) +> wa,
          ((B.Cast (typb, eb),typ),[ib1;ib2])
        )))))

  | A.SizeOfExpr (ia1, ea), ((B.SizeOfExpr (eb), typ),ii) -> 
      let ib1 = tuple_of_list1 ii in
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        return (
          ((A.SizeOfExpr (ia1, ea))) +> wa,
          ((B.SizeOfExpr (eb), typ),[ib1])
      )))

  | A.SizeOfType (ia1, ia2, typa, ia3), ((B.SizeOfType typb, typ),ii) -> 
      let (ib1,ib2,ib3) = tuple_of_list3 ii in
      fullType typa typb >>= (fun typa typb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
      tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
        return (
          ((A.SizeOfType (ia1, ia2, typa, ia3))) +> wa,
          ((B.SizeOfType (typb),typ),[ib1;ib2;ib3])
      )))))


  (* todo? iso ? allow all the combinations ? *)
  | A.Paren (ia1, ea, ia2), ((B.ParenExpr (eb), typ),ii) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        return (
          ((A.Paren (ia1, ea, ia2))) +> wa,
          ((B.ParenExpr (eb), typ), [ib1;ib2])
      ))))




  | A.NestExpr _, _ -> failwith "not my job to handle NestExpr"
  
  (* only in arg lists *)
  | A.MetaExprList _, _   
  | A.TypeExp _, _ 
  | A.EComma _, _  
  | A.Ecircles _, _ 
  | A.Estars _, _   
      -> raise Impossible

  | A.DisjExpr eas, eb -> 
      eas +> List.fold_left (fun acc ea -> acc >|+|> (expression ea eb)) fail

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
and (ident: info_ident -> (Ast_cocci.ident, string * Ast_c.info) matcher) = 
 fun infoidb ida (idb, iib) -> 
  match A.unwrap ida with
  | A.Id sa -> 
      if (term sa) =$= idb then
      tokenf sa iib >>= (fun sa iib -> 
        return (
          ((A.Id sa)) +> A.rewrap ida,
          (idb, iib)
        ))
      else fail


  | A.MetaId(mida,keep,inherited) -> 
      (* get binding, assert =*=,  distribute info *)
      X.envf keep inherited (term mida, Ast_c.MetaIdVal (idb)) >>= (fun _s v ->
        match v with
        | Ast_c.MetaIdVal sa -> 
            if (sa =$= idb) 
            then 
              tokenf mida iib >>= (fun mida iib -> 
                return (
                  ((A.MetaId (mida, keep, inherited)) +> A.rewrap ida,
                  (idb, iib)
                  )))
            else fail
        | _ -> raise Impossible
      )

  | A.MetaFunc(mida,keep,inherited) -> 
      (match infoidb with 
      | LocalFunction | Function -> 
          X.envf keep inherited(term mida,Ast_c.MetaFuncVal idb)>>=(fun _s v ->
            match v with
            | Ast_c.MetaFuncVal sa -> 
                if(sa =$= idb) 
                then 
                  tokenf mida iib >>= (fun mida iib -> 
                    return (
                      ((A.MetaFunc(mida,keep,inherited))) +> A.rewrap ida,
                      (idb, iib)
                    ))
                else fail
            | _ -> raise Impossible
          )
      | DontKnow -> failwith "MetaFunc, need more semantic info about id"
      )

  | A.MetaLocalFunc(mida,keep,inherited) -> 
      (match infoidb with 
      | LocalFunction -> 
          X.envf keep inherited (term mida,Ast_c.MetaLocalFuncVal idb) >>=
            (fun _s v ->
            match v with
            | Ast_c.MetaLocalFuncVal sa -> 
                if(sa =$= idb) 
                then 
                  tokenf mida iib >>= (fun mida iib -> 
                    return (
                      ((A.MetaFunc(mida,keep,inherited))) +> A.rewrap ida,
                      (idb, iib)
                    ))
                else fail
            | _ -> raise Impossible
          )
      | Function -> fail
      | DontKnow -> failwith "MetaLocalFunc, need more semantic info about id"
      )

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
      arguments_bis eas (Ast_c.split_comma ebs) >>= (fun eas ebs_splitted -> 
        return (eas, (Ast_c.unsplit_comma ebs_splitted))
      )

(* todo: because '...' can match nothing, need to take care when have 
 * ', ...'   or '...,'  as in  f(..., X, Y, ...). It must match
 * f(1,2) for instance.
 * 
 * old: Must do some try, for instance when f(...,X,Y,...) have to
 * test the transfo for all the combinaitions (and if multiple transfo
 * possible ? pb ? => the type is to return a expression option ? use
 * some combinators to help ?
 * update: with the tag-SP approach, no more a problem.
 *)

and arguments_bis = fun eas ebs -> 
  match eas, ebs with
  | [], [] -> return ([], [])
  | [], eb::ebs -> fail
  | ea::eas, ebs -> 
      (match A.unwrap ea, ebs with
      | A.Edots (mcode, optexpr), ys -> 
          (* todo: if optexpr, then a WHEN and so may have to filter yys *)
          if optexpr <> None then failwith "not handling when in argument";

          (* '...' can take more or less the beginnings of the arguments *)
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (
              X.distrf_args mcode startxs >>= (fun mcode startxs ->
              arguments_bis eas endxs >>= (fun eas endxs -> 
                return (
                  (A.Edots (mcode, optexpr) +> A.rewrap ea) ::eas,
                  startxs ++ endxs
                )))
              )
            ) fail 

      | A.EComma ia1, Right ii::ebs -> 
          let ib1 = tuple_of_list1 ii in
          tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
          arguments_bis eas ebs >>= (fun eas ebs -> 
            return (
              (A.EComma ia1 +> A.rewrap ea)::eas,
              (Right [ib1])::ebs
            )
          ))
      | A.EComma ia1, _ -> fail

      | A.MetaExprList (ida, keep, inherited), ys -> 
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            let startxs' = Ast_c.unsplit_comma startxs in
            acc >||> (
              X.envf keep inherited (term ida, Ast_c.MetaExprListVal startxs')
              >>= (fun _s v -> 
                  match v with
                  | Ast_c.MetaExprListVal startxsenv -> 
                      (* TODO
                      if (Lib_parsing_c.al_expr expa =*= 
                          Lib_parsing_c.al_expr expb)
                      *)
                     X.distrf_args ida (Ast_c.split_comma startxs')
                  | _ -> raise Impossible) 
              >>= (fun ida startxs -> 
                  arguments_bis eas endxs >>= (fun eas endxs -> 
                    return (
                      (A.MetaExprList(ida,keep,inherited)) +> A.rewrap ea::eas,
                      startxs ++ endxs
                    ))
                  )
                )
            ) fail 


      | _unwrapx, (Left eb)::ebs -> 
          argument ea eb >>= (fun ea eb -> 
          arguments_bis eas ebs >>= (fun eas ebs -> 
            return (ea::eas, Left eb::ebs)
          ))
      | _unwrapx, (Right y)::ys -> raise Impossible
      | _unwrapx, [] -> fail
      )
            
      
and argument arga argb = 
   match A.unwrap arga, argb with
  | A.TypeExp tya,  Right (B.ArgType (tyb, (sto, iisto))) ->
      if sto <> (B.NoSto, false)
      then failwith "the argument have a storage and ast_cocci does not have"
      else 
        fullType tya tyb >>= (fun tya tyb -> 
          return (
            (A.TypeExp tya) +> A.rewrap arga,
            (Right (B.ArgType (tyb, (sto, iisto))))
        ))

  | A.TypeExp tya,  _                                  -> fail
  | _,              Right (B.ArgType (tyb, sto_iisto)) -> fail
  | _, Left argb -> 
      expression arga argb >>= (fun arga argb -> 
        return (arga, Left argb)
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


(* ------------------------------------------------------------------------- *)

and (transform_initialiser: (Ast_cocci.initialiser, Ast_c.initialiser) matcher)
  =  fun inia inib -> 
    raise Todo

and transform_initialisers = fun ias ibs ->
  raise Todo

(* ------------------------------------------------------------------------- *)
and (fullType: (Ast_cocci.fullType, Ast_c.fullType) matcher) = 
 fun typa typb -> 

   match A.unwrap typa, typb with
   | A.Type(cv,ty1), ((qu,il),ty2) ->

       if qu.B.const && qu.B.volatile 
       then pr2 ("warning: the type is both const & volatile but cocci " ^ 
                 "does not handle that");

	(* Drop out the const/volatile part that has been matched.
         * This is because a SP can contain  const T v; in which case
         * later in match_t_t when we encounter a T, we must not add in
         * the environment the whole type.
         *)
       

       (match cv with
       (* "iso-by-absence" *)
       | None -> fullTypebis ty1 ((qu,il), ty2) >>= (fun ty1 fullty2 -> 
           return (
             (A.Type(None, ty1)) +> A.rewrap typa,
             fullty2
           ))
       | Some x -> raise Todo
(* XXX
	let new_il todrop = List.filter (fun (pi,_) -> 
          not (pi.Common.str = todrop)) 
        in

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
*)


       )

   | A.OptType(_), _  | A.UniqueType(_), _ | A.MultiType(_), _ 
       -> failwith "not handling Opt/Unique/Multi on type"

 

(*
 * Why not (Ast_cocci.typeC, Ast_c.typeC) matcher ?
 * because when there is MetaType, we want that T record the whole type, 
 * including the qualifier, and so this type (and the new_il function in
 * preceding function).
*)

and (fullTypebis: (Ast_cocci.typeC, Ast_c.fullType) matcher) = 
  fun ta tb -> 
  match A.unwrap ta, tb with

  (* cas general *)
  | A.MetaType(ida,keep, inherited),  typb -> 
      X.envf keep inherited (term ida, B.MetaTypeVal typb) >>= (fun _s v ->  
        match v with
        | B.MetaTypeVal typa  -> 
          if (Lib_parsing_c.al_type typa =*= Lib_parsing_c.al_type typb)
          then X.distrf_type ida typb >>= (fun ida typb -> 
            return (
              A.MetaType(ida,keep, inherited) +> A.rewrap ta,
              typb
            ))
          else fail
        | _ -> raise Impossible
      )
  | unwrap, (qub, typb) -> 
      typeC ta typb >>= (fun ta typb -> 
        return (ta, (qub, typb))
      )


and (typeC: (Ast_cocci.typeC, Ast_c.typeC) matcher) = 
  fun ta tb -> 
  match A.unwrap ta, tb with
  | A.BaseType (basea, signaopt), (B.BaseType baseb, ii) -> 
      (* In ii there is a list, sometimes of length 1 or 2 or 3.
       * And even if in baseb we have a Signed Int, that does not mean
       * that ii is of length 2, cos Signed is the default, so if in signa
       * we have Signed explicitely ? we cant "accrocher" this mcode to 
       * something :( So for the moment when there is signed in cocci,
       * we force that there is a signed in c too (done in pattern.ml).
       *)
      let signbopt, iibaseb = split_signb_baseb_ii (baseb, ii) in

        
      (* handle some iso on type ? (cf complex C rule for possible implicit
	 casting) *)
      (match term basea, baseb with
      | A.VoidType,  B.Void 
      | A.FloatType, B.FloatType (B.CFloat)
      | A.DoubleType, B.FloatType (B.CDouble) -> 
           assert (signaopt = None); 
           let (ibaseb) = tuple_of_list1 ii in 
           tokenf basea ibaseb >>= (fun basea ibaseb -> 
             return (
               (A.BaseType (basea, signaopt)) +> A.rewrap ta,
               (B.BaseType baseb, [ibaseb])
             ))
            
      | A.CharType,  B.IntType B.CChar when signaopt = None -> 
          let ibaseb = tuple_of_list1 ii in
           tokenf basea ibaseb >>= (fun basea ibaseb -> 
             return (
               (A.BaseType (basea, signaopt)) +> A.rewrap ta,
               (B.BaseType (B.IntType B.CChar), [ibaseb])
             ))
            
      | A.CharType,B.IntType (B.Si (_sign, B.CChar2)) when signaopt <> None -> 
          let ibaseb = tuple_of_list1 iibaseb in
          sign signaopt signbopt >>= (fun signaopt iisignbopt -> 
          tokenf basea ibaseb >>= (fun basea ibaseb -> 
            return (
               (A.BaseType (basea, signaopt)) +> A.rewrap ta,
               (B.BaseType (baseb), iisignbopt ++ [ibaseb])
               )))
          
      | A.ShortType, B.IntType (B.Si (_, B.CShort)) 
      | A.IntType,   B.IntType (B.Si (_, B.CInt))   
      | A.LongType,  B.IntType (B.Si (_, B.CLong))  ->
          let ibaseb = tuple_of_list1 iibaseb in
          sign signaopt signbopt >>= (fun signaopt iisignbopt -> 
          tokenf basea ibaseb >>= (fun basea ibaseb -> 
            return (
               (A.BaseType (basea, signaopt)) +> A.rewrap ta,
               (B.BaseType (baseb), iisignbopt ++ [ibaseb])
               )))

            
      | _, B.IntType (B.Si (_, B.CLongLong)) 
      | _, B.FloatType B.CLongDouble 
          -> 
          pr2 "warning: long long or long double not handled by ast_cocci";
          fail
          
          
      | _, _ -> fail
          
          
      )


    (* todo? iso with array *)
    | A.Pointer (typa, iamult),            (B.Pointer typb, ii) -> 
        let (ibmult) = tuple_of_list1 ii in 
        fullType typa typb >>= (fun typa typb -> 
        tokenf iamult ibmult >>= (fun iamult ibmult -> 
          return (
            (A.Pointer (typa, iamult)) +> A.rewrap ta,
            (B.Pointer typb, [ibmult])
          )))
        
    (* todo: handle the iso on optionnal size specifification ? *)
    | A.Array (typa, ia1, eaopt, ia2), (B.Array (ebopt, typb), ii) -> 
        let (ib1, ib2) = tuple_of_list2 ii in
        fullType typa typb >>= (fun typa typb -> 
        option expression eaopt ebopt >>= (fun eaopt ebopt -> 
        tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
          return (
            (A.Array (typa, ia1, eaopt, ia2)) +> A.rewrap ta,
            (B.Array (ebopt, typb), [ib1;ib2])
          )))))


     (* todo: could also match a Struct that has provided a name *)
    | A.StructUnionName(sua, sa), (B.StructUnionName (sb, sub), ii) -> 
        (* sa is now an ident, not an mcode, old: ... && (term sa) =$= sb *)
        let (ib1, ib2) = tuple_of_list2 ii in
        if equal_structUnion  (term sua) sub 
        then
          ident DontKnow sa (sb, ib2) >>= (fun sa (sb, ib2) -> 
          tokenf sua ib1 >>= (fun sua ib1 -> 
            return (
              (A.StructUnionName (sua, sa)) +> A.rewrap ta,
              (B.StructUnionName (sb, sub), [ib1;ib2])
              )))
        else fail
        

    | A.StructUnionDef(sua, sa, lb, decls, rb), _ -> 
	failwith "to be filled in"

   (* todo? handle isomorphisms ? because Unsigned Int can be match on a 
    * uint in the C code. But some CEs consists in renaming some types,
    * so we don't want apply isomorphisms every time. 
    *) 
    | A.TypeName sa,  (B.TypeName sb, ii) ->
        let (isb) = tuple_of_list1 ii in
        if (term sa) =$= sb
        then 
          tokenf sa isb >>= (fun sa isb -> 
          return (
            (A.TypeName sa) +> A.rewrap ta,
            (B.TypeName sb, [isb])
          ))
        else fail
          
    | _, _ -> fail

(* todo: iso on sign, if not mentioned then free.  tochange? 
 * but that require to know if signed int because explicit
 * signed int,  or because implicit signed int.
 *)

and sign signa signb = 
  match signa, signb with
  | None, None -> return (None, [])
  | Some signa,  Some (signb, ib) -> 
      if equal_sign (term signa) signb
      then tokenf signa ib >>= (fun signa ib -> 
        return (Some signa, [ib])
      )
      else fail
  | _, _ -> fail

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let (rule_elem_node: (Ast_cocci.rule_elem, Control_flow_c.node) matcher) = 
 fun re node -> 
  let rewrap x = 
    x >>= (fun a b -> return (A.rewrap re a, F.rewrap node b))
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

  | A.MetaRuleElem(mcode,keep,inherited), unwrap_node -> 
      raise Todo
(* XXX
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

*)


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
  | A.MetaStmt (ida,keep,_metainfo,inherited),  unwrap_node -> 
      (* match only "header"-statement *)
      raise Todo
(* XXX
     (match Control_flow_c.extract_fullstatement node with
     | Some stb -> 
         envf inherited (term ida, Ast_c.MetaStmtVal stb) >>= (fun v -> 
           (* do the match v with ... ? *)
           return unwrap_node
         )
     | None -> fail
     )
*)

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

      X.cocciExp expression exp node >>= (fun exp node -> 
        return (
          A.Exp exp,
          F.unwrap node
        )
      )

  | A.FunHeader (need_to_do_something_with_this_mcodekind,
		 allminus, stoa, tya, ida, oparen, paramsa, cparen),
    F.FunHeader ((idb, (retb, (paramsb, (isvaargs, iidotsb))), stob), ii) -> 
      raise Todo
(* XXX
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
            then distrf distrf_type (Ast_cocci.MINUS(None(*?*),[])) retb
            else return retb
        | Some tya -> fullType tya retb
        ) >>= (fun retb' -> 
        parameters (seqstyle paramsa) (A.undots paramsa) paramsb 
          >>= (fun paramsb' -> 
        (let stob' = stob in
        let (iistob') = iistob in
         (* TODO manage storage *)
        return 
          (F.FunHeader 
              ((idb', (retb', (paramsb', (isvaargs, iidotsb'))), stob'), 
              (iidb'++iiparensb'++iistob'))
          )
        )
          ))))
     | _ -> raise Impossible
     )
*)





  | A.Decl (need_to_do_something_with_this_mcodekind,decla), F.Decl declb -> 
      raise Todo
(* XXX
      declaration decla declb >>= (fun decl' -> 
        return (F.Decl (decl'))
      )
*)


  | A.SeqStart mcode, F.SeqStart (st, level, i1) -> 
      tokenf mcode i1 >>= (fun mcode i1 -> 
        return (
          A.SeqStart mcode, 
          F.SeqStart (st, level, i1)
        ))

  | A.SeqEnd mcode, F.SeqEnd (level, i1) -> 
      tokenf mcode i1 >>= (fun mcode i1 -> 
        return (
          A.SeqEnd mcode,
          F.SeqEnd (level, i1)
          ))

  | A.ExprStatement (ea, ia1), F.ExprStatement (st, (Some eb, ii)) -> 
      let ib1 = tuple_of_list1 ii in 
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        return (
          A.ExprStatement (ea, ia1),
          F.ExprStatement (st, (Some eb, [ib1]))
        )
      ))


  | A.IfHeader (ia1,ia2, ea, ia3), F.IfHeader (st, (eb,ii)) -> 
      let (ib1, ib2, ib3) = tuple_of_list3 ii in
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
      tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
        return (
          A.IfHeader (ia1, ia2, ea, ia3),
          F.IfHeader (st, (eb,[ib1;ib2;ib3]))
        )))))

  | A.Else ia, F.Else ib -> 
      tokenf ia ib >>= (fun ia ib -> 
        return (A.Else ia, F.Else ib)
      )

  | A.WhileHeader (ia1, ia2, ea, ia3), F.WhileHeader (st, (eb, ii)) -> 
      let (ib1, ib2, ib3) = tuple_of_list3 ii in
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
      tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
        return (
          A.WhileHeader (ia1, ia2, ea, ia3), 
          F.WhileHeader (st, (eb, [ib1;ib2;ib3]))
        )))))

  | A.DoHeader ia, F.DoHeader (st, ib) -> 
      tokenf ia ib >>= (fun ia ib -> 
        return (
          A.DoHeader ia, 
          F.DoHeader (st, ib)
        ))
  | A.WhileTail (ia1,ia2,ea,ia3,ia4), F.DoWhileTail (eb, ii) -> 
      let (ib1, ib2, ib3, ib4) = tuple_of_list4 ii in
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
      tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
      tokenf ia4 ib4 >>= (fun ia4 ib4 -> 
        return (
          A.WhileTail (ia1,ia2,ea,ia3,ia4), 
          F.DoWhileTail (eb, [ib1;ib2;ib3;ib4])
        ))))))
      

  | A.ForHeader (ia1, ia2, ea1opt, ia3, ea2opt, ia4, ea3opt, ia5), 
    F.ForHeader (st, (((eb1opt,ib3s), (eb2opt,ib4s), (eb3opt,ib4vide)), ii))
    -> 
      assert (null ib4vide);
      let (ib1, ib2, ib5) = tuple_of_list3 ii in
      let ib3 = tuple_of_list1 ib3s in
      let ib4 = tuple_of_list1 ib4s in
      
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
      tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
      tokenf ia4 ib4 >>= (fun ia4 ib4 -> 
      tokenf ia5 ib5 >>= (fun ia5 ib5 -> 
      option expression ea1opt eb1opt >>= (fun ea1opt eb1opt -> 
      option expression ea2opt eb2opt >>= (fun ea2opt eb2opt -> 
      option expression ea3opt eb3opt >>= (fun ea3opt eb3opt -> 
        return (
          A.ForHeader (ia1, ia2, ea1opt, ia3, ea2opt, ia4, ea3opt, ia5),
          F.ForHeader (st, (((eb1opt,[ib3]), (eb2opt,[ib4]), (eb3opt,[])),
                           [ib1;ib2;ib5]))

        )))))))))


  | A.Break (ia1, ia2), F.Break (st, ((),ii)) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        return (
          A.Break (ia1, ia2), 
          F.Break (st, ((),[ib1;ib2]))
        )))

  | A.Continue (ia1, ia2), F.Continue (st, ((),ii)) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        return (
          A.Continue (ia1, ia2), 
          F.Continue (st, ((),[ib1;ib2]))
        )))

  | A.Return (ia1, ia2), F.Return (st, ((),ii)) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        return (
          A.Return (ia1, ia2), 
          F.Return (st, ((),[ib1;ib2]))
        )))

  | A.ReturnExpr (ia1, ea, ia2), F.ReturnExpr (st, (eb, ii)) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
      expression ea eb >>= (fun ea eb -> 
        return (
          A.ReturnExpr (ia1, ea, ia2), 
          F.ReturnExpr (st, (eb, [ib1;ib2]))
        ))))


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

