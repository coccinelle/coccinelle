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


let mcode_contain_plus = function
  | Ast_cocci.CONTEXT (_,Ast_cocci.NOTHING) -> false
  | Ast_cocci.CONTEXT _ -> true
  | Ast_cocci.MINUS (_,[]) -> false
  | Ast_cocci.MINUS (_,x::xs) -> true
  | Ast_cocci.PLUS -> raise Impossible

let mcode_simple_minus = function
  | Ast_cocci.MINUS (_,[]) -> true
  | _ -> false


(* In transformation.ml sometime I build some mcodekind myself and
 * julia has put None for the pos. But there is no possible raise
 * NoMatch in those cases because it is for the minusall trick or for
 * the distribute, so either have to build those pos, in fact a range,
 * because for the distribute have to erase a fullType with one
 * mcodekind, or add an argument to tag_with_mck such as "safe" that
 * don't do the check_pos. Hence this DontCarePos constructor. *)

let minusizer = 
  "fake", 
  {Ast_cocci.line = 0; column =0},
  (Ast_cocci.MINUS(Ast_cocci.DontCarePos, []))

let generalize_mcode ia = 
  let (s1, i, mck) = ia in
  (s1, i, 
  match mck with
  | Ast_cocci.PLUS -> raise Impossible
  | Ast_cocci.CONTEXT (Ast_cocci.NoPos,x) -> 
      Ast_cocci.CONTEXT (Ast_cocci.DontCarePos,x)
  | Ast_cocci.MINUS   (Ast_cocci.NoPos,x) -> 
      Ast_cocci.MINUS   (Ast_cocci.DontCarePos,x)
  | _ -> raise Impossible
  )



(*---------------------------------------------------------------------------*)


(* 0x0 is equivalent to 0,  value format isomorphism *)
let equal_c_int s1 s2 = 
  try 
    int_of_string s1 = int_of_string s2
  with Failure("int_of_string") -> 
    s1 = s2



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
(* could put in ast_c.ml, next to the split/unsplit_comma *)
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

      | B.Signed, B.CInt, ["signed",i1;] -> 
          Some (B.Signed, i1), []
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

type mode = PatternMode | TransformMode

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

    val (>&&>) : (tin -> bool) -> (tin -> 'x tout) -> (tin -> 'x tout)

    type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout

    val mode : mode

    val tokenf : ('a A.mcode, B.info) matcher
    val tokenf_mck : (A.mcodekind, B.info) matcher

    val distrf_e : (string A.mcode, B.expression) matcher
    val distrf_args : 
      (string A.mcode, (Ast_c.argument, Ast_c.il) either list) matcher
    val distrf_type : (string A.mcode, Ast_c.fullType) matcher
    val distrf_params : 
      (string A.mcode, (Ast_c.parameterType, Ast_c.il) either list) matcher
    val distrf_param : 
      (string A.mcode, Ast_c.parameterType) matcher
    val distrf_node : (string A.mcode, Control_flow_c.node) matcher

    val cocciExp : 
      (A.expression, B.expression) matcher -> (A.expression, F.node) matcher

    val cocciTy : 
      (A.fullType, B.fullType) matcher -> (A.fullType, F.node) matcher

    val envf : 
      A.keep_binding -> A.inherited -> 
      string * Ast_c.metavar_binding_kind ->
      tin -> 
      (string * Ast_c.metavar_binding_kind) tout

    val all_bound : string list -> (tin -> bool)

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
let (>&&>) = X.(>&&>)

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
  X.all_bound (A.get_inherited ea) >&&>
  let wa x = A.rewrap ea x  in
  match A.unwrap ea, eb with
  
  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr (ida,keep,opttypa,inherited), (((expr, opttypb), ii) as expb) ->

      (match opttypa, opttypb with
        | None, _ -> return ((),())
        | Some tas, Some tb -> 
            tas +> List.fold_left (fun acc ta ->  
              acc >||> (
                if Types.compatible_type ta tb
                then return ((),())
                else fail
              )) fail
        | Some _, None -> 
            pr2 ("I don't have the type information. Certainly a pb in " ^
                         "annotate_typer.ml");
            fail
      ) >>= (fun () () ->


      (* get binding, assert =*=,  distribute info in ida *)
      X.envf keep inherited (term ida, Ast_c.MetaExprVal expb) >>= (fun _s v ->
        (* todo: now that we have tagged SP, useless to check if what is
         * in env match what is in C because the tag on ida will detect
         * it also sooner or later
         *)
           
        match v with
        (* the expa is 'abstract-lined' so should not be the base of 
         *  futur processing. Just here to check. Then use expb! 
         *)
        | Ast_c.MetaExprVal expa -> 
            if (Lib_parsing_c.al_expr expa =*= Lib_parsing_c.al_expr expb)
            then 
              X.distrf_e ida expb >>= (fun ida expb -> 
                return (
                  A.MetaExpr (ida,keep,opttypa,inherited)+> A.rewrap ea,
                  expb
                ))
            else fail
        | _ -> raise Impossible
      ))

  (* old: 
   * | A.MetaExpr(ida,false,opttypa,_inherited), expb ->
   *   D.distribute_mck (mcodekind ida) D.distribute_mck_e expb binding
   * but bug! because if have not tagged SP, then transform without doing
   * any checks. Hopefully now have tagged SP technique.
   *)


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
      let do1 () = 
        let ib1 = tuple_of_list1 ii in 
        tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
          return ( 
            ((A.Constant ia1)) +> wa, 
            ((B.Constant (ib), typ),[ib1])
          ))
      in

      (match term ia1, ib with 
      | A.Int x, B.Int y when equal_c_int x y -> do1 ()
      | A.Char x, B.Char (y,_) when x =$= y  (* todo: use kind ? *)
          -> do1 ()
      | A.Float x, B.Float (y,_) when x =$= y (* todo: use floatType ? *)
          -> do1 ()

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

 (* Because of Exp cant put a raise Impossible; have to put a fail *)

 (* have not a counter part in coccinelle, for the moment *) 
  | _, ((B.Sequence _,_),_) 
  | _, ((B.StatementExpr _,_),_) 
  | _, ((B.Constructor,_),_) 
    -> fail

  | _, _ -> fail



(* ------------------------------------------------------------------------- *)
and (ident: info_ident -> (Ast_cocci.ident, string * Ast_c.info) matcher) = 
 fun infoidb ida (idb, iib) -> 
  X.all_bound (A.get_inherited ida) >&&>
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
  match seqstyle with
  | Unordered -> failwith "not handling ooo"
  | Ordered -> 
      arguments_bis eas (Ast_c.split_comma ebs) >>= (fun eas ebs_splitted -> 
        return (eas, (Ast_c.unsplit_comma ebs_splitted))
      )
(* because '...' can match nothing, need to take care when have 
 * ', ...'   or '...,'  as in  f(..., X, Y, ...). It must match
 * f(1,2) for instance.
 * So I have added special cases such as (if startxs = []) and code
 * in the Ecomma matching rule.
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
      X.all_bound (A.get_inherited ea) >&&>
      (match A.unwrap ea, ebs with
      | A.Edots (mcode, optexpr), ys -> 
          (* todo: if optexpr, then a WHEN and so may have to filter yys *)
          if optexpr <> None then failwith "not handling when in argument";

          (* '...' can take more or less the beginnings of the arguments *)
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (

              (* allow '...', and maybe its associated ',' to match nothing *)
              (if startxs = []
              then
                if mcode_contain_plus (mcodekind mcode)
                then fail 
                  (* failwith "I have no token that I could accroche myself on" *)
                else return (mcode, [])
              else 
                (* subtil: we dont want the '...' to match until the
                 * comma. cf -test pb_params_iso. We would get at
                 * "already tagged" error.
                 * this is because both f (... x, ...) and f (..., x, ...)
                 * would match a  f(x,3)  with our "optional-comma" strategy.
                 *)
                  (match Common.last startxs with
                  | Right _ -> fail
                  | Left _ -> 
                      X.distrf_args mcode startxs
                  )
              )
              >>= (fun mcode startxs ->
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
      | A.EComma ia1, ebs -> 
          (* allow ',' to maching nothing *)
          if mcode_contain_plus (mcodekind ia1)
          then fail
          else 
            (arguments_bis eas ebs) (* try optional comma trick *)

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
  X.all_bound (A.get_inherited arga) >&&>
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
(* todo? facto code with argument ? *)
and (parameters: sequence -> 
      (Ast_cocci.parameterTypeDef list, Ast_c.parameterType Ast_c.wrap2 list)
        matcher) = 
 fun seqstyle eas ebs ->
  match seqstyle with
  | Unordered -> failwith "not handling ooo"
  | Ordered -> 
      parameters_bis eas (Ast_c.split_comma ebs) >>= (fun eas ebs_splitted -> 
        return (eas, (Ast_c.unsplit_comma ebs_splitted))
      )


and parameters_bis eas ebs = 
  match eas, ebs with
  | [], [] -> return ([], [])
  | [], eb::ebs -> fail
  | ea::eas, ebs -> 
      X.all_bound (A.get_inherited ea) >&&>
      (match A.unwrap ea, ebs with
      | A.Pdots (mcode), ys -> 

          (* '...' can take more or less the beginnings of the arguments *)
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (

              (if startxs = []
              then
                if mcode_contain_plus (mcodekind mcode)
                then fail 
                  (* failwith "I have no token that I could accroche myself on" *)
                else return (mcode, [])
              else 
                (match Common.last startxs with
                | Right _ -> fail
                | Left _ -> 
                    X.distrf_params mcode startxs
                )
              ) >>= (fun mcode startxs ->
              parameters_bis eas endxs >>= (fun eas endxs -> 
                return (
                  (A.Pdots (mcode) +> A.rewrap ea) ::eas,
                  startxs ++ endxs
                )))
              )
            ) fail 

      | A.PComma ia1, Right ii::ebs -> 
          let ib1 = tuple_of_list1 ii in
          tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
          parameters_bis eas ebs >>= (fun eas ebs -> 
            return (
              (A.PComma ia1 +> A.rewrap ea)::eas,
              (Right [ib1])::ebs
            )
          ))

      | A.PComma ia1, ebs -> 
          if mcode_contain_plus (mcodekind ia1)
          then fail
          else 
            (parameters_bis eas ebs) (* try optional comma trick *)


      | A.MetaParamList (ida, keep, inherited), ys -> 
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            let startxs' = Ast_c.unsplit_comma startxs in
            acc >||> (
              X.envf keep inherited (term ida, Ast_c.MetaParamListVal startxs')
              >>= (fun _s v -> 
                  match v with
                  | Ast_c.MetaParamListVal startxsenv -> 
                      (* TODO
                      if (Lib_parsing_c.al_expr expa =*= 
                          Lib_parsing_c.al_expr expb)
                      *)
                     X.distrf_params ida (Ast_c.split_comma startxs')
                  | _ -> raise Impossible) 
              >>= (fun ida startxs -> 
                  parameters_bis eas endxs >>= (fun eas endxs -> 
                    return (
                      (A.MetaParamList(ida,keep,inherited))+> A.rewrap ea::eas,
                      startxs ++ endxs
                    ))
                  )
                )
            ) fail 


      | A.VoidParam _, _ -> failwith "handling VoidParam"
          (* XXX
                  assert (null ys);
                  assert (
                    match typb with 
                    | (_qua, (B.BaseType B.Void,_)) -> true
                    | _ -> false
                          );
   
                  return false
              
          *)


      | (A.OptParam _ | A.UniqueParam _), _ -> 
              failwith "handling Opt/Unique/Multi for Param"

      | A.Pcircles (_), ys -> raise Impossible (* in Ordered mode *)


      | A.MetaParam (ida,keep,inherited), (Left eb)::ebs -> 
          (* todo: use quaopt, hasreg ? *)
          X.envf keep inherited (term ida, Ast_c.MetaParamVal eb) >>= 
            (fun _s v -> 
              match v with
              | Ast_c.MetaParamVal ea -> 
                  (* TODO
                     if (Lib_parsing_c.al_expr expa =*= 
                     Lib_parsing_c.al_expr expb)
                  *)
                  X.distrf_param ida eb
              | _ -> raise Impossible
            ) >>= (fun ida eb -> 
              parameters_bis eas ebs >>= (fun eas ebs -> 
                return (
                  (A.MetaParam(ida,keep,inherited))+> A.rewrap ea::eas,
                  (Left eb)::ebs
                )))


      | A.Param (typa, idaopt), (Left eb)::ebs -> 
	  (*this should succeed if the C code has a name, and fail otherwise*)
          parameter (idaopt, typa) eb >>= (fun (idaopt, typa) eb -> 
          parameters_bis eas ebs >>= (fun eas ebs -> 
            return (
              (A.Param (typa, idaopt))+> A.rewrap ea :: eas,
              (Left eb)::ebs
            )))
          
      | _unwrapx, (Right y)::ys -> raise Impossible
      | _unwrapx, [] -> fail
      )
  




and parameter = fun (idaopt, typa)   ((hasreg, idbopt, typb), ii_b_s) ->
  fullType typa typb >>= (fun typa typb -> 
  match idaopt, Ast_c.split_register_param (hasreg, idbopt, ii_b_s) with
  | Some ida, Left (idb, iihasreg, iidb) -> 
      (* todo: if minus on ida, should also minus the iihasreg ? *)
      ident DontKnow ida (idb,iidb) >>= (fun ida (idb,iidb) -> 
        return (
          (Some ida, typa),
          ((hasreg, Some idb, typb), iihasreg++[iidb])
        ))
        
  | None, Right iihasreg -> 
      return (
        (None, typa),
        ((hasreg, None, typb), iihasreg)
      )
      

  (* why handle this case ? because of transform_proto ? we may not
   * have an ident in the proto.
   * If have some plus on ida ? do nothing about ida ? 
   *)
  | _, Right iihasreg -> 
      return (
        (idaopt, typa),
        ((hasreg, None, typb), iihasreg)
      )
  | None, Left _ -> fail
  )




(* ------------------------------------------------------------------------- *)
and (declaration: (A.mcodekind * A.declaration, B.declaration) matcher) =
 fun (mckstart, decla) declb -> 

(* XXX
 fun decla (B.DeclList (xs, _)) -> 
   xs +> List.fold_left (fun acc var -> acc >||> match_re_onedecl decla var)
     (return false)
*)

   match declb with
  | (B.DeclList ([var], iiptvirgb::iifakestart::iisto)) -> 
      onedecl decla (var,iiptvirgb,iisto) >>=(fun decla (var,iiptvirgb,iisto)->
      X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart -> 
      return (
        (mckstart, decla),
        (B.DeclList ([var], iiptvirgb::iifakestart::iisto))
      )))
        
  | (B.DeclList (xs, iiptvirgb::iifakestart::iisto)) -> 
      if X.mode = PatternMode
      then
        xs +> List.fold_left (fun acc var -> 
          acc >||> (
            X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart ->
              onedecl decla (var, iiptvirgb, iisto) >>= 
                (fun decla (var, iiptvirgb, iisto) -> 
                  return (
                    (mckstart, decla),
                    (B.DeclList ([var], iiptvirgb::iifakestart::iisto))
                  )))))
          fail
      else 
        failwith "More that one variable in decl. Have to split to transform."
  
  | _ -> raise Impossible                


and storage stoa stob =
  (* "iso-by-absence" for storage. *)
  match stoa, stob with 
  | None, _ -> 
      return (None, stob)
  | Some x, ((stobis, inline),iistob) -> 
      if equal_storage (term x) stobis
      then 
        match iistob with
        | [i1] ->
           tokenf x i1 >>= (fun x i1 -> 
             return (Some x,  ((stobis, inline), [i1]))
           )
       (* or if have inline ? have to do a split_storage_inline a la 
        * split_signb_baseb_ii *)
        | _ -> raise Impossible 
      else fail
  





and onedecl = fun decla (declb, iiptvirgb, iistob) -> 
  X.all_bound (A.get_inherited decla) >&&>
   match A.unwrap decla, declb with
  (* Un MetaDecl est introduit dans l'asttoctl pour sauter au dessus
   * de toutes les declarations qui sont au debut d'un fonction et
   * commencer le reste du match au premier statement. Alors, ca matche
   * n'importe quelle declaration. On n'a pas besoin d'ajouter
   * quoi que ce soit dans l'environnement. C'est une sorte de DDots.
   * 
   * When the SP want to remove the whole function, the minus is not
   * on the MetaDecl but on the MetaRuleElem. So there should
   * be no transform of MetaDecl, just matching are allowed.
   *)
   | A.MetaDecl(ida,_keep,_inherited), _ -> (* keep ? inherited ? *)
       (* todo: should not happen in transform mode *)
       return (decla, (declb, iiptvirgb, iistob))


    (* could handle iso here but handled in standard.iso *)
   | A.UnInit (stoa, typa, ida, ptvirga), 
     ((Some ((idb, None),[iidb]), typb, stob), iivirg) -> 
       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb -> 
       fullType typa typb >>= (fun typa typb -> 
       ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
       storage stoa (stob, iistob) >>= (fun stoa (stob, iistob) -> 
         return (
           (A.UnInit (stoa, typa, ida, ptvirga)) +>  A.rewrap decla,
           (((Some ((idb,None),[iidb]),typb,stob),iivirg),iiptvirgb,iistob)
         )))))



   | A.Init (stoa, typa, ida, eqa, inia, ptvirga), 
     ((Some((idb,Some inib),[iidb;iieqb]),typb,stob),iivirg)
       ->
       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb -> 
       tokenf eqa iieqb >>= (fun eqa iieqb -> 
       fullType typa typb >>= (fun typa typb -> 
       ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
       storage stoa (stob, iistob) >>= (fun stoa (stob, iistob) -> 
       initialiser inia inib >>= (fun inia inib -> 
         return (
           (A.Init (stoa, typa, ida, eqa, inia, ptvirga)) +> A.rewrap decla,
           (((Some((idb,Some inib),[iidb;iieqb]),typb,stob),iivirg),
           iiptvirgb,iistob)
         )))))))
           

   | A.TyDecl (typa, _), _ ->
      (* accept only '((None, typb, sto), _)' or do iso-by-absence here ?
         allow typedecl and var ? *)
       failwith "fill something in for a declaration that is just a type"
       
   | _, ((None, typb, sto), _) -> 
       (* old:   failwith "no variable in this declaration, wierd" *)
       fail


   | A.DisjDecl declas, declb -> 
       declas +> List.fold_left (fun acc decla -> 
         acc >|+|> (onedecl decla (declb,iiptvirgb, iistob))) fail
            
   | A.OptDecl _, _ | A.UniqueDecl _, _ | A.MultiDecl _, _ -> 
       failwith "not handling Opt/Unique/Multi Decl"
   | _, _ -> fail



(* ------------------------------------------------------------------------- *)

and (initialiser: (Ast_cocci.initialiser, Ast_c.initialiser) matcher)
  =  fun ia ib -> 
    X.all_bound (A.get_inherited ia) >&&>
    match (A.unwrap ia,ib) with
    
    | (A.InitExpr expa,(B.InitExpr expb, ii)) -> 
        assert (null ii);
        expression expa expb >>= (fun expa expb -> 
          return (
            (A.InitExpr expa) +> A.rewrap ia,
            (B.InitExpr expb, ii)
          ))

    | (A.InitList (ia1, ias, allminuses, ia2, []), (B.InitList ibs, ii)) -> 
        (match ii with 
        | ib1::ib2::iicommaopt -> 
            tokenf ia1 ib1 >>= (fun ia1 ib1 ->
            tokenf ia2 ib2 >>= (fun ia2 ib2 ->
            initialisers ias (Ast_c.split_comma ibs) >>= (fun ias ibs_split ->
              let ibs = Ast_c.unsplit_comma ibs_split in
              return (
                (A.InitList (ia1, ias, allminuses, ia2, [])) +> A.rewrap ia,
                (B.InitList ibs, ib1::ib2::iicommaopt)
              ))))
              
        | _ -> raise Impossible
        )

    | (A.InitList (i1, ias, allminuses, i2, whencode),(B.InitList ibs, _ii)) ->
        failwith "TODO: not handling whencode in initialisers"


    | (A.InitGccDotName (ia1, ida, ia2, inia), (B.InitGcc (idb, inib), ii)) -> 
        (match ii with 
        | [iidot;iidb;iieq] -> 
            tokenf ia1 iidot >>= (fun ia1 iidot -> 
            tokenf ia2 iieq >>= (fun ia2 iieq -> 
            ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
            initialiser inia inib >>= (fun inia inib -> 
              return (
                (A.InitGccDotName (ia1, ida, ia2, inia)) +> A.rewrap ia,
                (B.InitGcc (idb, inib), [iidot;iidb;iieq])
              )))))
        | _ -> fail
        )



    | (A.InitGccName (ida, ia1, inia), (B.InitGcc (idb, inib), ii)) -> 
        (match ii with 
        | [iidb;iicolon] -> 
            ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
            initialiser inia inib >>= (fun inia inib -> 
            tokenf ia1 iicolon >>= (fun ia1 iicolon -> 
              return (
                (A.InitGccName (ida, ia1, inia)) +> A.rewrap ia,
                (B.InitGcc (idb, inib), [iidb;iicolon])
              ))))
        | _ -> fail
        )



    | (A.InitGccIndex (ia1,ea,ia2,ia3,inia),(B.InitGccIndex (eb, inib),ii)) ->
        let (ib1, ib2, ib3) = tuple_of_list3 ii in
        tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
        expression ea eb >>= (fun ea eb -> 
        initialiser inia inib >>= (fun inia inib -> 
          return (
            (A.InitGccIndex (ia1,ea,ia2,ia3,inia)) +> A.rewrap ia,
            (B.InitGccIndex (eb, inib),[ib1;ib2;ib3])
          ))))))

    | (A.InitGccRange (ia1,e1a,ia2,e2a,ia3,ia4,inia), 
      (B.InitGccRange (e1b, e2b, inib), ii)) -> 

        let (ib1, ib2, ib3, ib4) = tuple_of_list4 ii in
        tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
        tokenf ia4 ib4 >>= (fun ia4 ib4 -> 
        expression e1a e1b >>= (fun e1a e1b -> 
        expression e2a e2b >>= (fun e2a e2b -> 
        initialiser inia inib >>= (fun inia inib -> 
          return (
            (A.InitGccRange (ia1,e1a,ia2,e2a,ia3,ia4,inia)) +> A.rewrap ia,
            (B.InitGccRange (e1b, e2b, inib), [ib1;ib2;ib3;ib4])
          ))))))))

    | A.MultiIni _, _ | A.UniqueIni _,_ | A.OptIni _,_ -> 
      failwith "not handling Opt/Unique/Multi on initialisers"
          
    | _, _ -> fail



and initialisers = fun ias ibs ->
  match ias, ibs with
  | [], ys -> return ([], ys)
  | x::xs, ys -> 

      let permut = Common.uncons_permut ys in
      permut +> List.fold_left (fun acc ((e, pos), rest) -> 
        acc >||> 
          (
            (match e with 
            | Left y -> 
                initialiser x y >>= (fun x y -> 
                  return (x, Left y)
                )
            | Right y -> fail
            ) >>= (fun x e -> 
            initialisers xs rest >>= (fun xs rest -> 
              return (
                x::xs,
                Common.insert_elem_pos (e, pos) rest
              ))))) fail
       

(* ------------------------------------------------------------------------- *)
and (fullType: (Ast_cocci.fullType, Ast_c.fullType) matcher) = 
 fun typa typb -> 
   X.all_bound (A.get_inherited typa) >&&>
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

  | A.DisjType typas, typb -> 
      typas +>
      List.fold_left (fun acc typa -> acc >|+|> (fullType typa typb)) fail

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
  X.all_bound (A.get_inherited ta) >&&> 
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
          (match iibaseb with 
          | [] -> 
              (* iso-by-presence ? *)
              (* when unsigned int in SP,  allow have just unsigned in C ? *)
              if mcode_contain_plus (mcodekind basea)
              then fail
              else 
                
                sign signaopt signbopt >>= (fun signaopt iisignbopt -> 
                    return (
                      (A.BaseType (basea, signaopt)) +> A.rewrap ta,
                      (B.BaseType (baseb), iisignbopt ++ [])
                    ))
              

          | x::y::ys -> raise Impossible
          | [ibaseb] -> 
          sign signaopt signbopt >>= (fun signaopt iisignbopt -> 
          tokenf basea ibaseb >>= (fun basea ibaseb -> 
            return (
               (A.BaseType (basea, signaopt)) +> A.rewrap ta,
               (B.BaseType (baseb), iisignbopt ++ [ibaseb])
               )))
          )

            
      | _, B.IntType (B.Si (_, B.CLongLong)) 
      | _, B.FloatType B.CLongDouble 
          -> 
          pr2 "warning: long long or long double not handled by ast_cocci";
          fail
          
          
      | _, _ -> fail
          
          
      )

    | A.ImplicitInt (signa),   (B.BaseType baseb, ii) -> 
        let signbopt, iibaseb = split_signb_baseb_ii (baseb, ii) in
        (match iibaseb, baseb with
        | [], B.IntType (B.Si (_sign, B.CInt)) -> 
            sign (Some signa) signbopt >>= (fun signaopt iisignbopt -> 
              match signaopt with
              | None -> raise Impossible
              | Some signa -> 
                  return (
                    (A.ImplicitInt (signa)) +> A.rewrap ta,
                    (B.BaseType baseb, iisignbopt)
                  )
            )
        | _ -> fail
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

    | A.FunctionPointer(tya,lp1a,stara,rp1a,lp2a,paramsa,rp2a), 
        (B.ParenType t1, ii) ->
        let (lp1b, rp1b) = tuple_of_list2 ii in
        let (qu1b, t1b) = t1 in
        (match t1b with
        | B.Pointer t2, ii -> 
            let (starb) = tuple_of_list1 ii in
            let (qu2b, t2b) = t2 in
            (match t2b with
            | B.FunctionType (tyb, (paramsb, (isvaargs, iidotsb))), ii -> 
                let (lp2b, rp2b) = tuple_of_list2 ii in

                if isvaargs 
                then begin 
                  pr2 "Not handling well variable length arguments func. ";
                  pr2 "You have been warned";
                end;


                fullType tya tyb >>= (fun tya tyb -> 
                tokenf lp1a lp1b >>= (fun lp1a lp1b -> 
                tokenf rp1a rp1b >>= (fun rp1a rp1b -> 
                tokenf lp2a lp2b >>= (fun lp2a lp2b -> 
                tokenf rp2a rp2b >>= (fun rp2a rp2b -> 
                tokenf stara starb >>= (fun stara starb -> 
                parameters (seqstyle paramsa) (A.undots paramsa) paramsb >>=
                (fun paramsaundots paramsb -> 
                  let paramsa = redots paramsa paramsaundots in

                  let t2 = 
                    (qu2b, 
                    (B.FunctionType (tyb, (paramsb, (isvaargs, iidotsb))),
                    [lp2b;rp2b])) 
                  in
                  let t1 = 
                    (qu1b,
                    (B.Pointer t2, [starb]))
                  in
                  
                  return (
                    (A.FunctionPointer(tya,lp1a,stara,rp1a,lp2a,paramsa,rp2a))
                    +> A.rewrap ta,
                    (B.ParenType t1, [lp1b;rp1b])
                  )
                )))))))



            | _ -> fail
            )
        | _ -> fail
        )
        
        

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
    | A.StructUnionName(sua, sa), (B.StructUnionName (sub, sb), ii) -> 
        (* sa is now an ident, not an mcode, old: ... && (term sa) =$= sb *)
        let (ib1, ib2) = tuple_of_list2 ii in
        if equal_structUnion  (term sua) sub 
        then
          ident DontKnow sa (sb, ib2) >>= (fun sa (sb, ib2) -> 
          tokenf sua ib1 >>= (fun sua ib1 -> 
            return (
              (A.StructUnionName (sua, sa)) +> A.rewrap ta,
              (B.StructUnionName (sub, sb), [ib1;ib2])
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


let minusize_list iixs = 
  iixs +> List.fold_left (fun acc ii -> 
    acc >>= (fun xs ys -> 
    tokenf minusizer ii >>= (fun minus ii -> 
      return (minus::xs, ii::ys)
    ))) (return ([],[]))
   >>= (fun _xsminys ys -> 
     return ((), List.rev ys)
   )

let storage_optional_allminus allminus stoa (stob, iistob) = 
  (* "iso-by-absence" for storage, and return type. *)
  match stoa with
  | None -> 
      if allminus 
      then 
        minusize_list iistob >>= (fun () iistob -> 
          return (None, (stob, iistob))
        )
      else return (None, (stob, iistob))
  | Some stoa -> 
      storage (Some stoa) (stob, iistob)



let fullType_optional_allminus allminus tya retb = 
  match tya with 
  | None -> 
      if allminus
      then 
        X.distrf_type minusizer retb >>= (fun _x retb -> 
          return (None, retb)
        )

      else return (None, retb)
  | Some tya -> 
      fullType tya retb >>= (fun tya retb -> 
        return (Some tya, retb)
      )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let (rule_elem_node: (Ast_cocci.rule_elem, Control_flow_c.node) matcher) = 
 fun re node -> 
  let rewrap x = 
    x >>= (fun a b -> return (A.rewrap re a, F.rewrap node b))
  in
  X.all_bound (A.get_inherited re) >&&>

  rewrap (
  match A.unwrap re, F.unwrap node with

  (* note: the order of the clauses is important. *)

  | _, F.Enter | _, F.Exit | _, F.ErrorExit -> fail2

  (* the metaRuleElem contains just '-' information. We dont need to add
   * stuff in the environment. If we need stuff in environment, because
   * there is a + S somewhere, then this will be done via MetaStmt, not
   * via MetaRuleElem. 
   * Can match TrueNode/FalseNode/... so must be placed before those cases.
   *)

  | A.MetaRuleElem(mcode,keep,inherited), unwrap_node -> 
      let default = A.MetaRuleElem(mcode,keep,inherited), unwrap_node in
      (match unwrap_node with
      | F.CaseNode _
      | F.TrueNode | F.FalseNode | F.AfterNode | F.FallThroughNode -> 
          if X.mode = PatternMode 
          then return default 
          else
            if mcode_contain_plus (mcodekind mcode)
            then failwith "try add stuff on fake node"
              (* minusize or contextize a fake node is ok *)
            else return default

      | F.EndStatement None -> 
          if X.mode = PatternMode then return default 
          else 
              (* DEAD CODE NOW ? only useful in -no_cocci_vs_c_3 ?
                 if mcode_contain_plus (mcodekind mcode)
                 then
                 let fake_info = Ast_c.fakeInfo() in
                 distrf distrf_node (mcodekind mcode) 
                 (F.EndStatement (Some fake_info)) 
                 else return unwrap_node
              *)
            raise Todo
              
      | F.EndStatement (Some i1) -> 
          tokenf mcode i1 >>= (fun mcode i1 -> 
            return (
              A.MetaRuleElem (mcode,keep, inherited),
              F.EndStatement (Some i1)
            ))

      | F.FunHeader _ -> 
          if X.mode = PatternMode then return default
          else failwith "a MetaRuleElem can't transform a headfunc"
      | _n -> 
          if X.mode = PatternMode then return default 
          else 
          X.distrf_node (generalize_mcode mcode) node >>= (fun mcode node -> 
            return (
              A.MetaRuleElem(mcode,keep, inherited),
              F.unwrap node
            ))
      )


  (* rene cant have found that a state containing a fake/exit/... should be 
   * transformed 
   * TODO: and F.Fake ?
   *)
  | _, F.EndStatement _ | _, F.CaseNode _
  | _, F.TrueNode | _, F.FalseNode | _, F.AfterNode | _, F.FallThroughNode
    -> fail2

  (* really ? diff between pattern.ml and transformation.ml *)
  | _, F.Fake -> fail2


  (* cas general: a Meta can match everything. It matches only
   * "header"-statement. We transform only MetaRuleElem, not MetaStmt.
   * So can't have been called in transform. 
   *)
  | A.MetaStmt (ida,keep,metainfoMaybeTodo,inherited),  unwrap_node -> 
      (* todo: should not happen in transform mode *)

      (match Control_flow_c.extract_fullstatement node with
      | Some stb -> 
         X.envf keep inherited (term ida, Ast_c.MetaStmtVal stb) >>= 
           (fun _s v -> 
             match v with
             | B.MetaStmtVal sta -> 
                 if (Lib_parsing_c.al_statement sta 
                      =*= 
                     Lib_parsing_c.al_statement stb)
                 then
                   (* no need tag ida, we can't be called in transform-mode *)
                   return (
                     A.MetaStmt (ida, keep, metainfoMaybeTodo, inherited),
                     unwrap_node
                   )
                 else fail
             | _ -> raise Impossible
           )
           
      | None -> fail
      )

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

  | A.Ty ty, nodeb -> 
      X.cocciTy fullType ty node >>= (fun ty node -> 
        return (
          A.Ty ty,
          F.unwrap node
        )
      )


  | A.FunHeader (mckstart, allminus, stoa, tya, ida, oparen, paramsa, cparen),
    F.FunHeader ((idb, (retb, (paramsb, (isvaargs, iidotsb))), stob), ii) -> 

      (match ii with
      | iidb::ioparenb::icparenb::iifakestart::iistob -> 

          (* maybe important to put ident as the first tokens to transform.
           * It's related to transform_proto. So don't change order
           * between the >>=.
           *)
          ident LocalFunction ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
          X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart -> 
          tokenf oparen ioparenb >>= (fun oparen ioparenb ->
          tokenf cparen icparenb >>= (fun cparen icparenb ->
          parameters (seqstyle paramsa) 
            (A.undots paramsa) paramsb >>=
            (fun paramsaundots paramsb -> 
              let paramsa = redots paramsa paramsaundots in
          storage_optional_allminus allminus 
            stoa (stob, iistob) >>= (fun stoa (stob, iistob) -> 
              (
                if isvaargs 
                then begin 
                  pr2 "Not handling well variable length arguments func. ";
                  pr2 "You have been warned";
                end;
                if allminus
                then minusize_list iidotsb
                else return ((),iidotsb)
              ) >>= (fun () iidotsb -> 
            
           fullType_optional_allminus allminus tya retb >>= (fun tya retb -> 
             return (
               A.FunHeader(mckstart,allminus,stoa,tya,ida,oparen,
                          paramsa,cparen),
               F.FunHeader ((idb, (retb, (paramsb, (isvaargs, iidotsb))), 
                            stob), 
                           iidb::ioparenb::icparenb::iifakestart::iistob)
                )
              ))))))))
      | _ -> raise Impossible
      )






  | A.Decl (mckstart,decla), F.Decl declb -> 
      declaration (mckstart,decla) declb >>= (fun (mckstart,decla) declb -> 
        return (
          A.Decl (mckstart,decla),
          F.Decl declb
        ))


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


  | A.SwitchHeader(ia1,ia2,ea,ia3), F.SwitchHeader (st, (eb,ii)) ->
      let (ib1, ib2, ib3) = tuple_of_list3 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
      tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
      expression ea eb >>= (fun ea eb -> 
        return (
          A.SwitchHeader(ia1,ia2,ea,ia3), 
          F.SwitchHeader (st, (eb,[ib1;ib2;ib3]))
        )))))
      

  (* julia: goto is just created by asttoctl2, with no +- info *)
  | A.Goto,                  F.Goto (a,b)       -> 
      return (A.Goto, F.Goto (a,b))
      

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



  | A.Include(incla,filea), F.CPPInclude (filebstr, ii) ->
      let (inclb, fileb) = tuple_of_list2 ii in 
      if ((term filea) =$= filebstr)
      then 
        tokenf incla inclb >>= (fun incla inclb -> 
        tokenf filea fileb >>= (fun filea fileb -> 
          return (
            A.Include(incla, filea),
            F.CPPInclude (filebstr, [inclb;fileb])
          )))
      else fail



  | A.Define(definea,ida,bodya), F.CPPDefine ((idb, bodyb), ii) ->
      let (defineb, iidb, iibodyb) = tuple_of_list3 ii in
      ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
(*      all_bound (A.get_inherited ida) >&&> *)
      tokenf definea defineb >>= (fun definea defineb -> 
        (match A.unwrap bodya with
        | A.DMetaId (idbodya, keep) -> 
            let inherited = false (* TODO ? *) in
            X.envf keep inherited (term idbodya, B.MetaTextVal bodyb) >>=
              (fun _s v -> 
                match v with
                | B.MetaTextVal sa -> 
                    if (sa =$= bodyb)
                    then 
                      tokenf idbodya iibodyb >>= (fun idbodya iibodyb -> 
                        return (
                          A.Define(definea,ida, 
                                  (A.DMetaId (idbodya, keep)
                                    +> A.rewrap bodya)),
                          F.CPPDefine ((idb, bodyb), [defineb;iidb;iibodyb])
                        ))
                    else fail
                | _ -> raise Impossible
              )


                
        | A.Ddots (dots) -> 
            tokenf dots iibodyb >>= (fun dots iibodyb -> 
              return (
                A.Define(definea,ida, (A.Ddots (dots) +> A.rewrap bodya)),
                F.CPPDefine ((idb, bodyb), [defineb;iidb;iibodyb])
              ))
        )))


  | A.Default(def,colon), F.Default (st, ((),ii)) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf def ib1 >>= (fun def ib1 -> 
      tokenf colon ib2 >>= (fun colon ib2 -> 
        return (
          A.Default(def,colon), 
          F.Default (st, ((),[ib1;ib2]))
        )))

      
      
  | A.Case(case,ea,colon), F.Case (st, (eb,ii)) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf case ib1 >>= (fun case ib1 -> 
      expression ea eb >>= (fun ea eb -> 
      tokenf colon ib2 >>= (fun colon ib2 -> 
        return (
          A.Case(case,ea,colon), 
          F.Case (st, (eb,[ib1;ib2]))
        ))))
      

  | _, F.ExprStatement (_, (None, ii)) -> fail (* happen ? *)

  (* have not a counter part in coccinelle, for the moment *)
  (* todo?: print a warning at least ? *)
  | _, F.Label _
  | _, F.CaseRange _  
  | _, F.Asm
  | _, F.IfCpp _
    -> fail2


  | _, _ -> fail 
  )
end

