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

    type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout

    val tokenf : ('a A.mcode, B.info) matcher

    val distrf_e : (string A.mcode, B.expression) matcher
    (*
    val distrf_type : Ast_c.fullType tdistr
    val distrf_node : Control_flow_c.node2 tdistr
    val distrf_args : (Ast_c.argument, Ast_c.il) either list tdistr
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
          

  | A.Constant (ia1), ((B.Constant (ib) , typ),ii) -> 
      (match term ia1, ib with 
      | A.Int inta, B.Int intb when inta =$= intb -> 
         let ib1 = tuple_of_list1 ii in 
         tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
           return ( 
             ((A.Constant ia1)) +> wa, 
             ((B.Constant (B.Int intb), typ),[ib1])
           ))
      | _, _ -> fail
      )

  | A.FunCall (ea, ia1, eas, ia2),  ((B.FunCall (eb, ebs), typ),ii) -> 
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

  | A.RecordAccess (ea, ia1, ida), ((B.RecordAccess (eb, idb), typ),ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      ident DontKnow ida (idb, ib2) >>= (fun ida (idb, ib2) -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      expression ea eb >>= (fun ea eb -> 
        return (
          ((A.RecordAccess (ea, ia1, ida))) +> wa,
          ((B.RecordAccess (eb, idb), typ), [ib1;ib2])
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
      X.envf keep inherited (term mida, Ast_c.MetaIdVal (idb)) >>= (fun _s v ->
        match v with
        | Ast_c.MetaIdVal sa -> 
            if (sa =$= idb) 
            then 
              tokenf mida iib >>= (fun mida iib -> 
                return (
                  ((A.MetaId (mida, true, inherited)) +> A.rewrap ida,
                  (idb, iib)
                  )))
            else fail
        | _ -> raise Impossible
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

(* because '...' can match nothing, need to take care when have 
 * ', ...'   or '...,'  as in  f(..., X, Y, ...). It must match
 * f(1,2) for instance.
 *)
and arguments_bis = fun eas ebs -> 
  match eas, ebs with
  | [], [] -> return ([], [])
  | [], eb::ebs -> fail
  | ea::eas, ebs -> 
      (match A.unwrap ea, ebs with
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
  | _, Left argb -> 
      expression arga argb >>= (fun arga argb -> 
        return (arga, Left argb)
      )
  | _, _ -> raise Todo

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
      raise Todo

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

  | A.ExprStatement (ea, ia1), F.ExprStatement (st, (Some eb, ii)) -> 
      let ib1 = tuple_of_list1 ii in 
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        return (
          A.ExprStatement (ea, ia1),
          F.ExprStatement (st, (Some eb, [ib1]))
        )
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

