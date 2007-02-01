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
        

type info_ident = 
  | DontKnow

let term      (s,i,mc) = s
let mcodekind (s,i,mc) = mc

let tuple_of_list1 = function [a] -> a | _ -> failwith "tuple_of_list1"
let tuple_of_list2 = function [a;b] -> a,b | _ -> failwith "tuple_of_list2"
let tuple_of_list3 = function [a;b;c] -> a,b,c | _ -> failwith "tuple_of_list3"
let tuple_of_list4 = function [a;b;c;d] -> a,b,c,d | _ -> failwith "tuple_of_list4"
let tuple_of_list5 = function [a;b;c;d;e] -> a,b,c,d,e | _ -> failwith "tuple_of_list5"

(*****************************************************************************)
(* combinators *)
(*****************************************************************************)
(* monad like stuff
 * src: papers on parser combinators in haskell (cf a pearl by meijer in ICFP)
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

    val tokenf : 
      'a A.mcode -> Ast_c.info -> tin -> ('a A.mcode * Ast_c.info) tout

    val distrf_e : 
     'a A.mcode -> Ast_c.expression -> tin -> 
      ('a A.mcode * Ast_c.expression) tout

    val cocciExp : 
      (A.expression->B.expression -> tin -> (A.expression*B.expression)tout) ->
      A.expression -> F.node -> (tin -> (A.expression * F.node) tout)

    (* val cocciTy *)
    val envf : 
      bool (*keep*) -> A.inherited -> 
      string * Ast_c.metavar_binding_kind ->
      tin -> 
      (string * Ast_c.metavar_binding_kind) tout



  end


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

(*****************************************************************************)
(* "Cocci vs C" *) 
(*****************************************************************************)

let rec (expression: (Ast_cocci.expression, Ast_c.expression) matcher) =
 fun ea eb -> 
  let wa x = A.rewrap ea x  in
  match A.unwrap ea, eb with
  
  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr (ida,keep,opttypa,inherited), (((expr, opttypb), ii) as expb) ->
      X.envf keep inherited (term ida, Ast_c.MetaExprVal expb) >>= (fun _s v ->
        match v with
        | Ast_c.MetaExprVal expa -> 
            if (Lib_parsing_c.al_expr expa =*= Lib_parsing_c.al_expr expb)
            then 
              X.distrf_e ida expb >>= (fun ida expb -> 
                return (
                  A.MetaExpr (ida,keep,opttypa,inherited)+> A.rewrap ea,
                  expb
                )
              )
            else fail
        | _ -> raise Impossible
      )

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

  | A.DisjExpr eas, eb -> 
      eas +> List.fold_left (fun acc ea -> 
        acc >||> (expression ea eb)
      ) fail

  | _, _ -> fail



(* ------------------------------------------------------------------------- *)
and (ident: info_ident -> (Ast_cocci.ident, string * Ast_c.info) matcher) = 
 fun infoidb ida (idb, iib) -> 
  match A.unwrap ida with
  | A.Id sa when (term sa) =$= idb -> 
      tokenf sa iib >>= (fun sa iib -> 
        return (
          ((A.Id sa)) +> A.rewrap ida,
          (idb, iib)
        ))


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

  | _ -> fail


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
  | _, F.EndStatement _ | _, F.CaseNode _
  | _, F.TrueNode | _, F.FalseNode | _, F.AfterNode | _, F.FallThroughNode
    -> fail2
  | _, F.Fake -> fail2


  | A.Exp exp, nodeb -> 
      X.cocciExp expression exp node >>= (fun exp node -> 
        return (
          A.Exp exp,
          F.unwrap node
        )
      )

  | A.ExprStatement (ea, ia1), F.ExprStatement (st, (Some eb, ii)) -> 
      let ib1 = tuple_of_list1 ii in 
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        return (
          A.ExprStatement (ea, ia1),
          F.ExprStatement (st, (Some eb, [ib1]))
        )
      ))

  | _, _ -> fail 
  )
end

