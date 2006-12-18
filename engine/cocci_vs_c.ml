open Commonop

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
(*
 * version0: 
 *   type ('a, 'b) matcher = 'a -> 'b -> bool
 *
 * version1: same but with a global variable holding the current binding
 *  BUT bug
 *   - can have multiple possibilities
 *   - globals sux
 *   - sometimes have to undo, cos if start match, then it binds, and if later
 *     it does not match, then must undo the first binds.
 *     ex: when match parameters, can  try to match, but then we found far 
 *     later that the last argument of a function does not match
 *      => have to undo the binding !!!
 *      (can handle that too with a global, by saving the global, ... but sux)
 *   => better not use global
 * 
 * version2: 
 *    type ('a, 'b) matcher = binding -> 'a -> 'b -> binding list
 *
 *  Empty list mean failure (let matchfailure = []).
 *  To be able to have pretty code, have to use partial application powa, and 
 *  so the type is in fact
 *
 * version3:
 *    type ('a, 'b) matcher =  'a -> 'b -> binding -> binding list
 *
 *  Then by defining the correct combinators, can have quite pretty code (that 
 *  looks like the clean code of version0).
 * 
 * opti: return a lazy list of possible matchs ?
 *
 * 
 * 
 * 
 * version0: 
 *  type ('a, 'b) transformer = 
 *    'a -> 'b -> Lib_engine.metavars_binding -> 'b
 *  exception NoMatch 
 * 
 * version1:
 *   type ('a, 'b) transformer = 
 *    'a -> 'b -> Lib_engine.metavars_binding -> 'b option
 * 
 *)

(* monad like stuff
 * src: papers on parser combinators in haskell (cf a pearl by meijer in ICFP)
 *)

type tin = unit
type 'b tout = 'b option

type ('a, 'b) matcher = 'a -> 'b  -> tin -> 'b tout


let (>>=) m f =
  match m () with
  | None -> None
  | Some x -> f x

let return x = fun tin -> 
  Some x

let fail = fun tin -> 
  None

let option f t1 t2 =
  match (t1,t2) with
  | (Some t1, Some t2) -> 
      f t1 t2 >>= (fun x -> return (Some x))
  | (None, None) -> return None
  | _ -> fail



(*****************************************************************************)
(* Environment *) 
(*****************************************************************************)

let envf inherited (s, value) = raise Todo

(*****************************************************************************)
(* Tokens *) 
(*****************************************************************************)

let tokenf xs ys = raise Todo

let tokenf_wrap xs ys e = 
  tokenf xs ys >>= (fun ii' ->  return (e, ii'))

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let distrf distrop mck x   = raise Todo

let distrf_e x = raise Todo

(*****************************************************************************)
(* "Cocci vs C" *) 
(*****************************************************************************)

let rec (expression: (Ast_cocci.expression, Ast_c.expression) matcher) =
 fun ea eb -> 
  match A.unwrap ep, ec with
  
  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr (ida,opttypa,inherited),  (((expr, opttypb), ii) as expb) -> 
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


 (* todo: handle some isomorphisms in int/float ? can have different
  * format : 1l can match a 1. TODO: normally string can contain some
  * metavar too, so should recurse on the string *)
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
      let seqstyle = 
        (match A.unwrap eas with 
        | A.DOTS _ -> Ordered 
        | A.CIRCLES _ -> Unordered 
        | A.STARS _ -> failwith "not handling stars"
        )  
      in
      expression ea eb >>= (fun e' -> 
        arguments seqstyle (A.undots eas) ebs >>= (fun arg' -> 
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
        option expressio ea2opt eb2opt >>= (fun e2opt' -> 
          expression ea3 eb3 >>= (fun e3' -> 
            (B.CondExpr (e1', e2opt', e3'),typ) +> tokenf_wrap [i1;i2] ii
          )))

  | A.Postfix (ea, opa), ((B.Postfix (eb, opb), typ),ii) -> 
      if equal_fixOp (term opa) opb
      then (B.Postfix (transform_e_e ea eb binding, opb), typ),
      tag_symbols [opa] ii  binding
      else raise NoMatch
        
        
  | A.Infix (ea, opa), ((B.Infix (eb, opb), typ),ii) -> 
      if equal_fixOp (term opa) opb
      then (B.Infix (transform_e_e ea eb binding, opb), typ),
      tag_symbols [opa] ii  binding
      else raise NoMatch

  | A.Unary (ea, opa), ((B.Unary (eb, opb), typ),ii) -> 
      if equal_unaryOp (term opa) opb
      then (B.Unary (transform_e_e ea eb binding, opb), typ),
      tag_symbols [opa] ii  binding
      else raise NoMatch


  | A.Binary (ea1, opa, ea2), ((B.Binary (eb1, opb, eb2), typ),ii) -> 
      if equal_binaryOp (term opa) opb
      then (B.Binary (transform_e_e ea1 eb1   binding, 
                     opb,  
                     transform_e_e ea2 eb2  binding), typ),
      tag_symbols [opa] ii binding
      else raise NoMatch


  | A.ArrayAccess (ea1, i1, ea2, i2), ((B.ArrayAccess (eb1, eb2), typ),ii) -> 
      (B.ArrayAccess (transform_e_e ea1 eb1 binding,
                     transform_e_e ea2 eb2 binding),typ),
      tag_symbols [i1;i2] ii  binding
        
  | A.RecordAccess (ea, dot, ida), ((B.RecordAccess (eb, idb), typ),ii) ->
      (match ii with
      | [i1;i2] -> 
          let (idb', i2') = 
            transform_ident DontKnow ida (idb, [i2])   binding 
          in
          let i1' = tag_symbols [dot] [i1] binding in
          (B.RecordAccess (transform_e_e ea eb binding, idb'), typ), i1' ++ i2'
      | _ -> raise Impossible
      )


  | A.RecordPtAccess (ea,fleche,ida),((B.RecordPtAccess (eb, idb), typ), ii) ->
      (match ii with
      | [i1;i2] -> 
          let (idb', i2') = 
            transform_ident DontKnow ida (idb, [i2])   binding 
          in
          let i1' = tag_symbols [fleche] [i1] binding in
          (B.RecordPtAccess (transform_e_e ea eb binding,idb'),typ), i1' ++ i2'
      | _ -> raise Impossible
      )

  | A.Cast (i1, typa, i2, ea), ((B.Cast (typb, eb), typ),ii) -> 
      (B.Cast (transform_ft_ft typa typb  binding,
              transform_e_e ea eb binding),typ),
      tag_symbols [i1;i2]  ii binding

  | A.SizeOfExpr (i1, ea), ((B.SizeOfExpr (eb), typ),ii) -> 
      (B.SizeOfExpr (transform_e_e ea eb binding), typ),
      tag_symbols [i1]  ii binding

  | A.SizeOfType (i1, i2, typa, i3), ((B.SizeOfType typb, typ),ii) -> 
      (B.SizeOfType (transform_ft_ft typa typb  binding),typ),
      tag_symbols [i1;i2;i3]  ii binding

  | A.TypeExp(ty),_ ->
      failwith
	"transformation.ml: need to fill in something for the case of a type as an expression"


  | A.Paren (i1, ea, i2), ((B.ParenExpr (eb), typ),ii) -> 
      (B.ParenExpr (transform_e_e ea eb  binding), typ),
      tag_symbols [i1;i2] ii  binding


  | A.NestExpr _, _ -> failwith "not my job to handle NestExpr"


  | A.MetaExprList _, _   -> raise Impossible (* only in arg lists *)

  | A.EComma _, _   -> raise Impossible (* can have EComma only in arg lists *)


  | A.Ecircles _, _ -> raise Impossible (* can have EComma only in arg lists *)
  | A.Estars _, _   -> raise Impossible (* can have EComma only in arg lists *)


  | A.DisjExpr eas, eb -> 
      eas +> Common.fold_k (fun acc ea k -> 
        try transform_e_e ea acc  binding
        with NoMatch -> k acc
        ) 
        (fun _ -> raise NoMatch)
        eb

  | A.MultiExp _, _ | A.UniqueExp _,_ | A.OptExp _,_ -> 
      failwith "not handling Opt/Unique/Multi on expr"



 (* Because of Exp cant put a raise Impossible; have to put a raise NoMatch; *)

 (* have not a counter part in coccinelle, for the moment *) 
  | _, ((B.Sequence _,_),_) 

  | _, ((B.StatementExpr _,_),_) 
  | _, ((B.Constructor,_),_) 
  | _, ((B.MacroCall _,_),_) 
    -> raise NoMatch

  | _, _ -> raise NoMatch



(* ------------------------------------------------------------------------- *)
and (ident: info_ident -> (Ast_cocci.ident, string Ast_c.wrap) matcher) = 
 fun infoidb ida (idb, ii) -> 
   raise Common.Todo

(* ------------------------------------------------------------------------- *)
and (arguments: sequence -> 
      (Ast_cocci.expression list, Ast_c.argument Ast_c.wrap2 list) matcher) = 
 fun seqstyle eas ebs ->
   raise Common.Todo

and argument arga argb = 
   raise Common.Todo

(* ------------------------------------------------------------------------- *)
and (parameters: sequence -> 
      (Ast_cocci.parameterTypeDef list, Ast_c.parameterType Ast_c.wrap2 list)
        matcher) = 
 fun seqstyle pas pbs ->
   raise Common.Todo

and (parameter: (Ast_cocci.parameterTypeDef, (Ast_c.parameterType)) matcher) = 
 fun pa pb  -> 
   raise Common.Todo


(* ------------------------------------------------------------------------- *)
and (declaration: (Ast_cocci.declaration, Ast_c.declaration) matcher) =
 fun decla declb -> 
   raise Common.Todo

and storage stoa stob =
  raise Common.Todo

and onedecl = fun decla declb -> 
   raise Common.Todo


and (fullType: (Ast_cocci.fullType, Ast_c.fullType) matcher) = 
 fun typa typb -> 
   raise Common.Todo

and (typeC: (Ast_cocci.typeC, Ast_c.fullType) matcher) = 
 fun typa typb -> 
   raise Common.Todo

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let (rule_elem_node: (Ast_cocci.rule_elem, Control_flow_c.node) matcher) = 
  fun re node -> 
   raise Common.Todo
