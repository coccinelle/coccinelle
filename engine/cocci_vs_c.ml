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


let ((>>=): (tin -> 'c tout)  -> ('c -> (tin -> 'b tout)) -> (tin -> 'b tout))
 = fun  m f -> fun tin -> 
  match m tin with
  | None -> None
  | Some x -> f x tin

let (return: 'b -> tin -> 'b tout) = fun x -> fun tin -> 
  Some x

let (fail: tin -> 'b tout) = fun tin -> 
  None

(* should be raise Impossible when transformation.ml *)
let fail2 = fail

let (>||>) m1 m2 = fun tin -> 
  match m1 tin with
  | None -> m2 tin
  | Some x -> Some x


let (option: 
     ('a, 'b) matcher -> 'a option -> 'b option -> tin -> 'b option tout) = 
 fun f t1 t2 ->
  match (t1,t2) with
  | (Some t1, Some t2) -> 
      f t1 t2 >>= (fun (x : 'b) -> 
        return (Some x)
      )
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

let tokenf_one xs ys = raise Todo

let tokenf_wrap xs ys e = 
  tokenf xs ys >>= (fun ii' ->  return (e, ii'))

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let distrf distrop mck x   = raise Todo

let distrf_e x = raise Todo

let distrf_node x = raise Todo

(*****************************************************************************)
(* "Cocci vs C" *) 
(*****************************************************************************)

let rec (expression: (Ast_cocci.expression, Ast_c.expression) matcher) =
 fun ea eb -> 
  match A.unwrap ea, eb with
  
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
     (* todo: do special case to allow IdMetaFunc, cos doing the
      * recursive call will be too late, match_ident will not have the
      * info whether it was a function. todo: but how detect when do
      * x.field = f; how know that f is a Func ? By having computed
      * some information before the matching!
      * 
      * Allow match with FunCall containing types. Now ast_cocci allow
      * type in parameter, and morover ast_cocci allow f(...) and those
      * ... could match type. *)

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

  | A.MetaId(ida,inherited) -> 
      (* get binding, assert =*=,  distribute info in i1 *)
      envf inherited (term ida, Ast_c.MetaIdVal (idb)) >>= (fun v -> 
        match v with
        | Ast_c.MetaIdVal sa -> 
            if (sa =$= idb) 
            then idb +> tokenf_wrap [ida] ii
            else fail
        | _ -> raise Impossible
      )
  | A.MetaFunc(ida,inherited) -> 
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
          failwith "MetaFunc and MetaLocalFunc, need more semantic info about id"
      )
      
  | A.MetaLocalFunc(ida,inherited) -> 
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
          failwith "MetaFunc and MetaLocalFunc, need more semantic info about id"
      )
        
  | A.OptIdent _ | A.UniqueIdent _ | A.MultiIdent _ -> 
      failwith "not handling Opt/Unique/Multi for ident"


(* ------------------------------------------------------------------------- *)
and (arguments: sequence -> 
      (Ast_cocci.expression list, Ast_c.argument Ast_c.wrap2 list) matcher) = 
 fun seqstyle eas ebs ->
   raise Todo

and argument arga argb = 
   raise Todo

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

and storage stoa stob =
  raise Todo

and onedecl = fun decla declb -> 
   raise Todo


and (fullType: (Ast_cocci.fullType, Ast_c.fullType) matcher) = 
 fun typa typb -> 
   raise Todo

and (typeC: (Ast_cocci.typeC, Ast_c.fullType) matcher) = 
 fun typa typb -> 
   raise Todo

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
  | A.MetaRuleElem(mcode,inherited), unwrap_node -> 
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
  | A.MetaStmt (ida,_metainfo,inherited),  unwrap_node -> 
     (* match only "header"-statement *)
     (match Control_flow_c.extract_fullstatement node with
     | Some stb -> 
         envf inherited (term ida, Ast_c.MetaStmtVal stb) >>= (fun v -> 
           (* do the match v with ... ? *)
           return unwrap_node
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
      raise Todo
(*
      (match ii with
      | iidb::ioparenb::icparenb::iistob -> 

      let stob' = stob in
      let (iistob') = 
        match stoa, fst stob, iistob with
        | None, _, _ -> 
            if allminus 
            then 
              let minusizer = iistob +> List.map (fun _ -> 
                "fake", {Ast_cocci.line = 0; column =0},(Ast_cocci.MINUS [])
                ) in
              tag_symbols minusizer iistob binding
            else iistob
        | Some x, B.Sto B.Static, stostatic::stoinline -> 
           assert (term x = A.Static);
            tag_symbols [wrap_mcode x] [stostatic] binding ++ stoinline
           
        | _ -> raise NoMatch

      in
      let retb' = 
        match tya with
        | None -> 
            if allminus 
            then D.distribute_mck (Ast_cocci.MINUS [])  D.distribute_mck_type
                     retb binding       
            else retb
        | Some tya -> transform_ft_ft tya retb binding
      in

      let (idb', iidb') = 
        transform_ident LocalFunction ida (idb, [iidb])   binding 
      in
      
      let iiparensb' = tag_symbols [oparen;cparen] [ioparenb;icparenb] binding
      in

      let seqstyle = 
        (match A.unwrap paramsa with 
        | A.DOTS _ -> Ordered 
        | A.CIRCLES _ -> Unordered 
        | A.STARS _ -> failwith "not yet handling stars (interprocedural stuff)"
        ) 
      in
      let paramsb' = 
        transform_params seqstyle (A.undots paramsa) paramsb    binding 
      in

      if isvaargs then failwith "not handling variable length arguments func";
      let iidotsb' = iidotsb in (* todo *)

      F.FunHeader 
        ((idb', (retb', (paramsb', (isvaargs, iidotsb'))), stob'), 
         (iidb'++iiparensb'++iistob'))
      | _ -> raise Impossible
      )
*)
      

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

