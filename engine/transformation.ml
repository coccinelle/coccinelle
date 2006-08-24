open Common open Commonop

open Ograph_extended

module A = Ast_cocci
module B = Ast_c
module F = Control_flow_c
module D = Distribute_mcodekind

(*****************************************************************************)
(* todo: Must do some try, for instance when f(...,X,Y,...) have to test the 
 * transfo for all the combinaitions (and if multiple transfo possible ? pb ? 
 * => the type is to return a   expression option ? use some combinators to 
 *  help ?
 *
 * For some nodes I dont have all the info, for instance for } I need to modify
 * the node of the start, it is where the info is.
 * Same for Else. 
 *)
(*****************************************************************************)
type ('a, 'b) transformer = 'a -> 'b -> Lib_engine.metavars_binding -> 'b

exception NoMatch 

type sequence_processing_style = Ordered | Unordered

(* ------------------------------------------------------------------------- *)
let term ((s,_,_) : 'a Ast_cocci.mcode) = s
let mcodekind (_,i,mc) = mc
let wrap_mcode (_,i,mc) = ("fake", i, mc)

(* ------------------------------------------------------------------------- *)
let (tag_symbols: ('a A.mcode) list -> B.il -> B.metavars_binding -> B.il)
  = fun xs ys binding ->
  assert (List.length xs = List.length ys);
  zip xs ys +> List.map (fun ((s1,_,x),   (s2, (oldmcode, oldenv))) -> 
    (* assert s1 = s2 ? no more cos now have some "fake" string,
     * and also because now s1:'a, no more s1:string
     *)
    (s2, (x, binding)))

let tag_one_symbol = fun ia ib  binding -> 
  let (s1,_,x) = ia in
  let (s2, (oldmcode, oldenv)) = ib in
  (s2, (x, binding))



(*****************************************************************************)

let rec 
  (transform_re_node: (Ast_cocci.rule_elem, Control_flow_c.node) transformer) =
 fun re node -> 
  fun binding -> 

  F.rewrap node (
  match A.unwrap re, F.unwrap node with

  | _, F.Enter | _, F.Exit | _, F.ErrorExit -> raise Impossible

  | A.MetaRuleElem mcode, unwrap_node -> 
     (match unwrap_node with
     | F.Fake  | F.CaseNode _
     | F.TrueNode | F.FalseNode | F.AfterNode | F.FallThroughNode
       -> unwrap_node

     | F.FunHeader _ -> failwith "a MetaRuleElem can't transform a headfunc"

     | n -> D.distribute_mck (mcodekind mcode) D.distribute_mck_node n binding
     )


  (* rene cant have found that a state containing a fake/exit/... should be 
   * transformed 
   *)
  | _, F.Fake | _, F.CaseNode _
  | _, F.TrueNode | _, F.FalseNode | _, F.AfterNode | _, F.FallThroughNode
    -> raise Impossible
 

  | A.MetaStmt _,  _ -> 
      failwith "I cant have been called. I can only transform MetaRuleElem."
  | A.MetaStmtList _, _ -> 
      failwith "not handling MetaStmtList"

  (* It is important to put this case before the one that follows, cos
     want to transform a switch, even if cocci does not have a switch
     statement, because we may have put an Exp, and so have to
     transform the expressions inside the switch. *)

  | A.Exp exp, nodeb -> 
      let bigf = { Visitor_c.default_visitor_c_s with Visitor_c.kexpr_s = 
             (fun (k,_) e -> 
               let e' = k e in (* go inside first *)
               try transform_e_e exp e'   binding 
               with NoMatch -> e'
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
                       (((e1opt +> map_option visitor_e, i1),
                         (e2opt +> map_option visitor_e, i2),
                         (e3opt +> map_option visitor_e, i3)),
                        ii))
            
      | F.ReturnExpr (st, (e,ii)) -> F.ReturnExpr (st, (visitor_e e, ii))
            
      | F.Case  (st, (e,ii)) -> F.Case (st, (visitor_e e, ii))
      | F.CaseRange (st, ((e1, e2),ii)) -> 
          F.CaseRange (st, ((visitor_e e1, visitor_e e2), ii))

      (* called on transforming a node that does not contain any expr *)
      | _ -> raise Impossible 
      )

  | A.FunHeader (allminus, stoa, tya, ida, oparen, paramsa, cparen),
    F.FunHeader ((idb, (retb, (paramsb, (isvaargs, iidotsb))), stob), ii) -> 
      (match ii with
      | iidb::ioparenb::icparenb::iistob -> 

	  (* Need to do something with allminus and tya *)

      let stob' = 
        (match stoa with 
        | None -> stob 
        | Some x -> failwith "not handling storage"
        ) in
      let iistob' = iistob in (* todo *)

      let (idb', iidb') = 
        transform_ident Pattern.LocalFunction ida (idb, [iidb])   binding 
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
        ((idb', (retb, (paramsb', (isvaargs, iidotsb'))), stob'), 
         (iidb'++iiparensb'++iistob'))
      | _ -> raise Impossible
      )
      

  | A.Decl decla, F.Decl declb -> 
      F.Decl (transform_de_de decla declb  binding) 

  | A.SeqStart mcode, F.SeqStart (st, level, i1) -> 
      F.SeqStart (st, level, tag_one_symbol mcode i1 binding)

  | A.SeqEnd mcode, F.SeqEnd (level, i2) -> 
      F.SeqEnd (level, tag_one_symbol mcode i2 binding)


  | A.ExprStatement (ea, i1), F.ExprStatement (st, (Some eb, ii)) -> 
      F.ExprStatement (st, (Some (transform_e_e ea eb  binding), 
                            tag_symbols [i1] ii  binding ))

  | A.IfHeader (i1,i2, ea, i3), F.IfHeader (st, (eb,ii)) -> 
      F.IfHeader (st, (transform_e_e ea eb  binding,
                       tag_symbols [i1;i2;i3] ii binding))
  | A.Else ia, F.Else ib -> F.Else (tag_one_symbol ia ib binding)
  | A.WhileHeader (i1, i2, ea, i3), F.WhileHeader (st, (eb, ii)) -> 
      F.WhileHeader (st, (transform_e_e ea eb  binding, 
                          tag_symbols [i1;i2;i3] ii  binding))
  | A.DoHeader ia, F.DoHeader (st, ib) -> 
      F.DoHeader (st, tag_one_symbol ia ib  binding)
  | A.WhileTail (i1,i2,ea,i3,i4), F.DoWhileTail (eb, ii) -> 
      F.DoWhileTail (transform_e_e ea eb binding, 
                     tag_symbols [i1;i2;i3;i4] ii  binding)
  | A.ForHeader (i1, i2, ea1opt, i3, ea2opt, i4, ea3opt, i5), 
    F.ForHeader (st, (((eb1opt,ib1), (eb2opt,ib2), (eb3opt,ib3)), ii))
    -> 
      let transform (ea, ia) (eb, ib) = 
        transform_option (fun ea eb -> transform_e_e ea eb binding) ea eb, 
        tag_symbols ia ib   binding
      in
      F.ForHeader (st,
            ((transform (ea1opt, [i3]) (eb1opt, ib1),
              transform (ea2opt, [i4]) (eb2opt, ib2),
              transform (ea3opt, []) (eb2opt, ib3)),
            tag_symbols [i1;i2;i5] ii  binding))


  | A.Return (i1, i2), F.Return (st, ((),ii)) -> 
      F.Return (st, ((), tag_symbols [i1;i2] ii   binding))
  | A.ReturnExpr (i1, ea, i2), F.ReturnExpr (st, (eb, ii)) -> 
      F.ReturnExpr (st, (transform_e_e ea eb binding, 
                         tag_symbols [i1;i2] ii   binding))

  | _, F.ExprStatement (_, (None, ii)) -> raise NoMatch (* happen ? *)

  (* have not a counter part in coccinelle, for the moment *)
  | _, F.SwitchHeader _ 
  | _, F.Label _
  | _, F.Case _  | _, F.CaseRange _  | _, F.Default _
  | _, F.Goto _ | _, F.Break _ | _, F.Continue _ 
  | _, F.Asm
    -> raise Impossible

  | _, _ -> raise NoMatch
  )

(* ------------------------------------------------------------------------- *)

and (transform_de_de: (Ast_cocci.declaration, Ast_c.declaration) transformer) =
 fun decla declb -> 
  fun binding -> 
  match declb with
  | (B.DeclList ([var], iiptvirgb::iisto)) -> 
      let (var', iiptvirgb') = transform_onedecl decla (var, iiptvirgb) binding
      in
      B.DeclList ([var'], iiptvirgb'::iisto)

  | (B.DeclList (x::y::xs, iiptvirgb::iisto)) -> 
      failwith "More that one variable in decl. Have to split to transform."
  
  | _ -> raise Impossible                

and transform_onedecl = fun decla declb -> 
 fun binding -> 
   match A.unwrap decla, declb with
   | A.UnInit (typa, ida, ptvirga), 
     (((Some ((idb, None),iidb::iini), typb, stob), iivirg), iiptvirgb) -> 
       assert (null iini);

       let iiptvirgb' = tag_symbols [ptvirga] [iiptvirgb] binding  in

       let typb' = transform_ft_ft typa typb  binding in
       let (idb', iidb') = 
         transform_ident Pattern.DontKnow ida (idb, [iidb])  binding 
       in
       ((Some ((idb', None), iidb'++iini), typb', stob), iivirg), 
       List.hd iiptvirgb'

   | A.Init (typa, ida, eqa, expa, ptvirga), 
     (((Some ((idb, Some ini),[iidb;iieqb]), typb, stob), iivirg),iiptvirgb) ->

       let iiptvirgb' = tag_symbols [ptvirga] [iiptvirgb] binding  in
       let iieqb' = tag_symbols [eqa] [iieqb] binding in
       let typb' = transform_ft_ft typa typb  binding in
       let (idb', iidb') = 
         transform_ident Pattern.DontKnow ida (idb, [iidb])  binding 
       in
       let ini' = 
         match ini with
         | B.InitExpr expb, ii -> 
             assert (null ii);
             B.InitExpr (transform_e_e expa  expb binding), ii
         | _ -> 
             pr2 "warning: complex initializer, cocci does not handle that";
             raise NoMatch
       in
       ((Some ((idb', Some ini'), iidb'++iieqb'), typb', stob), iivirg), 
        List.hd iiptvirgb'
       
       
   | _, (((None, typb, sto), _),_) -> 
       failwith "no variable in this declaration, wierd"

   | A.MetaDecl ida, _ -> 
       failwith "impossible ? can we transform MetaDecl ? I thought julia never do that"

   | A.DisjDecl xs, declb -> 
       xs +> Common.fold_k (fun acc decla k -> 
         try transform_onedecl decla acc  binding
         with NoMatch -> k acc
        ) declb

            
   | A.OptDecl _, _ | A.UniqueDecl _, _ | A.MultiDecl _, _ -> 
       failwith "not handling Opt/Unique/Multi Decl"
   | _, _ -> raise NoMatch

  
(* ------------------------------------------------------------------------- *)

and (transform_e_e: (Ast_cocci.expression, Ast_c.expression) transformer) = 
 fun ep ec -> 
  fun binding -> 
  
  match A.unwrap ep, ec with

  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr (ida, opttypa),  (((expr, opttypb), ii) as expb) -> 
      (match opttypa, opttypb with
      | None, _ -> ()
      | Some tas, Some tb -> 
	  failwith "transformation on types not supported"
	  (*
          if (not 
                (tas +> 
                 List.exists (fun ta -> 
                   List.length (Pattern.match_ft_ft ta tb binding) >= 1)))
          then raise NoMatch *)
      | Some _, None -> 
          failwith ("I have not the type information. Certainly a pb in " ^
                    "annotate_typer.ml")
      );


     (* get binding, assert =*=,  distribute info in ida *)
      let v = binding +> List.assoc (term ida) in
      (match v with
      | B.MetaExprVal expa -> 
          if (Abstract_line_c.al_expr expa =*= Abstract_line_c.al_expr expb)
          then D.distribute_mck (mcodekind ida) D.distribute_mck_e expb binding
          else raise NoMatch
      | _ -> raise Impossible
      )

  (* todo: in fact can also have the Edots family inside nest, as in 
     if(<... x ... y ...>) or even in simple expr as in x[...] *)
  | A.Edots (mcode, None), expb    -> 
      D.distribute_mck (mcodekind mcode) D.distribute_mck_e expb   binding

  | A.Edots (_, Some expr), _    -> failwith "not handling when on Edots"


  | A.MetaConst _, _ -> failwith "not handling MetaConst"
  | A.MetaErr _, _ -> failwith "not handling MetaErr"
      
  | A.Ident ida,                ((B.Ident idb, typ),ii) ->
      let (idb', ii') = transform_ident Pattern.DontKnow ida (idb, ii) binding 
      in
      (B.Ident idb', typ),ii'


  | A.Constant ((A.Int ia,_,_) as i1), ((B.Constant (B.Int ib) , typ),ii)
    when ia =$= ib ->  
      (B.Constant (B.Int ib), typ), 
      tag_symbols [i1] ii binding

  | A.Constant ((A.Char ia,_,_) as i1), ((B.Constant (B.Char (ib,t)), typ),ii)
    when ia =$= ib ->  
      (B.Constant (B.Char (ib, t)), typ), 
      tag_symbols [i1] ii binding

  | A.Constant ((A.String ia,_,_)as i1),((B.Constant (B.String (ib,t)),typ),ii)
    when ia =$= ib ->  
      (B.Constant (B.String (ib, t)), typ),
      tag_symbols [i1] ii binding

  | A.Constant ((A.Float ia,_,_) as i1),((B.Constant (B.Float (ib,t)),typ),ii)
    when ia =$= ib ->  
      (B.Constant (B.Float (ib,t)), typ),
      tag_symbols [i1] ii binding


  | A.FunCall (ea, i2, eas, i3),  ((B.FunCall (eb, ebs), typ),ii) -> 
      let seqstyle = 
        (match A.unwrap eas with 
        | A.DOTS _ -> Ordered 
        | A.CIRCLES _ -> Unordered 
        | A.STARS _ -> failwith "not handling stars"
        )  
      in
      
      (B.FunCall (transform_e_e ea eb binding,  
                 transform_arguments seqstyle (A.undots eas) ebs binding),typ),
      tag_symbols [i2;i3] ii  binding


  | A.Assignment (ea1, opa, ea2), ((B.Assignment (eb1, opb, eb2), typ),ii) -> 
     if Pattern.equal_assignOp (term opa) opb 
     then
       (B.Assignment (transform_e_e ea1 eb1 binding, 
                     opb, 
                     transform_e_e ea2 eb2 binding), typ),
       tag_symbols [opa] ii  binding
     else raise NoMatch

  | A.CondExpr (ea1,i1,ea2opt,i2,ea3),((B.CondExpr (eb1,eb2opt,eb3),typ),ii) ->
      (B.CondExpr (transform_e_e ea1 eb1  binding,
                  transform_option (fun a b -> transform_e_e a b binding) 
                    ea2opt eb2opt,
                  transform_e_e ea3 eb3 binding),typ),
      tag_symbols [i1;i2] ii   binding

  | A.Postfix (ea, opa), ((B.Postfix (eb, opb), typ),ii) -> 
      if (Pattern.equal_fixOp (term opa) opb)
      then (B.Postfix (transform_e_e ea eb binding, opb), typ),
           tag_symbols [opa] ii  binding
      else raise NoMatch
                   
                 
  | A.Infix (ea, opa), ((B.Infix (eb, opb), typ),ii) -> 
      if (Pattern.equal_fixOp (term opa) opb)
      then (B.Infix (transform_e_e ea eb binding, opb), typ),
           tag_symbols [opa] ii  binding
      else raise NoMatch

  | A.Unary (ea, opa), ((B.Unary (eb, opb), typ),ii) -> 
      if (Pattern.equal_unaryOp (term opa) opb)
      then (B.Unary (transform_e_e ea eb binding, opb), typ),
           tag_symbols [opa] ii  binding
      else raise NoMatch


  | A.Binary (ea1, opa, ea2), ((B.Binary (eb1, opb, eb2), typ),ii) -> 
      if (Pattern.equal_binaryOp (term opa) opb)
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
            transform_ident Pattern.DontKnow ida (idb, [i2])   binding 
          in
          let i1' = tag_symbols [dot] [i1] binding in
          (B.RecordAccess (transform_e_e ea eb binding, idb'), typ), i1' ++ i2'
      | _ -> raise Impossible
      )


  | A.RecordPtAccess (ea,fleche,ida),((B.RecordPtAccess (eb, idb), typ), ii) ->
      (match ii with
      | [i1;i2] -> 
          let (idb', i2') = 
            transform_ident Pattern.DontKnow ida (idb, [i2])   binding 
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
        ) eb

  | A.MultiExp _, _ | A.UniqueExp _,_ | A.OptExp _,_ -> 
      failwith "not handling Opt/Unique/Multi on expr"



 (* Because of Exp cant put a raise Impossible; have to put a raise NoMatch; *)

 (* have not a counter part in coccinelle, for the moment *) 
  | _, ((B.Sequence _,_),_) 

  | _, ((B.StatementExpr _,_),_) 
  | _, ((B.Constructor,_),_) 
  | _, ((B.MacroCall _,_),_) 
  | _, ((B.MacroCall2 _,_),_)
    -> raise NoMatch

  | _, _ -> raise NoMatch






(* ------------------------------------------------------------------------- *)

and (transform_arguments: 
   sequence_processing_style -> 
   (Ast_cocci.expression list, 
    ((Ast_c.expression, Ast_c.fullType * (Ast_c.storage * Ast_c.il)) either * 
     Ast_c.il) 
    list) 
   transformer) = 
 fun seqstyle eas ebs ->
  fun binding -> 
    (* TODO and when have dots ? *)
    match eas, ebs with
    | [], [] -> []

    | [ea], [Left eb, ii] -> 
        assert (null ii);
        [Left (transform_e_e ea eb binding),  []]

    | ea::eas,  (Left eb, ii)::ebs -> 
	(match (A.unwrap ea,eas) with
	  ((A.EComma i1),ea::eas) ->
            let ii' = tag_symbols [i1] ii   binding in
            (Left (transform_e_e  ea eb binding), ii')::
	    transform_arguments seqstyle eas ebs   binding
	| _ ->
            assert (null ii);
            (Left (transform_e_e  ea eb binding), [])::
	    transform_arguments seqstyle eas ebs   binding)
    | _ -> raise Impossible


and (transform_params: 
   sequence_processing_style -> 
   (Ast_cocci.parameterTypeDef list, 
    ((Ast_c.parameterType * Ast_c.il) list)) 
   transformer) = 
 fun seqstyle pas pbs ->
  fun binding -> 
    match pas, pbs with
    | [], [] -> []
    | [pa], [pb, ii] -> 
        assert (null ii);
        [transform_param pa pb binding, ii]
    | pa::pas, (pb, ii)::pbs -> 
	(match (A.unwrap pa,pas) with
        | ((A.PComma i1), pa::pas) ->
            let ii' = tag_symbols [i1] ii binding in
            (transform_param pa pb binding, ii')::
	    transform_params seqstyle pas pbs  binding
	| _ ->
            assert (null ii);
            ((transform_param pa pb binding),[])::
              transform_params seqstyle pas pbs binding
        )

    | _ -> raise Impossible

and (transform_param: 
     (Ast_cocci.parameterTypeDef, (Ast_c.parameterType)) transformer) = 
 fun pa pb  -> 
  fun binding -> 
    match A.unwrap pa, pb with
    | A.Param (ida, typa), ((hasreg, idb, typb), ii_b_s) -> 
        
        let kindparam = 
          (match hasreg, idb,  ii_b_s with
          | false, Some s, [i1] -> Left (s, [], i1)
          | true, Some s, [i1;i2] -> Left (s, [i1], i2)
          | _, None, ii -> 
              pr2 "NORMALLY IMPOSSIBLE. The Cocci Param has an ident but not the C";
              Right ii
              
              
          | _ -> raise Impossible
          )
        in
        (match kindparam with
        | Left (idb, iihasreg, iidb) -> 
            let (idb', iidb') = 
              transform_ident Pattern.DontKnow ida (idb, [iidb])   binding 
            in
            let typb' = transform_ft_ft typa typb binding in
            (hasreg, Some idb', typb'), (iihasreg++iidb') 
        | Right iihasreg -> 
            let typb' = transform_ft_ft typa typb binding in
            (hasreg, None, typb'), (iihasreg) 
        )
            
        
        
    | A.PComma _, _ -> raise Impossible
    | _ -> raise Todo

(* ------------------------------------------------------------------------- *)
and (transform_ft_ft: (Ast_cocci.fullType, Ast_c.fullType) transformer) = 
 fun typa typb -> 
  fun binding -> 
    match A.unwrap typa, typb with
    | A.Type(cv,ty1), ((qu,il),ty2) ->

	(match cv with
          (* "iso-by-absence" *)
        | None -> transform_t_t ty1 typb  binding
        | Some x -> raise Todo
        )

    | A.OptType(_), _  | A.UniqueType(_), _ | A.MultiType(_), _ 
      -> failwith "not handling Opt/Unique/Multi on type"



and (transform_t_t: (Ast_cocci.typeC, Ast_c.fullType) transformer) = 
 fun typa typb -> 
  fun binding -> 
    match A.unwrap typa, typb with

     (* cas general *)
    | A.MetaType ida,  typb -> 
        (* get binding, assert =*=,  distribute info in ida *)
        (match binding +> List.assoc (term ida) with
        | B.MetaTypeVal typa -> 
          if (Abstract_line_c.al_type typa =*= Abstract_line_c.al_type typb)
          then 
            D.distribute_mck (mcodekind ida) D.distribute_mck_type typb binding
          else raise NoMatch
        | _ -> raise Impossible
      )

    | A.BaseType (basea, signaopt),   (qu, (B.BaseType baseb, ii)) -> 
       (* In ii there is a list, sometimes of length 1 or 2 or 3.
        * And even if in baseb we have a Signed Int, that does not mean
        * that ii is of length 2, cos Signed is the default, so if in signa
        * we have Signed explicitely ? we cant "accrocher" this mcode to 
        * something :( So for the moment when there is signed in cocci,
        * we force that there is a signed in c too (done in pattern.ml).
        *)
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
        in
        let signbopt, iibaseb = split_signb_baseb_ii (baseb, ii) in

	let transform_sign signa signb = 
          match signa, signb with
          | None, None -> []
          | Some signa,  Some (signb, ib) -> 
              if Pattern.equal_sign (term signa) signb
              then [tag_one_symbol signa ib  binding]
              else raise NoMatch
          | _, _ -> raise NoMatch
        in
        
        let qu' = qu in (* todo ? or done in transform_ft_ft ? *)
        qu', 
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

    | A.Pointer (typa, imult),            (qu, (B.Pointer typb, ii)) -> 
        let ii' = tag_symbols [imult] ii binding in
        let typb' = transform_ft_ft typa typb  binding in
        (qu, (B.Pointer typb', ii'))
        
    | A.Array (typa, _, eaopt, _), (qu, (B.Array (ebopt, typb), _)) -> 
        raise Todo
    | A.StructUnionName(sa, sua), (qu, (B.StructUnionName (sb, sub), ii)) -> 
        if Pattern.equal_structUnion  (term sua) sub && (term sa) =$= sb
        then
          let ii' = tag_symbols [wrap_mcode sua; sa] ii  binding in
          (qu, (B.StructUnionName (sb, sub), ii'))
        else raise NoMatch
        

    | A.TypeName sa,  (qu, (B.TypeName sb, ii)) ->
        if (term sa) =$= sb
        then
          let ii' = tag_symbols  [sa] ii binding in
          qu, (B.TypeName sb, ii')
        else raise NoMatch
        
        

    | _ -> raise NoMatch

        


(* ------------------------------------------------------------------------- *)

and (transform_ident: 
      Pattern.semantic_info_ident -> 
      (Ast_cocci.ident, (string * Ast_c.il)) transformer) = 
 fun seminfo_idb ida (idb, ii) -> 
  fun binding -> 
    match A.unwrap ida with
    | A.Id sa -> 
        if (term sa) =$= idb
        then idb, tag_symbols [sa] ii binding
        else raise NoMatch

    | A.MetaId ida -> 
      (* get binding, assert =*=,  distribute info in i1 *)
      let v = binding +> List.assoc ((term ida) : string) in
      (match v with
      | B.MetaIdVal sa -> 
          if(sa =$= idb) 
          then idb, tag_symbols [ida] ii binding
          else raise NoMatch
      | _ -> raise Impossible
      )
 | A.MetaFunc ida -> 
     (match seminfo_idb with 
     | Pattern.LocalFunction | Pattern.Function -> 
         let v = binding +> List.assoc ((term ida) : string) in
         (match v with
         | B.MetaFuncVal sa -> 
             if(sa =$= idb) 
             then idb, tag_symbols [ida] ii binding
             else raise NoMatch
         | _ -> raise Impossible
         )
     | Pattern.DontKnow -> 
        failwith "MetaFunc and MetaLocalFunc, need more semantic info about id"
     )
      
 | A.MetaLocalFunc ida -> 
     (match seminfo_idb with
     | Pattern.LocalFunction -> 
         let v = binding +> List.assoc ((term ida) : string) in
         (match v with
         | B.MetaLocalFuncVal sa -> 
             if(sa =$= idb) 
             then idb, tag_symbols [ida] ii binding
             else raise NoMatch
         | _ -> raise Impossible
         )


     | Pattern.Function -> raise NoMatch
     | Pattern.DontKnow -> 
        failwith "MetaFunc and MetaLocalFunc, need more semantic info about id"
     )

 | A.OptIdent _ | A.UniqueIdent _ | A.MultiIdent _ -> 
     failwith "not handling Opt/Unique/Multi for ident"
        


(* ------------------------------------------------------------------------- *)
and transform_option f t1 t2 =
  match (t1,t2) with
  | (Some t1, Some t2) -> Some (f t1 t2)
  | (None, None) -> None
  | _ -> raise NoMatch




(*****************************************************************************)
(* Entry points *)

let rec (transform: Lib_engine.transformation_info -> F.cflow -> F.cflow) = 
 fun xs cflow -> 
  (* find the node, transform, update the node,  and iter for all elements *)

   xs +> List.fold_left (fun acc (nodei, binding, rule_elem) -> 
      (* subtil: not cflow#nodes but acc#nodes *)
      let node  = acc#nodes#assoc nodei in 

      pr2 "transform one node";
      let node' = transform_re_node rule_elem node binding in

      (* assert that have done something. But with metaruleElem sometimes 
         dont modify fake nodes. So special case before on Fake nodes. *)
      (match F.unwrap node with
      | F.Enter | F.Exit | F.ErrorExit
      | F.Fake | F.CaseNode _        
      | F.TrueNode | F.FalseNode | F.AfterNode | F.FallThroughNode 
          -> ()
      | _ -> assert (not (node =*= node'));
      );

      acc#replace_node (nodei, node')
     ) cflow

