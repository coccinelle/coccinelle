open Common open Commonop

open Ograph_extended

module A = Ast_cocci
module B = Ast_c

module F = Control_flow_c

module D = Distribute_mcodekind

(******************************************************************************)
(* todo: Must do some try, for instance when f(...,X,Y,...) have to test the 
   transfo for all the combinaitions (and if multiple transfo possible ? pb ? 
   => the type is to return a   expression option ? use some combinators to 
    help ?

   For some nodes I dont have all the info, for instance for } I need to modify 
   the node of the start, it is where the info is.
   Same for Else. 

*)
(******************************************************************************)

type ('a, 'b) transformer = 'a -> 'b -> Lib_engine.metavars_binding -> 'b

exception NoMatch 

type sequence_processing_style = Ordered | Unordered

(* -------------------------------------------------------------------------- *)
let term ((s,_,_) : 'a Ast_cocci.mcode) = s
let wrap_mcode (_,i,mc) = "fake", i, mc
let mcodekind (_,i,mc) = mc


(******************************************************************************)

let rec (transform_re_node: 
    (Ast_cocci.rule_elem, Control_flow_c.node) transformer) = 
 fun re node -> 
  fun binding -> 

  match A.unwrap re, F.unwrap node with

  | A.MetaRuleElem mcode, unwrap_node -> 
     (match unwrap_node with
     | F.Enter | F.Exit    -> node
     | F.Fake              -> node
     | F.CaseNode _        -> node
     | F.TrueNode | F.FalseNode | F.AfterNode | F.FallThroughNode -> node
     | F.ErrorExit -> node
     | F.StartBrace (level, statement, i1) -> 
         (F.StartBrace 
            (level, statement, tag_one_symbol (wrap_mcode mcode) i1 binding))
         +> F.rewrap node
     | F.EndBrace (level, i2) -> 
         F.EndBrace (level, tag_one_symbol (wrap_mcode mcode) i2 binding)
         +> F.rewrap node
     | F.Declaration decl -> 
         F.Declaration (
           D.distribute_mck (mcodekind mcode) D.distribute_mck_decl decl binding
            ) +> F.rewrap node
     | F.HeadFunc _ -> failwith "a MetaRuleElem can't transform a headfunc"
     | F.Statement stat -> 
         F.Statement (
           D.distribute_mck (mcodekind mcode) D.distribute_mck_stat stat binding
         ) +> F.rewrap node
      )

  (* rene cant have found that a state containing a fake/exit/... should be 
     transformed *)
  | _, F.Enter | _, F.Exit -> raise Impossible
  | _, F.Fake              -> raise Impossible 
  | _, F.CaseNode _        -> raise Impossible
  | _, F.TrueNode | _, F.FalseNode 
  | _, F.AfterNode | _, F.FallThroughNode | _, F.ErrorExit -> 
      raise Impossible
 


  (* todo?: it can match a MetaStmt too !! and we have to get all the 
     concerned nodes *)
  | A.SeqStart mcode, F.StartBrace (level, statement, i1) -> 
      (F.StartBrace 
         (level, statement, tag_one_symbol (wrap_mcode mcode) i1 binding ))
      +> F.rewrap node 

  | A.SeqEnd mcode, F.EndBrace (level, i2) -> 
      F.EndBrace (level, tag_one_symbol (wrap_mcode mcode) i2 binding)
      +> F.rewrap node

  | A.MetaStmt ((ida,_,i1)),  stb -> 
      failwith "I cant have been called. I can only transform MetaRuleElem."

  | A.SeqStart _, _ | _, F.StartBrace _ -> raise NoMatch
  | A.SeqEnd _, _   | _, F.EndBrace _ -> raise NoMatch


  | A.Decl decla, F.Declaration declb -> 
      F.Declaration (transform_de_de decla declb  binding) +> F.rewrap node

  | A.Exp exp, F.Declaration declb -> 
      (* todo?: assert have done something  statement' <> statement *)
     F.Declaration 
        (declb +> Visitor_c.visitor_decl_k_s { 
        Visitor_c.default_visitor_c_s with
        Visitor_c.kexpr_s = (fun (k,_) e -> 
          let e' = k e in (* go inside first *)
          try transform_e_e exp e'   binding 
          with NoMatch -> e'
          )
          }
        ) +> F.rewrap node

  | A.Decl _, _ | _, F.Declaration _ -> raise NoMatch

  | A.FunHeader (stoa, ida, oparen, paramsa, cparen),
    F.HeadFunc ((idb, (retb, (paramsb, (isvaargs, iidotsb))), stob, compoundb), 
                ii)
    -> 
      (match ii with
      | iidb::ioparenb::icparenb::iobraceb::icbraceb::iistob -> 

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

      let typb' = (retb, (paramsb', (isvaargs, iidotsb'))) in
      Control_flow_c.HeadFunc 
        ((idb', typb' , stob', compoundb), 
         (iidb'++iiparensb'++[iobraceb;icbraceb]++iistob'))
        +> F.rewrap node
      | _ -> raise Impossible
      )
      

  | A.FunHeader _,_ | _,F.HeadFunc _ -> raise NoMatch

  | _, F.Statement st -> 
      F.Statement (transform_re_st re st binding) +> F.rewrap node





(* -------------------------------------------------------------------------- *)

and (transform_re_st: (Ast_cocci.rule_elem, Ast_c.statement) transformer)  = 
 fun re st -> 
  fun binding -> 

  match A.unwrap re, st with
  (* this is done in transform_re_node, or transform_re_decl *)
  | A.FunHeader _, _  | A.Decl _, _ | A.SeqStart _, _ | A.SeqEnd _, _ -> 
      raise Impossible 
  | A.MetaRuleElem _, _ -> raise Impossible

  (* cas general: a Meta can match everything *)
  (* obsolete: if stb is a compound ? *)
  | A.MetaStmt ((ida,_,i1)),  stb -> 
      failwith "I cant have been called. I can only transform MetaRuleElem."

  | A.MetaStmtList _, _ -> failwith "not handling MetaStmtList"
      

  | A.ExprStatement (ea, i1), (B.ExprStatement (Some eb), ii) -> 
      B.ExprStatement (Some (transform_e_e ea eb  binding)), 
      tag_symbols [i1] ii  binding 

  | A.IfHeader (i1,i2, ea, i3), (B.Selection (B.If (eb, st1b, st2b)), ii) -> 
      B.Selection (B.If (transform_e_e ea eb  binding, st1b, st2b)),
      tag_symbols [i1;i2;i3] ii binding

  | A.Else _, _ -> failwith "not handling Else"

  | A.WhileHeader (i1, i2, ea, i3), (B.Iteration  (B.While (eb, stb)), ii) -> 
      B.Iteration (B.While (transform_e_e ea eb  binding, stb)), 
      tag_symbols [i1;i2;i3] ii  binding
        
  | A.ForHeader (i1, i2, ea1opt, i3, ea2opt, i4, ea3opt, i5), 
    (B.Iteration  (B.For ((eb1opt,ib1), (eb2opt,ib2), (eb3opt,ib3), stb)), ii)
    -> 
      let transform (ea, ia) (eb, ib) = 
        transform_option (fun ea eb -> transform_e_e ea eb binding) ea eb, 
        tag_symbols ia ib   binding
      in

      B.Iteration 
        (B.For (
            transform (ea1opt, [i3]) (eb1opt, ib1),
            transform (ea2opt, [i4]) (eb2opt, ib2),
            transform (ea3opt, []) (eb2opt, ib3),
            stb)), 
      tag_symbols [i1;i2;i5] ii  binding
        
         


  | A.DoHeader _, (B.Iteration  (B.DoWhile (eb, stb)), ii) -> 
      failwith "not handling dowhile, the info is not in the good place in cfg"
  | A.WhileTail _, _ -> 
      failwith "not handling dowhile, the info is not in the good place in cfg"


  | A.Return (i1, i2), (B.Jump (B.Return), ii) -> 
      B.Jump (B.Return), tag_symbols [i1;i2] ii   binding
  | A.ReturnExpr (i1, ea, i2), (B.Jump (B.ReturnExpr eb), ii) -> 
      B.Jump (B.ReturnExpr (transform_e_e ea eb binding)), 
      tag_symbols [i1;i2] ii   binding




  (* It is important to put this case before the one that follows, cos want to
     transform a switch, even if cocci does not have a switch statement, because
     we may have put an Exp, and so have to transform the expressions inside the
     switch. *)
  | A.Exp exp, statement -> 
      (* todo?: assert have done something  statement' <> statement *)
      statement +> Visitor_c.visitor_statement_k_s { 
        Visitor_c.default_visitor_c_s with
        Visitor_c.kexpr_s = (fun (k,_) e -> 
          let e' = k e in (* go inside first *)
          try transform_e_e exp e'   binding 
          with NoMatch -> e'
          )
          }

  | _, (B.Compound _, ii) -> raise Impossible (* can only have SeqStart ? *)

  | _, (B.ExprStatement None, ii) -> raise NoMatch (* happen ? *)

  (* have not a counter part in coccinelle, for the moment *)
  | _, (B.Labeled _, ii)              -> raise Impossible
  | _, (B.Asm , ii)                   -> raise Impossible
  | _, (B.Selection (B.Switch _), ii) -> raise Impossible
  | _, (B.Jump (B.Goto _), ii)        -> raise Impossible
  | _, (B.Jump (B.Break), ii)         -> raise Impossible
  | _, (B.Jump (B.Continue), ii)      -> raise Impossible

  | _, _ -> raise NoMatch


(* -------------------------------------------------------------------------- *)

and (transform_de_de: (Ast_cocci.declaration, Ast_c.declaration) transformer) =
 fun decla declb -> 
  fun binding -> 
  match declb with
    (B.DeclList ([var], iiptvirgb::iisto)) -> 
      (match A.unwrap decla with
      | A.UnInit (typa, ida, ptvirga) ->
	  let iiptvirgb' = tag_symbols [ptvirga] [iiptvirgb] binding  in
	  (match var with
	  | (Some ((idb, None), iidb::iini), typb, stob), iivirg -> 
              assert (null iini);
              let typb' = transform_ft_ft typa typb  binding in
              let (idb', iidb') = 
                transform_ident Pattern.DontKnow ida (idb, [iidb])  binding 
              in
              let var' = (Some ((idb', None), iidb'), typb', stob), iivirg
              in
              B.DeclList ([var'], (iiptvirgb'++iisto))
          
	  | _ -> failwith "no variable in this declaration, wierd"
          )
      |	A.DisjDecl xs -> 
          xs +> List.fold_left (fun acc decla -> 
            try transform_de_de decla acc  binding
            with NoMatch -> acc
            ) declb
            
      | A.Init _ -> 
          pr2 "warning: not handling yet initializer patterns"; 
          raise NoMatch
      | A.OptDecl _ | A.UniqueDecl _ | A.MultiDecl _ -> 
          failwith "not handling Opt/Unique/Multi Decl"

      )
  | (B.DeclList (xs, iiptvirgb::iisto)) -> 
      failwith "More that one variable in one decl. Have to split to transform."
  
  | _ -> raise Impossible                
  
(* -------------------------------------------------------------------------- *)

and (transform_e_e: (Ast_cocci.expression, Ast_c.expression) transformer) = 
 fun ep ec -> 
  fun binding -> 
  
  match A.unwrap ep, ec with

  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr (ida, opttypa),  ((expr, opttypb, ii) as expb) -> 
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
          then D.distribute_mck (mcodekind ida) D.distribute_mck_e expb  binding
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
      
  | A.Ident ida,                ((B.Ident idb) , typ,ii) ->
      let (idb', ii') = 
        transform_ident Pattern.DontKnow ida (idb, ii)   binding 
      in
      B.Ident idb', typ,ii'


  | A.Constant ((A.Int ia,_,_) as i1) , 
  ((B.Constant (B.Int ib) , typ,ii)) when ia =$= ib ->  
    B.Constant (B.Int ib), typ, tag_symbols [wrap_mcode i1] ii binding

  | A.Constant ((A.Char ia,_,_) as i1) , 
  ((B.Constant (B.Char (ib,chartype)) , typ,ii)) when ia =$= ib ->  
    B.Constant (B.Char (ib, chartype)), typ, 
     tag_symbols [wrap_mcode i1] ii binding

  | A.Constant ((A.String ia,_,_) as i1),               
  ((B.Constant (B.String (ib,stringtype)) , typ,ii)) when ia =$= ib ->  
    B.Constant (B.String (ib, stringtype)), typ,
    tag_symbols [wrap_mcode i1] ii binding

  | A.Constant ((A.Float ia,_,_) as i1) ,                
  ((B.Constant (B.Float (ib,ftyp)) , typ,ii)) when ia =$= ib ->  
    B.Constant (B.Float (ib,ftyp)), typ,
    tag_symbols [wrap_mcode i1] ii binding


  | A.FunCall (ea, i2, eas, i3),  (B.FunCall (eb, ebs), typ,ii) -> 
      let seqstyle = 
        (match A.unwrap eas with 
        | A.DOTS _ -> Ordered 
        | A.CIRCLES _ -> Unordered 
        | A.STARS _ -> failwith "not handling stars"
        )  
      in
      
      B.FunCall (transform_e_e ea eb binding,  
                 transform_arguments seqstyle (A.undots eas) ebs   binding), 
      typ, tag_symbols [i2;i3] ii  binding


  | A.Assignment (ea1, opa, ea2),   
   (B.Assignment (eb1, opb, eb2), typ,ii) -> 
     if Pattern.equal_assignOp (term opa) opb 
     then
       B.Assignment (transform_e_e ea1 eb1 binding, 
                     opb, 
                     transform_e_e ea2 eb2 binding), 
       typ, tag_symbols [wrap_mcode opa] ii  binding
     else raise NoMatch

  | A.CondExpr (ea1, i1, ea2opt, i2, ea3), 
   (B.CondExpr (eb1, eb2opt, eb3), typ,ii) -> 
      B.CondExpr (transform_e_e ea1 eb1  binding,
                  transform_option (fun a b -> transform_e_e a b binding) 
                    ea2opt eb2opt,
                  transform_e_e ea3 eb3 binding),
      typ, tag_symbols [i1;i2] ii   binding

  | A.Postfix (ea, opa), (B.Postfix (eb, opb), typ,ii) -> 
      if (Pattern.equal_fixOp (term opa) opb)
      then B.Postfix (transform_e_e ea eb binding, opb), 
           typ, tag_symbols [wrap_mcode opa] ii  binding
      else raise NoMatch
                   
                 
  | A.Infix (ea, opa), (B.Infix (eb, opb), typ,ii) -> 
      if (Pattern.equal_fixOp (term opa) opb)
      then B.Infix (transform_e_e ea eb binding, opb), 
           typ, tag_symbols [wrap_mcode opa] ii  binding
      else raise NoMatch

  | A.Unary (ea, opa), (B.Unary (eb, opb), typ,ii) -> 
      if (Pattern.equal_unaryOp (term opa) opb)
      then B.Unary (transform_e_e ea eb binding, opb), 
           typ, tag_symbols [wrap_mcode opa] ii  binding
      else raise NoMatch


  | A.Binary (ea1, opa, ea2), (B.Binary (eb1, opb, eb2), typ,ii) -> 
      if (Pattern.equal_binaryOp (term opa) opb)
      then B.Binary (transform_e_e ea1 eb1   binding, 
                     opb,  
                     transform_e_e ea2 eb2  binding),  
           typ, tag_symbols [wrap_mcode opa] ii binding
      else raise NoMatch


  | A.ArrayAccess (ea1, i1, ea2, i2), (B.ArrayAccess (eb1, eb2), typ,ii) -> 
      B.ArrayAccess (transform_e_e ea1 eb1 binding,
                     transform_e_e ea2 eb2 binding),
      typ, tag_symbols [i1;i2] ii  binding
      
  | A.RecordAccess (ea, dot, ida), (B.RecordAccess (eb, idb), typ,ii) ->
      (match ii with
      | [i1;i2] -> 
          let (idb', i2') = 
            transform_ident Pattern.DontKnow ida (idb, [i2])   binding 
          in
          let i1' = tag_symbols [dot] [i1] binding in
          B.RecordAccess (transform_e_e ea eb binding, idb'), typ, i1' ++ i2'
      | _ -> raise Impossible
      )


  | A.RecordPtAccess (ea, fleche, ida), (B.RecordPtAccess (eb, idb), typ, ii) ->
      (match ii with
      | [i1;i2] -> 
          let (idb', i2') = 
            transform_ident Pattern.DontKnow ida (idb, [i2])   binding 
          in
          let i1' = tag_symbols [fleche] [i1] binding in
          B.RecordPtAccess (transform_e_e ea eb binding, idb'), typ, i1' ++ i2'
      | _ -> raise Impossible
      )

  | A.Cast (i1, typa, i2, ea), (B.Cast (typb, eb), typ,ii) -> 
      B.Cast (transform_ft_ft typa typb  binding,
              transform_e_e ea eb binding),
      typ, tag_symbols [i1;i2]  ii binding

  | A.SizeOfExpr (i1, ea), (B.SizeOfExpr (eb), typ,ii) -> 
      B.SizeOfExpr (transform_e_e ea eb binding),
      typ, tag_symbols [i1]  ii binding

  | A.SizeOfType (i1, i2, typa, i3), (B.SizeOfType (typb), typ,ii) -> 
      B.SizeOfType (transform_ft_ft typa typb  binding),
      typ, tag_symbols [i1;i2;i3]  ii binding


  | A.Paren (i1, ea, i2), (B.ParenExpr (eb), typ,ii) -> 
      B.ParenExpr (transform_e_e ea eb  binding),
      typ, tag_symbols [i1;i2] ii  binding


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



 (* Because of Exp, cant put a raise Impossible; have to put a raise NoMatch; *)

 (* have not a counter part in coccinelle, for the moment *)
  | _, (B.Sequence _,_,_) -> raise NoMatch

  | _, (B.StatementExpr _,_,_) -> raise NoMatch  (* todo ? *)
  | _, (B.Constructor,_,_) -> raise NoMatch
  | _, (B.MacroCall _,_,_) -> raise NoMatch
  | _, (B.MacroCall2 _,_,_) -> raise NoMatch

  | _, _ -> raise NoMatch






(* -------------------------------------------------------------------------- *)

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
        
        let idb, iihasreg, iidb = 
          (match hasreg, idb,  ii_b_s with
          | false, Some s, [i1] -> s, [], i1
          | true, Some s, [i1;i2] -> s, [i1], i2
          | _, None, _ -> raise Impossible
          | _ -> raise Impossible
          )
        in

        let (idb', iidb') = 
          transform_ident Pattern.DontKnow ida (idb, [iidb])   binding 
        in
        let typb' = transform_ft_ft typa typb binding in
        (hasreg, Some idb', typb'), (iihasreg++iidb') 
        
        
    | A.PComma _, _ -> raise Impossible
    | _ -> raise Todo

(* -------------------------------------------------------------------------- *)
and (transform_ft_ft: (Ast_cocci.fullType, Ast_c.fullType) transformer) = 
 fun typa typb -> 
  fun binding -> 
    match (A.unwrap typa,typb) with
    | (A.Type(cv,ty1),((qu,il),ty2)) ->
        (* TODO handle qualifier *)
        let typb' = transform_t_t ty1 typb  binding in
        typb'
    | _ -> raise Todo   


and (transform_t_t: (Ast_cocci.typeC, Ast_c.fullType) transformer) = 
 fun typa typb -> 
  fun binding -> 
    match A.unwrap typa, typb with
      (* cas general *)
    | A.MetaType ida,  typb -> 
        (* get binding, assert =*=,  distribute info in ida *)
        let v = binding +> List.assoc (term ida) in
        (match v with
        | B.MetaTypeVal typa -> 
          if (Abstract_line_c.al_type typa =*= Abstract_line_c.al_type typb)
          then 
            D.distribute_mck (mcodekind ida) D.distribute_mck_type typb  binding
          else raise NoMatch
      | _ -> raise Impossible
      )

    | A.BaseType (basea, signaopt),   (qu, (B.BaseType baseb, ii)) -> 
       (* In ii there is a list, sometimes of length 1 or 2 or 3.
          And even if in baseb we have a Signed Int, that does not mean
          that ii is of length 2, cos Signed is the default, so
          if in signa we have Signed explicitely ? we cant "accrocher" this
          mcode to something :( 
          TODO
        *)

	let _transform_sign signa signb = 
          (match signa, signb with
          (* iso on sign, if not mentioned then free.  tochange? *)
          | None, _ -> []
          | Some a,  b -> 
              if Pattern.equal_sign (term a) b
              then raise Todo
              else raise NoMatch
          ) in
        
        let qu' = qu in (* todo ? or done in transform_ft_ft ? *)
        qu', 
	(match term basea, baseb with
        |  A.VoidType,  B.Void -> assert (signaopt = None); 
            let ii' = tag_symbols [wrap_mcode basea] ii binding in
            (B.BaseType B.Void, ii')
	| A.CharType,  B.IntType B.CChar -> 
            let ii' = tag_symbols [wrap_mcode basea] ii binding in
            (B.BaseType (B.IntType B.CChar), ii')
	| A.ShortType, B.IntType (B.Si (signb, B.CShort)) ->
            raise Todo
	| A.IntType,   B.IntType (B.Si (signb, B.CInt))   ->
            if List.length ii = 1 
            then 
              let ii' = tag_symbols [wrap_mcode basea] ii binding in
              (B.BaseType (B.IntType (B.Si (signb, B.CInt))), ii')
                  
            else raise Todo
	| A.LongType,  B.IntType (B.Si (signb, B.CLong))  ->
            raise Todo
	| A.FloatType, B.FloatType (B.CFloat) -> 
            raise Todo
	| A.DoubleType, B.FloatType (B.CDouble) -> 
            raise Todo

        | _ -> raise NoMatch
            

        )
    | A.Pointer (typa, imult),            (qu, (B.Pointer typb, ii)) -> 
        let ii' = tag_symbols [imult] ii binding in
        let typb' = transform_ft_ft typa typb  binding in
        (qu, (B.Pointer typb', ii'))
        
    | A.Array (typa, _, eaopt, _), (qu, (B.Array (ebopt, typb), _)) -> 
        raise Todo
    | A.StructUnionName(sa, sua), 
      (qu, (B.StructUnionName (sb, sub), ii)) -> 
        if Pattern.equal_structUnion  (term sua) sub && (term sa) =$= sb
        then
          let ii' = tag_symbols [wrap_mcode sua; sa] ii  binding in
          (qu, (B.StructUnionName (sb, sub), ii'))
        else raise NoMatch
        

    | A.TypeName sa,  (qu, (B.TypeName sb, ii)) ->
        if (term sa) =$= sb
        then
          let ii' = tag_symbols [wrap_mcode sa] ii binding in
          qu, (B.TypeName sb, ii')
        else raise NoMatch
        
        

    | _ -> raise NoMatch

        


(* -------------------------------------------------------------------------- *)

and (transform_ident: 
      Pattern.semantic_info_ident -> 
      (Ast_cocci.ident, (string * Ast_c.il)) transformer) = 
 fun seminfo_idb ida (idb, ii) -> 
  fun binding -> 
    match A.unwrap ida with
    | A.Id sa -> 
        if (term sa) =$= idb
        then idb, tag_symbols [wrap_mcode sa] ii binding
        else raise NoMatch

    | A.MetaId ida -> 
      (* get binding, assert =*=,  distribute info in i1 *)
      let v = binding +> List.assoc ((term ida) : string) in
      (match v with
      | B.MetaIdVal sa -> 
          if(sa =$= idb) 
          then idb, tag_symbols [wrap_mcode ida] ii binding
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
             then idb, tag_symbols [wrap_mcode ida] ii binding
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
             then idb, tag_symbols [wrap_mcode ida] ii binding
             else raise NoMatch
         | _ -> raise Impossible
         )


     | Pattern.Function -> raise NoMatch
     | Pattern.DontKnow -> 
         failwith "MetaFunc and MetaLocalFunc, need more semantic info about id"
     )

 | A.OptIdent _ | A.UniqueIdent _ | A.MultiIdent _ -> 
     failwith "not handling Opt/Unique/Multi for ident"
        


(* -------------------------------------------------------------------------- *)
and transform_option f t1 t2 =
  match (t1,t2) with
  | (Some t1, Some t2) -> Some (f t1 t2)
  | (None, None) -> None
  | _ -> raise NoMatch


(******************************************************************************)

and (tag_symbols: 
      (string Ast_cocci.mcode) list -> 
      Ast_c.il -> 
      Ast_c.metavars_binding -> 
      Ast_c.il) = 
 fun xs ys binding ->
  assert (List.length xs = List.length ys);
  zip xs ys +> List.map (fun ((s1,_,x),   (s2, (oldmcode, oldenv))) -> 
    (* assert s1 = s2 ? no more cos now have some "fake" string *)
    (s2, (x, binding)))

and tag_one_symbol = fun ia ib  binding -> 
  let (s1,_,x) = ia in
  let (s2, (oldmcode, oldenv)) = ib in
  (s2, (x, binding))



(******************************************************************************)
(* Entry points *)

let rec (transform: 
    Lib_engine.transformation_info -> 
    (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
    (Control_flow_c.node, Control_flow_c.edge) ograph_extended) = 
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
      | F.Enter | F.Exit    -> ()
      | F.Fake              -> ()
      | F.CaseNode _        -> ()
      | F.TrueNode | F.FalseNode 
      | F.AfterNode | F.FallThroughNode | F.ErrorExit -> ()
      | _ -> assert (not (node =*= node'));
      );

      acc#replace_node (nodei, node')
     ) cflow

