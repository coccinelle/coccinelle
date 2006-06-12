open Common open Commonop

open Ograph_extended

module A = Ast_cocci
module B = Ast_c

module F = Control_flow_c

type transformation_info = (nodei * Ast_c.metavars_binding * Ast_cocci.rule_elem) list

(******************************************************************************)
(*
TODO
 adapt what rene returns,  flatten, simplify env (no parenvar), simplify pred (just take the rule_elem)


 must do some try, for instance when f(...,X,Y,...) have to test the transfo for all
  the combinaitions (and if multiple transfo possible ? pb ? 
 => the type is to return a   expression option ? use some combinators to help ?

 for some nodes I dont have all the info, for instance for } 
  I need to modify the node of the start, it is where the info is.
  Same for Else
 
 if add NestedExp to Mini_cocci ? have to transform when inside.

 
 checks?: if touch same nodes two times ? check compatibility of bindings, etc

*)
(******************************************************************************)

type ('a, 'b) transformer = 'a -> 'b -> Ast_c.metavars_binding -> 'b



type sequence_processing_style = Ordered | Unordered

(******************************************************************************)

let rec (transform_re_node: (Ast_cocci.rule_elem, Control_flow_c.node) transformer) = fun re (node, s) -> 
  fun binding -> 

  match re, node with

  | A.SeqStart _mcode, F.StartBrace (_level, _statement) -> raise Todo
  | A.SeqEnd _mcode, F.EndBrace _level -> raise Todo  (* problematic *)

  | _, F.StartBrace _ 
  | _, F.EndBrace _ 
  | _, F.Enter 
  | _, F.Exit 
  | _, F.Fake -> raise Impossible (* rene cant have found that a state containing a fake/exit/... should be transformed *)

  | re, F.Statement st -> Control_flow_c.Statement (transform_re_st  re st  binding), "ici2"

  | _ -> raise Todo




and (transform_re_st: (Ast_cocci.rule_elem, Ast_c.statement) transformer)  = fun re st -> 
  fun binding -> 

  match re, st with

  | A.ExprStatement (ea, i1), (B.ExprStatement (Some eb), ii) -> 
      let ii' = tagge_symbols [i1] ii  binding in
      B.ExprStatement (Some (transform_e_e ea eb  binding)), ii'

  | A.IfHeader (i1,i2, ea, i3), (B.Selection (B.If (eb, st1b, st2b)), ii) -> 
      let ii' = tagge_symbols [i1;i2;i3] ii binding in
      B.Selection (B.If (transform_e_e ea eb  binding, st1b, st2b)), ii'


  (* Else ???  Dots Nest *)

  (* not me?: Disj *)


  | _ -> raise Todo (* except if have NestedExp *)


and (transform_e_e: (Ast_cocci.expression, Ast_c.expression) transformer) = fun ep ec -> 
  fun binding -> 
  
  match ep, ec with
  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr ((ida,i1),_typeopt),  expb -> 
     (* get binding, assert =*=,  distribute info in i1 *)
      let v = binding +> List.assoc (ida: string) in
      (match v with
      | B.MetaExpr expa -> 
          assert (expa =*= Ast_c.al_expr expb);
          distribute_minus_plus_e i1 expb   binding
      | _ -> raise Impossible
      )
      
  | A.Ident ida,                ((B.Ident idb) , ii) ->
      let (idb', ii') = transform_ident ida (idb, ii)   binding in
      (B.Ident idb', ii')

  | A.Constant ((A.Int ia),i1) ,                ((B.Constant (B.Int ib) , ii)) ->  
      assert (ia =$= ib);
      let ii' = tagge_symbols [ ia, i1  ] ii binding in
      B.Constant (B.Int ib), ii'
      


  | A.FunCall (ea, i2, eas, i3),  (B.FunCall (eb, ebs), ii) -> 
      let ii' = tagge_symbols [i2;i3] ii  binding in
      let eas' = A.undots eas in
      let seqstyle = (match eas with A.DOTS _ -> Ordered | A.CIRCLES _ -> Unordered | A.STARS _ -> raise Todo)  in
      
      B.FunCall (transform_e_e ea eb binding,  transform_arguments seqstyle eas' ebs   binding), ii'

  | A.EComma _, _   -> raise Impossible (* can have EComma only in arg lists *)
  | A.Edots _, _    -> raise Impossible (* can have EComma only in arg lists *)

  | A.DisjExpr eas, eb -> 
      raise Todo

  | _ -> raise Impossible




and (transform_arguments: sequence_processing_style -> (Ast_cocci.expression list, ((Ast_c.expression, Ast_c.fullType * (Ast_c.storage * Ast_c.il)) Common.either * Ast_c.il) list) transformer)  = fun seqstyle eas ebs ->
  fun binding -> 
    (* TODO and when have dots ? *)
    match eas, ebs with
    | [], [] -> []

    | [ea], [Left eb, ii] -> 
        assert (null ii);
        [Left (transform_e_e ea eb binding),  []]

    | A.EComma i1::ea::eas,  (Left eb, ii)::ebs -> 
        let ii' = tagge_symbols [i1] ii   binding in
        (Left (transform_e_e  ea eb binding), ii')::transform_arguments seqstyle eas ebs   binding
       
    | ea::eas,  (Left eb, ii)::ebs -> 
        assert (null ii);
        (Left (transform_e_e  ea eb binding), [])::transform_arguments seqstyle eas ebs   binding
    | _ -> raise Impossible

and (transform_ident: (Ast_cocci.ident, (string * Ast_c.il)) transformer) = fun ida (idb, ii) -> 
  fun binding -> 
    match ida, idb with
    | A.Id (sa,i1), sb when sa =$= sb -> 
        let ii' = tagge_symbols [sa, i1] ii binding in
        idb, ii'
    | _ -> raise Todo
  (* get binding, assert =*=,  tagge *)

and (tagge_symbols: (string Ast_cocci.mcode) list -> Ast_c.il -> Ast_c.metavars_binding -> Ast_c.il) = 
  fun xs ys binding ->
  assert (List.length xs = List.length ys);
  zip xs ys +> List.map (fun ((s1,x),   (s2, (oldmcode, oldenv))) -> 
    (* assert s1 = s2 ? *)
    (s2, (x, binding)))


and distribute_minus_plus_e mcode  expr binding = 
  match mcode with
  | Ast_cocci.MINUS (i,any_xxs) -> 
      distribute_minus_plus_e_apply_op 
        (minusize_token, add_left (!any_xxs, binding), nothing_right)
        expr
  | Ast_cocci.CONTEXT (i, any_befaft) -> 
        (match !any_befaft with
        | Ast_cocci.NOTHING -> expr

        | Ast_cocci.BEFORE xxs -> 
            distribute_minus_plus_e_apply_op
              (no_minusize, add_left (xxs, binding), nothing_right)
              expr
        | Ast_cocci.AFTER xxs ->  
            distribute_minus_plus_e_apply_op
              (no_minusize, nothing_left, add_right (xxs, binding))
              expr
        | Ast_cocci.BEFOREAFTER (xxs, yys) -> 
            distribute_minus_plus_e_apply_op
              (no_minusize, add_left (xxs, binding) , add_right (xxs, binding))
              expr
        )
  | Ast_cocci.PLUS _ -> raise Impossible

(* op = minusize operator, lop = stuff to do on the left, rop = stuff to do on the right *)
and distribute_minus_plus_e_apply_op (op, lop, rop) expr = 
  let rec aux (op, lop, rop) expr = match expr with
  | Ast_c.Constant (Ast_c.Int i),  [i1] -> 
      Ast_c.Constant (Ast_c.Int i),  [i1 +> op +> lop +> rop]
  | Ast_c.Ident s,  [i1] -> 
      Ast_c.Ident s,  [i1 +> op +> lop +> rop] 
(* TODO distribute to the expression on the left *)
  | Ast_c.FunCall (e, xs), [i2;i3] -> 
      let xs' = xs +> List.map (function 
        | (Left e, ii) -> 
            Left (aux (op, nothing_left, nothing_right) e),
            (ii +> List.map op)
        | (Right e, ii) -> raise Todo
        ) in
       let e' =aux (op, lop, nothing_right) e in
        Ast_c.FunCall (e', xs'), [i2 +> op; i3 +> op +> rop]
  | x -> error_cant_have x

  in aux (op, lop, rop) expr


  
and (minusize_token: Ast_c.info -> Ast_c.info) = fun (s, (mcode,env))  -> 
  let mcode' =
    match mcode with
    | Ast_cocci.CONTEXT (i, {contents = Ast_cocci.NOTHING}) -> 
        Ast_cocci.MINUS (i, {contents = []})
    | _ -> failwith "already done the minusize"
  in
  (s, (mcode', env))



and add_left (xxs, binding) = fun (s, (mcode,env))  -> 
  let mcode' = 
    match mcode with
    | Ast_cocci.MINUS (i, { contents = [] }) -> 
        Ast_cocci.MINUS (i, { contents = xxs})
    | Ast_cocci.MINUS (i, { contents = _ }) -> failwith "have already added stuff on this token"

    | Ast_cocci.CONTEXT (i, {contents = Ast_cocci.NOTHING}) -> 
        Ast_cocci.CONTEXT (i, {contents = (Ast_cocci.BEFORE xxs)})
    | Ast_cocci.CONTEXT (i, {contents = Ast_cocci.AFTER yys}) -> 
        Ast_cocci.CONTEXT (i, {contents = (Ast_cocci.BEFOREAFTER (xxs, yys))})
    | _ -> raise Impossible

  in
  s, (mcode', binding)


and add_right (yys, binding) = fun (s,(mcode,env))  -> 
  let mcode' = 
    match mcode with
    | Ast_cocci.MINUS (i, { contents = [] }) -> 
        Ast_cocci.MINUS (i, { contents = yys})
    | Ast_cocci.MINUS (i,{ contents = _ }) -> failwith "have already added stuff on this token"


    | Ast_cocci.CONTEXT (i,{contents = Ast_cocci.NOTHING}) -> 
        Ast_cocci.CONTEXT (i,{contents = (Ast_cocci.AFTER yys)})
    | Ast_cocci.CONTEXT (i,{contents = Ast_cocci.BEFORE xxs}) -> 
        Ast_cocci.CONTEXT (i,{contents = (Ast_cocci.BEFOREAFTER (xxs, yys))})
    | _ -> raise Impossible
  in
  s, (mcode', binding)


and no_minusize x = x
and nothing_right x = x
and nothing_left  x = x






let rec (transform: 
       transformation_info ->
       (Control_flow_c.node, Control_flow_c.edge) ograph_extended -> 
       (Control_flow_c.node, Control_flow_c.edge) ograph_extended
    ) = fun xs cflow -> 

      (* find the node, transform, update the node,    and iter for all elements *)
      xs +> List.fold_left (fun acc (nodei, binding, rule_elem) -> 
        let node  = cflow#nodes#assoc nodei in
        let node' = transform_re_node rule_elem node binding in
        acc#replace_node (nodei, node')
        ) cflow

