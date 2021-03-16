(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common

module Flag_engine = Flag_matcher
(*****************************************************************************)
(* The functor argument  *)
(*****************************************************************************)

(* info passed recursively in monad in addition to binding *)
type xinfo = {
  optional_storage_iso : bool;
  optional_qualifier_iso : bool;
  value_format_iso : bool;
  optional_declarer_semicolon_iso : bool;
  optional_attributes_iso : bool;
}

module XMATCH = struct

  (* ------------------------------------------------------------------------*)
  (* Combinators history *)
  (* ------------------------------------------------------------------------*)
  (*
   * version0:
   *   type ('a, 'b) matcher = 'a -> 'b -> bool
   *
   * version1: same but with a global variable holding the current binding
   *  BUT bug
   *   - can have multiple possibilities
   *   - globals sux
   *   - sometimes have to undo, cos if start match, then it binds,
   *     and if later it does not match, then must undo the first binds.
   *     ex: when match parameters, can  try to match, but then we found far
   *     later that the last argument of a function does not match
   *      => have to uando the binding !!!
   *      (can handle that too with a global, by saving the
   *      global, ... but sux)
   *   => better not use global
   *
   * version2:
   *    type ('a, 'b) matcher = binding -> 'a -> 'b -> binding list
   *
   * Empty list mean failure (let matchfailure = []).
   * To be able to have pretty code, have to use partial application
   * powa, and so the type is in fact
   *
   * version3:
   *    type ('a, 'b) matcher =  'a -> 'b -> binding -> binding list
   *
   * Then by defining the correct combinators, can have quite pretty code (that
   * looks like the clean code of version0).
   *
   * opti: return a lazy list of possible matches ?
   *
   * version4: type tin = Lib_engine.metavars_binding
   *)

  (* ------------------------------------------------------------------------*)
  (* Standard type and operators  *)
  (* ------------------------------------------------------------------------*)

  type tin = {
    extra: xinfo;
    binding: Lib_engine.metavars_binding;
    binding0: Lib_engine.metavars_binding; (* inherited bindings *)
  }
  (* 'x is a ('a * 'b) but in fact don't care about 'b, we just tag the SP *)
  (* opti? use set instead of list *)
  type 'x tout = ('x * Lib_engine.metavars_binding) list

  type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout

  let constraint_checker = ref (fun _ -> failwith "unbound constraint_checker")

  (* was >&&> *)
  let (>>=) m1 m2 = fun tin ->
    let xs = m1 tin in
    let xxs = xs +> List.map (fun ((a,b), binding) ->
      m2 a b {tin with binding = binding}
    ) in
    List.flatten xxs

  (* Je compare les bindings retournés par les differentes branches.
   * Si la deuxieme branche amene a des bindings qui sont deja presents
   * dans la premiere branche, alors je ne les accepte pas.
   *
   * update: still useful now that julia better handle Exp directly via
   * ctl tricks using positions ?
   *)
  let (>|+|>) m1 m2 = fun tin ->
(* CHOICE
      let xs = m1 tin in
      if null xs
      then m2 tin
      else xs
*)
    let res1 = m1 tin in
    let res2 = m2 tin in
    if res2 = [] (*try to avoid a trivial @*)
    then res1
    else
      res1 @
	(res2 +> List.filter (fun (x, binding) ->
          not
            (res1 +> List.exists (fun (_,already) ->
              Lib_engine.equal_binding binding already))
      ))




  let (>||>) m1 m2 = fun tin ->
(* CHOICE
      let xs = m1 tin in
      if null xs
      then m2 tin
      else xs
*)
    (* opti? use set instead of list *)
    let l1 = m1 tin in
    let l2 = m2 tin in l1 @ l2


  let return res = fun tin ->
    [res, tin.binding]

  let fail = fun tin ->
    []

  let (>&&>) f m = fun tin ->
    if f tin
    then m tin
    else fail tin

  let mnot f res = fun tin ->
    match f tin with
      [] -> return res tin
    | _ -> fail tin


  let mode = Cocci_vs_c.PatternMode

  (* ------------------------------------------------------------------------*)
  (* Exp  *)
  (* ------------------------------------------------------------------------*)
  let cocciExp = fun expf expa node -> fun tin ->

    let globals = ref [] in
    let bigf = {
      (* julia's style *)
      Visitor_c.default_visitor_c with
      Visitor_c.kexpr = (fun (k, bigf) expb ->
	match expf expa expb tin with
	| [] -> (* failed *) k expb
	| xs ->
            globals := xs @ !globals;
            if not !Flag_engine.disallow_nested_exps then k expb (* CHOICE *)
      );
      (* pad's style.
       * push2 expr globals;  k expr
       *  ...
       *  !globals +> List.fold_left (fun acc e -> acc >||> match_e_e expr e)
       * (return false)
       *
       *)
    }
    in
    Visitor_c.vk_node bigf node;
    !globals +> List.map (fun ((a, _exp), binding) ->
      (a, node), binding
    )

  (* same as cocciExp, but for expressions in an expression, not expressions
     in a node *)
  let cocciExpExp = fun _ expf expa expb -> fun tin ->

    let globals = ref [] in
    let bigf = {
      (* julia's style *)
      Visitor_c.default_visitor_c with
      Visitor_c.kexpr = (fun (k, bigf) expb ->
	match expf expa expb tin with
	| [] -> (* failed *) k expb
	| xs ->
            globals := xs @ !globals;
            if not !Flag_engine.disallow_nested_exps then k expb (* CHOICE *)
      );
      (* pad's style.
       * push2 expr globals;  k expr
       *  ...
       *  !globals +> List.fold_left (fun acc e -> acc >||> match_e_e expr e)
       * (return false)
       *
       *)
    }
    in
    Visitor_c.vk_expr bigf expb;
    !globals +> List.map (fun ((a, _exp), binding) ->
      (a, expb), binding
    )

  let cocciTy = fun expf expa node -> fun tin ->

    let globals = ref [] in
    let bigf = {
      Visitor_c.default_visitor_c with
        Visitor_c.ktype = (fun (k, bigf) expb ->
	  match expf expa expb tin with
	  | [] -> (* failed *) k expb
	  | xs -> globals := xs @ !globals);
	Visitor_c.kdecl = (fun (k, bigf) expb ->
	  let iif ii = List.iter (Visitor_c.vk_info bigf) ii in
	  match expb with
	    Ast_c.DeclList(xs,iis) ->
	      iif iis;
	      (match xs with
		(x,ii)::xs ->
		  iif ii;
		  Visitor_c.vk_onedecl bigf x;
		  List.iter
		    (fun (x,ii) ->
		      iif ii;
		      Visitor_c.vk_onedecl_opt false bigf x)
		    xs
	      | _ -> failwith "no decls")
	  | _ -> k expb)

    }
    in
    Visitor_c.vk_node bigf node;
    !globals +> List.map (fun ((a, _exp), binding) ->
      (a, node), binding
    )

  let cocciId = fun expf expa node -> fun tin ->
    (* This is not correct.  It should not match type names, ie name
       defined by a typedef, and it should match struct and enum names,
       which are currently not names.  TODO *)
    let globals = ref [] in
    let bigf = {
      Visitor_c.default_visitor_c with
        Visitor_c.kname = (fun (k, bigf) expb ->
	  match expf expa expb tin with
	  | [] -> (* failed *) k expb
	  | xs -> globals := xs @ !globals)
    }
    in
    Visitor_c.vk_node bigf node;
    !globals +> List.map (fun ((a, _exp), binding) ->
      (a, node), binding
    )

  let cocciInit = fun expf expa node -> fun tin ->

    let globals = ref [] in
    let bigf = {
      Visitor_c.default_visitor_c with
        Visitor_c.kini = (fun (k, bigf) expb ->
	match expf expa expb tin with
	| [] -> (* failed *) k expb
	| xs -> globals := xs @ !globals);

    }
    in
    Visitor_c.vk_node bigf node;
    !globals +> List.map (fun ((a, _exp), binding) ->
      (a, node), binding
    )


  (* ------------------------------------------------------------------------*)
  (* Distribute mcode *)
  (* ------------------------------------------------------------------------*)
  let tag_mck_pos mck posmck =
    match mck with
    | Ast_cocci.PLUS c -> Ast_cocci.PLUS c
    | Ast_cocci.CONTEXT (pos, xs) ->
        assert (pos = Ast_cocci.NoPos || pos = Ast_cocci.DontCarePos);
        Ast_cocci.CONTEXT (posmck, xs)
    | Ast_cocci.MINUS (pos, inst, adj, xs) ->
        assert (pos = Ast_cocci.NoPos || pos = Ast_cocci.DontCarePos);
        Ast_cocci.MINUS (posmck, inst, adj, xs)


  let tag_mck_pos_mcode (x,info,mck,pos) posmck stuff = fun tin ->
    [((x, info, tag_mck_pos mck posmck, pos),stuff), tin.binding]

  let is_abstract ii =
    match Ast_c.pinfo_of_info ii with
      Ast_c.AbstractLineTok pi -> true
    | _ -> false

  let is_fake ii = (* likely an invisible comma *)
    match Ast_c.pinfo_of_info ii with
      Ast_c.FakeTok _ -> true
    | _ -> false

  let distrf (ii_of_x_f) =
    fun mcode x -> fun tin ->
      let iis = ii_of_x_f x in
      let all_abstract =
	(* this occurs when matching a metavar type against a type *)
	List.for_all is_abstract iis in
      if all_abstract
      then tag_mck_pos_mcode mcode Ast_cocci.NoPos x tin (* do nothing *)
      else
	let (max, min) = Lib_parsing_c.max_min_by_pos iis in
	let posmck =
	  Ast_cocci.FixPos (min, max) (* subtil: and not max, min !!*)
	in
	tag_mck_pos_mcode mcode posmck x tin

  let distrf_e              = distrf Lib_parsing_c.ii_of_expr
  let distrf_assignOp       = distrf Lib_parsing_c.ii_of_assignOp
  let distrf_binaryOp       = distrf Lib_parsing_c.ii_of_binaryOp
  let distrf_args           = distrf Lib_parsing_c.ii_of_args
  let distrf_type           = distrf Lib_parsing_c.ii_of_type
  let distrf_param          = distrf Lib_parsing_c.ii_of_param
  let distrf_params         = distrf Lib_parsing_c.ii_of_params
  let distrf_ini            = distrf Lib_parsing_c.ii_of_ini
  let distrf_inis           = distrf Lib_parsing_c.ii_of_inis
  let distrf_decl           = distrf Lib_parsing_c.ii_of_decl
  let distrf_field          = distrf Lib_parsing_c.ii_of_field
  let distrf_node           = distrf Lib_parsing_c.ii_of_node
  let distrf_fragments      = distrf Lib_parsing_c.ii_of_fragments
  let distrf_format         = distrf Lib_parsing_c.ii_of_format
  let distrf_enum_fields    = distrf Lib_parsing_c.ii_of_enum_fields
  let distrf_struct_fields  = distrf Lib_parsing_c.ii_of_struct_fields
  let distrf_cst            = distrf Lib_parsing_c.ii_of_cst
  let distrf_define_params  = distrf Lib_parsing_c.ii_of_define_params
  let distrf_ident_list     = distrf Lib_parsing_c.ii_of_ident_list
  let distrf_exec_code_list = distrf Lib_parsing_c.ii_of_exec_code_list
  let distrf_attr           = distrf Lib_parsing_c.ii_of_attr
  let distrf_attr_arg       = distrf Lib_parsing_c.ii_of_attr_arg
  let distrf_attrs          = distrf Lib_parsing_c.ii_of_attrs


  (* ------------------------------------------------------------------------*)
  (* Constraints on position metavariables *)
  (* ------------------------------------------------------------------------*)

 let check_constraints_ne matcher constraints exp = fun f tin ->
    let rec loop = function
        [] -> f () tin (* success *)
      | c::cs ->
          match matcher c exp tin with
            [] (* failure *) -> loop cs
          | _ (* success *) -> fail tin in
    loop constraints

  let check_constraints ida idb constraints f =
    (fun tin ->
      !constraint_checker ida idb
	(function id -> tin.binding0 +> List.assoc id)
	constraints tin)
      >>= (fun _ _ -> f ())

  let check_re_constraints pname constraints f =
    (fun tin ->
      let myrule = fst pname in
      try
	let pvalu = List.assoc pname tin.binding in
	!constraint_checker pname pvalu
	  (fun ((rl,_) as name) ->
	    let env = if rl = myrule then tin.binding else tin.binding0 in
	    List.assoc name env)
	  constraints tin
      with Not_found -> return ((),()) tin)
      >>= (fun _ _ -> f ())

  let check_pos_constraints pname pvalu constraints f =
    (fun tin ->
      !constraint_checker pname pvalu
	(fun name -> List.assoc name tin.binding0)
	constraints tin)
      >>= (fun _ _ -> f)

  (* ------------------------------------------------------------------------*)
  (* Environment *)
  (* ------------------------------------------------------------------------*)
  (* pre: if have declared a new metavar that hide another one, then
   * must be passed with a binding that deleted this metavar
   *
   * Here we don't use the keep argument of julia. cf f(X,X), J'ai
   * besoin de garder le X en interne, meme si julia s'en fout elle du
   * X et qu'elle a mis X a DontSaved.
   *)
  let check_add_metavars_binding strip _keep inherited = fun (k, valu) tin ->
    if inherited
    then
      match Common.optionise (fun () -> tin.binding0 +> List.assoc k) with
      | Some (valu') ->
          if Cocci_vs_c.equal_inh_metavarval valu valu'
          then Some tin.binding
          else None
      |	None -> None
    else
      match Common.optionise (fun () -> tin.binding +> List.assoc k) with
      | Some (valu') ->
          if Cocci_vs_c.equal_metavarval valu valu'
          then Some tin.binding
          else None

      | None ->
	  let success valu' =
	    Some (tin.binding +> Common.insert_assoc (k, valu')) in
          (match valu with
            Ast_c.MetaIdVal (a)    ->
	      success(Ast_c.MetaIdVal(a))
          | Ast_c.MetaAssignOpVal op      ->
	      success(Ast_c.MetaAssignOpVal op)
          | Ast_c.MetaBinaryOpVal op      ->
	      success(Ast_c.MetaBinaryOpVal op)
          | Ast_c.MetaFuncVal a      ->
	      success(Ast_c.MetaFuncVal a)
          | Ast_c.MetaLocalFuncVal a ->
	      success(Ast_c.MetaLocalFuncVal a) (*more?*)
          | Ast_c.MetaExprVal (a,c,ty) ->
	      (* c in the value is only to prepare for the future in which
		 we figure out how to have subterm constraints on unbound
		 variables.  Now an environment will only contain expression
		 values with empty constraints, as all constraints are
		 resolved at binding time *)
	      let stripped =
		if strip
		then Lib_parsing_c.al_expr a
		else Lib_parsing_c.semi_al_expr a in
	      let inh_stripped = Lib_parsing_c.al_inh_expr a in
	      let rec loop = function
		  [] -> success(Ast_c.MetaExprVal(stripped,[],ty))
		| c::cs ->
		    let tmp =
		      Common.optionise
			(fun () -> tin.binding0 +> List.assoc c) in
		    (match tmp with
		      Some (Ast_c.MetaExprVal(v,_,_)) ->
			if C_vs_c.subexpression_of_expression inh_stripped v
			then loop cs (* forget satisfied constraints *)
			else None (* failure *)
		    | Some _ -> failwith "check add metavars: not possible"
		      (* fail if this should be a subterm of something that
			 doesn't exist *)
		    | None -> None) in
	      loop c
          | Ast_c.MetaExprListVal a ->
	      success
		(Ast_c.MetaExprListVal
		   (if strip
		   then Lib_parsing_c.al_arguments a
		   else Lib_parsing_c.semi_al_arguments a))

          | Ast_c.MetaDeclVal(a,original) ->
	      let stripped =
		if strip
		then Lib_parsing_c.al_declaration a
		else Lib_parsing_c.semi_al_declaration a in
	      success(Ast_c.MetaDeclVal(stripped,original))
          | Ast_c.MetaFieldVal a ->
	      success
		(Ast_c.MetaFieldVal
		   (if strip
		   then Lib_parsing_c.al_field a
		   else Lib_parsing_c.semi_al_field a))
          | Ast_c.MetaFieldListVal a ->
	      success
		(Ast_c.MetaFieldListVal
		   (if strip
		   then Lib_parsing_c.al_fields a
		   else Lib_parsing_c.semi_al_fields a))
          | Ast_c.MetaStmtVal(a,original,ty) ->
	      let stripped =
		if strip
		then Lib_parsing_c.al_statement a
		else Lib_parsing_c.semi_al_statement a in
	      success(Ast_c.MetaStmtVal(stripped,original,ty))
          | Ast_c.MetaStmtListVal(a,ty) ->
	      let stripped =
		if strip
		then Lib_parsing_c.al_statement_seq_list a
		else Lib_parsing_c.semi_al_statement_seq_list a in
	      success(Ast_c.MetaStmtListVal(stripped,ty))
          | Ast_c.MetaTypeVal a ->
	      success
		(Ast_c.MetaTypeVal
		   (if strip
		   then Lib_parsing_c.al_type a
		   else Lib_parsing_c.semi_al_type a))

          | Ast_c.MetaInitVal a ->
	      success
		(Ast_c.MetaInitVal
		   (if strip
		   then Lib_parsing_c.al_init a
		   else Lib_parsing_c.semi_al_init a))

          | Ast_c.MetaInitListVal a ->
	      success
		(Ast_c.MetaInitListVal
		   (if strip
		   then Lib_parsing_c.al_inits a
		   else Lib_parsing_c.semi_al_inits a))

          | Ast_c.MetaListlenVal a -> success(Ast_c.MetaListlenVal a)

          | Ast_c.MetaParamVal a ->
	      success
		(Ast_c.MetaParamVal
		   (if strip
		   then Lib_parsing_c.al_param a
		   else Lib_parsing_c.semi_al_param a))
          | Ast_c.MetaParamListVal a ->
	      success
		(Ast_c.MetaParamListVal
		   (if strip
		   then Lib_parsing_c.al_params a
		   else Lib_parsing_c.semi_al_params a))

          | Ast_c.MetaDParamListVal a ->
	      success
		(Ast_c.MetaDParamListVal
		   (if strip
		   then Lib_parsing_c.al_define_params a
		   else Lib_parsing_c.semi_al_define_params a))

          | Ast_c.MetaFragListVal a ->
	      success
		(Ast_c.MetaFragListVal
		   (if strip
		   then Lib_parsing_c.al_string_fragments a
		   else Lib_parsing_c.semi_al_string_fragments a))
          | Ast_c.MetaFmtVal a ->
	      success
		(Ast_c.MetaFmtVal
		   (if strip
		   then Lib_parsing_c.al_string_format a
		   else Lib_parsing_c.semi_al_string_format a))
          | Ast_c.MetaAttrArgVal a ->
	      success
		(Ast_c.MetaAttrArgVal
		   (if strip
		   then Lib_parsing_c.al_attr_arg a
		   else Lib_parsing_c.semi_al_attr_arg a))

          | Ast_c.MetaPosVal (pos1,pos2) ->
	      success(Ast_c.MetaPosVal (pos1,pos2))
          | Ast_c.MetaPosValList l -> success (Ast_c.MetaPosValList l)
          | Ast_c.MetaComValList l -> success (Ast_c.MetaComValList l)
	  | Ast_c.MetaNoVal -> None)

  let pos_variables tin ia get_pvalu finish =
    match Ast_cocci.get_pos_var ia with
      [] -> finish tin
    | positions ->
	match get_pvalu() with
	  [] -> finish tin
	| infos ->
	    let pvalu =
	      lazy
		(let (fname,current_element,st,ed) =
		  Lib_parsing_c.lin_col_by_pos infos in
		[(fname,current_element,
		  Some(Lazy.force (!Flag.current_element_pos)),st,ed)]) in
	    let cvalu =
	      lazy
		(let infos =
		  List.filter (function ii -> not (Ast_c.is_fake ii)) infos in
		let rec uniq = function
		    x::y::xs ->
		      if Ast_c.compare_pos x y = 0
		      then uniq (x::xs)
		      else x :: uniq (y :: xs)
		  | l -> l in
		let infos = uniq(List.sort Ast_c.compare_pos infos) in
		match (infos,List.rev infos) with
		  ([],_) | (_,[]) -> [([],[],[])]
		| (fst::mid,last::_) ->
		    let get_real_comments l =
		      List.filter
			(function
			    (Token_c.TComment,_)
			  | (Token_c.TCommentCpp _,_) -> true
			  | _ -> false)
			l in
		    let before =
		      get_real_comments(Ast_c.get_comments_before fst) in
		    let after =
		      get_real_comments(Ast_c.get_comments_after last) in
		    let mid =
		      get_real_comments
			(List.concat
			   (List.map Ast_c.get_comments_before mid)) in
		    [(before,mid,after)]) in
	    let rec loop tin = function
		[] -> finish tin
	      | Ast_cocci.MetaPos(name,constraints,per,keep,inherited)::rest ->
		  let pvalu = Ast_c.MetaPosValList (Lazy.force pvalu) in
		  let name' = Ast_cocci.unwrap_mcode name in
		  check_pos_constraints name' pvalu constraints
		    (fun new_tin ->
		      (* constraints are satisfied, now see if we are
			 compatible with existing bindings *)
		      let new_binding =
			check_add_metavars_binding false keep inherited
			  (name', pvalu) tin in
		      (match new_binding with
			Some binding ->
			  loop {new_tin with binding = binding} rest
		      | None -> fail tin))
		    tin
	      | Ast_cocci.MetaCom(name,constraints,keep,inherited)::rest ->
		  let cvalu = Ast_c.MetaComValList (Lazy.force cvalu) in
		  let name' = Ast_cocci.unwrap_mcode name in
		  check_pos_constraints name' cvalu constraints
		    (fun new_tin ->
		      let new_binding =
			check_add_metavars_binding false keep inherited
			  (name', cvalu) tin in
		      (match new_binding with
			Some binding ->
			  loop {new_tin with binding = binding} rest
		      | None -> fail tin))
		    tin in
	    loop tin positions

  let envf keep inherited = fun (k, valu, get_max_min) f tin ->
    let x = Ast_cocci.unwrap_mcode k in
    match check_add_metavars_binding true keep inherited (x, valu) tin with
    | Some binding ->
	let new_tin = {tin with binding = binding} in
	pos_variables new_tin k get_max_min (f ())
    | None -> fail tin

  (* ------------------------------------------------------------------------*)
  (* Environment, allbounds *)
  (* ------------------------------------------------------------------------*)
  (* all referenced inherited variables have to be bound. This would
   * be naturally checked for the minus or context ones in the
   * matching process, but have to check the plus ones as well. The
   * result of get_inherited contains all of these, but the potential
   * redundant checking for the minus and context ones is probably not
   * a big deal. If it's a problem, could fix free_vars to distinguish
   * between + variables and the other ones. *)

  let (all_bound : Ast_cocci.meta_name list -> tin -> bool) = fun l tin ->
    l +> List.for_all (fun inhvar ->
      match Common.optionise (fun () -> tin.binding0 +> List.assoc inhvar) with
      | Some _ -> true
      | None -> false
    )

  let optional_storage_flag f = fun tin ->
    f (tin.extra.optional_storage_iso) tin

  let optional_qualifier_flag f = fun tin ->
    f (tin.extra.optional_qualifier_iso) tin

  let value_format_flag f = fun tin ->
    f (tin.extra.value_format_iso) tin

  let optional_declarer_semicolon_flag f = fun tin ->
    f (tin.extra.optional_declarer_semicolon_iso) tin

  let optional_attributes_flag f = fun tin ->
    f (tin.extra.optional_attributes_iso) tin

  (* ------------------------------------------------------------------------*)
  (* Tokens *)
  (* ------------------------------------------------------------------------*)
  let tokenf ia ib = fun tin ->
    if is_abstract ib
    then
      (* for meta var type against type case *)
      let posmck = Ast_cocci.NoPos in
      tag_mck_pos_mcode ia posmck ib tin
    else
      let pos = Ast_c.info_to_fixpos ib in
      let posmck = Ast_cocci.FixPos (pos, pos) in
      let finish tin = tag_mck_pos_mcode ia posmck ib tin in
      pos_variables tin ia
	(function _ ->
	  if is_fake ib
	  then []
	  else [ib])
	finish

  let tokenf_mck mck ib = fun tin ->
    let pos = Ast_c.info_to_fixpos ib in
    let posmck = Ast_cocci.FixPos (pos, pos) in
    [(tag_mck_pos mck posmck, ib), tin.binding]

end

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)
module MATCH  = Cocci_vs_c.COCCI_VS_C (XMATCH)


let match_re_node2 dropped_isos a b binding0 =

  let tin = {
    XMATCH.extra = {
      optional_storage_iso   = not(List.mem "optional_storage"   dropped_isos);
      optional_qualifier_iso = not(List.mem "optional_qualifier" dropped_isos);
      value_format_iso       = not(List.mem "value_format"       dropped_isos);
      optional_declarer_semicolon_iso =
        not(List.mem "optional_declarer_semicolon"   dropped_isos);
      optional_attributes_iso =
        not(List.mem "optional_attributes" dropped_isos);
    };
    XMATCH.binding = [];
    XMATCH.binding0 = binding0;
  } in

  MATCH.rule_elem_node a b tin
  (* take only the tagged-SP, the 'a' *)
  +> List.map (fun ((a,_b), binding) -> a, binding)


let match_re_node a b c d =
  (*Common.profile_code "Pattern3.match_re_node"
    (fun () -> *) match_re_node2 a b c d
   (* ) *)
