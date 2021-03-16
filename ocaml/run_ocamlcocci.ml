open Common

let string_value = function
    Ast_c.MetaPosValList l ->
      let locs =
	List.map
	  (function
	      (fname,current_element,current_element_pos,
	       (line,col),(line_end,col_end)) ->
		 let (current_element_line,current_element_col,
		      current_element_line_end,current_element_col_end) =
		   match current_element_pos with
		     Some
		       ((current_element_line,current_element_col),
			(current_element_line_end,current_element_col_end)) ->
			  (current_element_line,current_element_col,
			   current_element_line_end,current_element_col_end)
		   | None -> (-1,-1,-1,-1) in
		{ Coccilib.current_element = current_element;
		  Coccilib.current_element_line = current_element_line;
		  Coccilib.current_element_col = current_element_col;
		  Coccilib.current_element_line_end = current_element_line_end;
		  Coccilib.current_element_col_end = current_element_col_end;
		  Coccilib.file = fname;
		  Coccilib.line = line;
		  Coccilib.col = col;
		  Coccilib.line_end = line_end;
		  Coccilib.col_end = col_end }) l in
      Coccilib.Pos locs
  | Ast_c.MetaComValList l ->
      Coccilib.Com
	(List.map
	   (function (bef,mid,aft) ->
	     let com_strings l =
	       List.rev
		 (List.fold_left
		    (fun prev cur ->
		      match cur with
			(Token_c.TComment,_) ->
			  (Token_c.str_of_token cur) :: prev
		      | (Token_c.TCommentCpp _,_) ->
			  (Token_c.str_of_token cur) :: prev
		      | _ -> prev)
		    [] l) in
	     (com_strings bef,com_strings mid,com_strings aft))
	   l)
  | Ast_c.MetaListlenVal n -> Coccilib.Int n
  | v -> Coccilib.Str (Ocamlcocci_aux.stringrep v)

let string_binding vl = function
    None -> []
  | Some _ -> [string_value vl]

let ast_binding vl = function
    None -> []
  | Some _ ->
      [match vl with
	Ast_c.MetaIdVal id | Ast_c.MetaFuncVal id
      | Ast_c.MetaLocalFuncVal id ->
	  Coccilib.Str id
      | Ast_c.MetaAssignOpVal op -> Coccilib.AssignOp op
      | Ast_c.MetaBinaryOpVal op -> Coccilib.BinaryOp op
      | Ast_c.MetaExprVal(expr,_,_) -> Coccilib.Expr expr
      | Ast_c.MetaExprListVal arglist -> Coccilib.ExprList arglist
      | Ast_c.MetaParamVal param -> Coccilib.Param param
      | Ast_c.MetaParamListVal paramlist -> Coccilib.ParamList paramlist
      | Ast_c.MetaDParamListVal paramlist -> Coccilib.DParamList paramlist

      | Ast_c.MetaTypeVal ty -> Coccilib.Type ty
      | Ast_c.MetaInitVal init -> Coccilib.Init init
      | Ast_c.MetaInitListVal init -> Coccilib.InitList init
      | Ast_c.MetaDeclVal(decl,_) -> Coccilib.Decl decl
      | Ast_c.MetaFieldVal field -> Coccilib.Field field
      | Ast_c.MetaFieldListVal field -> Coccilib.FieldList field
      | Ast_c.MetaStmtVal(stm,_,_) -> Coccilib.Stmt stm
      | Ast_c.MetaStmtListVal(stm,_) -> Coccilib.StmtList stm
      | Ast_c.MetaFragListVal frags -> Coccilib.FragList frags
      | Ast_c.MetaFmtVal fmt -> Coccilib.Fmt fmt
      | Ast_c.MetaAttrArgVal name -> Coccilib.AttrArg name
      | Ast_c.MetaNoVal -> failwith "no value for script metavariable"
      | Ast_c.MetaComValList l -> Coccilib.AstCom l

      | Ast_c.MetaPosVal _ | Ast_c.MetaPosValList _
      | Ast_c.MetaListlenVal _ ->
	  failwith "not associated with a declared metavariable"]

let run mv ve script_vars name code =
  (* set up variables *)
  let find_binding (r,m) =
    try
      let elem =
	List.find (function ((re,rm),_) -> r = re && m = rm) ve in
      Some elem
    with Not_found -> None in

  let args =
    List.concat
      (List.map
	 (function ((str_name,ast_name),(r,m),_,init) ->
	   match find_binding (r,m) with
	     None ->
	       (match init with
		 Ast_cocci.NoMVInit -> failwith "no value for ocaml metavars"
	       | Ast_cocci.MVInitString s -> [Coccilib.Str s]
	       | Ast_cocci.MVInitPosList -> [Coccilib.Pos []])
	   | Some (_,vl) ->
	       (string_binding vl str_name) @ (ast_binding vl ast_name))
	 mv) in

  let script_args = List.map (function _ -> ref Ast_c.MetaNoVal) script_vars in

  (* call the function *)
  Coccilib.include_match true;
  Coccilib.exited := false;
  let fn =
    try Hashtbl.find Coccilib.fcts name
    with Not_found -> failwith (Printf.sprintf "%s not found" name) in
  fn args script_args;
  List.map (function x -> !x) script_args

let run_constraint ocamlname args =
  let args = List.map string_value args in
  let fn = Hashtbl.find Coccilib.bool_fcts ocamlname in
  fn args

let run_fresh_id ocamlname args =
  let args = List.map string_value args in
  let fn = Hashtbl.find Coccilib.string_fcts ocamlname in
  fn args
