open Common

let string_binding vl = function
    None -> []
  | Some _ ->
      [match vl with
	Ast_c.MetaPosValList l ->
	       let locs =
		 List.map
		   (function
		       (fname,current_element,(line,col),(line_end,col_end)) ->
			 { Coccilib.current_element = current_element;
			   Coccilib.file = fname;
			   Coccilib.line = line;
			   Coccilib.col = col;
			   Coccilib.line_end = line_end;
			   Coccilib.col_end = col_end }) l in
	       Coccilib.Pos locs
      |	Ast_c.MetaListlenVal n -> Coccilib.Int n
      |	_ -> Coccilib.Str (Ocamlcocci_aux.stringrep vl)]

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

      | Ast_c.MetaTypeVal ty -> Coccilib.Type ty
      | Ast_c.MetaInitVal init -> Coccilib.Init init
      | Ast_c.MetaInitListVal init -> Coccilib.InitList init
      | Ast_c.MetaDeclVal decl -> Coccilib.Decl decl
      | Ast_c.MetaFieldVal field -> Coccilib.Field field
      | Ast_c.MetaFieldListVal field -> Coccilib.FieldList field
      | Ast_c.MetaStmtVal(stm,_) -> Coccilib.Stmt stm
      | Ast_c.MetaFragListVal frags -> Coccilib.FragList frags
      | Ast_c.MetaFmtVal fmt -> Coccilib.Fmt fmt
      | Ast_c.MetaNoVal -> failwith "no value for script metavariable"

      | Ast_c.MetaPosVal _ | Ast_c.MetaPosValList _ | Ast_c.MetaListlenVal _ ->
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
	 (function ((str_name,ast_name),(r,m),_,_) ->
	   match find_binding (r,m) with
	     None -> []
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
