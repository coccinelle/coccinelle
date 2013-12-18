open Common

let nested_start indent index typelabel start len
  print_indent indent;
  Printf.printf
    "<tree type = \"%d\" label = \"\" typeLabel = \"%s\" pos = \"%d\" length = \"%d\">\n"
    index typelabel start len

let nested_start_list indent start len =
  let index = list_index in
  let typelabel = list_label in
  nested_start indent index typelabel start len

let nested_end indent =
  print_indent indent;
  Printf.printf "</tree>\n"

let leaf indent index typelabel start len str =
  print_indent indent;
  Printf.printf
    "<tree type = \"%d\" label = \"%s\" typeLabel = \"%s\" pos = \"%d\" length = \"%d\"/>\n"
    index str typelabel start len

let leaf_token indent index typelabel start len =
  let str = "" in
  leaf indent index typelabel start len str

let leaf_string indent str start =
  let index = string_index in
  let typelabel = string_label in
  let len = String.length str in
  leaf indent index typelabel start len str

(* ------------------------------------------------------------------------- *)

let get_start_len fn thing =
  let (max,min) =
    Lib_parsing_c.max_min_ii_by_pos (fn oe) in
  (Common.opos_of_info min,
   Common.opos_of_info max + String.length (Ast_c.str_of_info max))

(* ------------------------------------------------------------------------- *)

and pp_arg_list indent es ii =
  let (start,len) = get_start_len (fun x -> x) ii in
  nested_start_list indent start len;
  List.iter (pp_argument (indent + 1)) es;
  nested_end indent

and pp_argument indent argument =
  let strcode = get_strcode argument in
  let rec pp_action (ActMisc ii) = ii +> List.iter pr_elem in
  match argument with
  | Left e ->
      let strname = "Left" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_expr e in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | Right weird ->
      let strname = "Right" in
      let strcode = get_strcode weird in
      (match weird with
      | ArgType param -> pp_param param
      | ArgAction action -> pp_action action)

and fullType = typeQualifier * typeC
 and typeC = typeCbis wrap (* todo reput wrap3 *)

let split_signb_baseb_ii (baseb, ii) =
  let iis = ii +> List.map (fun info -> (Ast_c.str_of_info info), info) in
  match baseb, iis with

  | Ast_c.Void, ["void",i1] -> None, [i1]

  | Ast_c.FloatType (Ast_c.CFloat),["float",i1] -> None, [i1]
  | Ast_c.FloatType (Ast_c.CDouble),["double",i1] -> None, [i1]
  | Ast_c.FloatType (Ast_c.CLongDouble),["long",i1;"double",i2] -> None,[i1;i2]

  | Ast_c.IntType (Ast_c.CChar), ["char",i1] -> None, [i1]

  | Ast_c.IntType (Ast_c.Si (sign, base)), xs ->
      let (signed,rest) =
	match (sign,xs) with
	  (_,[]) -> None,[]
	| (Ast_c.Signed,(("signed",i1)::rest)) -> (Some (Ast_c.Signed,i1),rest)
	| (Ast_c.Signed,rest) -> (None,rest)
	| (Ast_c.UnSigned,(("unsigned",i1)::rest)) ->
	    (Some (Ast_c.UnSigned,i1),rest)
	| (Ast_c.UnSigned,rest) -> (* is this case possible? *) (None,rest) in
      (* The original code only allowed explicit signed and unsigned for char,
	 while this code allows char by itself.  Not sure that needs to be
	 checked for here.  If it does, then add a special case. *)
      let base_res =
	match (base,rest) with
	  Ast_c.CInt, ["int",i1] -> [i1]
	| Ast_c.CInt, [] -> []

	| Ast_c.CInt, ["",i1] -> (* no type is specified at all *)
	    (match i1.Ast_c.pinfo with
	      Ast_c.FakeTok(_,_) -> []
	    | _ -> error [i1] ("unrecognized signed int: "^
			      (String.concat " "(List.map fst iis))))

	| Ast_c.CChar2, ["char",i2] -> [i2]

	| Ast_c.CShort, ["short",i1] -> [i1]
	| Ast_c.CShort, ["short",i1;"int",i2] -> [i1;i2]

	| Ast_c.CLong, ["long",i1] -> [i1]
	| Ast_c.CLong, ["long",i1;"int",i2] -> [i1;i2]

	| Ast_c.CLongLong, ["long",i1;"long",i2] -> [i1;i2]
	| Ast_c.CLongLong, ["long",i1;"long",i2;"int",i3] -> [i1;i2;i3]

	| _ ->
	    error (List.map snd iis)
	      ("strange type1, maybe because of weird order: "^
	       (String.concat " " (List.map fst iis))) in
      (signed,base_res)

  | Ast_c.SizeType, ["size_t",i1] -> None, [i1]
  | Ast_c.SSizeType, ["ssize_t",i1] -> None, [i1]
  | Ast_c.PtrDiffType, ["ptrdiff_t",i1] -> None, [i1]

  | _ ->
      error (List.map snd iis)
	("strange type2, maybe because of weird order: "^
	 (String.concat " " (List.map fst iis)))

and pp_typeC ty =
  let strcode = get_wstrcode ty in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_type ty in
  let ii = snd ty in
  match Ast_c.unwrap ty with
    NoType ->
      let strname = "NoType" in
      leaf indent "" strcode strname start len
  | BaseType(ty) ->
      let strname = "BaseType" in
      nested_start indent strcode strname start len;
      pp_basetype (indent + 1) ty start len (split_signb_baseb_ii ty ii);
      nested_end indent
  | Pointer(ty) ->
      let strname = "Pointer" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | Array(Some e,ty) ->
      let strname = "Array" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | Array(None,ty) ->
      let strname = "Array" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | Decimal(e1,Some e2) ->
      let strname = "Decimal" in
      nested_start indent strcode strname start len;
      nested_end indent
  | Decimal(e1,None) ->
      let strname = "Decimal" in
      nested_start indent strcode strname start len;
      nested_end indent
  | FunctionType(funtype) ->
      let strname = "FunctionType" in
      nested_start indent strcode strname start len;
      nested_end indent
  | Enum(Some name,et) ->
      let strname = "Enum" in
      nested_start indent strcode strname start len;
      nested_end indent
  | Enum(None,et) ->
      let strname = "Enum" in
      nested_start indent strcode strname start len;
      nested_end indent
  | StructUnion(su,Some name,ty) ->
      let strname = "StructUnion" in
      nested_start indent strcode strname start len;
      nested_end indent
  | StructUnion(su,None,ty) ->
      let strname = "StructUnion" in
      nested_start indent strcode strname start len;
      nested_end indent
  | EnumName(name) ->
      let strname = "EnumName" in
      leaf indent name strcode strname start len
  | StructUnionName(su,name) ->
      let strname = "StructUnionName" in
  | TypeName(name,_) ->
      let strname = "TypeName" in
      leaf indent name strcode strname start len
  | ParenType(ft) ->
      let strname = "ParenType" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | TypeOfExpr(exp) ->
      let strname = "TypeOfExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp;
      nested_end indent
  | TypeOfType(ft) ->
      let strname = "TypeOfType" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent

(* -------------------------------------- *)

and pp_basetype indent ty start len sign_and_base =
  let strcode = get_strcode ty in
  match ty with
    Void ->
      let strname = "Void" in
      leaf indent "" strcode strname start len
  | IntType(i) ->
      let strname = "IntType" in
      nested_start indent strcode strname start len;
      pp_inttype (indent + 1) i start len sign_and_base;
      nested_end indent
  | FloatType(f) ->
      let strname = "FloatType" in
      nested_start indent strcode strname start len;
      pp_floattype (indent + 1) f start len;
      nested_end indent
  | SizeType ->
      let strname = "SizeType" in
      leaf indent "" strcode strname start len
  | SSizeType ->
      let strname = "SSizeType" in
      leaf indent "" strcode strname start len
  | PtrDiffType ->
      let strname = "PtrDiffType" in
      leaf indent "" strcode strname start len

and pp_inttype indent ty start len (sign_ii,base_ii) =
  let strcode = get_strcode ty in
  match ty with
    CChar ->
      let strname = "CChar" in
      leaf indent "" strcode strname start len
  | Si(sign,base) ->
      let strname = "Si" in
      nested_start indent strcode strname start len;
      pp_sign (indent + 1) sign sign_ii;
      pp_base (indent + 1) base base_ii in
      nested_end indent

and pp_sign indent ty sign_ii =
  let strcode = get_strcode ty in
  let (start,len) = get_start_len (fun x -> x) sign_ii in
  match ty with
    Signed ->
      let strname = "Signed" in
      leaf indent "" strname start len;
      len
  | UnSigned ->
      let strname = "UnSigned" in
      leaf indent "" strname start len;
      len

and pp_base indent ty base_ii =
  let strcode = get_strcode ty in
  let (start,len) = get_start_len (fun x -> x) base_ii in
  match ty with
    CChar2 ->
      let strname = "CChar2" in
      leaf indent "" strname start len;
  | CShort ->
      let strname = "CShort" in
      leaf indent "" strname start len;
  | CInt ->
      let strname = "CInt" in
      leaf indent "" strname start len;
  | CLong ->
      let strname = "CLong" in
      leaf indent "" strname start len;
  | CLongLong ->
      let strname = "CLongLong" in
      leaf indent "" strname start len;

and pp_floattype indent ty start len =
  let strcode = get_strcode ty in
  match ty with
    CFloat ->
      let strname = "CFloat" in
      leaf indent "" strcode strname start len
  | CDouble ->
      let strname = "CDouble" in
      leaf indent "" strcode strname start len
  | CLongDouble ->
      let strname = "CLongDouble" in
      leaf indent "" strcode strname start len

(* -------------------------------------- *)

and pp_structunion indent ty start len =
  let strcode = get_strcode ty in
  match ty with
    Struct ->
      let strname = "Struct" in
      let len = String.length strname in (* fake, hope for the best *)
      leaf indent "" strcode strname start len
  | Union ->
      let strname = "Union" in
      let len = String.length strname in (* fake, hope for the best *)
      leaf indent "" strcode strname start len

     and structType  = field list

and pp_field =
  let strcode = get_strcode fld in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_field fld in
  match fld with
    DeclarationField(fd) ->
      let strname = "DeclarationField" in
      nested_start indent strcode strname start len;
      pp_field_declaration (indent + 1) fd;
      nested_end indent
  | EmptyField(_) ->
      let strname = "EmptyField" in
      leaf_token indent strcode strname start len
  | MacroDeclField of (string * argument wrap2 list) wrap (* optional ';'*)

            (* cppext: *)
  | CppDirectiveStruct(cppd) ->
      let strname = "CppDirectiveStruct" in
      nested_start indent strcode strname start len;
      pp_cpp_directive (indent + 1) cppd;
      nested_end indent
  | IfdefStruct of ifdef_directive (* * field list list *)

and pp_field_declaration indent fd =
  List.iter (function fk -> pp_fieldkind indent (Ast_c.unwrap fk))
    (Ast_c.unwrap fd)

and pp_fieldkind indent fk =
  let strcode = get_strcode fk in
  match fk with
  | Simple(None,ft) ->
      let strname = "Simple" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_type ft in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | Simple(Some name,ft) ->
      let strname = "Simple" in
      let nmstart = get_name_start name in
      let (ftstart,ftlen) = get_start_len Lib_parsing_c.ii_of_type ft in
      let len = (max nmstart ftstart) - (min nmstart ftstart) + ftlen in
      nested_start indent strcode strname start len;
      pp_name (indent + 1) name;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | BitField(None,ft,_,exp) ->
      let strname = "BitField" in
      let (ftstart,ftlen) = get_start_len Lib_parsing_c.ii_of_type ft in
      let (exstart,exlen) = get_start_len Lib_parsing_c.ii_of_expr exp in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      pp_expression (indent + 1) e;
      nested_end indent
  | BitField(Some name,ft,_,exp) ->
      let strname = "BitField" in
      let nmstart = get_name_start name in
      let (exstart,exlen) = get_start_len Lib_parsing_c.ii_of_expr exp in
      let len = (max nmstart exstart) - (min nmstart exstart) + exlen in
      nested_start indent strcode strname start len;
      pp_name (indent + 1) name;
      pp_fullType (indent + 1) ft;
      pp_expression (indent + 1) e;
      nested_end indent

and get_name_start = function
    RegularName(wstr) ->
      let (start,len) = get_start_len (fun x -> x) (snd wstr) in start
  | CppConcatenatedName(lst) ->
      (match lst with
	x::_ ->
	  let wstr = Ast_c.unwrap x in
	  let (start,len) = get_start_len (fun x -> x) (snd wstr) in start
      | _ -> failwith "CppConcatenatedName with no arguments?")
  | CppVariadicName(var_name) ->
      let (start,len) = get_start_len (fun x -> x) (snd var_name) in start
  | CppIdentBuilder _ -> failwith "CppIdentBuilder not supported"

and pp_name indent name =
  let start = get_name_start name in
  let s = Ast_c.str_of_name e in
  leaf_string indent s start



     (* -------------------------------------- *)
     and enumType = oneEnumType wrap2 (* , *) list
                   (* => string * int list *)

     and oneEnumType = name * (info (* = *) * expression) option

     (* -------------------------------------- *)
     (* return * (params * has "...") *)
     and functionType = fullType * (parameterType wrap2 list * bool wrap)
        and parameterType =
        { p_namei: name option;
          p_register: bool wrap;
          p_type: fullType;
        }
        (* => (bool (register) * fullType) list * bool *)


and typeQualifier = typeQualifierbis wrap
and typeQualifierbis = {const: bool; volatile: bool}

(* gccext: cppext: *)
and attribute = attributebis wrap
  and attributebis =
    | Attribute of string

(* ------------------------------------------------------------------------- *)
(* C expression *)
(* ------------------------------------------------------------------------- *)

and pp_expression indent = fun ((exp, typ), ii) as oe ->
  let strcode = get_strcode oe in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_expr oe in
  match exp with
  | Ident (ident) ->
      let strname = "Ident" in
      nested_start indent strcode strname start len;
      pp_name (ident + 1) ident;
      nested_end indent
    (* only a MultiString can have multiple ii *)
  | Constant (MultiString _), is     ->
      let strname = "Constant" in
      let s = String.concat "" (List.map Ast_c.str_of_info is) in
      leaf indent s strcode strname start len
  | Constant c ->
      let strname = "Constant" in
      let s = Ast_c.str_of_info (List.hd ii) in
      leaf indent s strcode strname start len
  | StringConstant(s,os,w) ->
      let strname = "StringConstant" in
      leaf indent os strcode strname start len
  | FunCall (e, es) ->
      let strname = "FunCall" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_arg_list (indent + 1) es (snd oe);
      nested_end indent
  | CondExpr (e1, e2, e3) ->
      let strname = "CondExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      do_option (function x -> pp_expression (indent + 1) x) e2;
      pp_expression (indent + 1) e3;
      nested_end indent
  | Sequence (e1, e2) ->
      let strname = "Sequence" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      pp_expression (indent + 1) e2;
      nested_end indent
  | Assignment (e1, op, e2) ->
      let strname = "Assignment" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      pp_op (indent + 1) op i;
      pp_expression (indent + 1) e2;
      nested_end indent
  | Postfix (e, op) ->
      let strname = "Postfix" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_op (indent + 1) op i;
      nested_end indent
  | Infix (e, op) ->
      let strname = "Infix" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_op (indent + 1) op i;
      nested_end indent
  | Unary (e, op) ->
      let strname = "Unary" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_op (indent + 1) op i;
      nested_end indent
  | Binary (e1, op, e2) ->
      let strname = "Binary" in
      let opbi = tuple_of_list1 ii in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      let (start,len) = get_start_len (fun x -> x) [opbi] in
      leaf_string (indent + 1) (TH.string_of_token opbi) start;
      pp_op (indent + 1) op i;
      pp_expression (indent + 1) e2;
      nested_end indent
  | ArrayAccess (e1, e2) ->
      let strname = "ArrayAccess" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      pp_expression (indent + 1) e2;
      nested_end indent
  | RecordAccess (e, name) ->
      let strname = "RecordAccess" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      pp_name (indent + 1) e2;
      nested_end indent
  | RecordPtAccess (e, name) ->
      let strname = "RecordPtAccess" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      pp_name (indent + 1) e2;
      nested_end indent
  | SizeOfExpr (e) ->
      let strname = "SizeOfExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | SizeOfType (t) ->
      let strname = "SizeOfType" in
      nested_start indent strcode strname start len;
      pp_type (indent + 1) t;
      nested_end indent
  | Cast (t, e) ->
      let strname = "Cast" in
      nested_start indent strcode strname start len;
      pp_type (indent + 1) t;
      pp_expression (indent + 1) e;
      nested_end indent
  | StatementExpr (statxs, [ii1;ii2]) ->
      let strname = "StatementExpr" in
      nested_start indent str code strname start len;
      List.iter (pp_statement (indent + 1)) statxs;
      nested_end indent
  | Constructor (t, init) ->
      let strname = "Constructor" in
      nested_start indent str code strname start len;
      pp_fullType (indent + 1) t;
      pp_initialiser (indent + 1) init;
      nested_end indent
  | ParenExpr (e) ->
      let strname = "ParenExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | New (None, t) ->
      let strname = "New" in
      nested_start indent strcode strname start len;
      pp_argument (indent + 1) t;
      nested_end indent
  | New (Some ts, t) ->
      let strname = "New" in
      nested_start indent strcode strname start len;
      pp_arg_list (indent + 1) ts (List.tl(snd oe));
      pp_argument (indent + 1) t;
      nested_end indent
  | Delete(t) ->
      let strname = "Delete" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) t;
      nested_end indent

and pp_statement indent st =
  let strcode = get_wstrcode st in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_stmt st in
  match Ast_c.unwrap st with
  | Labeled com -> pp_jump indent com start len
  | Compound statxs ->
      let strname = "Compound" in
      nested_start indent str code strname start len;
      List.iter (pp_statement (indent + 1)) statxs;
      nested_end indent
	
  | ExprStatement es ->
      let strname = "ExprStatement" in
      nested_start indent str code strname start len;
      pp_expression_statement (indent + 1) es (snd st) start len;
      nested_end indent
	
  | Selection com -> pp_selection indent com start len
  | Iteration com -> pp_iteration indent com start len
  | Jump com -> pp_jump indent com start len
  | Decl decl -> pp_decl indent decl
	
  | Asm asmbody, ii -> failwith "not supporting assembly code"
	
  | NestedFunc def -> pp_def indent def
  | MacroStmt ii ->
      let strname = "MacroStmt" in
      let s = String.concat "" (List.map Ast_c.str_of_info ii) in
      leaf indent s strcode strname start len
	
and pp_label indent s start len =
  let strcode = get_strcode s in
  match s with
  | Label (name, st) ->
      let strname = "Label" in
      nested_start indent str code strname start len;
      pp_name (indent + 1) name;
      pp_statement (indent + 1) st;
      nested_end indent
  | Case (e, st) ->
      let strname = "Case" in
      nested_start indent str code strname start len;
      pp_expression (indent + 1) e;
      pp_statement (indent + 1) st;
      nested_end indent
  | CaseRange (e, e2, st) ->
      let strname = "CaseRange" in
      nested_start indent str code strname start len;
      pp_expression (indent + 1) e;
      pp_expression (indent + 1) e2;
      pp_statement (indent + 1) st;
      nested_end indent
  | Default st ->
      let strname = "Default" in
      nested_start indent str code strname start len;
      pp_statement (indent + 1) st;
      nested_end indent
	
and pp_jump indent s start len =
  let strcode = get_strcode s in
  match s with
  | Goto name ->
      let strname = "Goto" in
      nested_start indent str code strname start len;
      pp_name (indent + 1) name;
      nested_end indent
  | Continue ->
      let strname = "Continue" in
      leaf_token indent strcode strname start len
  | Break ->
      let strname = "Break" in
      leaf_token indent strcode strname start len
  | Return ->
      let strname = "Return" in
      leaf_token indent strcode strname start len
  | ReturnExpr e ->
      let strname = "ReturnExpr" in
      nested_start indent str code strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | GotoComputed e ->
      let strname = "GotoComputed" in
      nested_start indent str code strname start len;
      pp_expression (indent + 1) e;
      nested_end indent

let pp_expression_statement indent es ii start len =
  let strcode = get_strcode es in
  let do_ii _ =
    match ii with
      [i] ->
	let (start,_) = get_start_len (fun x -> x) ii in
	leaf_string (indent + 1) (TH.string_of_tok i) start;
    | _ -> () in
  match es with
    Some e ->
      let strname = "Some" in
      nested_start indent str code strname start len;
      pp_expression (indent + 1) e;
      do_ii();
      nested_end indent
  | None ->
      let strname = "None" in
      nested_start indent str code strname start len;
      do_ii();
      nested_end indent

and pp_selection indent s start len =
  let strcode = get_strcode s in
  match s with
  | If (e, st1, st2) ->
      let strname = "If" in
      nested_start indent str code strname start len;
      pp_expression (indent + 1) e;
      pp_statement (indent + 1) st1;
      (match Ast_c.unwrap st2 with
      | ExprStatement None -> ()
      | _ -> pp_statement (indent + 1) st2);
      nested_end indent
  | Switch (e, st) ->
      nested_start indent str code strname start len;
      pp_expression (indent + 1) e;
      pp_statement (indent + 1) st;
      nested_end indent

and pp_iteration indent s start len =
  let strcode = get_strcode s in
  match s with
  | While (e, st) ->
      let strname = "While" in
      nested_start indent str code strname start len;
      pp_expression (indent + 1) e;
      pp_statement (indent + 1) st;
      nested_end indent

  | DoWhile (st, e) ->
      let strname = "DoWhile" in
      nested_start indent str code strname start len;
      pp_statement (indent + 1) st;
      pp_expression (indent + 1) e;
      nested_end indent

  | For (first,(e2opt,il2),(e3opt, il3),st) ->
      let strname = "For" in
      nested_start indent str code strname start len;
      (match first with
	ForExp (e1opt,il1) ->
          pp_statement (indent + 1) (Ast_c.mk_st (ExprStatement e1opt) il1)
      | ForDecl decl -> pp_decl (indent + 1) decl);
      pp_statement (indent + 1) (Ast_c.mk_st (ExprStatement e2opt) il2);
      pp_statement (indent + 1) (Ast_c.mk_st (ExprStatement e3opt) il3);
      pp_statement (indent + 1) st;
      nested_end indent

  | MacroIteration (s,es,st) ->
      let strname = "MacroIteration" in
      nested_start indent str code strname start len;
      leaf_string start (String.length s) s;
      es +> List.iter (fun (e, opt) -> pp_argument (indent + 1) e);
      pp_statement (indent + 1) st;
      nested_end indent

and pp_compound indent stms start len =
  nested_start_list indent start len;
  List.iter (pp_statement_sequencable (indent + 1) stms;
  nested_end indent

  (* cppext: easier to put at statement_list level than statement level *)
and pp_statement_sequencable indent = function
    StmtElem s -> pp_statement indent s
  | CppDirectiveStmt cppd -> pp_cpp_directive indent cppd
  | IfdefStmt idd -> pp_ifdef_directive indent idd
  | IfdefStmt2 _ ->
      (* supporting this would require finding start and length of subterms *)
      failwith "IfdefStmt2 not supported"

  and exprStatement = expression option

  and declOrExpr = ForDecl of declaration | ForExp of expression option wrap


  (* gccext: *)
  and asmbody = il (* string list *) * colon wrap (* : *) list
      and colon = Colon of colon_option wrap2 list
      and colon_option = colon_option_bis wrap
          and colon_option_bis = ColonMisc | ColonExpr of expression


(* ------------------------------------------------------------------------- *)
(* Declaration *)
(* ------------------------------------------------------------------------- *)
(* (string * ...) option cos can have empty declaration or struct tag
 * declaration.
 *
 * Before I had a Typedef constructor, but why make this special case and not
 * have StructDef, EnumDef, ... so that 'struct t {...} v' will generate 2
 * declarations ? So I try to generalise and not have Typedef either. This
 * requires more work in parsing. Better to separate concern.
 *
 * Before the need for unparser, I didn't have a DeclList but just a Decl.
 *
 * I am not sure what it means to declare a prototype inline, but gcc
 * accepts it.
 *)

and pp_decl indent decl =
  let strcode = get_strcode decl in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_decl decl in
  match decl with
  | DeclList of onedecl wrap2 (* , *) list wrap (* ; fakestart sto *)
      let strname = "DeclList" in
      nested_start_list indent start len;
      ???
      nested_end indent
  | MacroDecl arg ->
      let strname = "MacroDecl" in
      let (str,args,_) = Ast_c.unwrap arg in
      nested_start_list indent start len;
      leaf_string (indent + 1) str start;
      pp_arg_list (indent + 1) (fst args) (snd args);
      nested_end indent
  | MacroDeclInit arg ->
      let strname = "MacroDeclInit" in
      let (str,args,init) = Ast_c.unwrap arg in
      nested_start_list indent start len;
      leaf_string (indent + 1) str start;
      pp_arg_list (indent + 1) (fst args) (snd args);
      pp_initialiser (indent + 1) init;
      nested_end indent

     and onedecl =
       { v_namei: (name * v_init) option;
         v_type: fullType;
         (* semantic: set in type annotated and used in cocci_vs_c
          * when we transform some initialisation into affectation
          *)
         v_type_bis: fullType (* Type_c.completed_and_simplified *) option ref;
         v_storage: storage;
         v_local: local_decl; (* cocci: *)
         v_attr: attribute list; (* gccext: *)
       }
     and v_init =
       NoInit | ValInit of info * initialiser
     | ConstrInit of argument wrap2 (* , *) list wrap
     and storage       = storagebis * bool (* gccext: inline or not *)
     and storagebis    = NoSto | StoTypedef | Sto of storageClass
     and storageClass  = Auto  | Static | Register | Extern

     and local_decl = LocalDecl | NotLocalDecl

     (* fullType is the type used if the type should be converted to
	an assignment.  It can be adjusted in the type annotation
	phase when typedef information is available *)

and pp_initialiser indent = fun init ->
  let strcode = get_strcode init in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_init init in
  match init with
    InitExpr(e) ->
      let strname = "InitExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | InitList(inits) ->
      let strname = "InitList" in
      nested_start indent strcode strname start len;
      List.iter (function (i,_) -> pp_initialiser (indent + 1) u) inits;
      nested_end indent
  | InitDesignators(desigs,init) ->
      let strname = "InitDesignators" in
      nested_start indent strcode strname start len;
      List.iter (pp_designator (indent + 1)) desigs;
      pp_initialiser (indent + 1) init;
      nested_end indent
  | InitFieldOld(str,init) ->
      let strname = "InitFieldOld" in
      nested_start indent strcode strname start len;
      leaf_string (indent + 1) str start;
      pp_initialiser (indent + 1) init;
      nested_end indent
  | InitIndexOld(exp,init) ->
      let strname = "InitIndexOld" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp;
      pp_initialiser (indent + 1) init;
      nested_end indent

and pp_designator indent = fun desig ->
  let strcode = get_strcode desig in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_desig desig in
  match desig with
    DesignatorField(str) ->
      let strname = "DesignatorField" in
      leaf indent strcode strname start len str
  | DesignatorIndex(exp) ->
      let strname = "DesignatorIndex" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp;
      nested_end indent
  | DesignatorRange(exp1,exp2) ->
      let strname = "DesignatorRange" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp1;
      pp_expression (indent + 1) exp2;
      nested_end indent

(* ------------------------------------------------------------------------- *)
(* Function definition *)
(* ------------------------------------------------------------------------- *)
(* Normally we should define another type functionType2 because there
 * are more restrictions on what can define a function than a pointer
 * function. For instance a function declaration can omit the name of the
 * parameter whereas a function definition can not. But, in some cases such
 * as 'f(void) {', there is no name too, so I simplified and reused the
 * same functionType type for both declaration and function definition.
 *
 * Also old style C does not have type in the parameter, so again simpler
 * to abuse the functionType and allow missing type.
 *)
and definition = definitionbis wrap (* ( ) { } fakestart sto *)
  and definitionbis =
  { f_name: name;
    f_type: functionType; (* less? a functionType2 ? *)
    f_storage: storage;
    f_body: compound;
    f_attr: attribute list; (* gccext: *)
    f_old_c_style: declaration list option;
  }
  (* cppext: IfdefFunHeader TODO *)

(* ------------------------------------------------------------------------- *)
(* cppext: cpp directives, #ifdef, #define and #include body *)
(* ------------------------------------------------------------------------- *)
and cpp_directive cppd =
  let strcode = get_strcode cppd in
  match cppd with
    Define(def,(kind,vl)) ->
      let strname = "Define" in
      let (start,len) = get_start_len (fun x -> x) (snd def) in
      nested_start indent strcode strname start len;
      pp_define_kind (indent + 1) kind start len;
      pp_define_val (indent + 1) vl start len;
      nested_end indent
  | Include(includ) ->
      let strname = "Include" in
      let ii = snd includ.Ast_c.i_include in
      let (start,len) =	get_start_len (fun x -> x) ii in
      nested_start indent strcode strname start len;
      let (_inc,file) = tuple_of_list2 ii in
      let (start,len) = get_start_len (fun x -> x) [file] in
      leaf_string (indent + 1) (TH.string_of_tok file) start;
      nested_end indent
  | Pragma of string wrap * pragmainfo
      let strname = "Pragma" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_cppd cppd in
  | OtherDirective(il) ->
      let strname = "OtherDirective" in
      let (start,len) = get_start_len (fun x -> x) il in
      nested_start indent strcode strname start len;
      List.iter
	(function i ->
	  let (start,len) = get_start_len (fun x -> x) [i] in
	  leaf_string (indent + 1) (TH.string_of_tok i) start)
	il;
      nested_end indent

and pp_define_kind indent kind start len =
  let strcode = get_strcode kind in
  match kind with
    DefineVar ->
      let strname = "DefineVar" in
      leaf_token indent strcode strname start len
  | DefineFunc(args)
      let strname = "DefineFunc" in
      let (start,len) = get_start_len (fun x -> x) (snd args) in
      nested_start indent strcode strname start len;
      List.iter
	(function (wstr,_) ->
	  let (start,len) = get_start_len (fun x -> x) (snd wstr) in
	  leaf_string indent (Ast.unwrap wstr) start)
	args;
      nested_end indent
  | Undef ->
      let strname = "Undef" in
      leaf_token indent strcode strname start len

and pp_define_val indent d =
  let strcode = get_strcode desig in
  match d with
    DefineExpr(exp) ->
      let strname = "DefineExpr" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_expr exp in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp;
      nested_end indent
  | DefineStmt(stm) ->
      let strname = "DefineStmt" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_stmt stm in
      nested_start indent strcode strname start len;
      pp_statement (indent + 1) stm;
      nested_end indent
  | DefineType(ft) ->
      let strname = "DefineType" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_type ft in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | DefineDoWhileZero of (statement * expression) wrap (* do { } while(0) *)
  | DefineFunction(def) ->
      let strname = "DefineFunction" in
      nested_start indent strcode strname start len;
      pp_definition (indent + 1) def;
      nested_end indent
  | DefineInit(init) ->
      let strname = "DefineInit" in
      nested_start indent strcode strname start len;
      pp_intialiser (indent + 1) init;
      nested_end indent
  | DefineMulti(stmts) ->
      let strname = "DefineMulti" in
      nested_start indent strcode strname start len;
      List.iter (pp_statement (indent + 1)) stmts;
      nested_end indent
  | DefineText(wstr) ->
      let strname = "DefineText" in
      let (start,len) = get_start_len (fun x -> x) (snd wstr) in
      leaf_string indent (Ast_c.unwrap wstr) start
  | DefineEmpty ->
      let strname = "DefineEmpty" in
      leaf_token indent strcode strname start len
  | DefineTodo ->
      let strname = "DefineTodo" in
      leaf_token indent strcode strname start len

and pp_pragmainfo indent pi =
  let strcode = get_strcode pi in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_pargmainfo pi in
  match pi with
    PragmaTuple args ->
      let strname = "PragmaTuple" in
      nested_start indent strcode strname start len;
      pp_arg_list (indent + 1) args;
      nested_end indent
  | PragmaIdList _ ->
      failwith "PragmaIdList not supported"

and ifdef_directive indent id =
  let strcode = get_strcode id in
  match id with
    IfdefDirective(dir) ->
      let strname = "IfdefDirective" in
      let ii = snd dir in
      let (start,len) = get_start_len (fun x -> x) ii in
      let s = String.concat "" (List.map Ast_c.str_of_info ii) in
      leaf indent s strcode strname start len

(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
(* ------------------------------------------------------------------------- *)
and pp_toplevel indent tl =
  let strcode = get_strcode tl in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_toplevel tl in
  | Declaration decl
      let strname = "Declaration" in
      nested_start indent strcode strname start len;
      pp_decl (indent + 1) decl;
      nested_end indent
  | Definition def
      let strname = "Definition" in
      nested_start indent strcode strname start len;
      pp_def (indent + 1) def;
      nested_end indent
  | CppTop(cppd) ->
      let strname = "CppTop" in
      nested_start indent strcode strname start len;
      pp_cpp_directive (indent + 1) cppd;
      nested_end indent
  | IfdefTop of ifdef_directive (* * toplevel list *)

  (* cppext: *)
  | MacroTop of string * argument wrap2 list * il

  | EmptyDef il ->
      let strname = "EmptyDef" in
      let s = String.concat "" (List.map Ast_c.str_of_info il) in
      leaf indent s strcode strname start len
  | NotParsedCorrectly il ->
      let strname = "NotParsedCorrectly" in
      let s = String.concat "" (List.map Ast_c.str_of_info il) in
      leaf indent s strcode strname start len
  | FinalDef info ->
      let strname = "FinalDef" in
      let s = Ast_c.str_of_info info in
      leaf indent s strcode strname start len
  | Namespace _ -> failwith "not supporting c++"

(* ------------------------------------------------------------------------- *)
and program = toplevel list
