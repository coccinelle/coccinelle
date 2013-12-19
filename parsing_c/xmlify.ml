open Common

type entries =
  PP_ARG_LIST
| PP_ARGUMENT
| PP_ACTION
| PP_FULLTYPE
| PP_TYPE_QUALIFIER
| PP_TYPEC
| PP_BASETYPE
| PP_INTTYPE
| PP_SIGN
| PP_BASE
| PP_FLOATTYPE
| PP_STRUCTUNION
| PP_STRUCTTYPE
| PP_FIELD
| PP_FIELD_DECLARATION
| PP_FIELDKIND
| PP_ENUMTYPE
| PP_ONEENUMTYPE
| PP_NAME
| PP_PARAMLIST
| PP_PARAMETERTYPE
| PP_ATTRIBUTE
| PP_EXPRESSION
| PP_STATEMENT
| PP_LABEL
| PP_JUMP
| PP_EXPRESSION_STATEMENT
| PP_SELECTION
| PP_ITERATION
| PP_STATEMENT_SEQUENCABLE
| PP_COMPOUND
| PP_STORAGE
| PP_DECL
| PP_INITIALISER
| PP_DESIGNATOR
| PP_DEFINITION
| PP_DECL_LIST
| PP_CPP_DIRECTIVE
| PP_DEFINE_KIND
| PP_DEFINE_VAL
| PP_PRAGMAINFO
| PP_IFDEF_DIRECTIVE
| PP_TOPLEVEL
| PP_PROGRAM
| LIST
| STRING

(* add 1 everywhere, because multiplying by 0 is not very useful (there
   are two zero values - tagged and untagged) *)
let int_multiplier = 0
let tag_multiplier = 100
let strcode a =
  let s = Dumper.dump a in
  match String.get s 0 with
    '(' -> 1 * tag_multiplier
  | 'T' ->
      (match Str.split (Str.regexp " ") s with
	tag::_ ->
	  (1 + int_of_string (String.sub s 3 (String.length s - 3))) *
	    tag_multiplier
      |	_ -> failwith ("bad tag string: "^s))
  | _ -> (1 + int_of_string s) * int_multiplier

let entry_multiplier = 10000 (* entry should never be a tag *)
let term_multiplier = 0
let get_strcode entry term =
  let e = strcode entry * entry_multiplier in
  let t = strcode term * term_multiplier in
  e + t

let get_entry_strcode entry =
  strcode entry * entry_multiplier

let get_wstrcode entry term = get_strcode entry (Ast_c.unwrap term)

(* ------------------------------------------------------------------------- *)

let print_indent n =
  Printf.printf "%s" (String.make n ' ')

let nested_start indent index typelabel start len =
  print_indent indent;
  Printf.printf
    "<tree type = \"%d\" label = \"\" typeLabel = \"%s\" pos = \"%d\" length = \"%d\">\n"
    index typelabel start len

let nested_start_list indent start len =
  let index = get_entry_strcode LIST in
  let typelabel = "GenericList" in
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
  let index = get_entry_strcode STRING in
  let typelabel = "GenericString" in
  let len = String.length str in
  leaf indent index typelabel start len str

(* ------------------------------------------------------------------------- *)

let get_start_len fn thing =
  let (max,min) = Lib_parsing_c.max_min_ii_by_pos (fn thing) in
  (Ast_c.opos_of_info min,
   Ast_c.opos_of_info max + String.length (Ast_c.str_of_info max) -
     Ast_c.opos_of_info min)

(* ------------------------------------------------------------------------- *)

let rec pp_arg_list indent es ii =
  let (start,len) = get_start_len (fun x -> x) ii in
  nested_start_list indent start len;
  List.iter (fun x -> pp_argument (indent + 1) (Ast_c.unwrap x)) es;
  nested_end indent

and pp_argument indent argument =
  let strcode = get_strcode PP_ARGUMENT argument in
  match argument with
  | Left e ->
      let strname = "Left" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_expr e in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | Right weird ->
      (match weird with
	Ast_c.ArgType param -> pp_parameterType (indent + 1) param
      | Ast_c.ArgAction action -> pp_action (indent + 1) action)

and pp_action indent action =
  let strcode = get_strcode PP_ACTION action in
  match action with
    Ast_c.ActMisc il ->
      let strname = "ActMisc" in
      let (start,len) = get_start_len (fun x -> x) il in
      nested_start indent strcode strname start len;
      List.iter (pp_token_leaf (indent + 1)) il;
      nested_end indent

and pp_fullType indent ((tq, ty) as ft) =
  let strcode = get_entry_strcode PP_FULLTYPE in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_type ft in
  let strname = "FullType" in
  nested_start indent strcode strname start len;
  pp_type_qualifier (indent + 1) tq;
  pp_typeC (indent + 1) ty start len; (* not right, includes qualifier... *)
  nested_end indent

and pp_type_qualifier indent tq =
  let strcode = get_entry_strcode PP_TYPE_QUALIFIER in
  let il = snd tq in
  let (start,len) = get_start_len (fun x -> x) il in
  let strname = "TypeQualifier" in
  nested_start indent strcode strname start len;
  List.iter (pp_token_leaf (indent + 1)) il;
  nested_end indent

and split_signb_baseb_ii (baseb, ii) =
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
	    | _ -> failwith ("unrecognized signed int: "^
			      (String.concat " "(List.map fst iis))))

	| Ast_c.CChar2, ["char",i2] -> [i2]

	| Ast_c.CShort, ["short",i1] -> [i1]
	| Ast_c.CShort, ["short",i1;"int",i2] -> [i1;i2]

	| Ast_c.CLong, ["long",i1] -> [i1]
	| Ast_c.CLong, ["long",i1;"int",i2] -> [i1;i2]

	| Ast_c.CLongLong, ["long",i1;"long",i2] -> [i1;i2]
	| Ast_c.CLongLong, ["long",i1;"long",i2;"int",i3] -> [i1;i2;i3]

	| _ ->
	    failwith
	      ("strange type1, maybe because of weird order: "^
	       (String.concat " " (List.map fst iis))) in
      (signed,base_res)

  | Ast_c.SizeType, ["size_t",i1] -> None, [i1]
  | Ast_c.SSizeType, ["ssize_t",i1] -> None, [i1]
  | Ast_c.PtrDiffType, ["ptrdiff_t",i1] -> None, [i1]

  | _ ->
      failwith
	("strange type2, maybe because of weird order: "^
	 (String.concat " " (List.map fst iis)))

and pp_typeC indent ty start len =
  let strcode = get_wstrcode PP_TYPEC ty in
  let ii = snd ty in
  match Ast_c.unwrap ty with
    Ast_c.NoType ->
      let strname = "NoType" in
      leaf_token indent strcode strname start len
  | Ast_c.BaseType ty ->
      let strname = "BaseType" in
      nested_start indent strcode strname start len;
      pp_basetype (indent + 1) ty start len (split_signb_baseb_ii (ty, ii));
      nested_end indent
  | Ast_c.Pointer ty ->
      let strname = "Pointer" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ty;
      nested_end indent
  | Ast_c.Array(Some e,ty) ->
      let strname = "Array" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_fullType (indent + 1) ty;
      nested_end indent
  | Ast_c.Array(None,ty) ->
      let strname = "Array" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ty;
      nested_end indent
  | Ast_c.Decimal(e1,Some e2) ->
      let strname = "Decimal" in
      nested_start indent strcode strname start len;
      nested_end indent
  | Ast_c.Decimal(e1,None) ->
      let strname = "Decimal" in
      nested_start indent strcode strname start len;
      nested_end indent
  | Ast_c.FunctionType(funtype) ->
      let strname = "FunctionType" in
      let (lpb, rpb) = tuple_of_list2 ii in
      nested_start indent strcode strname start len;
      pp_functionType indent funtype [lpb;rpb];
      nested_end indent
  | Ast_c.Enum(Some name,et) ->
      let strname = "Enum" in
      let (ei,nmi,lb,rb,_) = tuple_of_list5 ii in
      nested_start indent strcode strname start len;
      let (nstart,_) = get_start_len (fun x -> x) [nmi] in
      leaf_string (indent + 1) name nstart;
      pp_enumType (indent + 1) et [lb;rb];
      nested_end indent
  | Ast_c.Enum(None,et) ->
      let strname = "Enum" in
      let (ei,lb,rb,_) = tuple_of_list4 ii in
      nested_start indent strcode strname start len;
      pp_enumType (indent + 1) et [lb;rb];
      nested_end indent
  | Ast_c.StructUnion(su,Some name,ty) ->
      let strname = "StructUnion" in
      let (sui,nmi,lb,rb) = tuple_of_list4 ii in
      nested_start indent strcode strname start len;
      pp_structunion (indent + 1) su sui;
      let (nstart,_) = get_start_len (fun x -> x) [nmi] in
      leaf_string (indent + 1) name nstart;
      pp_structType (indent + 1) ty [lb;rb];
      nested_end indent
  | Ast_c.StructUnion(su,None,ty) ->
      let strname = "StructUnion" in
      let (sui,lb,rb) = tuple_of_list3 ii in
      nested_start indent strcode strname start len;
      pp_structunion (indent + 1) su sui;
      pp_structType (indent + 1) ty [lb;rb];
      nested_end indent
  | Ast_c.EnumName(name) ->
      let strname = "EnumName" in
      leaf indent strcode strname start len name
  | Ast_c.StructUnionName(su,name) ->
      let strname = "StructUnionName" in
      leaf indent strcode strname start len name
  | Ast_c.TypeName(name,_) ->
      let strname = "TypeName" in
      nested_start indent strcode strname start len;
      pp_name (indent + 1) name;
      nested_end indent
  | Ast_c.ParenType(ft) ->
      let strname = "ParenType" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | Ast_c.TypeOfExpr(exp) ->
      let strname = "TypeOfExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp;
      nested_end indent
  | Ast_c.TypeOfType(ft) ->
      let strname = "TypeOfType" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent

(* -------------------------------------- *)

and pp_basetype indent ty start len sign_and_base =
  let strcode = get_strcode PP_BASETYPE ty in
  match ty with
    Ast_c.Void ->
      let strname = "Void" in
      leaf_token indent strcode strname start len
  | Ast_c.IntType(i) ->
      let strname = "IntType" in
      nested_start indent strcode strname start len;
      pp_inttype (indent + 1) i start len sign_and_base;
      nested_end indent
  | Ast_c.FloatType(f) ->
      let strname = "FloatType" in
      nested_start indent strcode strname start len;
      pp_floattype (indent + 1) f start len;
      nested_end indent
  | Ast_c.SizeType ->
      let strname = "SizeType" in
      leaf_token indent strcode strname start len
  | Ast_c.SSizeType ->
      let strname = "SSizeType" in
      leaf_token indent strcode strname start len
  | Ast_c.PtrDiffType ->
      let strname = "PtrDiffType" in
      leaf_token indent strcode strname start len

and pp_inttype indent ty start len (sign_ii,base_ii) =
  let strcode = get_strcode PP_INTTYPE ty in
  match ty,sign_ii with
    Ast_c.CChar,_ ->
      let strname = "CChar" in
      leaf_token indent strcode strname start len
  | Ast_c.Si(sign,base),Some(_,sign_ii) ->
      let strname = "Si" in
      nested_start indent strcode strname start len;
      pp_sign (indent + 1) sign [sign_ii];
      pp_base (indent + 1) base base_ii;
      nested_end indent
  | _ -> failwith "sign expected"

and pp_sign indent ty sign_ii =
  let strcode = get_strcode PP_SIGN ty in
  let (start,len) = get_start_len (fun x -> x) sign_ii in
  match ty with
    Ast_c.Signed ->
      let strname = "Signed" in
      leaf_token indent strcode strname start len
  | Ast_c.UnSigned ->
      let strname = "UnSigned" in
      leaf_token indent strcode strname start len

and pp_base indent ty base_ii =
  let strcode = get_strcode PP_BASE ty in
  let (start,len) = get_start_len (fun x -> x) base_ii in
  match ty with
    Ast_c.CChar2 ->
      let strname = "CChar2" in
      leaf_token indent strcode strname start len;
  | Ast_c.CShort ->
      let strname = "CShort" in
      leaf_token indent strcode strname start len;
  | Ast_c.CInt ->
      let strname = "CInt" in
      leaf_token indent strcode strname start len;
  | Ast_c.CLong ->
      let strname = "CLong" in
      leaf_token indent strcode strname start len;
  | Ast_c.CLongLong ->
      let strname = "CLongLong" in
      leaf_token indent strcode strname start len;

and pp_floattype indent ty start len =
  let strcode = get_strcode PP_FLOATTYPE ty in
  match ty with
    Ast_c.CFloat ->
      let strname = "CFloat" in
      leaf_token indent strcode strname start len
  | Ast_c.CDouble ->
      let strname = "CDouble" in
      leaf_token indent strcode strname start len
  | Ast_c.CLongDouble ->
      let strname = "CLongDouble" in
      leaf_token indent strcode strname start len

(* -------------------------------------- *)

and pp_structunion indent su i =
  let strcode = get_strcode PP_STRUCTUNION su in
  let (start,len) = get_start_len (fun x -> x) [i] in
  match su with
    Ast_c.Struct ->
      let strname = "Struct" in
      let len = String.length strname in (* fake, hope for the best *)
      leaf_token indent strcode strname start len
  | Ast_c.Union ->
      let strname = "Union" in
      let len = String.length strname in (* fake, hope for the best *)
      leaf_token indent strcode strname start len

and pp_structType indent fields ii =
  let strcode = get_entry_strcode PP_STRUCTTYPE in
  let strname = "StructType" in
  let (start,len) = get_start_len (fun x -> x) ii in
  nested_start indent strcode strname start len;
  List.iter (pp_field (indent + 1)) fields;
  nested_end indent

and pp_field indent fld =
  let strcode = get_strcode PP_FIELD fld in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_field fld in
  match fld with
    Ast_c.DeclarationField(fd) ->
      let strname = "DeclarationField" in
      nested_start indent strcode strname start len;
      pp_field_declaration (indent + 1) fd;
      nested_end indent
  | Ast_c.EmptyField(_) ->
      let strname = "EmptyField" in
      leaf_token indent strcode strname start len
  | Ast_c.MacroDeclField _ -> failwith "MacroDeclField not supported"
  | Ast_c.CppDirectiveStruct cppd ->
      let strname = "CppDirectiveStruct" in
      nested_start indent strcode strname start len;
      pp_cpp_directive (indent + 1) cppd;
      nested_end indent
  | Ast_c.IfdefStruct ifd ->
      let strname = "IfdefStruct" in
      nested_start indent strcode strname start len;
      pp_ifdef_directive (indent + 1) ifd;
      nested_end indent

and pp_field_declaration indent fd =
  match fd with
    Ast_c.FieldDeclList decls ->
      List.iter (function fk -> pp_fieldkind indent (Ast_c.unwrap fk))
	(Ast_c.unwrap decls)

and pp_fieldkind indent fk =
  let strcode = get_strcode PP_FIELDKIND fk in
  match fk with
  | Ast_c.Simple(None,ft) ->
      let strname = "Simple" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_type ft in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | Ast_c.Simple(Some name,ft) ->
      let strname = "Simple" in
      let nmstart = get_name_start name in
      let (ftstart,ftlen) = get_start_len Lib_parsing_c.ii_of_type ft in
      let len = (max nmstart ftstart) - (min nmstart ftstart) + ftlen in
      nested_start indent strcode strname ftstart len;
      pp_name (indent + 1) name;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | Ast_c.BitField(None,ft,_,exp) ->
      let strname = "BitField" in
      let (start,_) = get_start_len Lib_parsing_c.ii_of_type ft in
      let (start2,len) = get_start_len Lib_parsing_c.ii_of_expr exp in
      let len = start2 - start + len in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      pp_expression (indent + 1) exp;
      nested_end indent
  | Ast_c.BitField(Some name,ft,_,exp) ->
      let strname = "BitField" in
      let nmstart = get_name_start name in
      let (exstart,exlen) = get_start_len Lib_parsing_c.ii_of_expr exp in
      let len = (max nmstart exstart) - (min nmstart exstart) + exlen in
      nested_start indent strcode strname (min nmstart exstart) len;
      pp_name (indent + 1) name;
      pp_fullType (indent + 1) ft;
      pp_expression (indent + 1) exp;
      nested_end indent

and get_name_start = function
    Ast_c.RegularName(wstr) ->
      let (start,len) = get_start_len (fun x -> x) (snd wstr) in start
  | Ast_c.CppConcatenatedName(lst) ->
      (match lst with
	x::_ ->
	  let wstr = Ast_c.unwrap x in
	  let (start,len) = get_start_len (fun x -> x) (snd wstr) in start
      | _ -> failwith "CppConcatenatedName with no arguments?")
  | Ast_c.CppVariadicName(var_name) ->
      let (start,len) = get_start_len (fun x -> x) (snd var_name) in start
  | Ast_c.CppIdentBuilder _ -> failwith "CppIdentBuilder not supported"

and pp_name indent name =
  let start = get_name_start name in
  let s = Ast_c.str_of_name name in
  leaf_string indent s start

and pp_enumType indent enums ii =
  let strcode = get_entry_strcode PP_ENUMTYPE in
  let (start,len) = get_start_len (fun x -> x) ii in
  let strname = "EnumType" in
  nested_start indent strcode strname start len;
  List.iter (fun x -> pp_oneEnumType (indent + 1) (Ast_c.unwrap x)) enums;
  nested_end indent

and pp_oneEnumType indent et =
  let strcode = get_entry_strcode PP_ONEENUMTYPE in
  let strname = "OneEnumType" in
  match et with
    (name,Some(_,exp)) ->
      let start = get_name_start name in
      let (start2,len) = get_start_len Lib_parsing_c.ii_of_expr exp in
      let len = start2 - start + len in
      nested_start indent strcode strname start len;
      pp_name (indent + 1) name;
      pp_expression (indent + 1) exp;
      nested_end indent
  | (name,None) ->
      let start = get_name_start name in
      let s = Ast_c.str_of_name name in
      let len = String.length s in
      nested_start indent strcode strname start len;
      pp_name (indent + 1) name;
      nested_end indent

and pp_functionType indent ft param_ii =
  let (ft, (params, hasdots)) = ft in
  pp_fullType (indent + 1) ft;
  pp_param_list (indent + 1) params param_ii;
  (match hasdots with
    (false,_) -> ()
  | (true,[i]) ->
      let (start,len) = get_start_len (fun x -> x) [i] in
      leaf_string (indent + 1) (Ast_c.str_of_info i) start
  | _ -> failwith "unexpected hasdots")

and pp_param_list indent params ii =
  let strcode = get_entry_strcode PP_PARAMLIST in
  let strname = "ParamList" in
  let (start,len) = get_start_len (fun x -> x) ii in
  nested_start indent strcode strname start len;
  List.iter (fun x -> pp_parameterType (indent + 1) (Ast_c.unwrap x)) params;
  nested_end indent
  
and pp_parameterType indent param =
  let strcode = get_strcode PP_PARAMETERTYPE exp in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_param param in
  let strname = "ParameterType" in
  nested_start indent strcode strname start len;
  (match param.Ast_c.p_register with
    (false,_) -> ()
  | (true,[i]) ->
      let (start,len) = get_start_len (fun x -> x) [i] in
      leaf_string (indent + 1) (Ast_c.str_of_info i) start
  | _ -> failwith "unexpected hasdots");
  pp_fullType (indent + 1) param.Ast_c.p_type;
  (match param.Ast_c.p_namei with
    None -> ()
  | Some name -> pp_name (indent + 1) name);
  nested_end indent

and pp_attribute indent attr =
  let strcode = get_wstrcode PP_ATTRIBUTE attr in
  let strname = "Attribute" in
  let (start,len) = get_start_len (fun x -> x) (snd attr) in
  match Ast_c.unwrap attr with
    Ast_c.Attribute str -> leaf indent strcode strname start len str

(* ------------------------------------------------------------------------- *)
(* C expression *)
(* ------------------------------------------------------------------------- *)

and pp_expression indent = fun (((exp, typ), ii) as oe) ->
  let strcode = get_strcode PP_EXPRESSION exp in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_expr oe in
  match exp with
  | Ast_c.Ident (ident) ->
      let strname = "Ident" in
      nested_start indent strcode strname start len;
      pp_name (indent + 1) ident;
      nested_end indent
    (* only a MultiString can have multiple ii *)
  | Ast_c.Constant (Ast_c.MultiString _) ->
      let strname = "Constant" in
      let s = String.concat "" (List.map Ast_c.str_of_info ii) in
      leaf indent strcode strname start len s
  | Ast_c.Constant c ->
      let strname = "Constant" in
      let s = Ast_c.str_of_info (List.hd ii) in
      leaf indent strcode strname start len s
  | Ast_c.StringConstant(s,os,w) ->
      let strname = "StringConstant" in
      leaf indent strcode strname start len os
  | Ast_c.FunCall (e, es) ->
      let strname = "FunCall" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_arg_list (indent + 1) es ii;
      nested_end indent
  | Ast_c.CondExpr (e1, e2, e3) ->
      let strname = "CondExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      do_option (function x -> pp_expression (indent + 1) x) e2;
      pp_expression (indent + 1) e3;
      nested_end indent
  | Ast_c.Sequence (e1, e2) ->
      let strname = "Sequence" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      pp_expression (indent + 1) e2;
      nested_end indent
  | Ast_c.Assignment (e1, op, e2) ->
      let strname = "Assignment" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      List.iter (pp_token_leaf (indent + 1)) ii;
      pp_expression (indent + 1) e2;
      nested_end indent
  | Ast_c.Postfix (e, op) ->
      let strname = "Postfix" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      List.iter (pp_token_leaf (indent + 1)) ii;
      nested_end indent
  | Ast_c.Infix (e, op) ->
      let strname = "Infix" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      List.iter (pp_token_leaf (indent + 1)) ii;
      nested_end indent
  | Ast_c.Unary (e, op) ->
      let strname = "Unary" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      List.iter (pp_token_leaf (indent + 1)) ii;
      nested_end indent
  | Ast_c.Binary (e1, op, e2) ->
      let strname = "Binary" in
      let opbi = tuple_of_list1 ii in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      let (start,len) = get_start_len (fun x -> x) [opbi] in
      leaf_string (indent + 1) (Ast_c.str_of_info opbi) start;
      List.iter (pp_token_leaf (indent + 1)) ii;
      pp_expression (indent + 1) e2;
      nested_end indent
  | Ast_c.ArrayAccess (e1, e2) ->
      let strname = "ArrayAccess" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e1;
      pp_expression (indent + 1) e2;
      nested_end indent
  | Ast_c.RecordAccess (e, name) ->
      let strname = "RecordAccess" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_name (indent + 1) name;
      nested_end indent
  | Ast_c.RecordPtAccess (e, name) ->
      let strname = "RecordPtAccess" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_name (indent + 1) name;
      nested_end indent
  | Ast_c.SizeOfExpr (e) ->
      let strname = "SizeOfExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | Ast_c.SizeOfType (t) ->
      let strname = "SizeOfType" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) t;
      nested_end indent
  | Ast_c.Cast (t, e) ->
      let strname = "Cast" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) t;
      pp_expression (indent + 1) e;
      nested_end indent
  | Ast_c.StatementExpr (statxs, _) ->
      let strname = "StatementExpr" in
      nested_start indent strcode strname start len;
      List.iter (pp_statement_sequencable (indent + 1)) statxs;
      nested_end indent
  | Ast_c.Constructor (t, init) ->
      let strname = "Constructor" in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) t;
      pp_initialiser (indent + 1) init;
      nested_end indent
  | Ast_c.ParenExpr (e) ->
      let strname = "ParenExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | Ast_c.New (None, t) ->
      let strname = "New" in
      nested_start indent strcode strname start len;
      pp_argument (indent + 1) t;
      nested_end indent
  | Ast_c.New (Some ts, t) ->
      let strname = "New" in
      nested_start indent strcode strname start len;
      pp_arg_list (indent + 1) ts (List.tl(snd oe));
      pp_argument (indent + 1) t;
      nested_end indent
  | Ast_c.Delete(t) ->
      let strname = "Delete" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) t;
      nested_end indent

and pp_statement indent st =
  let strcode = get_wstrcode PP_STATEMENT st in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_stmt st in
  match Ast_c.unwrap st with
  | Ast_c.Labeled com -> pp_label indent com start len
  | Ast_c.Compound statxs -> pp_compound indent statxs start len
	
  | Ast_c.ExprStatement es ->
      let strname = "ExprStatement" in
      nested_start indent strcode strname start len;
      pp_expression_statement (indent + 1) es (snd st) start len;
      nested_end indent
	
  | Ast_c.Selection com -> pp_selection indent com start len
  | Ast_c.Iteration com -> pp_iteration indent com start len
  | Ast_c.Jump com -> pp_jump indent com start len
  | Ast_c.Decl decl -> pp_decl indent decl
	
  | Ast_c.Asm asmbody -> failwith "not supporting assembly code"
	
  | Ast_c.NestedFunc def -> pp_definition indent def
  | Ast_c.MacroStmt ->
      let ii = snd st in
      let strname = "MacroStmt" in
      nested_start indent strcode strname start len;
      List.iter (pp_token_leaf (indent + 1)) ii;
      nested_end indent

and pp_label indent s start len =
  let strcode = get_strcode PP_LABEL s in
  match s with
  | Ast_c.Label (name, st) ->
      let strname = "Label" in
      nested_start indent strcode strname start len;
      pp_name (indent + 1) name;
      pp_statement (indent + 1) st;
      nested_end indent
  | Ast_c.Case (e, st) ->
      let strname = "Case" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_statement (indent + 1) st;
      nested_end indent
  | Ast_c.CaseRange (e, e2, st) ->
      let strname = "CaseRange" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_expression (indent + 1) e2;
      pp_statement (indent + 1) st;
      nested_end indent
  | Ast_c.Default st ->
      let strname = "Default" in
      nested_start indent strcode strname start len;
      pp_statement (indent + 1) st;
      nested_end indent
	
and pp_jump indent s start len =
  let strcode = get_strcode PP_JUMP s in
  match s with
  | Ast_c.Goto name ->
      let strname = "Goto" in
      nested_start indent strcode strname start len;
      pp_name (indent + 1) name;
      nested_end indent
  | Ast_c.Continue ->
      let strname = "Continue" in
      leaf_token indent strcode strname start len
  | Ast_c.Break ->
      let strname = "Break" in
      leaf_token indent strcode strname start len
  | Ast_c.Return ->
      let strname = "Return" in
      leaf_token indent strcode strname start len
  | Ast_c.ReturnExpr e ->
      let strname = "ReturnExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | Ast_c.GotoComputed e ->
      let strname = "GotoComputed" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent

and pp_expression_statement indent es ii start len =
  let strcode = get_strcode PP_EXPRESSION_STATEMENT es in
  let do_ii _ =
    match ii with
      [i] ->
	let (start,_) = get_start_len (fun x -> x) ii in
	leaf_string (indent + 1) (Ast_c.str_of_info i) start;
    | _ -> () in
  match es with
    Some e ->
      let strname = "Some" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      do_ii();
      nested_end indent
  | None ->
      let strname = "None" in
      nested_start indent strcode strname start len;
      do_ii();
      nested_end indent

and pp_selection indent s start len =
  let strcode = get_strcode PP_SELECTION s in
  match s with
  | Ast_c.If (e, st1, st2) ->
      let strname = "If" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_statement (indent + 1) st1;
      (match Ast_c.unwrap st2 with
      | Ast_c.ExprStatement None -> ()
      | _ -> pp_statement (indent + 1) st2);
      nested_end indent
  | Ast_c.Switch (e, st) ->
      let strname = "Switch" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_statement (indent + 1) st;
      nested_end indent

and pp_iteration indent s start len =
  let strcode = get_strcode PP_ITERATION s in
  match s with
  | Ast_c.While (e, st) ->
      let strname = "While" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      pp_statement (indent + 1) st;
      nested_end indent

  | Ast_c.DoWhile (st, e) ->
      let strname = "DoWhile" in
      nested_start indent strcode strname start len;
      pp_statement (indent + 1) st;
      pp_expression (indent + 1) e;
      nested_end indent

  | Ast_c.For (first,(e2opt,il2),(e3opt, il3),st) ->
      let strname = "For" in
      nested_start indent strcode strname start len;
      (match first with
	Ast_c.ForExp (e1opt,il1) ->
          pp_statement (indent + 1)
	    (Ast_c.mk_st (Ast_c.ExprStatement e1opt) il1)
      | Ast_c.ForDecl decl -> pp_decl (indent + 1) decl);
      pp_statement (indent + 1) (Ast_c.mk_st (Ast_c.ExprStatement e2opt) il2);
      pp_statement (indent + 1) (Ast_c.mk_st (Ast_c.ExprStatement e3opt) il3);
      pp_statement (indent + 1) st;
      nested_end indent

  | Ast_c.MacroIteration (s,es,st) ->
      let strname = "MacroIteration" in
      nested_start indent strcode strname start len;
      leaf_string (indent + 1) s start;
      es +> List.iter (fun (e, opt) -> pp_argument (indent + 1) e);
      pp_statement (indent + 1) st;
      nested_end indent

and pp_compound indent statxs start len =
  let strcode = get_entry_strcode PP_COMPOUND in
  let strname = "Compound" in
  nested_start indent strcode strname start len;
  List.iter (pp_statement_sequencable (indent + 1)) statxs;
  nested_end indent

  (* cppext: easier to put at statement_list level than statement level *)
and pp_statement_sequencable indent = function
    Ast_c.StmtElem s -> pp_statement indent s
  | Ast_c.CppDirectiveStmt cppd -> pp_cpp_directive indent cppd
  | Ast_c.IfdefStmt idd -> pp_ifdef_directive indent idd
  | Ast_c.IfdefStmt2 _ ->
      (* supporting this would require finding start and length of subterms *)
      failwith "IfdefStmt2 not supported"

and pp_storage indent storage =
  let strcode = get_entry_strcode PP_STORAGE in
  let strname = "Storage" in
  let (start,len) = get_start_len (fun x -> x) storage in
  nested_start indent strcode strname start len;
  List.iter (pp_token_leaf (indent + 1)) storage;
  nested_end indent

and pp_decl indent decl =
  let strcode = get_strcode PP_DECL decl in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_decl decl in
  match decl with
  | Ast_c.DeclList decls ->
      let strname = "DeclList" in
      nested_start indent strcode strname start len;
      (match decls with
	((onedecl::_),(_::_::storage)) ->
	  pp_storage (indent + 1) storage;
	  pp_fullType (indent + 1) (Ast_c.unwrap onedecl).Ast_c.v_type
      |	_ -> failwith "empty declaration");
      List.iter (pp_one_decl (indent + 1)) (Ast_c.unwrap decls);
      nested_end indent
  | Ast_c.MacroDecl arg ->
      let strname = "MacroDecl" in
      let (str,args,_) = Ast_c.unwrap arg in
      nested_start indent strcode strname start len;
      leaf_string (indent + 1) str start;
      let (lp,rp) =
	match snd arg with
	   iisb::lpb::rpb::iiendb::iifakestart::iisto -> (lpb,rpb)
	| _ -> failwith "bad macrodecl" in
      pp_arg_list (indent + 1) args [lp;rp];
      nested_end indent
  | Ast_c.MacroDeclInit arg ->
      let strname = "MacroDeclInit" in
      let (str,args,init) = Ast_c.unwrap arg in
      nested_start indent strcode strname start len;
      leaf_string (indent + 1) str start;
      let (lp,rp) =
	match snd arg with
	   iisb::lpb::rpb::weqb::iiendb::iifakestart::iisto -> (lpb,rpb)
	| _ -> failwith "bad macrodecl" in
      pp_arg_list (indent + 1) args [lp;rp];
      pp_initialiser (indent + 1) init;
      nested_end indent

and pp_one_decl indent decl =
  let decl = Ast_c.unwrap decl in
  (match decl.Ast_c.v_namei with
    None -> ()
  | Some (nm,ini) ->
      pp_name indent nm;
      pp_v_init indent ini);
  List.iter (pp_attribute indent) decl.Ast_c.v_attr

and pp_v_init indent ini =
  match ini with
    Ast_c.NoInit -> ()
  | Ast_c.ValInit(_,init) ->
      pp_initialiser (indent + 1) init
  | Ast_c.ConstrInit args ->
      pp_arg_list (indent + 1) (Ast_c.unwrap args) (snd args)

and pp_initialiser indent = fun init ->
  let strcode = get_wstrcode PP_INITIALISER init in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_ini init in
  match Ast_c.unwrap init with
    Ast_c.InitExpr(e) ->
      let strname = "InitExpr" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) e;
      nested_end indent
  | Ast_c.InitList(inits) ->
      let strname = "InitList" in
      nested_start indent strcode strname start len;
      List.iter (function (i,_) -> pp_initialiser (indent + 1) i) inits;
      nested_end indent
  | Ast_c.InitDesignators(desigs,init) ->
      let strname = "InitDesignators" in
      nested_start indent strcode strname start len;
      List.iter (pp_designator (indent + 1)) desigs;
      pp_initialiser (indent + 1) init;
      nested_end indent
  | Ast_c.InitFieldOld(str,init) ->
      let strname = "InitFieldOld" in
      nested_start indent strcode strname start len;
      leaf_string (indent + 1) str start;
      pp_initialiser (indent + 1) init;
      nested_end indent
  | Ast_c.InitIndexOld(exp,init) ->
      let strname = "InitIndexOld" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp;
      pp_initialiser (indent + 1) init;
      nested_end indent

and pp_designator indent = fun desig ->
  let strcode = get_wstrcode PP_DESIGNATOR desig in
  let (start,len) = get_start_len (fun x -> x) (snd desig) in
  match Ast_c.unwrap desig with
    Ast_c.DesignatorField(str) ->
      let strname = "DesignatorField" in
      leaf indent strcode strname start len str
  | Ast_c.DesignatorIndex(exp) ->
      let strname = "DesignatorIndex" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp;
      nested_end indent
  | Ast_c.DesignatorRange(exp1,exp2) ->
      let strname = "DesignatorRange" in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp1;
      pp_expression (indent + 1) exp2;
      nested_end indent

and pp_definition indent d =
  let strcode = get_entry_strcode PP_DEFINITION in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_def d in
  let strname = "Definition" in
  let (lp,rp,lb,rb,storage) =
    match (snd d) with(* ( ) { } fakestart sto *)
      lp::rp::lb::rb::_::storage -> (lp,rp,lb,rb,storage)
    | _ -> failwith "bad definition" in
  let d = Ast_c.unwrap d in
  nested_start indent strcode strname start len;
  pp_storage (indent + 1) storage;
  pp_functionType (indent + 1) d.Ast_c.f_type [lp;rp];
  List.iter (pp_attribute (indent + 1)) d.Ast_c.f_attr;
  pp_name (indent + 1) d.Ast_c.f_name;
  (match d.Ast_c.f_old_c_style with
    Some decls -> pp_declaration_list (indent + 1) decls
  | None -> ());
  let (start,len) = get_start_len (fun x -> x) [lb;rb] in
  pp_compound (indent + 1) d.Ast_c.f_body start len;
  nested_end indent

and pp_declaration_list indent decls =
  let strcode = get_entry_strcode PP_DECL_LIST in
  match (decls,List.rev decls) with
    ((first::_),(last::_)) ->
      let (start,_) = get_start_len Lib_parsing_c.ii_of_decl first in
      let (start2,len) = get_start_len Lib_parsing_c.ii_of_decl last in
      let len = start2 - start + len in
      let strname = "DeclarationList" in
      nested_start indent strcode strname start len;
      List.iter (pp_decl (indent + 1)) decls;
      nested_end indent
  | ([],[]) -> ()
  | _ -> failwith "not possible"

and pp_cpp_directive indent cppd =
  let strcode = get_strcode PP_CPP_DIRECTIVE cppd in
  match cppd with
    Ast_c.Define(def,(kind,vl)) ->
      let strname = "Define" in
      let (start,len) = get_start_len (fun x -> x) (snd def) in
      nested_start indent strcode strname start len;
      pp_define_kind (indent + 1) kind start len;
      pp_define_val (indent + 1) vl start len;
      nested_end indent
  | Ast_c.Include(includ) ->
      let strname = "Include" in
      let ii = snd includ.Ast_c.i_include in
      let (start,len) =	get_start_len (fun x -> x) ii in
      nested_start indent strcode strname start len;
      let (_inc,file) = tuple_of_list2 ii in
      let (start,len) = get_start_len (fun x -> x) [file] in
      leaf_string (indent + 1) (Ast_c.str_of_info file) start;
      nested_end indent
  | Ast_c.Pragma(wstr,pi) ->
      let strname = "Pragma" in
      let (start,_) = get_start_len (fun x -> x) (snd wstr) in
      let (start2,len) = get_start_len Lib_parsing_c.ii_of_pragmainfo pi in
      let len = start2 - start + len in
      nested_start indent strcode strname start len;
      leaf_string (indent + 1) (Ast_c.unwrap wstr) start;
      pp_pragmainfo (indent + 1) pi;
      nested_end indent
  | Ast_c.OtherDirective(il) ->
      let strname = "OtherDirective" in
      let (start,len) = get_start_len (fun x -> x) il in
      nested_start indent strcode strname start len;
      List.iter (pp_token_leaf (indent + 1)) il;
      nested_end indent

and pp_token_leaf indent i =
  let (start,len) = get_start_len (fun x -> x) [i] in
  leaf_string (indent + 1) (Ast_c.str_of_info i) start

and pp_define_kind indent kind start len =
  let strcode = get_strcode PP_DEFINE_KIND kind in
  match kind with
    Ast_c.DefineVar ->
      let strname = "DefineVar" in
      (* no info, a general characterization of the #define *)
      leaf_token indent strcode strname start len
  | Ast_c.DefineFunc(args) ->
      let strname = "DefineFunc" in
      let (start,len) = get_start_len (fun x -> x) (snd args) in
      nested_start indent strcode strname start len;
      List.iter
	(function (wstr,_) ->
	  let (start,len) = get_start_len (fun x -> x) (snd wstr) in
	  leaf_string indent (Ast_c.unwrap wstr) start)
	(Ast_c.unwrap args);
      nested_end indent
  | Ast_c.Undef ->
      let strname = "Undef" in
      (* no info, a general characterization of the #define *)
      leaf_token indent strcode strname start len

and pp_define_val indent d parent_start parent_len =
  let strcode = get_strcode PP_DEFINE_VAL d in
  match d with
    Ast_c.DefineExpr(exp) ->
      let strname = "DefineExpr" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_expr exp in
      nested_start indent strcode strname start len;
      pp_expression (indent + 1) exp;
      nested_end indent
  | Ast_c.DefineStmt(stm) ->
      let strname = "DefineStmt" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_stmt stm in
      nested_start indent strcode strname start len;
      pp_statement (indent + 1) stm;
      nested_end indent
  | Ast_c.DefineType(ft) ->
      let strname = "DefineType" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_type ft in
      nested_start indent strcode strname start len;
      pp_fullType (indent + 1) ft;
      nested_end indent
  | Ast_c.DefineDoWhileZero se ->
      let strname = "DefineDoWhileZero" in
      let (start,len) = get_start_len (fun x -> x) (snd se) in
      let (stm,exp) = Ast_c.unwrap se in
      nested_start indent strcode strname start len;
      pp_statement (indent + 1) stm;
      pp_expression (indent + 1) exp;
      nested_end indent
  | Ast_c.DefineFunction(def) ->
      let strname = "DefineFunction" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_def def in
      nested_start indent strcode strname start len;
      pp_definition (indent + 1) def;
      nested_end indent
  | Ast_c.DefineInit(init) ->
      let strname = "DefineInit" in
      let (start,len) = get_start_len Lib_parsing_c.ii_of_ini init in
      nested_start indent strcode strname start len;
      pp_initialiser (indent + 1) init;
      nested_end indent
  | Ast_c.DefineMulti(stmts) ->
      let strname = "DefineMulti" in
      (match (stmts,List.rev stmts) with
	((first::_),(last::_)) ->
	  let (start,_) = get_start_len Lib_parsing_c.ii_of_stmt first in
	  let (start2,len) = get_start_len Lib_parsing_c.ii_of_stmt last in
	  let len = start2 - start + len in
	  nested_start indent strcode strname start len;
	  List.iter (pp_statement (indent + 1)) stmts;
	  nested_end indent
      | _ -> failwith "bad DefineMulti")
  | Ast_c.DefineText(wstr) ->
      let strname = "DefineText" in
      let (start,len) = get_start_len (fun x -> x) (snd wstr) in
      let s = Ast_c.unwrap wstr in
      leaf indent strcode strname start (String.length s) s
  | Ast_c.DefineEmpty ->
      let strname = "DefineEmpty" in
      leaf_token indent strcode strname  parent_start parent_len
  | Ast_c.DefineTodo ->
      let strname = "DefineTodo" in
      leaf_token indent strcode strname  parent_start parent_len

and pp_pragmainfo indent pi =
  let strcode = get_strcode PP_PRAGMAINFO pi in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_pragmainfo pi in
  match pi with
    Ast_c.PragmaTuple args ->
      let strname = "PragmaTuple" in
      nested_start indent strcode strname start len;
      pp_arg_list (indent + 1) (fst args) (snd args);
      nested_end indent
  | Ast_c.PragmaIdList _ ->
      failwith "PragmaIdList not supported"

and pp_ifdef_directive indent id =
  let strcode = get_strcode PP_IFDEF_DIRECTIVE id in
  match id with
    Ast_c.IfdefDirective(dir) ->
      let strname = "IfdefDirective" in
      let ii = snd dir in
      let (start,len) = get_start_len (fun x -> x) ii in
      nested_start indent strcode strname start len;
      List.iter (pp_token_leaf (indent + 1)) ii;
      nested_end indent

(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
(* ------------------------------------------------------------------------- *)
and pp_toplevel indent tl =
  let strcode = get_strcode PP_TOPLEVEL tl in
  let (start,len) = get_start_len Lib_parsing_c.ii_of_toplevel tl in
  match tl with
  | Ast_c.Declaration decl ->
      let strname = "Declaration" in
      nested_start indent strcode strname start len;
      pp_decl (indent + 1) decl;
      nested_end indent
  | Ast_c.Definition def ->
      let strname = "Definition" in
      nested_start indent strcode strname start len;
      pp_definition (indent + 1) def;
      nested_end indent
  | Ast_c.CppTop cppd ->
      let strname = "CppTop" in
      nested_start indent strcode strname start len;
      pp_cpp_directive (indent + 1) cppd;
      nested_end indent
  | Ast_c.IfdefTop ifd ->
      let strname = "IfdefTop" in
      nested_start indent strcode strname start len;
      pp_ifdef_directive (indent + 1) ifd;
      nested_end indent
  | Ast_c.MacroTop _ -> failwith "seems never to be used"
  | Ast_c.EmptyDef il ->
      let strname = "EmptyDef" in
      nested_start indent strcode strname start len;
      List.iter (pp_token_leaf (indent + 1)) il;
      nested_end indent
  | Ast_c.NotParsedCorrectly il ->
      let strname = "NotParsedCorrectly" in
      nested_start indent strcode strname start len;
      List.iter (pp_token_leaf (indent + 1)) il;
      nested_end indent
  | Ast_c.FinalDef info ->
      let strname = "FinalDef" in
      let s = Ast_c.str_of_info info in
      leaf indent strcode strname start len s
  | Ast_c.Namespace _ -> failwith "not supporting c++"

(* ------------------------------------------------------------------------- *)
and pp_program tll =
  let strcode = get_entry_strcode PP_PROGRAM in
  match (tll,List.rev tll) with
    ((first::_),(last::_)) ->
      let strname = "Program" in
      let (start,_) = get_start_len Lib_parsing_c.ii_of_toplevel first in
      let (start2,len) = get_start_len Lib_parsing_c.ii_of_toplevel last in
      let len = start2 - start + len in
      nested_start 0 strcode strname start len;
      List.iter (pp_toplevel 1) tll;
      nested_end 0
  | _ -> failwith "expected empty program"
