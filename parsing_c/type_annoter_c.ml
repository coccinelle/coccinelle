(* Yoann Padioleau
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2007, 2008 Ecole des Mines de Nantes,
 * Copyright (C) 2009 University of Urbana Champaign
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common

open Ast_c

module Lib = Lib_parsing_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* History:
 *  - Done a first type checker in 2002, cf typing-semantic/, but
 *    was assuming that have all type info, and so was assuming had called
 *    cpp and everything was right.
 *  - Wrote this file, in 2006?, as we added pattern matching on type
 *    in coccinelle. Partial type annotater.
 *  - Julia extended it in 2008? to have localvar/notlocalvar and
 *    test/notest information, again used by coccinelle.
 *  - I extended it in Fall 2008 to have more type information for the
 *    global analysis. I also added some optimisations to process
 *    included code faster.
 *
 *
 * Design choices. Can either do:
 *  - a kind of inferer
 *     - can first do a simple inferer, that just pass context
 *     - then a real inferer, managing partial info.
 *    type context = fullType option
 *
 *  - extract the information from the .h files
 *    (so no inference at all needed)
 *
 * Difference with julia's code in parsing_cocci/type_infer.ml:
 *  - She handles just the variable namespace. She does not type
 *    field access or enum or macros. This is because cocci programs are
 *     usually simple and have no structure definition or macro definitions
 *     that we need to type anyway.
 *  - She does more propagation.
 *  - She does not have to handle the typedef isomorphism which force me
 *    to use those typedef_fix and type_unfold_one_step
 *  - She does not handle I think the function pointer C isomorphism.
 *
 *  - She has a cleaner type_cocci without any info. In my case
 *    I need to do those ugly al_type, or generate fake infos.
 *  - She has more compact code. Perhaps because she does not have to
 *    handle the extra exp_info that she added on me :) So I need those
 *    do_with_type, make_info_xxx, etc.
 *
 * Note: if need to debug this annotater, use -show_trace_profile, it can
 * help. You can also set the typedef_debug flag below.
 *
 *
 *
 * todo: expression contain types, and statements,   which in turn can contain
 * expression, so need recurse. Need define an annote_statement and
 * annotate_type.
 *
 * todo: how deal with typedef isomorphisms ? How store them in Ast_c ?
 * store all posible variations in ast_c ? a list of type instead of just
 * the type ?
 *
 * todo: how to handle multiple possible definitions for entities like
 * struct or typedefs ? Because of ifdef, we should store list of
 * possibilities sometimes.
 *
 * todo: define a new type ? like type_cocci ? where have a bool ?
 *
 * semi: How handle scope ? When search for type of field, we return
 * a type, but this type makes sense only in a certain scope.
 * We could add a tag to each typedef, structUnionName to differentiate
 * them and also associate in ast_c to the type the scope
 * of this type, the env that were used to define this type.
 *
 * todo: handle better the search in previous env, the env'. Cf the
 * termination problem in typedef_fix when I was searching in the same
 * env.
 *
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_type

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

(* The different namespaces from stdC manual:
 *
 * You introduce two new name spaces with every block that you write.
 *
 * One name space includes all
 *  - functions,
 *  - objects,
 *  - type definitions,
 *  - and enumeration constants
 * that you declare or define within the  block.
 *
 * The other name space includes all
 *  - enumeration,
 *  - structure,
 *  - and union
 *  *tags* that you define within the block.
 *
 * You introduce a new member name space with every structure or union
 * whose content you define. You identify a member name space by the
 * type of left operand that you write for a member selection
 * operator, as in x.y or p->y. A member name space ends with the end
 * of the block in which you declare it.
 *
 * You introduce a new goto label name space with every function
 * definition you write. Each goto label name space ends with its
 * function definition.
 *)

(* But I don't try to do a type-checker, I try to "resolve" type of var
 * so don't need make difference between namespaces here.
 *
 * But, why not make simply a (string, kindstring) assoc ?
 * Because we dont want that a variable shadow a struct definition, because
 * they are still in 2 different namespace. But could for typedef,
 * because VarOrFunc and Typedef are in the same namespace.
 * But could do a record as in c_info.ml
 *)


(* This type contains all "ident" like notion of C. Each time in Ast_c
 * you have a string type (as in expression, function name, fields)
 * then you need to manage the scope of this ident.
 *
 * The wrap for StructUnionNameDef contain the whole ii, the i for
 * the string, the structUnion and the structType.
 *
 * Put Macro here ? after all the scoping rules for cpp macros is different
 * and so does not vanish after the closing '}'.
 *
 * todo: EnumDef
 *)
type namedef =
  | VarOrFunc of string * Ast_c.exp_type
  | EnumConstant of string * string option

  (* also used for macro type aliases *)
  | TypeDef   of string * fullType
  (* the structType contains nested "idents" with struct scope *)
  | StructUnionNameDef of string * (structUnion * structType) wrap

  (* cppext: *)
  | Macro        of string * (define_kind * define_val)

let print_scoped_env e =
  List.iter
    (function e ->
      List.iter
	(function
	    VarOrFunc(s,_) -> Printf.printf "%s " s
	  | EnumConstant(s,_) -> Printf.printf "%s " s
	  | TypeDef(s,t) -> Printf.printf "%s" s
	  | StructUnionNameDef(s,_) -> Printf.printf "%s " s
	  | Macro(s,_) -> Printf.printf "%s " s)
	e;
      Printf.printf "\n")
    e

(* Because have nested scope, have nested list, hence the list list.
 *
 * opti? use a hash to accelerate ? hmm but may have some problems
 * with hash to handle recursive lookup. For instance for the typedef
 * example where have mutually recursive definition of the type,
 * we must take care to not loop by starting the second search
 * from the previous environment. With the list scheme in
 * lookup_env below it's quite easy to do. With hash it may be
 * more complicated.
*)
type environment = namedef list list


(* ------------------------------------------------------------ *)
(* can be modified by the init_env function below, by
 * the file environment_unix.h
 *)
let initial_env = ref [
  [VarOrFunc("NULL",
            (Lib.al_type (Parse_c.type_of_string "void *"),
	    Ast_c.NotLocalVar));

  (*
   VarOrFunc("malloc",
            (Lib.al_type(Parse_c.type_of_string "void* ( * )(int size)"),
	    Ast_c.NotLocalVar));
   VarOrFunc("free",
            (Lib.al_type(Parse_c.type_of_string "void ( * )(void *ptr)"),
	    Ast_c.NotLocalVar));
  *)
  ]
]


let typedef_debug = ref false


(* ------------------------------------------------------------ *)
(* generic, lookup and also return remaining env for further lookup *)
let rec lookup_env2 f env =
  match env with
  | [] -> raise Not_found
  | []::zs -> lookup_env2 f zs
  | (x::xs)::zs ->
      (match f x with
      | None -> lookup_env2 f (xs::zs)
      | Some y -> y, xs::zs
      )
let lookup_env a b =
  Common.profile_code "TAC.lookup_env" (fun () -> lookup_env2  a b)



let member_env lookupf env =
  try
    let _ = lookupf env in
    true
  with Not_found -> false




(* ------------------------------------------------------------ *)


let lookup_var s env =
  let f = function
    | VarOrFunc (s2, typ) -> if s2 =$= s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let lookup_typedef s env =
  if !typedef_debug then pr2 ("looking for: " ^ s);
  let f = function
    | TypeDef (s2, typ) -> if s2 =$= s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let lookup_structunion (_su, s) env =
  let f = function
    | StructUnionNameDef (s2, typ) -> if s2 =$= s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let lookup_macro s env =
  let f = function
    | Macro (s2, typ) -> if s2 =$= s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let lookup_enum s env =
  let f = function
    | EnumConstant (s2, typ) -> if s2 =$= s then Some typ else None
    | _ -> None
  in
  lookup_env f env


let lookup_typedef a b =
  Common.profile_code "TAC.lookup_typedef" (fun () -> lookup_typedef  a b)



(*****************************************************************************)
(* "type-lookup"  *)
(*****************************************************************************)

(* find_final_type is used to know to what type a field correspond in
 * x.foo. Sometimes the type of x is a typedef or a structName in which
 * case we must look in environment to find the complete type, here
 * structUnion that contains the information.
 *
 * Because in C one can redefine in nested blocks some typedefs,
 * struct, or variables, we have a static scoping resolving process.
 * So, when we look for the type of a var, if this var is in an
 * enclosing block, then maybe its type refer to a typdef of this
 * enclosing block, so must restart the "type-resolving" of this
 * typedef from this enclosing block, not from the bottom. So our
 * "resolving-type functions" take an env and also return an env from
 * where the next search must be performed. *)

(*
let rec find_final_type ty env =

  match Ast_c.unwrap_typeC ty with
  | BaseType x  -> (BaseType x) +> Ast_c.rewrap_typeC ty

  | Pointer t -> (Pointer (find_final_type t env))  +> Ast_c.rewrap_typeC ty
  | Array (e, t) -> Array (e, find_final_type t env) +> Ast_c.rewrap_typeC ty

  | StructUnion (sopt, su) -> StructUnion (sopt, su)  +> Ast_c.rewrap_typeC ty

  | FunctionType t -> (FunctionType t) (* todo ? *) +> Ast_c.rewrap_typeC ty
  | Enum  (s, enumt) -> (Enum  (s, enumt)) (* todo? *) +> Ast_c.rewrap_typeC ty
  | EnumName s -> (EnumName s) (* todo? *) +> Ast_c.rewrap_typeC ty

  | StructUnionName (su, s) ->
      (try
          let ((structtyp,ii), env') = lookup_structunion (su, s) env in
          Ast_c.nQ, (StructUnion (Some s, structtyp), ii)
          (* old: +> Ast_c.rewrap_typeC ty
           * but must wrap with good ii, otherwise pretty_print_c
           * will be lost and raise some Impossible
           *)
       with Not_found ->
         ty
      )

  | TypeName s ->
      (try
          let (t', env') = lookup_typedef s env in
          find_final_type t' env'
        with Not_found ->
          ty
      )

  | ParenType t -> find_final_type t env
  | Typeof e -> failwith "typeof"
*)




(* ------------------------------------------------------------ *)
let rec type_unfold_one_step ty env =
  let rec loop seen ty env =

  match Ast_c.unwrap_typeC ty with
  | NoType        -> ty
  | BaseType x    -> ty
  | Pointer t     -> ty
  | Array (e, t)  -> ty

  | StructUnion (sopt, su, fields) -> ty

  | FunctionType t   -> ty
  | Enum  (s, enumt) -> ty

  | EnumName s       -> ty (* todo: look in env when will have EnumDef *)

  | StructUnionName (su, s) ->
      (try
          let (((su,fields),ii), env') = lookup_structunion (su, s) env in
          Ast_c.mk_ty (StructUnion (su, Some s, fields)) ii
          (* old: +> Ast_c.rewrap_typeC ty
           * but must wrap with good ii, otherwise pretty_print_c
           * will be lost and raise some Impossible
           *)
       with Not_found ->
         ty
      )

  | TypeName (name, _typ) ->
      let s = Ast_c.str_of_name name in
      (try
          if !typedef_debug then pr2 "type_unfold_one_step: lookup_typedef";
          let (t', env') = lookup_typedef s env in
	  if List.mem s seen (* avoid pb with recursive typedefs *)
	  then type_unfold_one_step t' env'
          else loop (s::seen) t' env
       with Not_found ->
          ty
      )

  | ParenType t -> type_unfold_one_step t env
  | TypeOfExpr e ->
      pr2_once ("Type_annoter: not handling typeof");
      ty
  | TypeOfType t -> type_unfold_one_step t env in
  loop [] ty env









(* normalizer. can be seen as the opposite of the previous function as
 * we "fold" at least for the structUnion. Should return something that
 * Type_c.is_completed_fullType likes, something that makes it easier
 * for the programmer to work on, that has all the needed information
 * for most tasks.
 *)
let rec typedef_fix ty env =
  let rec loop seen ty env =
    match Ast_c.unwrap_typeC ty with
    | NoType  ->
	ty
    | BaseType x  ->
	ty
    | Pointer t ->
	Pointer (typedef_fix t env)  +> Ast_c.rewrap_typeC ty
    | Array (e, t) ->
	Array (e, typedef_fix t env) +> Ast_c.rewrap_typeC ty
    | StructUnion (su, sopt, fields) ->
      (* normalize, fold.
	 * todo? but what if correspond to a nested struct def ?
      *)
	Type_c.structdef_to_struct_name ty
    | FunctionType ft ->
	(FunctionType ft) (* todo ? *) +> Ast_c.rewrap_typeC ty
    | Enum  (s, enumt) ->
	(Enum  (s, enumt)) (* todo? *) +> Ast_c.rewrap_typeC ty
    | EnumName s ->
	(EnumName s) (* todo? *) +> Ast_c.rewrap_typeC ty
	  
  (* we prefer StructUnionName to StructUnion when it comes to typed metavar *)
    | StructUnionName (su, s) ->
	ty
	  
  (* keep the typename but complete with more information *)
    | TypeName (name, typ) ->
	let s = Ast_c.str_of_name name in
	(match typ with
	| Some _ ->
            pr2 ("typedef value already there:" ^ s);
            ty
	| None ->
            (try
              if !typedef_debug then pr2 "typedef_fix: lookup_typedef";
              let (t', env') = lookup_typedef s env in
	      
          (* bugfix: termination bug if use env instead of env' below, because
             * can have some weird mutually recursive typedef which
             * each new type alias search for its mutual def.
	     * seen is an attempt to do better.
          *)
	      let fixed =
		if List.mem s seen
		then loop (s::seen) t' env
		else typedef_fix t' env' in
	      TypeName (name, Some fixed) +>
	      Ast_c.rewrap_typeC ty
            with Not_found ->
              ty))

  (* remove paren for better matching with typed metavar. kind of iso again *)
    | ParenType t ->
	typedef_fix t env
    | TypeOfExpr e ->
	pr2_once ("Type_annoter: not handling typeof");
	ty

    | TypeOfType t ->
	typedef_fix t env in
  loop [] ty env


(*****************************************************************************)
(* Helpers, part 1 *)
(*****************************************************************************)

let type_of_s2 s =
  (Lib.al_type (Parse_c.type_of_string s))
let type_of_s a =
  Common.profile_code "Type_c.type_of_s" (fun () -> type_of_s2 a)


(* pad: pb on:
 * /home/pad/software-os-src2/freebsd/contrib/ipfilter/netinet/ip_fil_freebsd.c
 * because in the code there is:
 *  	static iss_seq_off = 0;
 * which in the parser was generating a default int without a parse_info.
 * I now add a fake parse_info for such default int so no more failwith
 * normally.
 *)

let rec is_simple_expr expr =
  match Ast_c.unwrap_expr expr with
  (* todo? handle more special cases ? *)

  | Ident _ ->
      true
  | Constant (_)         ->
      true
  | Unary (op, e) ->
      true
  | Binary (e1, op, e2) ->
      true
  | Cast (t, e) ->
      true
  | ParenExpr (e) -> is_simple_expr e

  | _ -> false

(*****************************************************************************)
(* Typing rules *)
(*****************************************************************************)
(* now in type_c.ml *)



(*****************************************************************************)
(* (Semi) Globals, Julia's style *)
(*****************************************************************************)

(* opti: cache ? use hash ? *)
let _scoped_env = ref !initial_env

(* memoise unnanoted var, to avoid too much warning messages *)
let _notyped_var = ref (Hashtbl.create 100)

let new_scope() = _scoped_env := []::!_scoped_env
let del_scope() = _scoped_env := List.tl !_scoped_env

let do_in_new_scope f =
  begin
    new_scope();
    let res = f() in
    del_scope();
    res
  end

let add_in_scope namedef =
  let (current, older) = Common.uncons !_scoped_env in
  _scoped_env := (namedef::current)::older

(* ------------------------------------------------------------ *)

(* sort of hackish... *)
let islocal info =
  if List.length (!_scoped_env) =|= List.length !initial_env
  then Ast_c.NotLocalVar
  else Ast_c.LocalVar info

(* ------------------------------------------------------------ *)
(* the warning argument is here to allow some binding to overwrite an
 * existing one. With function, we first have the prototype and then the def,
 * and the def binding with the same string is not an error.
 *
 * todo?: but if we define two times the same function, then we will not
 * detect it :( it would require to make a diff between adding a binding
 * from a prototype and from a definition.
 *
 * opti: disabling the check_annotater flag have some important
 * performance benefit.
 *
 *)
let add_binding2 namedef warning =
  let (current_scope, _older_scope) = Common.uncons !_scoped_env in

  if !Flag_parsing_c.check_annotater then begin
    (match namedef with
    | VarOrFunc (s, typ) ->
        if Hashtbl.mem !_notyped_var s
        then pr2 ("warning: found typing information for a variable that was" ^
                     "previously unknown:" ^ s);
    | _ -> ()
    );

    let (memberf, s) =
      (match namedef with
      | VarOrFunc (s, typ) ->
          member_env (lookup_var s), s
      | TypeDef   (s, typ) ->
          member_env (lookup_typedef s), s
      | StructUnionNameDef (s, (su, typ)) ->
          member_env (lookup_structunion (su, s)), s
      | Macro (s, body) ->
          member_env (lookup_macro s), s
      | EnumConstant (s, body) ->
          member_env (lookup_enum s), s
      ) in

    if  memberf [current_scope] && warning
    then pr2 ("Type_annoter: warning, " ^ s ^
                 " is already in current binding" ^ "\n" ^
                 " so there is a weird shadowing");
  end;
  add_in_scope namedef

let add_binding namedef warning =
  Common.profile_code "TAC.add_binding" (fun () -> add_binding2 namedef warning)



(*****************************************************************************)
(* Helpers, part 2 *)
(*****************************************************************************)

let lookup_opt_env lookupf s =
  Common.optionise (fun () ->
    lookupf s !_scoped_env
  )

let unwrap_unfold_env2 typ =
  Ast_c.unwrap_typeC
    (type_unfold_one_step typ !_scoped_env)
let unwrap_unfold_env typ =
  Common.profile_code "TAC.unwrap_unfold_env" (fun () -> unwrap_unfold_env2 typ)

let typedef_fix a b =
  Common.profile_code "TAC.typedef_fix" (fun () -> typedef_fix a b)

let make_info_def_fix x =
  Type_c.make_info_def (typedef_fix x !_scoped_env)

let make_info_fix (typ, local) =
  Type_c.make_info ((typedef_fix typ !_scoped_env),local)


let make_info_def = Type_c.make_info_def

(*****************************************************************************)
(* Main typer code, put later in a visitor *)
(*****************************************************************************)

let annotater_expr_visitor_subpart = (fun (k,bigf) expr ->

  let ty =
    match Ast_c.unwrap_expr expr with

    (* -------------------------------------------------- *)
    (* todo: should analyse the 's' for int to know if unsigned or not *)
    | Constant (String (s,kind)) -> make_info_def (type_of_s "char []")
    | Constant MultiString _  -> make_info_def (type_of_s "char []")
    | Constant (Char   (s,kind)) -> make_info_def (type_of_s "char")
    | Constant (Int (s,kind)) ->
	(* this seems really unpleasant, but perhaps the type needs to be set
	   up in some way that allows pretty printing *)
	make_info_def
	  (match kind with
	  (* matches limited by what is generated in lexer_c.mll *)
	    Si(Signed,CInt) -> type_of_s "int"
	  | Si(UnSigned,CInt) -> type_of_s "unsigned int"
	  | Si(Signed,CLong) -> type_of_s "long"
	  | Si(UnSigned,CLong) -> type_of_s "unsigned long"
	  | Si(Signed,CLongLong) -> type_of_s "long long"
	  | Si(UnSigned,CLongLong) -> type_of_s "unsigned long long"
	  | _ -> failwith "unexpected kind for constant")
    | Constant (Float (s,kind)) ->
        let fake = Ast_c.fakeInfo (Common.fake_parse_info) in
        let fake = Ast_c.rewrap_str "float" fake in
        let iinull = [fake] in
        make_info_def (Ast_c.mk_ty (BaseType (FloatType kind)) iinull)


    (* -------------------------------------------------- *)
    (* note: could factorize this code with the code for Ident
     * and the other code for Funcall below. But as the Ident can be
     * a macro-func, I prefer to handle it separately. So
     * this rule can handle the macro-func, the Ident-rule can handle
     * the macro-var, and the other FunCall-rule the regular
     * function calls through fields.
     * Also as I don't want a warning on the Ident that are a FunCall,
     * easier to have a rule separate from the Ident rule.
     *)
    | FunCall (e1, args) ->
     (match Ast_c.unwrap_expr e1 with
     | Ident (ident) ->
        (* recurse *)
        args +> List.iter (fun (e,ii) ->
          (* could typecheck if arguments agree with prototype *)
          Visitor_c.vk_argument bigf e
        );
        let s = Ast_c.str_of_name ident in
        (match lookup_opt_env lookup_var s with
        | Some ((typ,local),_nextenv) ->

            (* set type for ident *)
            let tyinfo = make_info_fix (typ, local) in
            Ast_c.set_type_expr e1 tyinfo;

            (match unwrap_unfold_env typ  with
            | FunctionType (ret, params) -> make_info_def ret

            (* can be function pointer, C have an iso for that,
             * same pfn() syntax than regular function call.
             *)
            | Pointer (typ2) ->
                (match unwrap_unfold_env typ2 with
                | FunctionType (ret, params) -> make_info_def ret
                | _ -> Type_c.noTypeHere
                )
            | _ -> Type_c.noTypeHere
            )
        | None  ->

            (match lookup_opt_env lookup_macro s with
            | Some ((defkind, defval), _nextenv) ->
                (match defkind, defval with
                | DefineFunc _, DefineExpr e ->
                    let rettype = Ast_c.get_onlytype_expr e in

                    (* todo: could also set type for ident ?
                       have return type and at least type of concrete
                       parameters so can generate a fake FunctionType
                    *)
                    let macrotype_opt =
                      Type_c.fake_function_type rettype args
                    in

                    macrotype_opt +> Common.do_option (fun t ->
                      pr2 ("Type_annotater: generate fake function type" ^
                              "for macro: " ^ s);
                      let tyinfo = make_info_def_fix t in
                      Ast_c.set_type_expr e1 tyinfo;
                    );

                    Ast_c.get_type_expr e
                | DefineVar, _ ->
                    pr2 ("Type_annoter: not a macro-func: " ^ s);
                    Type_c.noTypeHere
                | Undef, _ ->
                    pr2 ("Type_annoter: not a macro-func: " ^ s);
                    Type_c.noTypeHere
                | DefineFunc _, _ ->
                    (* normally the FunCall case should have caught it *)
                    pr2 ("Type_annoter: not a macro-func-expr: " ^ s);
                    Type_c.noTypeHere
                )
            | None ->
                pr2_once ("type_annotater: no type for function ident: " ^ s);
                Type_c.noTypeHere
            )
        )


      | _e ->
        k expr;

        (Ast_c.get_type_expr e1) +> Type_c.do_with_type (fun typ ->
          (* copy paste of above *)
          (match unwrap_unfold_env typ with
          | FunctionType (ret, params) -> make_info_def ret
          | Pointer (typ) ->
              (match unwrap_unfold_env typ with
              | FunctionType (ret, params) -> make_info_def ret
              | _ -> Type_c.noTypeHere
              )
          | _ -> Type_c.noTypeHere
          )
        )
     )


    (* -------------------------------------------------- *)
    | Ident (ident) ->
        let s = Ast_c.str_of_name ident in
        (match lookup_opt_env lookup_var s with
        | Some ((typ,local),_nextenv) -> make_info_fix (typ,local)
        | None  ->
            (match lookup_opt_env lookup_macro s with
            | Some ((defkind, defval), _nextenv) ->
                (match defkind, defval with
                | DefineVar, DefineExpr e ->
                    Ast_c.get_type_expr e
                | DefineVar, _ ->
                    pr2 ("Type_annoter: not a expression: " ^ s);
                    Type_c.noTypeHere
                | DefineFunc _, _ ->
                    (* normally the FunCall case should have catch it *)
                    pr2 ("Type_annoter: not a macro-var: " ^ s);
                    Type_c.noTypeHere
                | Undef, _ ->
                    pr2 ("Type_annoter: not a expression: " ^ s);
                    Type_c.noTypeHere
                )
            | None ->
                (match lookup_opt_env lookup_enum s with
                | Some (_, _nextenv) ->
                    make_info_def (type_of_s "int")
                | None ->
                    if not (s =~ "[A-Z_]+") (* if macro then no warning *)
                    then
                      if !Flag_parsing_c.check_annotater then
                        if not (Hashtbl.mem !_notyped_var s)
                        then begin
                          pr2 ("Type_annoter: no type found for: " ^ s);
                          Hashtbl.add !_notyped_var s true;
                        end
                        else ()
                      else
                        pr2 ("Type_annoter: no type found for: " ^ s)
                    ;
                    Type_c.noTypeHere
                )
            )
        )

    (* -------------------------------------------------- *)
    (* C isomorphism on type on array and pointers *)
    | Unary (e, DeRef)
    | ArrayAccess (e, _) ->
        k expr; (* recurse to set the types-ref of sub expressions *)

        (Ast_c.get_type_expr e) +> Type_c.do_with_type (fun t ->
          (* todo: maybe not good env !! *)
          match unwrap_unfold_env t with
          | Pointer x
          | Array (_, x) ->
              make_info_def_fix x
          | _ -> Type_c.noTypeHere

        )

    | Unary (e, GetRef) ->
        k expr; (* recurse to set the types-ref of sub expressions *)

        (Ast_c.get_type_expr e) +> Type_c.do_with_type (fun t ->
          (* must generate an element so that '=' can be used
           * to compare type ?
           *)
          let fake = Ast_c.fakeInfo Common.fake_parse_info in
          let fake = Ast_c.rewrap_str "*" fake in

          let ft = Ast_c.mk_ty (Pointer t) [fake] in
          make_info_def_fix ft
        )

    (* -------------------------------------------------- *)
    (* fields *)
    | RecordAccess  (e, namefld)
    | RecordPtAccess (e, namefld) as x ->
        let fld = Ast_c.str_of_name namefld in

        k expr; (* recurse to set the types-ref of sub expressions *)

        (Ast_c.get_type_expr e) +> Type_c.do_with_type (fun t ->

          let topt =
            match x with
            | RecordAccess _ -> Some t
            | RecordPtAccess _ ->
                (match unwrap_unfold_env t with
                | Pointer (t) -> Some t
                | _ -> None
                )
            | _ -> raise (Impossible 159)

          in
          (match topt with
          | None -> Type_c.noTypeHere
          | Some t ->
              match unwrap_unfold_env t with
              | StructUnion (su, sopt, fields) ->
                  (try
                      (* todo: which env ? *)
                      make_info_def_fix
                        (Type_c.type_field fld (su, fields))
                    with
                    | Not_found ->
                        pr2 (spf
                                "TYPE-ERROR: field '%s' does not belong in struct %s"
                                fld (match sopt with Some s -> s |_ -> "<anon>"));
                        Type_c.noTypeHere
                    | Multi_found ->
                        pr2 "TAC:MultiFound";
                        Type_c.noTypeHere
                  )
              | _ -> Type_c.noTypeHere
          )
        )



    (* -------------------------------------------------- *)
    | Cast (t, e) ->
        k expr;
        (* todo: if infer, can "push" info ? add_types_expr [t] e ? *)
        make_info_def_fix (Lib.al_type t)

    (* todo? lub, hmm maybe not, cos type must be e1 *)
    | Assignment (e1, op, e2) ->
        k expr;
        (* value of an assignment is the value of the RHS expression, but its
           type is the type of the lhs expression.  Use the rhs exp if no
	   information is available *)
        (match Ast_c.get_type_expr e1 with
	  (None,_) -> Ast_c.get_type_expr e2
	| (Some ty,t) -> (Some ty,t))
    | Sequence (e1, e2) ->
        k expr;
        Ast_c.get_type_expr e2

    | Binary (e1, Logical _, e2) ->
        k expr;
        make_info_def (type_of_s "int")

    (* todo: lub *)
    | Binary (e1, Arith op, e2) ->
        k expr;
        Type_c.lub op (Type_c.get_opt_type e1) (Type_c.get_opt_type e2)

    | CondExpr (cond, e1opt, e2) ->
        k expr;
        Ast_c.get_type_expr e2


    | ParenExpr e ->
        k expr;
        Ast_c.get_type_expr e

    | Infix (e, op)  | Postfix (e, op) ->
        k expr;
        Ast_c.get_type_expr e

    (* pad: julia wrote this ? *)
    | Unary (e, UnPlus) ->
        k expr; (* recurse to set the types-ref of sub expressions *)
        make_info_def (type_of_s "int")
          (* todo? can convert from unsigned to signed if UnMinus ? *)
    | Unary (e, UnMinus) ->
        k expr; (* recurse to set the types-ref of sub expressions *)
        make_info_def (type_of_s "int")

    | SizeOfType _|SizeOfExpr _ ->
        k expr; (* recurse to set the types-ref of sub expressions *)
        make_info_def (type_of_s "size_t")

    | Constructor (ft, ini) ->
        k expr; (* recurse to set the types-ref of sub expressions *)
        make_info_def (Lib.al_type ft)

    | Unary (e, Not) ->
        k expr; (* recurse to set the types-ref of sub expressions *)
	(* the result of ! is always 0 or 1, not the argument type *)
        make_info_def (type_of_s "int")
    | Unary (e, Tilde) ->
        k expr; (* recurse to set the types-ref of sub expressions *)
        Ast_c.get_type_expr e

    (* -------------------------------------------------- *)
    (* todo *)
    | Unary (_, GetRefLabel) ->
        k expr; (* recurse to set the types-ref of sub expressions *)
        pr2_once "Type annotater:not handling GetRefLabel";
        Type_c.noTypeHere
          (* todo *)
    | StatementExpr _ ->
        k expr; (* recurse to set the types-ref of sub expressions *)
        pr2_once "Type annotater:not handling StatementExpr";
        Type_c.noTypeHere
          (*
            | _ -> k expr; Type_c.noTypeHere
          *)

    | New (_, ty) ->
	k expr;
	pr2_once "Type annotater:not handling New";
	Type_c.noTypeHere (* TODO *)

    | Delete e ->
	k expr;
	pr2_once "Type annotater:not handling Delete";
	Type_c.noTypeHere (* TODO *)

  in
  Ast_c.set_type_expr expr ty

)


(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

(* Processing includes that were added after a cpp_ast_c makes the
 * type annotater quite slow, especially when the depth of cpp_ast_c is
 * big. But for such includes the only thing we really want is to modify
 * the environment to have enough type information. We don't need
 * to type the expressions inside those includes (they will be typed
 * when we process the include file directly). Here the goal is
 * to not recurse.
 *
 * Note that as usually header files contain mostly structure
 * definitions and defines, that means we still have to do lots of work.
 * We only win on function definition bodies, but usually header files
 * have just prototypes, or inline function definitions which anyway have
 * usually a small body. But still, we win. It also makes clearer
 * that when processing include as we just need the environment, the caller
 * of this module can do further optimisations such as memorising the
 * state of the environment after each header files.
 *
 *
 * For sparse its makes the annotating speed goes from 9s to 4s
 * For Linux the speedup is even better, from ??? to ???.
 *
 * Because There would be some copy paste with annotate_program, it is
 * better to factorize code hence the just_add_in_env parameter below.
 *
 * todo? alternative optimisation for the include problem:
 *  - processing all headers files one time and construct big env
 *  - use hashtbl for env (but apparently not biggest problem)
 *)

let rec visit_toplevel ~just_add_in_env ~depth elem =
  let need_annotate_body = not just_add_in_env in

  let bigf = { Visitor_c.default_visitor_c with

    (* ------------------------------------------------------------ *)
    Visitor_c.kcppdirective = (fun (k, bigf) directive ->
      match directive with
      (* do error messages for type annotater only for the real body of the
       * file, not inside include.
       *)
      | Include {i_content = opt} ->
          opt +> Common.do_option (fun (filename, program) ->
            Common.save_excursion Flag_parsing_c.verbose_type (fun () ->
              Flag_parsing_c.verbose_type := false;

              (* old: Visitor_c.vk_program bigf program;
               * opti: set the just_add_in_env
               *)
              program +> List.iter (fun elem ->
                visit_toplevel ~just_add_in_env:true ~depth:(depth+1) elem
              )
            )
          )

      | Define ((s,ii), (defkind, defval)) ->


          (* even if we are in a just_add_in_env phase, such as when
           * we process include, as opposed to the body of functions,
           * with macros we still to type the body of the macro as
           * the macro has no type and so we infer its type from its
           * body (and one day later maybe from its use).
           *)
          (match defval with
          (* can try to optimize and recurse only when the define body
           * is simple ?
           *)

          | DefineExpr expr ->
	      (* prevent macro-declared variables from leaking out *)
	      do_in_new_scope (fun () ->
		if is_simple_expr expr
             (* even if not need_annotate_body, still recurse*)
		then k directive
		else
                  if need_annotate_body
                  then k directive)
          | _ ->
	      do_in_new_scope (fun () ->
		if need_annotate_body
		then k directive)
          );

          add_binding (Macro (s, (defkind, defval) )) true;

      | PragmaAndCo _ -> ()
    );

    (* ------------------------------------------------------------ *)
    (* main typer code *)
    (* ------------------------------------------------------------ *)
    Visitor_c.kexpr = annotater_expr_visitor_subpart;

    (* ------------------------------------------------------------ *)
    Visitor_c.kstatement = (fun (k, bigf) st ->
      match Ast_c.unwrap_st st with
      | Compound statxs -> do_in_new_scope (fun () -> k st);
      | _ -> k st
    );
    (* ------------------------------------------------------------ *)
    Visitor_c.kdecl = (fun (k, bigf) d ->
      (match d with
      | (DeclList (xs, ii)) ->
          xs +> List.iter (fun ({v_namei = var; v_type = t;
                                 v_storage = sto; v_local = local} as x
                                   , iicomma) ->

            (* to add possible definition in type found in Decl *)
            Visitor_c.vk_type bigf t;


	    let local =
	      match (sto,local) with
	      | (_,Ast_c.NotLocalDecl) -> Ast_c.NotLocalVar
	      |	((Ast_c.Sto Ast_c.Static, _), Ast_c.LocalDecl) ->
		  (match Ast_c.info_of_type t with
		    (* if there is no info about the type it must not be
		       present, so we don't know what the variable is *)
		    None -> Ast_c.NotLocalVar
		  | Some ii -> Ast_c.StaticLocalVar ii)
	      |	(_,Ast_c.LocalDecl) ->
		  (match Ast_c.info_of_type t with
		    (* if there is no info about the type it must not be
		       present, so we don't know what the variable is *)
		    None -> Ast_c.NotLocalVar
		  | Some ii -> Ast_c.LocalVar ii)
            in
            var +> Common.do_option (fun (name, iniopt) ->
              let s = Ast_c.str_of_name name in

              match sto with
              | StoTypedef, _inline ->
                  add_binding (TypeDef (s,Lib.al_type t)) true;
              | _ ->
                  add_binding (VarOrFunc (s, (Lib.al_type t, local))) true;

                  x.v_type_bis :=
                    Some (typedef_fix (Lib.al_type t) !_scoped_env);

                  if need_annotate_body then begin
                    (* int x = sizeof(x) is legal so need process ini *)
		    match iniopt with
		      Ast_c.NoInit -> ()
		    | Ast_c.ValInit(iini,init) -> Visitor_c.vk_ini bigf init
		    | Ast_c.ConstrInit((args,_)) ->
			args +> List.iter (fun (e,ii) ->
			  Visitor_c.vk_argument bigf e
			)
                  end
            );
          );
      | MacroDecl _ | MacroDeclInit _ ->
          if need_annotate_body
          then k d
      );

    );

    (* ------------------------------------------------------------ *)
    Visitor_c.ktype = (fun (k, bigf) typ ->
      (* bugfix: have a 'Lib.al_type typ' before, but because we can
       * have enum with possible expression, we don't want to change
       * the ref of abstract-lined types, but the real one, so
       * don't al_type here
       *)
      let (_q, tbis) = typ in
      match Ast_c.unwrap_typeC typ with
      | StructUnion  (su, Some s, structType) ->
          let structType' = Lib.al_fields structType in
          let ii = Ast_c.get_ii_typeC_take_care tbis in
          let ii' = Lib.al_ii ii in
          add_binding (StructUnionNameDef (s, ((su, structType'),ii')))  true;

          if need_annotate_body
          then k typ (* todo: restrict ? new scope so use do_in_scope ? *)

      | Enum (sopt, enums) ->

          enums +> List.iter (fun ((name, eopt), iicomma) ->

            let s = Ast_c.str_of_name name in

            if need_annotate_body
            then eopt +> Common.do_option (fun (ieq, e) ->
              Visitor_c.vk_expr bigf e
            );
            add_binding (EnumConstant (s, sopt)) true;
          );


      (* TODO: if have a TypeName, then maybe can fill the option
       * information.
       *)
      | _ ->
          if need_annotate_body
          then k typ

    );

    (* ------------------------------------------------------------ *)
    Visitor_c.ktoplevel = (fun (k, bigf) elem ->
      _notyped_var := Hashtbl.create 100;
      match elem with
      | Definition def ->
          let {f_name = name;
               f_type = ((returnt, (paramst, b)) as ftyp);
               f_storage = sto;
               f_body = statxs;
               f_old_c_style = oldstyle;
               },ii
            = def
          in
          let (i1, i2) =
            match ii with
	      (* what is iifunc1?  it should be a type.  jll
               * pad: it's the '(' in the function definition. The
               * return type is part of f_type.
               *)
            | iifunc1::iifunc2::ibrace1::ibrace2::ifakestart::isto ->
                iifunc1, iifunc2
            | _ -> raise (Impossible 160)
          in
          let funcs = Ast_c.str_of_name name in

          (match oldstyle with
          | None ->
              let typ' =
                Lib.al_type (Ast_c.mk_ty (FunctionType ftyp) [i1;i2]) in

              add_binding (VarOrFunc (funcs, (typ',islocal i1.Ast_c.pinfo)))
                false;

              if need_annotate_body then
                do_in_new_scope (fun () ->
                  paramst +> List.iter (fun ({p_namei= nameopt; p_type= t},_)->
                    match nameopt with
                    | Some name ->
                        let s = Ast_c.str_of_name name in
		        let local =
			  (match Ast_c.info_of_type t with
			    (* if there is no info about the type it must
			       not be present, so we don't know what the
			       variable is *)
			    None -> Ast_c.NotLocalVar
			  | Some ii -> Ast_c.LocalVar ii)
			in
		        add_binding (VarOrFunc (s,(Lib.al_type t,local))) true
                    | None ->
                    pr2 "no type, certainly because Void type ?"
                  );
                  (* recurse *)
                  k elem
                );
          | Some oldstyle ->
              (* generate regular function type *)

              pr2 "TODO generate type for function";
              (* add bindings *)
              if need_annotate_body then
                do_in_new_scope (fun () ->
                  (* recurse. should naturally call the kdecl visitor and
                   * add binding
                   *)
                  k elem;
                );

          );
      | CppTop x ->
          (match x with
          | Define ((s,ii), (DefineVar, DefineType t)) ->
              add_binding (TypeDef (s,Lib.al_type t)) true;
          | _ -> k elem
          )

      | Declaration _



      | IfdefTop _
      | MacroTop _
      | EmptyDef _
      | NotParsedCorrectly _
      | FinalDef _
      | Namespace _
          ->
          k elem
    );
  }
  in
  if just_add_in_env
  then
    if depth > 1
    then Visitor_c.vk_toplevel bigf elem
    else
      Common.profile_code "TAC.annotate_only_included" (fun () ->
        Visitor_c.vk_toplevel bigf elem
      )
  else Visitor_c.vk_toplevel bigf elem

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* catch all the decl to grow the environment *)


let rec (annotate_program2 :
  environment -> toplevel list -> (toplevel * environment Common.pair) list) =
 fun env prog ->

  (* globals (re)initialialisation *)
  _scoped_env := env;
  _notyped_var := (Hashtbl.create 100);

  prog +> List.map (fun elem ->
    let beforeenv = !_scoped_env in
    visit_toplevel ~just_add_in_env:false ~depth:0 elem;
    let afterenv = !_scoped_env in
    (elem, (beforeenv, afterenv))
  )




(*****************************************************************************)
(* Annotate test *)
(*****************************************************************************)

(* julia: for coccinelle *)
let annotate_test_expressions prog =
  let rec propagate_test e =
    let ((e_term,info),_) = e in
    let (ty,_) = !info in
    info := (ty,Test);
    match e_term with
      Binary(e1,Logical(AndLog),e2)
    | Binary(e1,Logical(OrLog),e2) -> propagate_test e1; propagate_test e2
    | Unary(e1,Not) -> propagate_test e1
    | ParenExpr(e) -> propagate_test e
    | FunCall(e,args) -> (* not very nice, but so painful otherwise *)
	(match (unwrap e,args) with
	  ((Ident(i),_),[(Left a,_)]) ->
	    let nm = str_of_name i in
	    if List.mem nm ["likely";"unlikely"]
	    then propagate_test a
		else ()
	| _ -> ())
    | _ -> () in

  let bigf = { Visitor_c.default_visitor_c with
    Visitor_c.kexpr = (fun (k,bigf) expr ->
      (match unwrap_expr expr with
	CondExpr(e,_,_) -> propagate_test e
      |	Binary(e1,Logical(AndLog),e2)
      | Binary(e1,Logical(OrLog),e2) -> propagate_test e1; propagate_test e2
      | Unary(e1,Not) -> propagate_test e1
      | _ -> ()
      );
      k expr
    );
    Visitor_c.kstatement = (fun (k, bigf) st ->
      match unwrap_st st with
	Selection(s) ->
	  (match s with If(e1,s1,s2) -> propagate_test e1 | _ -> ());
	  k st;
      |	Iteration(i) ->
	  (match i with
	    While(e,s) -> propagate_test e
	  | DoWhile(s,e) -> propagate_test e
	  | For(_,es,_,_) ->
	      (match unwrap es with Some e -> propagate_test e | None -> ())
	  | _ -> ());
	  k st
      | _ -> k st
    )
  } in
  (prog +> List.iter (fun elem ->
    Visitor_c.vk_toplevel bigf elem
  ))



(*****************************************************************************)
(* Annotate types *)
(*****************************************************************************)
let annotate_program env prog =
  Common.profile_code "TAC.annotate_program"
    (fun () ->
      let res = annotate_program2 env prog in
      annotate_test_expressions prog;
      res
    )

let annotate_type_and_localvar env prog =
  Common.profile_code "TAC.annotate_type"
    (fun () -> annotate_program2 env prog)


(*****************************************************************************)
(* changing default typing environment, do concatenation *)
let init_env filename =
  pr2 ("init_env: " ^ filename);
  let (ast2, _stat) = Parse_c.parse_c_and_cpp filename in
  let ast = Parse_c.program_of_program2 ast2 in

  let res = annotate_type_and_localvar !initial_env ast in
  match List.rev res with
  | [] -> pr2 "empty environment"
  | (_top,(env1,env2))::xs ->
      initial_env := !initial_env ++ env2;
      ()

