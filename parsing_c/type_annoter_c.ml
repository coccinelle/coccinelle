(* Copyright (C) 2007, 2008 Yoann Padioleau
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
(* can either:
 *
 *  - do a kind of inferer
 *     * can first do a simple inferer, that just pass context
 *     * then a real inferer, managing partial info.
 *    type context = fullType option
 *
 *  - extract the information from the .h files
 *   (so no inference at all needed)
 * 
 * todo: expression contain types, and statements,   which in turn can contain
 * expression, so need recurse. Need define an annote_statement and 
 * annotate_type.

 * todo: how deal with typedef isomorphisms ? How store them in Ast_c ?
 * store all posible variations in ast_c ? a list of type instead of just
 * the type ?
 * 
 * todo: define a new type ? like type_cocci ? where have a bool ?
 * 
 * How handle scope ? When search for type of field, we return 
 * a type, but this type makes sense only in a certain scope.
 * We could add a tag to each typedef, structUnionName to differentiate 
 * them and also associate in ast_c to the type the scope
 * of this type, the env that were used to define this type.
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2 s = 
  if !Flag_parsing_c.verbose_type
  then Common.pr2 s

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

(* the different namespaces from stdC manual:
 * 
 * You introduce two new name spaces with every block that you write.
 * One name space includes all functions, objects, type definitions,
 * and enumeration constants that you declare or define within the
 * block. The other name space includes all enumeration, structure, and
 * union tags that you define within the block.
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


(* the wrap for StructUnionNameDef contain the whole ii, the i for
 * the string, the structUnion and the structType
 *)
type namedef = 
  | VarOrFunc of string * Ast_c.exp_type
  | TypeDef of string * fullType
  | StructUnionNameDef of string * (structUnion * structType) wrap
  (* todo: EnumConstant *)
  (* todo: EnumDef *)

(* because have nested scope, have nested list, hence the list list *)
type environment = namedef list list 

let initial_env = [
  [VarOrFunc("NULL",(Lib.al_type (Parse_c.type_of_string "void *"),
		     Ast_c.NotLocalVar))]
]


let rec lookup_env f env = 
  match env with 
  | [] -> raise Not_found
  | []::zs -> lookup_env f zs
  | (x::xs)::zs -> 
      match f x with
      | None -> lookup_env f (xs::zs)
      | Some y -> y, xs::zs

    

let lookup_var s env = 
  let f = function
    | VarOrFunc (s2, typ) -> if s2 = s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let lookup_typedef s env = 
  let f = function
    | TypeDef (s2, typ) -> if s2 = s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let lookup_structunion (_su, s) env =
  let f = function
    | StructUnionNameDef (s2, typ) -> if s2 = s then Some typ else None
    | _ -> None
  in
  lookup_env f env

let member_env lookupf env = 
  try 
    let _ = lookupf env in
    true
  with Not_found -> false

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




let rec type_unfold_one_step ty env = 

  match Ast_c.unwrap_typeC ty with 
  | BaseType x  -> ty
  | Pointer t -> ty
  | Array (e, t) -> ty
  | StructUnion (sopt, su, fields) -> ty
      
  | FunctionType t -> ty
  | Enum  (s, enumt) -> ty
  | EnumName s -> ty
      
  | StructUnionName (su, s) -> 
      (try 
          let (((su,fields),ii), env') = lookup_structunion (su, s) env in
          Ast_c.nQ, (StructUnion (su, Some s, fields), ii)
          (* old: +> Ast_c.rewrap_typeC ty 
           * but must wrap with good ii, otherwise pretty_print_c
           * will be lost and raise some Impossible
           *)
       with Not_found -> 
         ty
      )
      
  | TypeName (s,_typ) -> 
      (try 
          let (t', env') = lookup_typedef s env in
          type_unfold_one_step t' env'
        with Not_found -> 
          ty
      )
      
  | ParenType t -> type_unfold_one_step t env
  | TypeOfExpr e -> 
      pr2_once ("Type_annoter: not handling typeof");
      ty
  | TypeOfType t -> type_unfold_one_step t env



let (type_field: 
  string -> (Ast_c.structUnion * Ast_c.structType) -> Ast_c.fullType) = 
  fun fld (su, fields) -> 
    fields +> Common.find_some (fun x -> 
      match Ast_c.unwrap x with
      | DeclarationField (FieldDeclList (onefield_multivars, iiptvirg)) -> 
          Common.optionise (fun () -> 
            onefield_multivars +> Common.find_some (fun fieldkind -> 

              match Ast_c.unwrap (Ast_c.unwrap fieldkind) with
              | Simple (Some s, t) | BitField (Some s, t, _) -> 
                  if s = fld then Some t else None
              | _ -> None
            )
          )
      | EmptyField -> None
      | MacroStructDeclTodo -> pr2 "DeclTodo"; None
      | CppDirectiveStruct _
      | IfdefStruct _ -> pr2 "StructCpp"; None
    )




let structdef_to_struct_name ty = 
  match ty with 
  | qu, (StructUnion (su, sopt, fields), iis) -> 
      (match sopt,iis with
      | Some s , [i1;i2;i3;i4] -> 
          qu, (StructUnionName (su, s), [i1;i2])
      | None, _ -> 
          ty
          
      | x -> raise Impossible
      )
  | _ -> raise Impossible



let rec typedef_fix ty env = 

  match Ast_c.unwrap_typeC ty with 
  | BaseType x  -> ty
  | Pointer t -> Pointer (typedef_fix t env)  +> Ast_c.rewrap_typeC ty
  | Array (e, t) -> Array (e, typedef_fix t env) +> Ast_c.rewrap_typeC ty
  | StructUnion (su, sopt, fields) -> structdef_to_struct_name ty
  | FunctionType ft -> 
      (FunctionType ft) (* todo ? *) +> Ast_c.rewrap_typeC ty
  | Enum  (s, enumt) -> 
      (Enum  (s, enumt)) (* todo? *) +> Ast_c.rewrap_typeC ty
  | EnumName s -> 
      (EnumName s) (* todo? *) +> Ast_c.rewrap_typeC ty

  (* we prefer StructUnionName to StructUnion when it comes to typed metavar *)
  | StructUnionName (su, s) -> ty
      
  | TypeName (s, _typ) -> 
      (try 
          let (t', env') = lookup_typedef s env in
          TypeName (s, Some (typedef_fix t' env)) +> Ast_c.rewrap_typeC ty
       with Not_found -> 
          ty
      )
      
  | ParenType t -> typedef_fix t env
  | TypeOfExpr e -> 
      pr2_once ("Type_annoter: not handling typeof");
      ty

  | TypeOfType t -> typedef_fix t env

(*****************************************************************************)
(* (Semi) Globals, Julia's style *)
(*****************************************************************************)

(* opti: cache ? use hash ? *)
let _scoped_env = ref initial_env

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

(* sort of hackish... *)
let islocal info =
  if List.length (!_scoped_env) = List.length initial_env
  then Ast_c.NotLocalVar
  else Ast_c.LocalVar info

(* the warning argument is here to allow some binding to overwrite an 
 * existing one. With function, we first have the protype and then the def
 * and the def binding the same string is not an error.
 * todo?: but if we define two times the same function, then we will not
 * detect it :( would require to make a diff between adding a binding 
 * from a prototype and from a definition.
 *)
let add_binding namedef warning = 
  let (current_scope, _older_scope) = Common.uncons !_scoped_env in

  (match namedef with
  | VarOrFunc (s, typ) -> 
      if Hashtbl.mem !_notyped_var s
      then pr2 ("warning: found typing information for a variable that was" ^
                   "previously unknown:" ^ s);
  | _ -> ()
  );

  let (memberf, s) = 
    (match namedef with
    | VarOrFunc (s, typ) -> member_env (lookup_var s), s
    | TypeDef   (s, typ) -> member_env (lookup_typedef s), s
    | StructUnionNameDef (s, (su, typ)) -> 
        member_env (lookup_structunion (su, s)), s
    ) in

  if  memberf [current_scope] && warning
  then pr2 ("Type_annoter: warning, " ^ s ^ 
            " is already in current binding" ^ "\n" ^
           " so there is a wierd shadowing");
  add_in_scope namedef
  

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let make_info t = (Some t,Ast_c.NotTest)

let type_of_s s = 
  (Lib.al_type (Parse_c.type_of_string s), Ast_c.NotLocalVar)

let noTypeHere = (None,Ast_c.NotTest)

let do_with_type f (t,_test) = 
  match t with
  | None -> noTypeHere
  | Some (t,_local) -> f t

  
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* catch all the decl to grow the environment *)

let rec (annotate_program2 : 
 environment -> toplevel list -> (toplevel * environment Common.pair) list
 ) = fun env prog ->

  (* globals (re)initialialisation *) 
  _scoped_env := env;
  _notyped_var := (Hashtbl.create 100);


  let bigf = { Visitor_c.default_visitor_c with 

    Visitor_c.kexpr = (fun (k,bigf) expr -> 
      k expr; (* recurse to set the types-ref of sub expressions *)
      let ty = 
        match Ast_c.unwrap_expr expr with
        (* todo: should analyse the 's' for int to know if unsigned or not *)
        | Constant (String (s,kind)) ->  make_info (type_of_s "char *")
        | Constant (Char   (s,kind)) ->  make_info (type_of_s "char")
        | Constant (Int (s)) ->          make_info (type_of_s "int")
        | Constant (Float (s,kind)) -> 
            let iinull = [] in
            make_info
	      ((Ast_c.nQ, (BaseType (FloatType kind), iinull)),
	       Ast_c.NotLocalVar)

        (* don't want a warning on the Ident that are a FunCall *)
        | FunCall (((Ident f, typ), ii), args) -> 
            args +> List.iter (fun (e,ii) -> 
              Visitor_c.vk_argument bigf e
            );
            noTypeHere
            
        | Ident (s) -> 
          (match (Common.optionise (fun () -> lookup_var s !_scoped_env)) with
          | Some ((typ,local),_nextenv) -> 
              make_info ((typedef_fix typ !_scoped_env),local)
          | None  -> 
              if not (s =~ "[A-Z_]+") (* if macro then no warning *)
              then 
                if not (Hashtbl.mem !_notyped_var s)
                then begin 
                  pr2 ("Type_annoter: not finding type for " ^ s);
                  Hashtbl.add !_notyped_var s true;
                end;
              noTypeHere
          )
        | Unary (e, UnMinus) | Unary (e, UnPlus) -> make_info (type_of_s "int")
        | Unary (e, DeRef)
        | ArrayAccess (e, _) ->
          (Ast_c.get_type_expr e) +> do_with_type (fun t -> 
            (* todo: maybe not good env !! *)
            match Ast_c.unwrap_typeC (type_unfold_one_step t !_scoped_env) with
            | Pointer x  
            | Array (_, x) -> 
                make_info ((typedef_fix x !_scoped_env),Ast_c.NotLocalVar)
            | _ -> noTypeHere
          )

        | RecordAccess  (e, fld) ->  
          (Ast_c.get_type_expr e) +> do_with_type (fun t -> 
            match Ast_c.unwrap_typeC (type_unfold_one_step t !_scoped_env) with
            | StructUnion (su, sopt, fields) -> 
                (try 
                  (* todo: which env ? *)
                  make_info
		    ((typedef_fix (type_field fld (su, fields)) !_scoped_env),
		     Ast_c.NotLocalVar)
                with Not_found -> 
                  pr2 
                    ("TYPE-ERROR: field '" ^ fld ^ "' does not belong in" ^
                     " struct '"^(match sopt with Some s -> s |_ -> "<anon>")^
                     "'");
                  noTypeHere
                )
            | _ -> noTypeHere
          )

        | RecordPtAccess (e, fld) -> 
          (Ast_c.get_type_expr e) +> do_with_type (fun t ->
          match Ast_c.unwrap_typeC (type_unfold_one_step t !_scoped_env) with 
          | Pointer (t) -> 
              (match Ast_c.unwrap_typeC (type_unfold_one_step t !_scoped_env) 
               with
              | StructUnion (su, sopt, fields) -> 
                (try 
                  (* todo: which env ? *)
                  make_info
		    ((typedef_fix (type_field fld (su, fields)) !_scoped_env),
		     Ast_c.NotLocalVar)
                 with Not_found -> 
                  pr2 
                    ("TYPE-ERROR: field '" ^ fld ^ "' does not belong in" ^
                     " struct '"^(match sopt with Some s -> s |_ -> "<anon>")^
                     "'");
                  noTypeHere
                )

              | _ -> noTypeHere
              )
          | _ -> noTypeHere
          )
        | Cast (t, e) -> 
            (* todo: add_types_expr [t] e ? *)
            make_info
	      ((typedef_fix (Lib.al_type t) !_scoped_env),Ast_c.NotLocalVar)

         (* todo: check e2 ? *)
        | Assignment (e1, op, e2) -> 
            Ast_c.get_type_expr e1
        | ParenExpr e -> 
            Ast_c.get_type_expr e

        | _ -> noTypeHere
      in
      Ast_c.set_type_expr expr ty
      
    );

    Visitor_c.kstatement = (fun (k, bigf) st -> 
      match st with 
      | Compound statxs, ii -> do_in_new_scope (fun () -> k st);
      | _ -> k st

    );
    Visitor_c.kdecl = (fun (k, bigf) d -> 
      (match d with
      | (DeclList (xs, ii)) -> 
          xs +> List.iter (fun ({v_namei = var; v_type = t;
                                 v_storage = sto; v_local = local}, iicomma) -> 

	    let local =
	      match local with
		Ast_c.NotLocalDecl -> Ast_c.NotLocalVar
	      |	Ast_c.LocalDecl -> Ast_c.LocalVar (offset t) in

            (* to add possible definition in type found in Decl *)
            Visitor_c.vk_type bigf t;
            
            var +> do_option (fun ((s, ini), ii_s_ini) -> 
              match sto with 
              | StoTypedef, _inline -> 
                  add_binding (TypeDef (s,Lib.al_type t)) true;
              | _ -> 
                  add_binding (VarOrFunc (s, (Lib.al_type t, local))) true;
                  (* int x = sizeof(x) is legal so need process ini *)
                  ini +> Common.do_option (fun ini -> 
                    Visitor_c.vk_ini bigf ini);
            );
          );
      | _ -> k d
      );
        
    );

    Visitor_c.ktype = (fun (k, bigf) typ -> 
      let (q, t) = Lib.al_type typ in
      match t with 
      | StructUnion  (su, Some s, structType),ii -> 
          add_binding (StructUnionNameDef (s, ((su, structType),ii))) true;
          k typ (* todo: restrict ? new scope so use do_in_scope ? *)


      (* TODO: if have a TypeName, then maybe can fill the option
       * information.
       *)
      | _ -> k typ
          
    );    

    Visitor_c.ktoplevel = (fun (k, bigf) elem -> 
      _notyped_var := Hashtbl.create 100;
      match elem with
      | Definition def -> 
          let {f_name = funcs;
               f_type = ((returnt, (paramst, b)) as ftyp);
               f_storage = sto;
               f_body = statxs},ii = def
          in
          let (i1, i2) = 
            match ii with 
            | is::iifunc1::iifunc2::ibrace1::ibrace2::ifakestart::isto -> 
                iifunc1, iifunc2
            | _ -> raise Impossible
          in
          let typ' = Lib.al_type (Ast_c.nQ, (FunctionType ftyp, [i1;i2])) in

          add_binding (VarOrFunc (funcs, (typ',islocal i1.Ast_c.pinfo))) false;
          do_in_new_scope (fun () -> 
            paramst +> List.iter (fun (((b, s, t), _),_) -> 
              match s with 
              | Some s ->
		  let local = Ast_c.LocalVar (offset t) in
		  add_binding (VarOrFunc (s,(Lib.al_type t,local))) true
              | None -> pr2 "no type, certainly because Void type ?"
            );
            k elem
          );
      | _ -> k elem
    );
  } 
  in

  prog +> List.map (fun elem -> 
    let beforeenv = !_scoped_env in
    Visitor_c.vk_toplevel bigf elem;
    let afterenv = !_scoped_env in
    (elem, (beforeenv, afterenv))
  )

and offset (_,(ty,iis)) =
  match iis with
    ii::_ -> ii.Ast_c.pinfo
  | _ -> failwith "type has no text; need to think again"
  
    
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
    | _ -> () in

  let bigf = { Visitor_c.default_visitor_c with
    Visitor_c.kexpr = (fun (k,bigf) expr ->
      (match unwrap expr with
	(CondExpr(e,_,_),_) -> propagate_test e
      |	_ -> ());
      k expr);
    Visitor_c.kstatement = (fun (k, bigf) st ->
      match unwrap st with 
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
      | _ -> k st) } in
  (prog +> List.iter (fun elem -> 
    Visitor_c.vk_toplevel bigf elem
  ))

let annotate_program a types_needed = 
  Common.profile_code "annotate_type"
    (fun () prog ->
      let res =
	if true (*types_needed*)
	then annotate_program2 a prog
	else prog +> List.map (fun c -> c, (initial_env, initial_env)) in
      annotate_test_expressions prog;
      res)
