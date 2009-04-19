(* Yoann Padioleau 
 *
 * Copyright (C) 2007, 2008, 2009 University of Urbana Champaign
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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* What info do we want in a clean C type ? Normally it would help
 * if we remove some of the complexity of C with for instance typedefs
 * by expanding those typedefs or structname and enumname to their
 * final value. Then, when we do pattern matching we can conveniently forget
 * to handle the typedef, enumname and similar cases. But sometimes,
 * in coccinelle for instance, we want to keep some of those original
 * info. So right now we have a in-the-middle solution by keeping
 * the original typename in the ast and expanding some of them
 * in the type_annotation phase. We don't do this expansion for
 * structname because usually when we have a struct we actually
 * prefer to just have the structname. It's only when we access
 * field that we need that information, but the type_annotater has
 * already done this job so no need in the parent expression to know
 * the full definition of the structure. But for typedef, this is different.
 * 
 * So really the finalType we want, the completed_type notion below,
 * corresponds to a type we think is useful enough to work on, to do
 * pattern matching on, and one where we have all the needed information
 * and we don't need to look again somewhere else to get the information.
 *
 * 
 * 
 * 
 * todo? define a new clean fulltype ? as julia did with type_cocci.ml
 * without the parsing info, with some normalization (for instance have
 * only structUnionName and enumName, and remove the ParenType), some
 * abstractions (don't care for instance about name in parameters of
 * functionType, or size of array), and with new types such as Unknown
 * or PartialFunctionType (when don't have type of return when infer
 * the type of function call not based on type of function but on the
 * type of its arguments).
 * 
 * 
 * 
 *)

type finalType = Ast_c.fullType

type completed_and_simplified = Ast_c.fullType

type completed_typedef = Ast_c.fullType
type removed_typedef = Ast_c.fullType


(* normally if the type annotated has done a good job, this should always
 * return true. Cf type_annotater_c.typedef_fix.
 *)
let rec is_completed_and_simplified ty = 
  match Ast_c.unwrap_typeC ty with 
  | BaseType x  -> true
  | Pointer t -> is_completed_and_simplified t
  | Array (e, t) -> is_completed_and_simplified t
  | StructUnion (su, sopt, fields) -> 
      (* recurse fields ? Normally actually don't want, 
       * prefer to have a StructUnionName when it's possible *)
      (match sopt with
      | None -> true
      | Some _ -> false (* should have transformed it in a StructUnionName *)
      )
  | FunctionType ft -> 
      (* todo? return type is completed ? params completed ? *)
      true
  | Enum  (s, enumt) -> 
      true
  | EnumName s -> 
      true

  (* we prefer StructUnionName to StructUnion when it comes to typed metavar *)
  | StructUnionName (su, s) -> true

  (* should have completed with more information *)
  | TypeName (_name, typ) -> 
      (match typ with
      | None -> false
      | Some t -> 
          (* recurse cos what if it's an alias of an alias ? *)
          is_completed_and_simplified t
      )

  (* should have removed paren, for better matching with typed metavar.
   * kind of iso again *)
  | ParenType t -> 
      false
  (* same *)
  | TypeOfType t -> 
      false

  | TypeOfExpr e -> 
      true (* well we don't handle it, so can't really say it's completed *)


let is_completed_typedef_fullType x = raise Todo  

let is_removed_typedef_fullType x = raise Todo
  
(*****************************************************************************)
(* more "virtual" fulltype, the fullType_with_no_typename *)
(*****************************************************************************)
let remove_typedef x = raise Todo

(*****************************************************************************)
(* expression exp_info annotation vs finalType *)
(*****************************************************************************)

(* builders, needed because julia added gradually more information in 
 * the expression reference annotation in ast_c.
 *)

let make_info x = 
  (Some x, Ast_c.NotTest)

let make_exp_type t = 
  (t, Ast_c.NotLocalVar)

let make_info_def t = 
  make_info (make_exp_type t)



let noTypeHere = 
  (None, Ast_c.NotTest)





let do_with_type f (t,_test) = 
  match t with
  | None -> noTypeHere
  | Some (t,_local) -> f t

let get_opt_type e = 
  match Ast_c.get_type_expr e with
  | Some (t,_), _test -> Some t
  | None, _test -> None



(*****************************************************************************)
(* Normalizers *)
(*****************************************************************************)


let structdef_to_struct_name ty = 
  match ty with 
  | qu, (StructUnion (su, sopt, fields), iis) -> 
      (match sopt,iis with
      (* todo? but what if correspond to a nested struct def ? *)
      | Some s , [i1;i2;i3;i4] -> 
          qu, (StructUnionName (su, s), [i1;i2])
      | None, _ -> 
          ty
      | x -> raise Impossible
      )
  | _ -> raise Impossible


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)


let type_of_function (def,ii) = 
  let ftyp = def.f_type in 

  (* could use the info in the 'ii' ? *)

  let fake = Ast_c.fakeInfo (Common.fake_parse_info) in
  let fake_oparen = Ast_c.rewrap_str "(" fake in
  let fake = Ast_c.fakeInfo (Common.fake_parse_info) in
  let fake_cparen = Ast_c.rewrap_str ")" fake in

  Ast_c.nQ, (FunctionType ftyp, [fake_oparen;fake_cparen])


(* pre: only a single variable *)
let type_of_decl decl = 
  match decl with
  | Ast_c.DeclList (xs,ii1) -> 
      (match xs with
      | [] -> raise Impossible
          
      (* todo? for other xs ? *)
      | (x,ii2)::xs -> 
          let {v_namei = _var; v_type = v_type;
               v_storage = (_storage,_inline)} = x in

          (* TODO normalize ? what if nested structure definition ? *)
          v_type
      )
  | Ast_c.MacroDecl _ -> 
      pr2_once "not handling MacroDecl type yet";
      raise Todo



(* pre: it is indeed a struct def decl, and only a single variable *)
let structdef_of_decl decl = 

  match decl with
  | Ast_c.DeclList (xs,ii1) -> 
      (match xs with
      | [] -> raise Impossible
          
      (* todo? for other xs ? *)
      | (x,ii2)::xs -> 
          let {v_namei = var; v_type = v_type;
               v_storage = (storage,inline)} = x in
          
          (match Ast_c.unwrap_typeC v_type with
          | Ast_c.StructUnion (su, _must_be_some, fields) -> 
              (su, fields)
          | _ -> raise Impossible
          )
      )
  | Ast_c.MacroDecl _ -> raise Impossible




(*****************************************************************************)
(* Type builder  *)
(*****************************************************************************)

let (fake_function_type: 
   fullType option -> argument wrap2 list -> fullType option) = 
 fun rettype args ->

  let fake = Ast_c.fakeInfo (Common.fake_parse_info) in
  let fake_oparen = Ast_c.rewrap_str "(" fake in
  let fake = Ast_c.fakeInfo (Common.fake_parse_info) in
  let fake_cparen = Ast_c.rewrap_str ")" fake in

  let (tyargs: parameterType wrap2 list) = 
    args +> Common.map_filter (fun (arg,ii) -> 
      match arg with
      | Left e -> 
          (match Ast_c.get_onlytype_expr e with
          | Some ft -> 
              let paramtype = 
                { Ast_c.p_namei = None;
                  p_register = false, Ast_c.noii;
                  p_type = ft;
                }
              in
              Some (paramtype, ii)
          | None -> None
          )
      | Right _ -> None
    )
  in
  if List.length args <> List.length tyargs
  then None
  else
    rettype +> Common.map_option (fun rettype -> 
      let (ftyp: functionType) = (rettype, (tyargs, (false,[]))) in
      let (t: fullType) = 
        (Ast_c.nQ, (FunctionType ftyp, [fake_oparen;fake_cparen]))  
      in
      t
    )


(*****************************************************************************)
(* Typing rules *)
(*****************************************************************************)


(* todo: the rules are far more complex, but I prefer to simplify for now.
 * todo: should take operator as a parameter.
 * 
 * todo: Also need handle pointer arithmetic! the type of 'pt + 2'
 * is still the type of pt. cf parsing_cocci/type_infer.ml
 * 
 * (* pad: in pointer arithmetic, as in ptr+1, the lub must be ptr *)
 * | (T.Pointer(ty1),T.Pointer(ty2)) ->
 * T.Pointer(loop(ty1,ty2))
 * | (ty1,T.Pointer(ty2)) -> T.Pointer(ty2)
 * | (T.Pointer(ty1),ty2) -> T.Pointer(ty1)
 * 
*)
let lub t1 t2 = 
  let ftopt = 
    match t1, t2 with
    | None, None -> None
    | Some t, None -> Some t
    | None, Some t -> Some t
    (* check equal ? no cos can have pointer arithmetic so t2 can be <> t1 
     * 
     * todo: right now I favor the first term because usually pointer 
     * arithmetic are written with the pointer in the first position.
     * 
     * Also when an expression contain a typedef, as in 
     * 'dma_addr + 1' where dma_addr was declared as a varialbe
     * of type dma_addr_t, then again I want to have in the lub
     * the typedef and it is often again in the first position.
     * 
    *)
    | Some t1, Some t2 -> 
        let t1bis = Ast_c.unwrap_typeC t1 in
        let t2bis = Ast_c.unwrap_typeC t2 in
        (match t1bis, t2bis with
        (* todo, Pointer, Typedef, etc *)
        | _, _ -> Some t1
        )

  in
  match ftopt with
  | None -> None, Ast_c.NotTest
  | Some ft ->  Some (ft, Ast_c.NotLocalVar), Ast_c.NotTest



(*****************************************************************************)
(* type lookup *)
(*****************************************************************************)

(* old: was using some nested find_some, but easier use ref 
 * update: handling union (used a lot in sparse)
 * note: it is independent of the environment. 
*)
let (type_field: 
  string -> (Ast_c.structUnion * Ast_c.structType) -> Ast_c.fullType) = 
 fun fld (su, fields) -> 

  let res = ref [] in
    
  let rec aux_fields fields = 
    fields +> List.iter (fun x -> 
      match x with
      | DeclarationField (FieldDeclList (onefield_multivars, iiptvirg)) -> 
          onefield_multivars +> List.iter (fun (fieldkind, iicomma) -> 
            match fieldkind with
            | Simple (Some name, t) | BitField (Some name, t, _, _) -> 
                let s = Ast_c.str_of_name name in
                if s =$= fld 
                then Common.push2 t res
                else ()
                  
            | Simple (None, t) -> 
                (match Ast_c.unwrap_typeC t with

                (* union *)
                | StructUnion (Union, _, fields) -> 
                    aux_fields fields
                      
                (* Special case of nested structure definition inside 
                 * structure without associated field variable as in 
                 * struct top = { ... struct xx { int subfield1; ... }; ... }
                 * cf sparse source, where can access subfields directly.
                 * It can also be used in conjunction with union.
                 *)
                | StructUnion (Struct, _, fields) -> 
                    aux_fields fields
                      
                | _ -> ()
                )
            | _ -> ()
          )
            
      | EmptyField info -> ()
      | MacroDeclField _ -> pr2_once "DeclTodo"; ()
          
      | CppDirectiveStruct _
      | IfdefStruct _ -> pr2_once "StructCpp"; 
    )
  in
  aux_fields fields;
  match !res with
  | [t] -> t
  | [] -> 
      raise Not_found
  | x::y::xs -> 
      pr2 ("MultiFound field: " ^ fld) ;
      x
    


(*****************************************************************************)
(* helpers *)
(*****************************************************************************)


(* was in aliasing_function_c.ml before*)

(* assume normalized/completed ? so no ParenType handling to do ? 
*)
let rec is_function_type x = 
  match Ast_c.unwrap_typeC x with
  | FunctionType _ -> true
  | _ -> false


(* assume normalized/completed ? so no ParenType handling to do ? *)
let rec function_pointer_type_opt x = 
  match Ast_c.unwrap_typeC x with
  | Pointer y -> 
      (match Ast_c.unwrap_typeC y with
      | FunctionType ft -> Some ft

      (* fix *)
      | TypeName (_name, Some ft2) -> 
          (match Ast_c.unwrap_typeC ft2 with
          | FunctionType ft -> Some ft
          | _ -> None
          )

      | _ -> None
      )
  (* bugfix: for many fields in structure, the field is a typename 
   * like irq_handler_t to a function pointer 
   *)
  | TypeName (_name, Some ft) -> 
      function_pointer_type_opt ft
  (* bugfix: in field, usually it has some ParenType *)

  | ParenType ft -> 
      function_pointer_type_opt ft

  | _ -> None


