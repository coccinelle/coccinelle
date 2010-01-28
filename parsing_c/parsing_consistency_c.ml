(* Yoann Padioleau
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2006, 2007, 2008 Ecole des Mines de Nantes
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


(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_err, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_parsing

(*****************************************************************************)
(* Consistency checking *)
(*****************************************************************************)

(* todo:
 *  could check that an ident has always the same class, be it a typedef
 *  (but sometimes do 'acpi_val acpi_val;'), an ident, a TMacroStatement,
 *  etc.
 *)

type class_ident =
  | CIdent (* can be var, func, field, tag, enum constant *)
  | CTypedef

let str_of_class_ident = function
  | CIdent -> "Ident"
  | CTypedef -> "Typedef"

(*
  | CMacro
  | CMacroString
  | CMacroStmt
  | CMacroDecl
  | CMacroIterator
  | CAttr

(* but take care that must still be able to use '=' *)
type context = InFunction | InEnum | InStruct | InInitializer | InParams
type class_token =
  | CIdent of class_ident

  | CComment
  | CSpace
  | CCommentCpp of cppkind
  | CCommentMisc
  | CCppDirective

  | COPar
  | CCPar
  | COBrace
  | CCBrace

  | CSymbol
  | CReservedKwd (type | decl | qualif | flow | misc | attr)
*)

let ident_to_typename ident : Ast_c.fullType =
  Ast_c.mk_ty (Ast_c.TypeName  (ident, Ast_c.noTypedefDef())) Ast_c.noii


(* parse_typedef_fix4 *)
let consistency_checking2 xs =

  (* first phase, gather data *)
  let stat = Hashtbl.create 101 in

  (* default value for hash *)
  let v1 () = Hashtbl.create 101 in
  let v2 () = ref 0 in

  let bigf = { Visitor_c.default_visitor_c with

    Visitor_c.kexpr = (fun (k,bigf) x ->
      match Ast_c.unwrap_expr x with
      | Ast_c.Ident (id) ->
          let s = Ast_c.str_of_name id in
          stat +>
            Common.hfind_default s v1 +> Common.hfind_default CIdent v2 +>
            (fun aref -> incr aref)

      | _ -> k x
    );
    Visitor_c.ktype = (fun (k,bigf) t ->
      match Ast_c.unwrap_typeC t with
      | Ast_c.TypeName (name,_typ) ->
          let s = Ast_c.str_of_name name in
          stat +>
            Common.hfind_default s v1 +> Common.hfind_default CTypedef v2 +>
            (fun aref -> incr aref)

      | _ -> k t
    );
  }
  in
  xs +> List.iter (fun (p) -> Visitor_c.vk_toplevel bigf p);


  let ident_to_type = ref [] in


  (* second phase, analyze data *)
  stat +> Hashtbl.iter (fun k v ->
    let xs = Common.hash_to_list v in
    if List.length xs >= 2
    then begin
      pr2_err ("TYPEDEF CONFLICT:" ^ k);
      let sorted = xs +> List.sort (fun (ka,va) (kb,vb) ->
        if !va =|= !vb then
          (match ka, kb with
          | CTypedef, _ -> 1 (* first is smaller *)
          | _, CTypedef -> -1
          | _ -> 0
          )
        else compare !va !vb
      ) in
      let sorted = List.rev sorted in
      match sorted with
      | [CTypedef, i1;CIdent, i2] ->
          pr2_err ("transforming some ident in typedef");
          push2 k ident_to_type;
      | [CIdent, i1;CTypedef, i2] ->
          pr2_err ("TODO:typedef now used as an identifier");
      | _ ->
          pr2_err ("TODO:other transforming?");

    end
  );

  (* third phase, update ast.
   * todo? but normally should try to handle correctly scope ? maybe sometime
   * sizeof(id) and even if id was for a long time an identifier, maybe
   * a few time, because of the scope it's actually really a type.
   *)
  if (null !ident_to_type)
  then xs
  else
    let bigf = { Visitor_c.default_visitor_c_s with
      Visitor_c.kdefineval_s = (fun (k,bigf) x ->
        match x with
        | Ast_c.DefineExpr e ->
            (match Ast_c.unwrap_expr e with
            | Ast_c.Ident (ident)  ->
                let s = Ast_c.str_of_name ident in
                if List.mem s !ident_to_type
                then
                  let t = ident_to_typename ident in
                  Ast_c.DefineType t
                else k x
            | _ -> k x
            )
        | _ -> k x
      );
      Visitor_c.kexpr_s = (fun (k, bigf) x ->
        match Ast_c.get_e_and_ii x with
        | (Ast_c.SizeOfExpr e, tref), isizeof ->
            let i1 = tuple_of_list1 isizeof in
            (match Ast_c.get_e_and_ii e with
            | (Ast_c.ParenExpr e, _), iiparen ->
                let (i2, i3) = tuple_of_list2 iiparen in
                (match Ast_c.get_e_and_ii e with
                | (Ast_c.Ident (ident), _), _ii  ->

                    let s = Ast_c.str_of_name ident in
                    if List.mem s !ident_to_type
                    then
                      let t = ident_to_typename ident in
                      (Ast_c.SizeOfType t, tref),[i1;i2;i3]
                    else  k x
                | _ -> k x
                )
            | _ -> k x
            )
        | _ -> k x
      );
    } in
    xs +> List.map (fun (p) ->
      Visitor_c.vk_toplevel_s bigf p
    )


let consistency_checking a  =
  Common.profile_code "C consistencycheck" (fun () -> consistency_checking2 a)



