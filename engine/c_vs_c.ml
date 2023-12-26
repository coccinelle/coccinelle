(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common

open Ast_c

(* For the moment I do only eq_type and not eq_expr, etc. The reason
 * for eq_type is related to the typedef and struct isomorphism. Sometimes
 * one use the typedef and sometimes the structname.
 *
 * TODO: should use the isomorphisms engine of julia.
 * Maybe I can transform my ast_c in ast_cocci, and use julia's code ?
 * Maybe I can add some Disj in my ast_c ?
 *)


module type PARAM =
  sig
    type tin
    type 'x tout

    type 'a matcher = 'a -> 'a  -> tin -> 'a tout

    val (>>=):
      (tin -> 'a tout)  ->
      ('a -> (tin -> 'b tout)) ->
      (tin -> 'b tout)

    val (>&&>) : bool -> (tin -> 'x tout) -> (tin -> 'x tout)

    val return : 'a -> tin -> 'a tout
    val fail : tin -> 'a tout
end


module C_VS_C =
  functor (X : PARAM) ->
struct

type 'a matcher = 'a -> 'a  -> X.tin -> 'a X.tout

let (>>=) = X.(>>=)
let (>&&>) = X.(>&&>)
let return = X.return
let fail = X.fail

let option: 'a matcher -> ('a option matcher) = fun f t1 t2 ->
  match (t1,t2) with
  | (Some t1, Some t2) ->
      f t1 t2 >>= (fun t ->
        return (Some t)
      )
  | (None, None) -> return None
  | _ -> fail


let same_s saopt sbopt =
  match saopt, sbopt with
  | None, None -> true
  | Some namea, Some nameb ->
      let sa = Ast_c.str_of_name namea in
      let sb = Ast_c.str_of_name nameb in
      sa = sb
  | _ -> false


let rec fullType a b =
  let ((qua,iiqa), attra, tya) = a in
  let ((qub,iiqb), attrb, tyb) = b in
  let compare_attr _ =
    let sattra =
      List.sort compare (List.map Lib_parsing_c.al_attribute attra) in
    let sattrb =
      List.sort compare (List.map Lib_parsing_c.al_attribute attrb) in
    sattra = sattrb in
  (qua.const = qub.const && qua.volatile = qub.volatile &&
   qua.restrict = qub.restrict && compare_attr()) >&&>

    let (qu,iiq) = (qua, iiqa) in
    typeC tya tyb >>= (fun ty ->
      return ((qu,iiq), attra, ty)
    )

and typeC tya tyb =
  let (a, iia) = tya in
  let (b, iib) = tyb in

  let iix = iia in

  match a, b with
  | BaseType a, BaseType b ->
      a = b >&&> return (BaseType a, iix)
  | Pointer a, Pointer b ->
      fullType a b >>= (fun x -> return (Pointer x, iix))

  | StructUnionName (sua, sa), StructUnionName (sub, sb) ->
      (sua = sub && sa = sb) >&&>
        return (StructUnionName (sua, sa), iix)

  | TypeName (namea, opta), TypeName (nameb, optb) ->
      let sa = Ast_c.str_of_name namea in
      let sb = Ast_c.str_of_name nameb in

      (* assert compatible opta optb ? *)
      (*option fullType opta optb*)
      sa = sb >&&>
       let opt =
         (match opta, optb with
         | None, None -> None

         | Some x, _
         | _, Some x

             -> Some x
         )
       in
       return (TypeName (namea, opt), iix)


  | Array (ea, a), Array (eb,b) ->
      let get_option f = function Some x -> Some (f x) | None -> None in
      let ea = get_option Lib_parsing_c.al_expr ea in
      let eb = get_option Lib_parsing_c.al_expr eb in
      ea = eb >&&> fullType a b >>= (fun x -> return (Array (ea, x), iix))

  | FunctionType (returna, paramsa), FunctionType (returnb, paramsb) ->
      let (tsa, (ba,iihas3dotsa)) = paramsa in
      let (tsb, (bb,iihas3dotsb)) = paramsb in

      let bx = ba in
      let iihas3dotsx = iihas3dotsa in

      (ba = bb && List.length tsa = List.length tsb) >&&>
      fullType returna returnb >>= (fun returnx ->

      Common.zip tsa tsb +> List.fold_left
        (fun acc ((parama,iia),(paramb,iib))->
          let iix = iia in
          acc >>= (fun xs ->

            let {p_register = (ba,iiba); p_namei = saopt; p_type = ta;
		  p_endattr = endattrsa} =
              parama in
            let {p_register = (bb,iibb); p_namei = sbopt; p_type = tb;
		  p_endattr = endattrsb} =
              paramb in

            let bx = ba in
            let iibx = iiba in

            let sxopt = saopt in

            let endattrsx = endattrsa in

            (* todo?  iso on name or argument ? *)
            (ba = bb && same_s saopt sbopt && endattrsa = endattrsb) >&&>
            fullType ta tb >>= (fun tx ->
              let paramx = { p_register = (bx, iibx);
                             p_namei = sxopt;
                             p_type = tx;
                             p_endattr = endattrsx; } in
              return ((paramx,iix)::xs)
            )
          )
        ) (return [])
      >>= (fun tsx ->
        let paramsx = (List.rev tsx, (bx, iihas3dotsx)) in
        return (FunctionType (returnx, paramsx), iix)
      ))

  | EnumDef (saopt, basea, enuma), EnumDef (sbopt, baseb, enumb) ->
      (saopt = sbopt &&
       List.length enuma = List.length enumb &&
       Common.zip enuma enumb +>
       List.for_all
	 (fun (((namesa,eopta), iicommaa), ((namesb,eoptb),iicommab)) ->
           let sa = str_of_name namesa in
           let sb = str_of_name namesb in
           sa = sb &&
           (* todo ? eopta and b can have some info so ok to use = ?  *)
           eopta = eoptb)) >&&>
      option fullType basea baseb >>= (fun x ->
        return (EnumDef (saopt, x, enuma), iix))

  | EnumName (sa, ia), EnumName (sb, ib) -> (sa = sb && ia = ib) >&&> return (EnumName (sa, ia), iix)

  | ParenType a, ParenType b ->
      (* iso here ? *)
      fullType a b >>= (fun x ->
        return (ParenType x, iix)
      )

  | TypeOfExpr ea, TypeOfExpr eb ->
      let ea = Lib_parsing_c.al_expr ea in
      let eb = Lib_parsing_c.al_expr eb in
      ea = eb >&&> return (TypeOfExpr ea, iix)

  | TypeOfType a, TypeOfType b ->
      fullType a b >>= (fun x -> return (TypeOfType x, iix))

(*  | TypeOfType a, b ->
    | a, TypeOfType b ->
*)

  | AutoType, AutoType -> return (AutoType, iix)

  | StructUnion (sua, saopt, optfinala, base_classesa, sta),
      StructUnion (sub, sbopt, optfinalb, base_classesb, stb) ->
      (sua = sub && saopt = sbopt && base_classesa = base_classesb &&
       same_optfinal optfinala optfinalb &&
       List.length sta = List.length stb)
      >&&>
      (function tin ->
	(* zip is only safe if the above succeeds *)
      (Common.zip sta stb +> List.fold_left
        (fun acc ((fielda), (fieldb)) ->
          acc >>= (fun xs ->
            match fielda, fieldb with
            | EmptyField iia, EmptyField iib ->
                let iix = iia in
                return ((EmptyField iix)::xs)

            | DeclarationField (FieldDeclList (fa, iipta)),
              DeclarationField (FieldDeclList (fb, iiptb)) ->
                let iipt = iipta in (* TODO ?*)
                (List.length fa = List.length fb) >&&>
		(function tin ->
		  (* only executable if the length is correct *)
                (Common.zip fa fb +> List.fold_left
                  (fun acc2 ((fielda,iia),(fieldb,iib))->
                    let iix = iia in
                    acc2 >>= (fun xs ->
                      match fielda, fieldb with
                      | Simple (nameaopt, ta, attrsa), Simple (namebopt, tb, attrsb)
			when attrsa = attrsb ->
                          same_s nameaopt namebopt >&&>
                          fullType ta tb >>= (fun tx ->
                            return (((Simple (nameaopt, tx, attrsa)), iix)::xs)
                          )

                      | BitField (nameopta, ta, infoa, ea),
                        BitField (nameoptb, tb, infob, eb) ->
                          let infox = infoa in
                          (same_s nameopta nameoptb && ea = eb) >&&>
                          fullType ta tb >>= (fun tx ->
                            return (((BitField (nameopta,tx,infox,ea)), iix)::xs)
                          )
                      | _,_ -> fail
                    )
                  ) (return [])) tin)
                 >>= (fun fx ->
                   return (((DeclarationField
                               (FieldDeclList (List.rev fx,iipt))))::xs)
                 )
	    (* TODO: MacroDeclField args may contain typedefs that will not be
	       handled by eq_type *)
	    | (MacroDeclField _,MacroDeclField _)
	    | (CppDirectiveStruct _,CppDirectiveStruct _)
	    | (IfdefStruct _,IfdefStruct _) ->
		if Lib_parsing_c.al_field fielda = Lib_parsing_c.al_field fieldb
		then return (fielda::xs)
		else fail
            | _ -> fail
          )


        ) (return [])
        >>= (fun stx ->
          return (StructUnion (sua, saopt, optfinala, base_classesa, List.rev stx), iix)
        )) tin)

  | (TemplateType(namea, argsa), TemplateType(nameb, argsb)) ->
      let namea = Lib_parsing_c.al_type namea in
      let nameb = Lib_parsing_c.al_type nameb in
      namea = nameb >&&>
      let argsa = Lib_parsing_c.al_arguments argsa in
      let argsb = Lib_parsing_c.al_arguments argsb in
      argsa = argsb >&&> return (TemplateType(namea, argsa), iix)

  (* choose the lub.
   * subtil: in the return must put iia, not iix, and in following case
   * must put iib and not iix, because we want the token corresponding
   * to the typedef.
   *)
  | TypeName (name, Some a), _ ->
      fullType a (Ast_c.nQ, [], tyb) >>= (fun x ->
        return (TypeName (name, Some x), iia)
      )

  | _, TypeName (name, Some b) ->
      fullType b (Ast_c.nQ, [], tya) >>= (fun x ->
        return (TypeName (name, Some x), iib) (* subtil: *)
      )

  | (BaseType _, _) | (_, BaseType _)
  | (Pointer _, _) | (_, Pointer _)
  | (Array _, _) | (_, Array _)
  | (Decimal _, _) | (_, Decimal _)
  | (FunctionType _, _) | (_, FunctionType _)
  | (EnumDef _, _) | (_, EnumDef _)
  | (StructUnion _, _) | (_, StructUnion _)
  | (EnumName _, _) | (_, EnumName _)
  | (StructUnionName _, _) | (_, StructUnionName _)
  | (TypeName _, _) | (_, TypeName _)
  | (FieldType _, _) | (_, FieldType _)
  | (ParenType _, _) | (_, ParenType _)
  | (TypeOfExpr _, _) | (_, TypeOfExpr _)
  | (TypeOfType _, _) | (_, TypeOfType _)
  | (AutoType, _) | (_, AutoType)
  | (TemplateType _, _) | (_, TemplateType _)
  | (NoType, _) -> fail

(* token doesn't matter, only presence or absence *)
and same_optfinal optfinala optfinalb =
  match optfinala, optfinalb with
    Some _, Some _ -> true
  | None, None -> true
  | _ -> false
end

module XEQ = struct
  type tin = unit
  type 'a tout = 'a option

  type 'a matcher = 'a -> 'a -> tin -> 'a tout

  let return x = fun tin -> Some x
  let fail = fun tin -> None

  let (>>=) m f = fun tin ->
    match m tin with
    | None -> None
    | Some x -> f x tin

  let (>&&>) b m = fun tin ->
    if b then m tin
    else fail tin

end

module EQ = C_VS_C (XEQ)


let eq_type2 a b = EQ.fullType a b () <> None
let merge_type2 a b = Common.some (EQ.fullType a b ())

let eq_type a b =
  Common.profile_code "C_vs_c" (fun () -> eq_type2 a b)

let merge_type a b =
  Common.profile_code "C_vs_c" (fun () -> merge_type2 a b)


(* ------------------------------------------------------------------------- *)

(* This seemed like a reasonable place to put this, given the file name,
but not sure that it is the case...  This has to be compatible with the
function equal_inh_metavarval.  It is indeed not so clear why that is
defined in cocci_vs_c.ml, and not here, since it is comparing C code to C
code. *)

let subexpression_of_expression small_exp big_exp =
  let res = ref false in (* because no appropriate functional visitor... *)
  let expr (k,bigf) big_exp =
    (* comparison used in Cocci_vs_c.equal_inh_metavarval *)
    (* have to strip each subexp, because stripping puts some offsets in the
       term rather than setting everything to 0.  No idea why... *)
    if small_exp = Lib_parsing_c.al_inh_expr big_exp
    then res := true
    else k big_exp in
  let bigf = { Visitor_c.default_visitor_c with Visitor_c.kexpr = expr } in
  Visitor_c.vk_expr bigf big_exp;
  (*Printf.printf "comparison gives %b\n" !res;
  Pretty_print_c.pp_expression_simple small_exp;
  Format.print_newline();
  Pretty_print_c.pp_expression_simple big_exp;
  Format.print_newline();
  Printf.printf "--------------------------------\n";*)
  !res
