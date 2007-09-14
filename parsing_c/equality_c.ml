open Common open Commonop

open Ast_c

(* TODO: should use the isomorphism engine of julia *)

(* for the moment I do only eq_type and not eq_expr, etc. The reason
 * for eq_type is realted to the typedef and struct isomorphism. Sometimes
 * one use the typedef and sometimes the structname.
 *)

let rec eq_type a b = 
  let ((qua,iiqa), (tya,iia)) = a in
  let ((qub,iiqb), (tyb,iib)) = b in
  qua.const =:= qub.const && qua.volatile =:= qub.volatile &&
  

  match tya, tyb with
  | BaseType a, BaseType b -> a =*= b
  | Pointer a, Pointer b -> eq_type a b
  | Array (ea, a), Array (eb,b) -> 
      (* dont care about size ? *)
      eq_type a b

  | FunctionType (returna, paramsa), FunctionType (returnb, paramsb) -> 
      eq_type returna returnb &&
      let (tsa, (ba,_iihas3dotsa)) = paramsa in
      let (tsb, (bb,_iihas3dotsb)) = paramsb in
      ba = bb && 
      List.length tsa = List.length tsb &&
      Common.zip tsa tsb +> List.for_all (fun ((parama,_iia),(paramb,_iib))->
        
        let (((ba, saopt, ta), ii_b_sa)) = parama in
        let (((bb, sbopt, tb), ii_b_sb)) = paramb in
        ba =:= bb && saopt =*= sbopt && 
        eq_type ta tb
      )

  | Enum (saopt, enuma), Enum (sbopt, enumb) -> 
      saopt =*= sbopt &&
      List.length enuma = List.length enumb && 
      Common.zip enuma enumb +> List.for_all (fun 
        ((((sa, eopta),ii_s_eqa), iicommaa), (((sb, eoptb),ii_s_eqb),iicommab))
          -> 
            sa =$= sb && 
            eopta =*= eoptb 
        )

  | StructUnion (sua, saopt, sta), StructUnion (sub, sbopt, stb) -> 
      sua =*= sub && 
      saopt =*= sbopt && 
      List.length sta = List.length stb && 
      Common.zip sta stb +> List.for_all (fun ((xfielda, iia), (xfieldb, iib)) 
        -> 
          match xfielda, xfieldb with 
          | EmptyField, EmptyField -> true
          | FieldDeclList fa, FieldDeclList fb -> 
              List.length fa =|= List.length fb && 
              Common.zip fa fb +> List.for_all (fun ((fielda,_),(fieldb,_))-> 
                match fst fielda, fst fieldb with
                | Simple (saopt, ta), Simple (sbopt, tb) -> 
                    saopt =*= sbopt && eq_type ta tb
                | BitField (sopta, ta, ea), BitField (soptb, tb, eb) -> 
                    sopta =*= soptb && 
                    eq_type ta tb &&
                    ea =*= eb
                | _,_ -> false
              )
          
          
          | _ -> false
        )

  | EnumName sa, EnumName sb -> sa =$= sb
  | StructUnionName (sua, sa), StructUnionName (sub, sb) -> 
      sua =*= sub && sa =$= sb
  | TypeName (sa, opta), TypeName (sb, optb) -> 
      (* assert compatible opta optb ? *)
      sa =$= sb
  | ParenType a, ParenType b -> 
      (* iso here ? *)
         eq_type a b

  | TypeOfExpr ea, TypeOfExpr eb -> 
      ea =*= eb 

  | TypeOfType a, TypeOfType b -> 
      eq_type a b

(*  | TypeOfType a, b -> 
    | a, TypeOfType b -> 
*)

  (* typedef iso *)
  | TypeName (_, Some a), tyb -> 
      eq_type a b (*toplevel b, not tyb *)
  | tya, TypeName (_, Some b) -> 
      eq_type a (*toplevel a, not tya*) b
  | _, _ -> false
