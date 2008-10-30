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

let (option: 'a matcher -> ('a option matcher)) = fun f t1 t2 ->
  match (t1,t2) with
  | (Some t1, Some t2) -> 
      f t1 t2 >>= (fun t -> 
        return (Some t)
      )
  | (None, None) -> return None
  | _ -> fail


let rec fullType a b = 
  let ((qua,iiqa), tya) = a in
  let ((qub,iiqb), tyb) = b in
  (qua.const =:= qub.const && qua.volatile =:= qub.volatile) >&&>

    let (qu,iiq) = (qua, iiqa) in 
    typeC tya tyb >>= (fun ty -> 
      return ((qu,iiq), ty)
    )

and typeC tya tyb = 
  let (a, iia) = tya in
  let (b, iib) = tyb in

  let iix = iia in

  match a, b with
  | BaseType a, BaseType b -> 
      a =*= b >&&> return (BaseType a, iix)
  | Pointer a, Pointer b -> 
      fullType a b >>= (fun x -> return (Pointer x, iix))

  | StructUnionName (sua, sa), StructUnionName (sub, sb) -> 
      (sua =*= sub && sa =$= sb) >&&> 
        return (StructUnionName (sua, sa), iix)

  | TypeName (sa, opta), TypeName (sb, optb) -> 
      (* assert compatible opta optb ? *)
      (*option fullType opta optb*)
      sa =$= sb >&&> 
       let opt = 
         (match opta, optb with
         | None, None -> None
         | Some x, _ 
         | _, Some x 
             -> Some x
         ) 
       in
       return (TypeName (sa, opt), iix)
      

  | Array (ea, a), Array (eb,b) -> 
      let get_option f = function Some x -> Some (f x) | None -> None in
      let ea = get_option Lib_parsing_c.al_expr ea in
      let eb = get_option Lib_parsing_c.al_expr eb in
      ea =*= eb >&&> fullType a b >>= (fun x -> return (Array (ea, x), iix))

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

            let (((ba, saopt, ta), ii_b_sa)) = parama in
            let (((bb, sbopt, tb), ii_b_sb)) = paramb in

            let bx = ba in
            let sxopt = saopt in
            let ii_b_sx = ii_b_sa in

            (ba =:= bb && saopt =*= sbopt) >&&>
            fullType ta tb >>= (fun tx -> 
              let paramx = (((bx, sxopt, tx), ii_b_sx)) in
              return ((paramx,iix)::xs)
            )
          )
        ) (return [])
      >>= (fun tsx -> 
        let paramsx = (List.rev tsx, (bx, iihas3dotsx)) in
        return (FunctionType (returnx, paramsx), iix)
      ))

  | Enum (saopt, enuma), Enum (sbopt, enumb) -> 
      (saopt =*= sbopt &&
      List.length enuma = List.length enumb && 
      Common.zip enuma enumb +> List.for_all (fun 
        ((((sa, eopta),ii_s_eqa), iicommaa), (((sb, eoptb),ii_s_eqb),iicommab))
          -> 
            sa =$= sb && 
            eopta =*= eoptb 
        )
      ) >&&>
        return (Enum (saopt, enuma), iix)

  | EnumName sa, EnumName sb -> sa =$= sb >&&> return (EnumName sa, iix)

  | ParenType a, ParenType b -> 
      (* iso here ? *)
      fullType a b >>= (fun x -> 
        return (ParenType x, iix)
      )

  | TypeOfExpr ea, TypeOfExpr eb -> 
      let ea = Lib_parsing_c.al_expr ea in
      let eb = Lib_parsing_c.al_expr eb in
      ea =*= eb >&&> return (TypeOfExpr ea, iix)

  | TypeOfType a, TypeOfType b -> 
      fullType a b >>= (fun x -> return (TypeOfType x, iix))

(*  | TypeOfType a, b -> 
    | a, TypeOfType b -> 
*)


  | StructUnion (sua, saopt, sta), StructUnion (sub, sbopt, stb) -> 
      (sua =*= sub && saopt =*= sbopt && List.length sta = List.length stb) 
      >&&> 
      Common.zip sta stb +> List.fold_left 
        (fun acc ((xfielda, iia), (xfieldb, iib)) -> 
          let iix = iia in
          acc >>= (fun xs -> 
            match xfielda, xfieldb with 
            | EmptyField, EmptyField -> return ((EmptyField, iix)::xs)

            | DeclarationField (FieldDeclList (fa, iipta)), 
              DeclarationField (FieldDeclList (fb, iiptb)) -> 
                let iipt = iipta in (* TODO ?*)
                (List.length fa =|= List.length fb) >&&> 

                Common.zip fa fb +> List.fold_left 
                  (fun acc2 ((fielda,iia),(fieldb,iib))-> 
                    let iix = iia in
                    acc2 >>= (fun xs -> 
                      let (fa, ii2a) = fielda in
                      let (fb, ii2b) = fieldb in
                      let ii2x = ii2a in
                      match fa, fb with
                      | Simple (saopt, ta), Simple (sbopt, tb) -> 
                          saopt =*= sbopt >&&> 
                          fullType ta tb >>= (fun tx -> 
                            return (((Simple (saopt, tx), ii2x), iix)::xs)
                          )
                          
                      | BitField (sopta, ta, ea), BitField (soptb, tb, eb) -> 
                          (sopta =*= soptb && ea =*= eb) >&&> 
                          fullType ta tb >>= (fun tx -> 
                            return (((BitField (sopta,tx,ea), ii2x), iix)::xs)
                          )
                      | _,_ -> fail
                    )
                  ) (return [])
                 >>= (fun fx -> 
                   return (((DeclarationField 
                               (FieldDeclList (List.rev fx,iipt))), iix)::xs)
                 )
            | _ -> fail
          )


        ) (return [])
        >>= (fun stx -> 
          return (StructUnion (sua, saopt, List.rev stx), iix)
        )



  (* choose the lub.
   * subtil: in the return must put iia, not iix, and in following case
   * must put iib and not iix, because we want the token corresponding
   * to the typedef.
   *)
  | TypeName (s, Some a), _ -> 
      fullType a (Ast_c.nQ, tyb) >>= (fun x -> 
        return (TypeName (s, Some x), iia) 
      )

  | _, TypeName (s, Some b) -> 
      fullType b (Ast_c.nQ, tya) >>= (fun x -> 
        return (TypeName (s, Some x), iib) (* subtil: *)
      )

  | _, _ -> fail



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
