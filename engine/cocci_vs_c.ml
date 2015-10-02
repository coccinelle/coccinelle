(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common

module A = Ast_cocci
module B = Ast_c

module F = Control_flow_c

module FlagM = Flag_matcher

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers FlagM.verbose_matcher

let (+++) a b = match a with Some x -> Some x | None -> b

let error ii str =
  match ii with
    [] -> failwith str
  | ii::_ ->
      failwith
	(Printf.sprintf "%s: %d: %s"
	   (Ast_c.file_of_info ii) (Ast_c.line_of_info ii) str)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type sequence = Ordered | Unordered

let seqstyle eas = Ordered

let (need_unordered_initialisers : B.initialiser B.wrap2 list -> bool) =
 fun ibs ->
   ibs +> List.exists (fun (ib, icomma) ->
     match B.unwrap ib with
     | B.InitDesignators _
     | B.InitFieldOld _
     | B.InitIndexOld _
       -> true
     | B.InitExpr _
     | B.InitList _
       -> false)

(* For the #include <linux/...> in the .cocci, need to find where is
 * the '+' attached to this element, to later find the first concrete
 * #include <linux/xxx.h> or last one in the series of #includes in the
 * .c.
 *)
type include_requirement =
  | IncludeMcodeBefore
  | IncludeMcodeAfter
  | IncludeNothing



(* todo? put in semantic_c.ml *)
type info_ident =
  | Function
  | LocalFunction (* entails Function *)
  | DontKnow


let term      mc = A.unwrap_mcode mc
let mcodekind mc = A.get_mcodekind mc


let mcode_contain_plus = function
  | A.CONTEXT (_,A.NOTHING) -> false
  | A.CONTEXT _ -> true
  | A.MINUS (_,_,_,A.NOREPLACEMENT) -> false
  | A.MINUS (_,_,_,A.REPLACEMENT _) -> true (* repl is nonempty *)
  | A.PLUS _ -> raise (Impossible 13)

let mcode_simple_minus = function
  | A.MINUS (_,_,_,A.NOREPLACEMENT) -> true
  | _ -> false


(* In transformation.ml sometime I build some mcodekind myself and
 * julia has put None for the pos. But there is no possible raise
 * NoMatch in those cases because it is for the minusall trick or for
 * the distribute, so either have to build those pos, in fact a range,
 * because for the distribute have to erase a fullType with one
 * mcodekind, or add an argument to tag_with_mck such as "safe" that
 * don't do the check_pos. Hence this DontCarePos constructor. *)

let minusizer =
  ("fake","fake"),
  {A.line = 0; A.column =0; A.strbef=[]; A.straft=[]; A.whitespace=""},
  (A.MINUS(A.DontCarePos,[],A.ALLMINUS,A.NOREPLACEMENT)),
  []

let generalize_mcode ia =
  let (s1, i, mck, pos) = ia in
  let new_mck =
    match mck with
    | A.PLUS _ -> raise (Impossible 14)
    | A.CONTEXT (A.NoPos,x) ->
	A.CONTEXT (A.DontCarePos,x)
    | A.MINUS   (A.NoPos,inst,adj,x) ->
	A.MINUS   (A.DontCarePos,inst,adj,x)

    | A.CONTEXT ((A.FixPos _|A.DontCarePos), _)
    | A.MINUS ((A.FixPos _|A.DontCarePos), _, _, _)
        ->
        raise (Impossible 15)
  in
  (s1, i, new_mck, pos)



(*---------------------------------------------------------------------------*)

(* 0x0 is equivalent to 0,  value format isomorphism *)
let equal_c_int s1 s2 =
  try
    int_of_string s1 = int_of_string s2
  with Failure("int_of_string") ->
    s1 = s2



(*---------------------------------------------------------------------------*)
(* Normally A should reuse some types of Ast_c, so those
 * functions should not exist.
 *
 * update: but now Ast_c depends on A, so can't make too
 * A depends on Ast_c, so have to stay with those equal_xxx
 * functions.
 *)

let equal_unaryOp a b =
  match a, b with
  | A.GetRef   , B.GetRef  -> true
  | A.GetRefLabel, B.GetRefLabel -> true
  | A.DeRef    , B.DeRef   -> true
  | A.UnPlus   , B.UnPlus  -> true
  | A.UnMinus  , B.UnMinus -> true
  | A.Tilde    , B.Tilde   -> true
  | A.Not      , B.Not     -> true
  | _, (B.Not|B.Tilde|B.UnMinus|B.UnPlus|B.DeRef|B.GetRef|B.GetRefLabel) ->
      false
let equal_arithOp a b =
  match (A.unwrap_mcode a), b with
  | A.Plus     , B.Plus     -> true
  | A.Minus    , B.Minus    -> true
  | A.Mul      , B.Mul      -> true
  | A.Div      , B.Div      -> true
  | A.Min      , B.Min      -> true
  | A.Max      , B.Max      -> true
  | A.Mod      , B.Mod      -> true
  | A.DecLeft  , B.DecLeft  -> true
  | A.DecRight , B.DecRight -> true
  | A.And      , B.And      -> true
  | A.Or       , B.Or       -> true
  | A.Xor      , B.Xor      -> true
  | _, (B.Xor|B.Or|B.And|B.DecRight|B.DecLeft|B.Mod|B.Div|B.Mul|B.Minus|B.Plus|B.Min|B.Max)
      -> false
let equal_logicalOp a b =
  match (A.unwrap_mcode a), b with
  | A.Inf    , B.Inf    -> true
  | A.Sup    , B.Sup    -> true
  | A.InfEq  , B.InfEq  -> true
  | A.SupEq  , B.SupEq  -> true
  | A.Eq     , B.Eq     -> true
  | A.NotEq  , B.NotEq  -> true
  | A.AndLog , B.AndLog -> true
  | A.OrLog  , B.OrLog  -> true
  | _, (B.OrLog|B.AndLog|B.NotEq|B.Eq|B.SupEq|B.InfEq|B.Sup|B.Inf)
      -> false
let equal_fixOp a b =
  match a, b with
  | A.Dec, B.Dec -> true
  | A.Inc, B.Inc -> true
  | _, (B.Inc|B.Dec) -> false

let equal_structUnion a b =
  match a, b with
  | A.Struct, B.Struct -> true
  | A.Union,  B.Union -> true
  | _, (B.Struct|B.Union) -> false

let equal_sign a b =
  match a, b with
  | A.Signed,    B.Signed   -> true
  | A.Unsigned,  B.UnSigned -> true
  | _, (B.UnSigned|B.Signed) -> false

let equal_storage a b =
  match a, b with
  | A.Static   , B.Sto B.Static
  | A.Auto     , B.Sto B.Auto
  | A.Register , B.Sto B.Register
  | A.Extern   , B.Sto B.Extern
      -> true
  | _, (B.NoSto | B.StoTypedef) -> false
  | _, (B.Sto (B.Register|B.Static|B.Auto|B.Extern)) -> false


(*---------------------------------------------------------------------------*)

let equal_metavarval valu valu' =
  match valu, valu' with
  | Ast_c.MetaIdVal a, Ast_c.MetaIdVal b -> a = b
  | Ast_c.MetaAssignOpVal a, Ast_c.MetaAssignOpVal b -> a = b
  | Ast_c.MetaBinaryOpVal a, Ast_c.MetaBinaryOpVal b -> a = b
  | Ast_c.MetaFuncVal a, Ast_c.MetaFuncVal b -> a = b
  | Ast_c.MetaLocalFuncVal a, Ast_c.MetaLocalFuncVal b ->
      (* do something more ? *)
      a = b

  (* al_expr before comparing !!! and accept when they match.
   * Note that here we have Astc._expression, so it is a match
   * modulo isomorphism (there is no metavariable involved here,
   * just isomorphisms). => TODO call isomorphism_c_c instead of
   * =. Maybe would be easier to transform ast_c in ast_cocci
   * and call the iso engine of julia. *)
  | Ast_c.MetaExprVal (a,_,_), Ast_c.MetaExprVal (b,_,_) ->
      Lib_parsing_c.al_expr a = Lib_parsing_c.al_expr b
  | Ast_c.MetaExprListVal a, Ast_c.MetaExprListVal b ->
      Lib_parsing_c.al_arguments a = Lib_parsing_c.al_arguments b

  | Ast_c.MetaFmtVal a, Ast_c.MetaFmtVal b ->
      Lib_parsing_c.al_string_format a = Lib_parsing_c.al_string_format b
  | Ast_c.MetaFragListVal a, Ast_c.MetaFragListVal b ->
      Lib_parsing_c.al_string_fragments a =
      Lib_parsing_c.al_string_fragments b

  | Ast_c.MetaDeclVal a, Ast_c.MetaDeclVal b ->
      Lib_parsing_c.al_declaration a = Lib_parsing_c.al_declaration b
  | Ast_c.MetaFieldVal a, Ast_c.MetaFieldVal b ->
      Lib_parsing_c.al_field a = Lib_parsing_c.al_field b
  | Ast_c.MetaFieldListVal a, Ast_c.MetaFieldListVal b ->
      Lib_parsing_c.al_fields a = Lib_parsing_c.al_fields b
  | Ast_c.MetaStmtVal(a,_), Ast_c.MetaStmtVal(b,_) ->
      Lib_parsing_c.al_statement a = Lib_parsing_c.al_statement b
  | Ast_c.MetaInitVal a, Ast_c.MetaInitVal b ->
      Lib_parsing_c.al_init a = Lib_parsing_c.al_init b
  | Ast_c.MetaInitListVal a, Ast_c.MetaInitListVal b ->
      Lib_parsing_c.al_inits a = Lib_parsing_c.al_inits b
  | Ast_c.MetaTypeVal a, Ast_c.MetaTypeVal b ->
      (* old: Lib_parsing_c.al_type a = Lib_parsing_c.al_type b *)
      C_vs_c.eq_type a b

  | Ast_c.MetaListlenVal a, Ast_c.MetaListlenVal b -> a = b

  | Ast_c.MetaParamVal a, Ast_c.MetaParamVal b ->
      Lib_parsing_c.al_param a = Lib_parsing_c.al_param b
  | Ast_c.MetaParamListVal a, Ast_c.MetaParamListVal b ->
      Lib_parsing_c.al_params a = Lib_parsing_c.al_params b

  | Ast_c.MetaPosVal (posa1,posa2), Ast_c.MetaPosVal (posb1,posb2) ->
      Ast_cocci.equal_pos posa1 posb1 && Ast_cocci.equal_pos posa2 posb2

  | Ast_c.MetaPosValList l1, Ast_c.MetaPosValList l2 ->
      List.exists
	(function (fla,cea,posa1,posa2) ->
	  List.exists
	    (function (flb,ceb,posb1,posb2) ->
	      fla = flb && cea = ceb &&
	      Ast_c.equal_posl posa1 posb1 && Ast_c.equal_posl posa2 posb2)
            l2)
	l1

  | (Ast_c.MetaNoVal, _) | (_, Ast_c.MetaNoVal) -> false

  | (B.MetaPosValList _|B.MetaListlenVal _|B.MetaPosVal _|B.MetaStmtVal _
      |B.MetaDeclVal _ |B.MetaFieldVal _ |B.MetaFieldListVal _
      |B.MetaTypeVal _ |B.MetaInitVal _ |B.MetaInitListVal _
      |B.MetaParamListVal _|B.MetaParamVal _|B.MetaExprListVal _
      |B.MetaExprVal _|B.MetaLocalFuncVal _|B.MetaFuncVal _|B.MetaIdVal _
      |B.MetaAssignOpVal _ | B.MetaBinaryOpVal _
      |B.MetaFmtVal _|B.MetaFragListVal _
    ), _
      -> raise (Impossible 16)

(* probably only one argument needs to be stripped, because inherited
metavariables containing expressions are stripped in advance. But don't
know which one is which... *)
let equal_inh_metavarval valu valu'=
  match valu, valu' with
  | Ast_c.MetaIdVal a, Ast_c.MetaIdVal b -> a = b
  | Ast_c.MetaAssignOpVal a, Ast_c.MetaAssignOpVal b -> a = b
  | Ast_c.MetaBinaryOpVal a, Ast_c.MetaBinaryOpVal b -> a = b
  | Ast_c.MetaFuncVal a, Ast_c.MetaFuncVal b -> a = b
  | Ast_c.MetaLocalFuncVal a, Ast_c.MetaLocalFuncVal b ->
      (* do something more ? *)
      a = b

  (* al_expr before comparing !!! and accept when they match.
   * Note that here we have Astc._expression, so it is a match
   * modulo isomorphism (there is no metavariable involved here,
   * just isomorphisms). => TODO call isomorphism_c_c instead of
   * =. Maybe would be easier to transform ast_c in ast_cocci
   * and call the iso engine of julia. *)
  | Ast_c.MetaExprVal (a,_,ty1), Ast_c.MetaExprVal (b,_,ty2) ->
      if ty1 = Ast_c.WITHOUT_TYPES || ty2 = Ast_c.WITHOUT_TYPES
      then Lib_parsing_c.real_al_expr a = Lib_parsing_c.real_al_expr b
      else Lib_parsing_c.al_inh_expr a = Lib_parsing_c.al_inh_expr b
  | Ast_c.MetaExprListVal a, Ast_c.MetaExprListVal b ->
      Lib_parsing_c.al_inh_arguments a = Lib_parsing_c.al_inh_arguments b

  | Ast_c.MetaFmtVal a, Ast_c.MetaFmtVal b ->
      Lib_parsing_c.al_inh_string_format a =
      Lib_parsing_c.al_inh_string_format b
  | Ast_c.MetaFragListVal a, Ast_c.MetaFragListVal b ->
      Lib_parsing_c.al_inh_string_fragments a =
      Lib_parsing_c.al_inh_string_fragments b

  | Ast_c.MetaDeclVal a, Ast_c.MetaDeclVal b ->
      Lib_parsing_c.al_inh_declaration a = Lib_parsing_c.al_inh_declaration b
  | Ast_c.MetaFieldVal a, Ast_c.MetaFieldVal b ->
      Lib_parsing_c.al_inh_field a = Lib_parsing_c.al_inh_field b
  | Ast_c.MetaFieldListVal a, Ast_c.MetaFieldListVal b ->
      Lib_parsing_c.al_inh_field_list a = Lib_parsing_c.al_inh_field_list b
  | Ast_c.MetaStmtVal(a,ty1), Ast_c.MetaStmtVal(b,ty2) ->
      if ty1 = Ast_c.WITHOUT_TYPES || ty2 = Ast_c.WITHOUT_TYPES
      then
	Lib_parsing_c.real_al_statement a = Lib_parsing_c.real_al_statement b
      else Lib_parsing_c.al_inh_statement a = Lib_parsing_c.al_inh_statement b
  | Ast_c.MetaInitVal a, Ast_c.MetaInitVal b ->
      Lib_parsing_c.al_inh_init a = Lib_parsing_c.al_inh_init b
  | Ast_c.MetaInitListVal a, Ast_c.MetaInitListVal b ->
      Lib_parsing_c.al_inh_inits a = Lib_parsing_c.al_inh_inits b
  | Ast_c.MetaTypeVal a, Ast_c.MetaTypeVal b ->
      (* old: Lib_parsing_c.al_inh_type a = Lib_parsing_c.al_inh_type b *)
      C_vs_c.eq_type a b

  | Ast_c.MetaListlenVal a, Ast_c.MetaListlenVal b -> a = b

  | Ast_c.MetaParamVal a, Ast_c.MetaParamVal b ->
      Lib_parsing_c.al_param a = Lib_parsing_c.al_param b
  | Ast_c.MetaParamListVal a, Ast_c.MetaParamListVal b ->
      Lib_parsing_c.al_params a = Lib_parsing_c.al_params b

  | Ast_c.MetaPosVal (posa1,posa2), Ast_c.MetaPosVal (posb1,posb2) ->
      Ast_cocci.equal_pos posa1 posb1 && Ast_cocci.equal_pos posa2 posb2

  | Ast_c.MetaPosValList l1, Ast_c.MetaPosValList l2 ->
      List.exists
	(function (fla,cea,posa1,posa2) ->
	  List.exists
	    (function (flb,ceb,posb1,posb2) ->
	      fla = flb && cea = ceb &&
	      Ast_c.equal_posl posa1 posb1 && Ast_c.equal_posl posa2 posb2)
            l2)
	l1

  | (Ast_c.MetaNoVal, _) | (_, Ast_c.MetaNoVal) -> false

  | (B.MetaPosValList _|B.MetaListlenVal _|B.MetaPosVal _|B.MetaStmtVal _
      |B.MetaDeclVal _ |B.MetaFieldVal _ |B.MetaFieldListVal _
      |B.MetaTypeVal _ |B.MetaInitVal _ |B.MetaInitListVal _
      |B.MetaParamListVal _|B.MetaParamVal _|B.MetaExprListVal _
      |B.MetaExprVal _|B.MetaLocalFuncVal _|B.MetaFuncVal _|B.MetaIdVal _
      |B.MetaAssignOpVal _ | B.MetaBinaryOpVal _
      |B.MetaFmtVal _|B.MetaFragListVal _
    ), _
      -> raise (Impossible 17)


(*---------------------------------------------------------------------------*)
(* could put in ast_c.ml, next to the split/unsplit_comma *)
let split_signb_baseb_ii (baseb, ii) =
  let iis = ii +> List.map (fun info -> (B.str_of_info info), info) in
  match baseb, iis with

  | B.Void, ["void",i1] -> None, [i1]

  | B.FloatType (B.CFloat),["float",i1] -> None, [i1]
  | B.FloatType (B.CDouble),["double",i1] -> None, [i1]
  | B.FloatType (B.CLongDouble),["long",i1;"double",i2] -> None,[i1;i2]

  | B.IntType (B.CChar), ["char",i1] -> None, [i1]


  | B.IntType (B.Si (sign, base)), xs ->
      let (signed,rest) =
	match (sign,xs) with
	  (_,[]) -> None,[]
	| (B.Signed,(("signed",i1)::rest)) -> (Some (B.Signed,i1),rest)
	| (B.Signed,rest) -> (None,rest)
	| (B.UnSigned,(("unsigned",i1)::rest)) -> (Some (B.UnSigned,i1),rest)
	| (B.UnSigned,rest) -> (* is this case possible? *) (None,rest) in
      (* The original code only allowed explicit signed and unsigned for char,
	 while this code allows char by itself.  Not sure that needs to be
	 checked for here.  If it does, then add a special case. *)
      let base_res =
	match (base,rest) with
	  B.CInt, ["int",i1] -> [i1]
	| B.CInt, [] -> []

	| B.CInt, ["",i1] -> (* no type is specified at all *)
	    (match i1.B.pinfo with
	      B.FakeTok(_,_) -> []
	    | _ -> error [i1] ("unrecognized signed int: "^
			      (String.concat " "(List.map fst iis))))

	| B.CChar2, ["char",i2] -> [i2]

	| B.CShort, ["short",i1] -> [i1]
	| B.CShort, ["short",i1;"int",i2] -> [i1;i2]

	| B.CLong, ["long",i1] -> [i1]
	| B.CLong, ["long",i1;"int",i2] -> [i1;i2]

	| B.CLongLong, ["long",i1;"long",i2] -> [i1;i2]
	| B.CLongLong, ["long",i1;"long",i2;"int",i3] -> [i1;i2;i3]

	| _ ->
	    error (List.map snd iis)
	      ("strange type1, maybe because of weird order: "^
	       (String.concat " " (List.map fst iis))) in
      (signed,base_res)

  | B.SizeType, ["size_t",i1] -> None, [i1]
  | B.SSizeType, ["ssize_t",i1] -> None, [i1]
  | B.PtrDiffType, ["ptrdiff_t",i1] -> None, [i1]

  | _ ->
      error (List.map snd iis)
	("strange type2, maybe because of weird order: "^
	 (String.concat " " (List.map fst iis)))

(*---------------------------------------------------------------------------*)

let rec unsplit_icomma xs =
  match xs with
  | [] -> []
  | x::y::xs ->
      (match A.unwrap y with
      | A.IComma mcode ->
          (x, y)::unsplit_icomma xs
      | _ -> failwith "wrong ast_cocci in initializer"
      )
  | _ ->
      failwith ("wrong ast_cocci in initializer, should have pair " ^
                "number of Icomma")



let resplit_initialiser ibs iicomma =
  match iicomma, ibs with
  | [], [] -> []
  | [], _ ->
      failwith "should have a iicomma, do you generate fakeInfo in parser?"
  | iicommas, [] ->
      error iicommas "shouldn't have a iicomma"
  | [iicomma], x::xs ->
      let elems = List.map fst (x::xs) in
      let commas = List.map snd (x::xs) +> List.flatten in
      let commas = commas @ [iicomma] in
      zip elems commas
  | _ -> raise (Impossible 18)



let rec split_icomma xs =
  match xs with
  | [] -> []
  | (x,y)::xs -> x::y::split_icomma xs

let rec unsplit_initialiser ibs_unsplit =
  match ibs_unsplit with
  | [] -> [],  [] (* empty iicomma *)
  | (x, commax)::xs ->
      let (xs, lastcomma) = unsplit_initialiser_bis commax xs in
      (x, [])::xs,  lastcomma

and unsplit_initialiser_bis comma_before = function
  | [] -> [], [comma_before]
  | (x, commax)::xs ->
      let (xs, lastcomma) = unsplit_initialiser_bis commax xs in
      (x, [comma_before])::xs,  lastcomma




(*---------------------------------------------------------------------------*)
(* coupling: same in type_annotater_c.ml *)
let structdef_to_struct_name ty =
  match ty with
  | qu, (B.StructUnion (su, sopt, fields), iis) ->
      (match sopt,iis with
      | Some s , [i1;i2;i3;i4] ->
          qu, (B.StructUnionName (su, s), [i1;i2])
      | None, _ ->
          ty

      | x -> raise (Impossible 19)
      )
  | _ -> raise (Impossible 20)

(*---------------------------------------------------------------------------*)
let one_initialisation_to_affectation x =
  let ({B.v_namei = var;
         B.v_type = returnType;
         B.v_type_bis = tybis;
         B.v_storage = storage;
         B.v_local = local},
       iisep) = x in
  match var with
  | Some (name, iniopt) ->
      (match iniopt with
      | B.ValInit (iini, (B.InitExpr e, ii_empty2)) ->
	  let local =
	    match local with
	      Ast_c.NotLocalDecl -> Ast_c.NotLocalVar
	    | Ast_c.LocalDecl ->
		(match Ast_c.info_of_type returnType with
		  None -> failwith "no returnType info"
		| Some ii -> Ast_c.LocalVar ii) in
          let typexp =
                    (* old: Lib_parsing_c.al_type returnType
                       * but this type has not the typename completed so
                       * instead try to use tybis
                    *)
            match !tybis with
            | Some ty_with_typename_completed -> ty_with_typename_completed
            | None -> raise (Impossible 21)
          in

          let typ = ref (Some (typexp,local), Ast_c.NotTest) in
          let ident = name in
          let idexpr = Ast_c.mk_e_bis (B.Ident ident) typ Ast_c.noii in
          let assign =
            Ast_c.mk_e (B.Assignment (idexpr, (B.SimpleAssign, [iini]), e)) [] in
          Some assign
      | _ -> None)
  | _ -> None

let initialisation_to_affectation decl =
  match decl with
  | B.MacroDecl _ -> F.Decl decl
  | B.MacroDeclInit _ -> F.Decl decl (* not sure... *)
  | B.DeclList (xs, iis) ->

      (* todo?: should not do that if the variable is an array cos
       *  will have x[] = , mais de toute facon ca sera pas un InitExp
       *)
      let possible_assignment =
	List.fold_left
	  (function prev ->
	    function x ->
	      match prev,one_initialisation_to_affectation x with
		_,None -> prev
	      |	None,Some x -> Some x
	      |	Some prev,Some x ->
		  (* [] is clearly an invalid ii value for a sequence.
		     hope that no one looks at it, since nothing will
		     match the sequence.  Fortunately, SmPL doesn't
		     support , expressions. *)
		  Some (Ast_c.mk_e (Ast_c.Sequence (prev, x)) []))
	  None xs in
      match possible_assignment with
	Some x -> F.DefineExpr x
      |	None -> F.Decl decl

(*****************************************************************************)
(* Functor parameter combinators *)
(*****************************************************************************)
(* monad like stuff
 * src: papers on parser combinators in haskell (cf a pearl by meijer in ICFP)
 *
 * version0: was not tagging the SP, so just tag the C
 *  val (>>=):
 *   (tin -> 'c tout)  -> ('c -> (tin -> 'b tout)) -> (tin -> 'b tout)
 *   val return : 'b -> tin -> 'b tout
 *   val fail : tin -> 'b tout
 *
 * version1: now also tag the SP so return a ('a * 'b)
 *)

type mode = PatternMode | TransformMode

module type PARAM =
  sig
    type tin
    type 'x tout


    type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout

    val mode : mode

    val (>>=):
      (tin -> ('a * 'b) tout)  ->
      ('a -> 'b -> (tin -> ('c * 'd) tout)) ->
      (tin -> ('c * 'd) tout)

    val return : ('a * 'b) -> tin -> ('a *'b) tout
    val fail : tin -> ('a * 'b) tout

    val (>||>) :
      (tin -> 'x tout) ->
      (tin -> 'x tout) ->
      (tin -> 'x tout)

    val (>|+|>) :
      (tin -> 'x tout) ->
      (tin -> 'x tout) ->
      (tin -> 'x tout)

    val (>&&>) : (tin -> bool) -> (tin -> 'x tout) -> (tin -> 'x tout)

    val tokenf : ('a A.mcode, B.info) matcher
    val tokenf_mck : (A.mcodekind, B.info) matcher

    val distrf_e :
      (A.meta_name A.mcode, B.expression) matcher
    val distrf_assignOp :
      (A.meta_name A.mcode, B.assignOp) matcher
    val distrf_binaryOp :
      (A.meta_name A.mcode, B.binaryOp) matcher
    val distrf_args :
      (A.meta_name A.mcode, (Ast_c.argument, Ast_c.il) either list) matcher
    val distrf_type :
      (A.meta_name A.mcode, Ast_c.fullType) matcher
    val distrf_params :
      (A.meta_name A.mcode,
       (Ast_c.parameterType, Ast_c.il) either list) matcher
    val distrf_param :
      (A.meta_name A.mcode, Ast_c.parameterType) matcher
    val distrf_ini :
      (A.meta_name A.mcode, Ast_c.initialiser) matcher
    val distrf_inis :
      (A.meta_name A.mcode, (Ast_c.initialiser, Ast_c.il) either list) matcher
    val distrf_decl :
      (A.meta_name A.mcode, Ast_c.declaration) matcher
    val distrf_field :
      (A.meta_name A.mcode, Ast_c.field) matcher
    val distrf_node :
      (A.meta_name A.mcode, F.node) matcher
    val distrf_fragments :
      (A.meta_name A.mcode, (Ast_c.string_fragment, Ast_c.il) either list)
      matcher
    val distrf_format :
      (A.meta_name A.mcode, Ast_c.string_format) matcher

    val distrf_define_params :
      (A.meta_name A.mcode, (string Ast_c.wrap, Ast_c.il) either list) matcher
    val distrf_pragmainfo :
      (A.meta_name A.mcode, Ast_c.pragmainfo) matcher

    val distrf_enum_fields :
      (A.meta_name A.mcode, (B.oneEnumType, B.il) either list) matcher

    val distrf_struct_fields :
      (A.meta_name A.mcode, B.field list) matcher

    val distrf_cst :
      (A.meta_name A.mcode, (B.constant, string) either B.wrap) matcher

    val distrf_ident_list :
      (A.meta_name A.mcode, (Ast_c.name, Ast_c.il) either list) matcher

    val distrf_exec_code_list :
      (A.meta_name A.mcode, (Ast_c.exec_code, Ast_c.il) either list) matcher

    val distrf_attrs :
      (A.meta_name A.mcode, (Ast_c.attribute, Ast_c.il) either list) matcher

    val cocciExp :
      (A.expression, B.expression) matcher -> (A.expression, F.node) matcher

    val cocciExpExp :
      A.mcodekind ->
      (A.expression, B.expression) matcher ->
	(A.expression, B.expression) matcher

    val cocciTy :
      (A.fullType, B.fullType) matcher -> (A.fullType, F.node) matcher

    val cocciId :
      (A.ident, Ast_c.name) matcher -> (A.ident, F.node) matcher

    val cocciInit :
      (A.initialiser, B.initialiser) matcher -> (A.initialiser, F.node) matcher

    val envf :
      A.keep_binding -> A.inherited ->
      A.meta_name A.mcode * Ast_c.metavar_binding_kind *
	  (unit -> Common.filename * string * Ast_c.posl * Ast_c.posl) ->
      (unit -> tin -> 'x tout) -> (tin -> 'x tout)

    val check_constraints :
      ('a -> 'b -> (A.meta_name -> B.metavar_binding_kind) -> bool) ->
	'a -> 'b ->
	(unit -> tin -> 'x tout) -> (tin -> 'x tout)

    val check_constraints_ne :
      ('a, 'b) matcher -> 'a list -> 'b ->
	(unit -> tin -> 'x tout) -> (tin -> 'x tout)

    val all_bound : A.meta_name list -> (tin -> bool)

    val optional_storage_flag : (bool -> tin -> 'x tout) -> (tin -> 'x tout)
    val optional_qualifier_flag : (bool -> tin -> 'x tout) -> (tin -> 'x tout)
    val value_format_flag : (bool -> tin -> 'x tout) -> (tin -> 'x tout)
    val optional_declarer_semicolon_flag :
	(bool -> tin -> 'x tout) -> (tin -> 'x tout)
    val optional_attributes_flag :
	(bool -> tin -> 'x tout) -> (tin -> 'x tout)

  end

(*****************************************************************************)
(* Functor code, "Cocci vs C" *)
(*****************************************************************************)

module COCCI_VS_C =
  functor (X : PARAM) ->
struct

type ('a, 'b) matcher = 'a -> 'b  -> X.tin -> ('a * 'b) X.tout

let (>>=) = X.(>>=)
let return = X.return
let fail = X.fail

let (>||>) = X.(>||>)
let (>|+|>) = X.(>|+|>)
let (>&&>) = X.(>&&>)

let tokenf = X.tokenf

(* should be raise Impossible when called from transformation.ml *)
let fail2 () =
  match X.mode with
  | PatternMode -> fail
  | TransformMode -> raise (Impossible 22)


let (option: ('a,'b) matcher -> ('a option,'b option) matcher)= fun f t1 t2 ->
  match (t1,t2) with
  | (Some t1, Some t2) ->
      f t1 t2 >>= (fun t1 t2 ->
        return (Some t1, Some t2)
      )
  | (None, None) -> return (None, None)
  | _ -> fail

(* Dots are sometimes used as metavariables, since like metavariables they
can match other things.  But they no longer have the same type.  Perhaps these
functions could be avoided by introducing an appropriate level of polymorphism,
but I don't know how to declare polymorphism across functors *)
let dots2metavar (_,info,mcodekind,pos) =
  (("","..."),info,mcodekind,pos)
let metavar2dots (_,info,mcodekind,pos) = ("...",info,mcodekind,pos)
let metavar2ndots (_,info,mcodekind,pos) = ("<+...",info,mcodekind,pos)

let satisfies_regexpconstraint c id _ : bool =
  match c with
    A.IdRegExp (_,recompiled)    -> Regexp.string_match recompiled id
  | A.IdNotRegExp (_,recompiled) -> not (Regexp.string_match recompiled id)

let satisfies_iconstraint (strs,metas) id env : bool =
  List.mem id strs ||
  (List.exists
     (function meta ->
       match Common.optionise (fun () -> env meta) with
	 Some (Ast_c.MetaIdVal(valu)) -> id = valu
       | _ -> false)
     metas)

let satisfies_niconstraint (strs,metas) id env : bool =
  not (List.mem id strs) &&
  (List.for_all
     (function meta ->
       match Common.optionise (fun () -> env meta) with
	 Some (Ast_c.MetaIdVal(valu)) -> not(id = valu)
       | _ -> true)
     metas)

let satisfies_econstraint c exp env : bool =
  let warning s = pr2_once ("WARNING: "^s); false in
  match Ast_c.unwrap_expr exp with
    Ast_c.Ident (name) ->
      (match name with
	Ast_c.RegularName     rname ->
	  satisfies_regexpconstraint c (Ast_c.unwrap_st rname) env
      | Ast_c.CppConcatenatedName _ ->
	  warning
	    "Unable to apply a constraint on a CppConcatenatedName identifier!"
      | Ast_c.CppVariadicName     _ ->
	  warning
	    "Unable to apply a constraint on a CppVariadicName identifier!"
      | Ast_c.CppIdentBuilder     _ ->
	  warning
	    "Unable to apply a constraint on a CppIdentBuilder identifier!")
  | Ast_c.Constant cst ->
      (match cst with
      |	Ast_c.String (str, _) -> satisfies_regexpconstraint c str env
      | Ast_c.MultiString strlist ->
	  warning "Unable to apply a constraint on a multistring constant!"
      | Ast_c.Char  (char , _) -> satisfies_regexpconstraint c char env
      | Ast_c.Int   (int  , _) -> satisfies_regexpconstraint c int env
      | Ast_c.Float (float, _) -> satisfies_regexpconstraint c float env
      | Ast_c.DecimalConst (d, n, p) ->
	  warning "Unable to apply a constraint on a decimal constant!")
  | Ast_c.StringConstant (cst,orig,w) -> satisfies_regexpconstraint c orig env
  | _ -> warning "Unable to apply a constraint on an expression!"


(* ------------------------------------------------------------------------- *)
(* This has to be up here to allow adequate polymorphism *)

let list_matcher match_dots rebuild_dots match_comma rebuild_comma
    match_metalist rebuild_metalist mktermval special_cases
    element distrf split_comma unsplit_comma get_iis lenfilter = fun eas ebs ->
  let rec loop = function
      [], [] -> return ([], [])
    | [], eb::ebs -> fail
    | ea::eas, ebs ->
	X.all_bound (A.get_inherited ea) >&&>
	let try_matches =
	  (match match_dots ea, ebs with
	    Some (mcode, optexpr), ys ->
          (* todo: if optexpr, then a WHEN and so may have to filter yys *)
              if optexpr <> None then failwith "not handling when in a list";

          (* '...' can take more or less the beginnings of the arguments *)
              let startendxs =
		(* if eas is empty there is only one possible match.
		   the same if eas is just a comma *)
		match eas with
		  [] -> [(ys,[])]
		| [c] when not(ys=[]) &&
		   (match match_comma c with Some _ -> true | None -> false) ->
		    let r = List.rev ys in
		    [(List.rev(List.tl r),[List.hd r])]
		| _ ->
		  Common.zip (Common.inits ys) (Common.tails ys) in
	      Some
		(startendxs +> List.fold_left (fun acc (startxs, endxs) ->
		  acc >||> (

              (* allow '...', and maybe its associated ',' to match nothing.
		 * for the associated ',' see below how we handle the EComma
		 * to match nothing.
              *)
		  (if startxs=[]
		  then
                    if mcode_contain_plus (mcodekind mcode)
                    then fail
                  (*failwith
		     "I have no token that I could accroche myself on"*)
                    else return (dots2metavar mcode, [])
		  else
                (* subtil: we don't want the '...' to match until the
                   * comma. cf -test pb_params_iso. We would get at
                   * "already tagged" error.
                   * this is because both f (... x, ...) and f (..., x, ...)
                   * would match a  f(x,3)  with our "optional-comma" strategy.
                *)
                    (match Common.last startxs with
                    | Right _ -> fail
                    | Left _ -> distrf (dots2metavar mcode) startxs))

		    >>= (fun mcode startxs ->
		      let mcode = metavar2dots mcode in
                      loop (eas, endxs) >>= (fun eas endxs ->
			return (
			(rebuild_dots (mcode, optexpr) +> A.rewrap ea) ::eas,
			startxs @ endxs
			  )))
		    )
		    ) fail)

	  | None,_ -> None)
	    +++
	    (match match_comma ea, ebs with
	    | Some ia1, Right ii::ebs ->
		Some
		  (let ib1 = tuple_of_list1 ii in
		  tokenf ia1 ib1 >>= (fun ia1 ib1 ->
		    loop (eas, ebs) >>= (fun eas ebs ->
		      return (
                      (rebuild_comma ia1 +> A.rewrap ea)::eas,
                      (Right [ib1])::ebs
				       )
			)))
	    | Some ia1, ebs ->
          (* allow ',' to matching nothing. optional comma trick *)
		Some
		  (if mcode_contain_plus (mcodekind ia1)
		  then fail
		  else loop (eas, ebs))
	    | None,_ -> None)
	    +++
	    (match match_metalist ea, ebs with
	      Some (ida,leninfo,keep,inherited,extra), ys ->
		let startendxs =
		  Common.zip (Common.inits ys) (Common.tails ys) in
		Some
		  (startendxs +> List.fold_left (fun acc (startxs, endxs) ->
		    acc >||> (
		    let ok =
		      if startxs=[]
		      then
			if mcode_contain_plus (mcodekind ida)
			then false
                    (* failwith "no token that I could accroche myself on" *)
			else true
		      else
			(match Common.last startxs with
			| Right _ -> false
			| Left _ -> true)
		    in
		    if not ok
		    then fail
		    else
		      let startxs' = unsplit_comma startxs in
		      (match lenfilter startxs' with
			None -> (function _ -> fail)
		      | Some infos ->
			  let len = List.length infos in

			  (match leninfo with
			  | A.MetaListLen (lenname,lenkeep,leninherited) ->
			      let max_min _ = failwith "no pos" in
			      X.envf lenkeep leninherited
				(lenname, Ast_c.MetaListlenVal (len), max_min)
			  | A.CstListLen n ->
			      if len = n
			      then (function f -> f())
			      else (function f -> fail)
			  | A.AnyListLen -> function f -> f()))
		      (fun () ->
			let max_min _ =
			  Lib_parsing_c.lin_col_by_pos (get_iis startxs) in
			(match extra with
			  Some extra ->
			    extra startxs' max_min
			      (fun _ -> return ((),()))
			| None -> return ((),()))
			  >>=
			(fun _ _ ->
			  X.envf keep inherited
			    (ida, mktermval startxs', max_min)
			    (fun () ->
			      if startxs=[]
			      then return (ida, [])
			      else distrf ida (split_comma startxs'))
			    >>= (fun ida startxs ->
			      loop (eas, endxs) >>= (fun eas endxs ->
				return (
				(rebuild_metalist ea
				   (ida,leninfo,keep,inherited))
				  +> A.rewrap ea::eas,
				startxs @ endxs
				  )))
				)
			    )
			)) fail)
	    | None,_ -> None)
	    +++
	    special_cases ea eas ebs in
	match try_matches with
	  Some res -> res
	| None ->
	    (match ebs with
	    | (Left eb)::ebs ->
		element ea eb >>= (fun ea eb ->
		  loop (eas, ebs) >>= (fun eas ebs ->
		    return (ea::eas, Left eb::ebs)))
	    | (Right y)::ys -> raise (Impossible 23)
	    | [] -> fail) in
  loop (eas,ebs)

(*---------------------------------------------------------------------------*)
(* toc:
 *  - expression
 *  - ident
 *  - arguments
 *  - parameters
 *  - declaration
 *  - initialisers
 *  - type
 *  - node
 *)

let arithA_of_arithB = function
  | B.Plus -> A.Plus
  | B.Minus -> A.Minus
  | B.Mul -> A.Mul
  | B.Div -> A.Div
  | B.Mod -> A.Mod
  | B.DecLeft -> A.DecLeft
  | B.DecRight -> A.DecRight
  | B.And -> A.And
  | B.Or -> A.Or
  | B.Xor -> A.Xor
  | B.Min -> A.Min
  | B.Max -> A.Max

let logicalA_of_logicalB = function
  | B.Inf -> A.Inf
  | B.Sup -> A.Sup
  | B.InfEq -> A.InfEq
  | B.SupEq -> A.SupEq
  | B.Eq -> A.Eq
  | B.NotEq -> A.NotEq
  | B.AndLog -> A.AndLog
  | B.OrLog -> A.OrLog

let assignOpA_of_assignOpB = function
  | B.SimpleAssign -> A.SimpleAssign (A.make_mcode "=")
  | B.OpAssign op -> A.OpAssign (A.make_mcode (arithA_of_arithB op))

let binaryOpA_of_binaryOpB = function
  | B.Arith op -> A.Arith (A.make_mcode (arithA_of_arithB op))
  | B.Logical op -> A.Logical (A.make_mcode (logicalA_of_logicalB op))

let assignOp_eq op1 op2 = match (op1, op2) with
  | A.SimpleAssign _, A.SimpleAssign _ -> true
  | A.OpAssign o1, A.OpAssign o2 -> (A.unwrap_mcode o1) = (A.unwrap_mcode o2)
  | _ -> false

let check_assignOp_constraint (opb',ii) = function
  | A.AssignOpNoConstraint -> true
  | A.AssignOpInSet ops ->
    let opb'' = (assignOpA_of_assignOpB opb') in
    List.exists (assignOp_eq opb'') (List.map A.unwrap ops)

let binaryOp_eq op1 op2 = match (op1, op2) with
  | A.Arith o1, A.Arith o2 -> (A.unwrap_mcode o1) = (A.unwrap_mcode o2)
  | A.Logical o1, A.Logical o2 -> (A.unwrap_mcode o1) = (A.unwrap_mcode o2)
  | _ -> false

let check_binaryOp_constraint (opb',ii) = function
  | A.BinaryOpNoConstraint -> true
  | A.BinaryOpInSet ops ->
    let opb'' = (binaryOpA_of_binaryOpB opb') in
    List.exists (binaryOp_eq opb'') (List.map A.unwrap ops)

(*---------------------------------------------------------------------------*)
let rec (rule_elem_node: (A.rule_elem, F.node) matcher) =
 fun re node ->

let rec (expression: (A.expression, Ast_c.expression) matcher) =
 fun ea eb ->
   if A.get_test_exp ea && not (Ast_c.is_test eb) then
     fail
   else
  X.all_bound (A.get_inherited ea) >&&>
  let wa x = A.rewrap ea x in
  match A.unwrap ea, eb with

  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr (ida,constraints,keep,opttypa,form,inherited),
    (((expr, opttypb), ii) as expb) ->

      (* old: before have a MetaConst. Now we factorize and use 'form' to
       * differentiate between different cases *)
      let rec matches_id = function
	  B.Ident(name) -> true
	| B.Cast(ty,e) -> matches_id (B.unwrap_expr e)
	| _ -> false in
      let form_ok =
	match (form,expr) with
	  (A.ANY,_) -> true
	| (A.CONST,e) ->
	    let rec matches = function
		B.Constant(c) -> true
              | B.Ident (nameidb) ->
                  let s = Ast_c.str_of_name nameidb in
                  if s =~ "^[A-Z_][A-Z_0-9]*$"
                  then begin
		    pr2_once ("warning: " ^ s ^ " treated as a constant");
		    true
                  end
                  else false
	      | B.Cast(ty,e) -> matches (B.unwrap_expr e)
	      |	B.Unary(e,B.UnMinus) -> matches (B.unwrap_expr e)
	      | B.SizeOfExpr(exp) -> true
	      | B.SizeOfType(ty) -> true
	      | _ -> false in
	    matches e
	| (A.LocalID,e) ->
	    (matches_id e) &&
	    (match !opttypb with
	      (Some (_,Ast_c.LocalVar _),_) -> true
	    | _ -> false)
	| (A.GlobalID,e) ->
	    (matches_id e) &&
	    (match !opttypb with
	      (Some (_,Ast_c.LocalVar _),_) -> false
	    | _ -> true)
	| (A.ID,e) -> matches_id e in
      if form_ok
      then
	(let (opttypb,_testb) = !opttypb in
	match opttypa, opttypb with
        | None, _ -> return ((),())
        | Some _, None ->
            pr2_once ("Missing type information. Certainly a pb in " ^
                      "annotate_typer.ml");
            fail

        | Some tas, Some tb ->
            tas +> List.fold_left (fun acc ta ->
              acc >|+|> compatible_type ta tb) fail
	) >>=
	(fun () () ->
	  (* wraps on C code, so has types *)
	  let meta_expr_val l x = Ast_c.MetaExprVal(x,l,Ast_c.WITH_TYPES) in
	  match constraints with
	    Ast_cocci.NoConstraint -> return (meta_expr_val [],())
	  | Ast_cocci.NotIdCstrt cstrt ->
	      X.check_constraints satisfies_econstraint cstrt eb
		(fun () -> return (meta_expr_val [],()))
	  | Ast_cocci.NotExpCstrt cstrts ->
	      X.check_constraints_ne expression cstrts eb
		(fun () -> return (meta_expr_val [],()))
	  | Ast_cocci.SubExpCstrt cstrts ->
	      return (meta_expr_val cstrts,()))
	  >>=
	(fun wrapper () ->
	  let max_min _ =
	    Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_expr expb) in
	  X.envf keep inherited (ida, wrapper expb, max_min)
	    (fun () ->
	      X.distrf_e ida expb >>=
	      (fun ida expb ->
		return (
		A.MetaExpr (ida,constraints,keep,opttypa,form,inherited)+>
		A.rewrap ea,
		expb
		  ))
		))
      else fail

  (* old:
   * | A.MetaExpr(ida,false,opttypa,_inherited), expb ->
   *   D.distribute_mck (mcodekind ida) D.distribute_mck_e expb binding
   *
   * but bug! because if have not tagged SP, then transform without doing
   * any checks. Hopefully now have tagged SP technique.
   *)

  | A.AsExpr(exp,asexp), expb ->
      expression exp expb >>= (fun exp expb ->
      expression asexp expb >>= (fun asexp expb ->
	return(
	  ((A.AsExpr(exp,asexp)) +> wa,
	   expb))))
  | A.AsSExpr(exp,asstm), expb ->
      expression exp expb >>= (fun exp expb ->
      rule_elem_node asstm node >>= (fun asstm _node ->
	return(
	  ((A.AsSExpr(exp,asstm)) +> wa,
	   expb))))

  (* old:
   * | A.Edots _, _ -> raise Impossible.
   *
   * In fact now can also have the Edots inside normal expression, not
   * just in arg lists. in 'x[...];' less: in if(<... x ... y ...>)
   *)
  | A.Edots (mcode, None), expb    ->
      X.distrf_e (dots2metavar mcode) expb >>= (fun mcode expb ->
        return (
        A.Edots (metavar2dots mcode, None) +> A.rewrap ea ,
        expb
          ))


  | A.Edots (_, Some expr), _    -> failwith "not handling when on Edots"


  | A.Ident ida,   ((B.Ident (nameidb), typ),noii) ->
      assert (noii = []);
      ident_cpp DontKnow ida nameidb >>= (fun ida nameidb ->
        return (
        ((A.Ident ida)) +> wa,
        ((B.Ident (nameidb), typ),Ast_c.noii)
          ))




  | A.MetaErr _,     _ -> failwith "not handling MetaErr"

  (* todo?: handle some isomorphisms in int/float ? can have different
   * format : 1l can match a 1.
   *
   * todo: normally string can contain some metavar too, so should
   * recurse on the string
   *)
  | A.Constant (ia1), ((B.Constant (ib) , typ),ii) ->
      (* for everything except the String case where can have multi elems *)
      let do1 () =
        let ib1 = tuple_of_list1 ii in
        tokenf ia1 ib1 >>= (fun ia1 ib1 ->
          return (
            ((A.Constant ia1)) +> wa,
            ((B.Constant (ib), typ),[ib1])
          ))
      in
      (match term ia1, ib with
      | A.Int x, B.Int (y,_) ->
          X.value_format_flag (fun use_value_equivalence ->
            if use_value_equivalence
            then
              if equal_c_int x y
              then do1()
              else fail
            else
              if x = y
              then do1()
            else fail
          )
      | A.Char x, B.Char (y,_) when x = y  (* todo: use kind ? *)
          -> do1()
      | A.Float x, B.Float (y,_) when x = y (* todo: use floatType ? *)
          -> do1()
      | A.DecimalConst (x,lx,px),B.DecimalConst (y,ly,py)
	when x = y && lx = ly && px = py(*lx etc perhaps implied by x=y*)
          -> do1()

      |	 A.String sa, B.String (sb,_kind) when sa = sb ->
          (match ii with
          | [ib1] ->
            tokenf ia1 ib1 >>= (fun ia1 ib1 ->
              return (
                ((A.Constant ia1)) +> wa,
                ((B.Constant (ib), typ),[ib1])
              ))
          |  _ -> fail (* multi string, not handled *)
          )

      | _, B.MultiString _ -> (* todo cocci? *) fail
      | _, (B.String _ | B.Float _ | B.Char _ | B.Int _ | B.DecimalConst _) ->
	  fail
      )

  | A.StringConstant (lq,frags1,rq),
      ((B.StringConstant (frags2,os,w), typ), ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf lq ib1 >>= (fun lq ib1 ->
      tokenf rq ib2 >>= (fun rq ib2 ->
      string_fragments (A.unwrap frags1) (B.split_nocomma frags2) >>=
	(fun frags1unwrap frags2splitted ->
        let frags1 = A.rewrap frags1 frags1unwrap in
	let frags2 = Ast_c.unsplit_nocomma frags2splitted in
	return (
	  ((A.StringConstant (lq,frags1,rq)) +> wa,
	   ((B.StringConstant (frags2,os,w), typ), [ib1; ib2]))))))

  | A.FunCall (ea, ia1, eas, ia2),  ((B.FunCall (eb, ebs), typ),ii) ->
      (* todo: do special case to allow IdMetaFunc, cos doing the
       * recursive call will be too late, match_ident will not have the
       * info whether it was a function. todo: but how detect when do
       * x.field = f; how know that f is a Func ? By having computed
       * some information before the matching!
       *
       * Allow match with FunCall containing types. Now ast_cocci allow
       * type in parameter, and morover ast_cocci allow f(...) and those
       * ... could match type.
       *)
      let (ib1, ib2) = tuple_of_list2 ii in
      expression ea eb >>= (fun ea eb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      arguments (seqstyle eas) (A.unwrap eas) ebs >>= (fun easunwrap ebs ->
        let eas = A.rewrap eas easunwrap in
        return (
          ((A.FunCall (ea, ia1, eas, ia2)) +> wa,
          ((B.FunCall (eb, ebs),typ), [ib1;ib2])
        ))))))

  | A.Assignment (ea1, opa, ea2, simple),
      ((B.Assignment (eb1, opb, eb2), typ),ii) ->
      if ii<>[] then failwith "In cocci_vs_c, ii for Assign should be empty."
      else (
      expression ea1 eb1 >>= (fun ea1 eb1 ->
      assignOp opa opb >>= (fun opa opb ->
      expression ea2 eb2 >>= (fun ea2 eb2 ->
        return (
          (A.Assignment (ea1, opa, ea2, simple)) +> wa,
          ((B.Assignment (eb1, opb, eb2), typ), [])
      )))))

  | A.Sequence (ea1, opa, ea2),
      ((B.Sequence (eb1, eb2), typ),ii) ->
      let (opbi) = tuple_of_list1 ii in
        expression ea1 eb1 >>= (fun ea1 eb1 ->
        expression ea2 eb2 >>= (fun ea2 eb2 ->
        tokenf opa opbi >>= (fun opa opbi ->
          return (
            (A.Sequence (ea1, opa, ea2)) +> wa,
            ((B.Sequence (eb1, eb2), typ), [opbi])
        ))))

  | A.CondExpr(ea1,ia1,ea2opt,ia2,ea3),((B.CondExpr(eb1,eb2opt,eb3),typ),ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      expression ea1 eb1 >>= (fun ea1 eb1 ->
      option expression ea2opt eb2opt >>= (fun  ea2opt eb2opt ->
      expression ea3 eb3 >>= (fun ea3 eb3 ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        return (
          ((A.CondExpr(ea1,ia1,ea2opt,ia2,ea3))) +> wa,
          ((B.CondExpr (eb1, eb2opt, eb3),typ), [ib1;ib2])
        ))))))

  (* todo?: handle some isomorphisms here ? *)
  | A.Postfix (ea, opa), ((B.Postfix (eb, opb), typ),ii) ->
      let opbi = tuple_of_list1 ii in
      if equal_fixOp (term opa) opb
      then
        expression ea eb >>= (fun ea eb ->
        tokenf opa opbi >>= (fun opa opbi ->
          return (
            ((A.Postfix (ea, opa))) +> wa,
            ((B.Postfix (eb, opb), typ),[opbi])
        )))
      else fail


  | A.Infix (ea, opa), ((B.Infix (eb, opb), typ),ii) ->
      let opbi = tuple_of_list1 ii in
      if equal_fixOp (term opa) opb
      then
        expression ea eb >>= (fun ea eb ->
        tokenf opa opbi >>= (fun opa opbi ->
          return (
            ((A.Infix (ea, opa))) +> wa,
            ((B.Infix (eb, opb), typ),[opbi])
        )))
      else fail

  | A.Unary (ea, opa), ((B.Unary (eb, opb), typ),ii) ->
      let opbi = tuple_of_list1 ii in
      if equal_unaryOp (term opa) opb
      then
        expression ea eb >>= (fun ea eb ->
        tokenf opa opbi >>= (fun opa opbi ->
          return (
            ((A.Unary (ea, opa))) +> wa,
            ((B.Unary (eb, opb), typ),[opbi])
        )))
      else fail

  | A.Binary (ea1, opa, ea2), ((B.Binary (eb1, opb, eb2), typ),ii) ->
      if ii <> []
      then failwith "cocci_vs_c: ii should be empty for binary operators."
      else
	expression ea1 eb1 >>= (fun ea1 eb1 ->
	binaryOp opa opb >>= (fun opa opb ->
	expression ea2 eb2 >>= (fun ea2 eb2 ->
	  return (
          ((A.Binary (ea1, opa, ea2))) +> wa,
          ((B.Binary (eb1, opb, eb2), typ),[]
	     )))))

  | A.Nested (ea1, opa, ea2), eb ->
      let rec loop eb =
	expression ea1 eb >|+|>
	(match eb with
	  ((B.Binary (eb1, opb, eb2), typ),ii) ->
	    if ii<>[]
	    then
	      failwith "cocci_vs_c: ii should be empty for nested operators."
            else
	      let left_to_right =
		expression ea1 eb1 >>= (fun ea1 eb1 ->
		binaryOp opa opb >>= (fun opa opb ->
		expression ea2 eb2 >>= (fun ea2 eb2 ->
		  return (
		  ((A.Nested (ea1, opa, ea2))) +> wa,
		  ((B.Binary (eb1, opb, eb2), typ),[]
		     ))))) in
	      let right_to_left =
		expression ea2 eb1 >>= (fun ea2 eb1 ->
                binaryOp opa opb >>= (fun opa opb ->
		expression ea1 eb2 >>= (fun ea1 eb2 ->
		  return (
		  ((A.Nested (ea1, opa, ea2))) +> wa,
		  ((B.Binary (eb1, opb, eb2), typ),[]
		     ))))) in
	      let in_left =
		expression ea2 eb2 >>= (fun ea2 eb2 ->
		binaryOp opa opb >>= (fun opa opb ->
		(* be last, to be sure the rest is marked *)
		loop eb1 >>= (fun ea1 eb1 ->
		  return (
		    ((A.Nested (ea1, opa, ea2))) +> wa,
		    ((B.Binary (eb1, opb, eb2), typ),[]
		       ))))) in
	      let in_right =
		expression ea2 eb1 >>= (fun ea2 eb1 ->
		binaryOp opa opb >>= (fun opa opb ->
		(* be last, to be sure the rest is marked *)
		loop eb2 >>= (fun ea1 eb2 ->
		  return (
		  ((A.Nested (ea1, opa, ea2))) +> wa,
		  ((B.Binary (eb1, opb, eb2), typ),[]
		     ))))) in
	      left_to_right >|+|> right_to_left >|+|> in_left >|+|> in_right
	    | _ -> fail) in
      loop eb

  (* todo?: handle some isomorphisms here ?  (with pointers = Unary Deref) *)
  | A.ArrayAccess (ea1, ia1, ea2, ia2),((B.ArrayAccess (eb1, eb2), typ),ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      expression ea1 eb1 >>= (fun ea1 eb1 ->
      expression ea2 eb2 >>= (fun ea2 eb2 ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        return (
          ((A.ArrayAccess (ea1, ia1, ea2, ia2))) +> wa,
          ((B.ArrayAccess (eb1, eb2),typ), [ib1;ib2])
        )))))

  (* todo?: handle some isomorphisms here ? *)
  | A.RecordAccess (ea, ia1, ida), ((B.RecordAccess (eb, idb), typ),ii) ->
      let (ib1) = tuple_of_list1 ii in
      ident_cpp DontKnow ida idb >>= (fun ida idb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      expression ea eb >>= (fun ea eb ->
        return (
          ((A.RecordAccess (ea, ia1, ida))) +> wa,
          ((B.RecordAccess (eb, idb), typ), [ib1])
        ))))



  | A.RecordPtAccess (ea,ia1,ida),((B.RecordPtAccess (eb, idb), typ), ii) ->
      let (ib1) = tuple_of_list1 ii in
      ident_cpp DontKnow ida idb >>= (fun ida idb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      expression ea eb >>= (fun ea eb ->
        return (
          ((A.RecordPtAccess (ea, ia1, ida))) +> wa,
          ((B.RecordPtAccess (eb, idb), typ), [ib1])
        ))))


  (* todo?: handle some isomorphisms here ?
   * todo?: do some iso-by-absence on cast ?
   *    by trying | ea, B.Case (typb, eb) -> match_e_e ea eb ?
   *)

  | A.Cast (ia1, typa, ia2, ea), ((B.Cast (typb, eb), typ),ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      fullType typa typb >>= (fun typa typb ->
      expression ea eb >>= (fun ea eb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        return (
          ((A.Cast (ia1, typa, ia2, ea))) +> wa,
          ((B.Cast (typb, eb),typ),[ib1;ib2])
        )))))

  | A.SizeOfExpr (ia1, ea), ((B.SizeOfExpr (eb), typ),ii) ->
      let ib1 = tuple_of_list1 ii in
      expression ea eb >>= (fun ea eb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
        return (
          ((A.SizeOfExpr (ia1, ea))) +> wa,
          ((B.SizeOfExpr (eb), typ),[ib1])
      )))

  | A.SizeOfType (ia1, ia2, typa, ia3), ((B.SizeOfType typb, typ),ii) ->
      let (ib1,ib2,ib3) = tuple_of_list3 ii in
      fullType typa typb >>= (fun typa typb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      tokenf ia3 ib3 >>= (fun ia3 ib3 ->
        return (
          ((A.SizeOfType (ia1, ia2, typa, ia3))) +> wa,
          ((B.SizeOfType (typb),typ),[ib1;ib2;ib3])
      )))))


  (* todo? iso ? allow all the combinations ? *)
  | A.Paren (ia1, ea, ia2), ((B.ParenExpr (eb), typ),ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      expression ea eb >>= (fun ea eb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        return (
          ((A.Paren (ia1, ea, ia2))) +> wa,
          ((B.ParenExpr (eb), typ), [ib1;ib2])
      ))))

  | A.NestExpr(starter,exps,ender,None,true), eb ->
      (match A.unwrap exps with
	[exp] ->
	  (* if minus and trafo do nothing *)
	  X.cocciExpExp (A.get_mcodekind starter)
	    expression exp eb >>= (fun exp eb ->
	  (* minus and trafo will do something here *)
          X.distrf_e (dots2metavar starter) eb >>= (fun mcode eb ->
            return (
            (A.NestExpr
	       (metavar2ndots mcode,
		A.rewrap exps [exp],ender,None,true)) +> wa,
            eb
            )
	  ))
      |	_ ->
	  failwith
	    "for nestexpr, only handling the case with dots and only one exp")

  | A.NestExpr _,     _ ->
      failwith "only handling multi and no when code in a nest expr"

  (* only in arg lists or in define body *)
  | A.TypeExp _,    _ -> fail

  | A.Constructor (ia1, typa, ia2, ia), ((B.Constructor (typb, ib), typ),ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      fullType typa typb >>= (fun typa typb ->
      initialiser ia ib >>= (fun ia ib ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        return (
          ((A.Constructor (ia1, typa, ia2, ia))) +> wa,
          ((B.Constructor (typb, ib),typ),[ib1;ib2])
        )))))

  (* only in arg lists *)
  | A.MetaExprList _,    _
  | A.EComma _,    _
      ->
	raise (Impossible 24)

  | A.DisjExpr eas, eb ->
      (* remains inside nests, not sure if this is necessary *)
      eas +> List.fold_left (fun acc ea -> acc >|+|> (expression ea eb)) fail

  | A.ConjExpr eas, eb ->
      let rec loop acc_exp eb = function
	  [] -> return (A.ConjExpr (List.rev acc_exp) +> wa, eb)
	| e::es ->
	    expression e eb >>= (fun exp eb ->
	      loop (exp::acc_exp) eb es) in
      loop [] eb eas

  | A.OptExp e,_ ->
      Pretty_print_cocci.expression e;
      Format.print_newline();
      failwith
        (Printf.sprintf "not handling Opt/Multi on expr on line %d"
           (A.get_line e))

 (* Because of Exp cant put a raise Impossible; have to put a fail *)

 (* have not a counter part in coccinelle, for the moment *)
  | _, ((B.Sequence _,_),_)
  | _, ((B.StatementExpr _,_),_)
  | _, ((B.New _,_),_)
  | _, ((B.Delete _,_),_)
  | _, ((B.Defined _,_),_)
    -> fail


  | _,
     (((B.Cast (_, _)|B.ParenExpr _|B.SizeOfType _|B.SizeOfExpr _|
     B.Constructor (_, _)|
     B.RecordPtAccess (_, _)|
     B.RecordAccess (_, _)|B.ArrayAccess (_, _)|
     B.Binary (_, _, _)|B.Unary (_, _)|
     B.Infix (_, _)|B.Postfix (_, _)|
     B.Assignment (_, _, _)|B.CondExpr (_, _, _)|
     B.FunCall (_, _)|B.Constant _|B.StringConstant _|B.Ident _),
     _),_)
       -> fail

(* Allow ... to match nothing.  Useful in for loop headers and in array
declarations.  Put a metavariable to require it to match something. *)
and (eoption:
       (A.expression,B.expression) matcher ->
	 (A.expression option,B.expression option) matcher) = fun f t1 t2 ->
  match (t1,t2) with
    (Some t, None) ->
      (match A.unwrap t with
	A.Edots(edots,None) ->
	  return (t1,t2)
      | _ -> option f t1 t2)
  | _ -> option f t1 t2

and assignOp opa opb =
  match (A.unwrap opa), opb with
    A.SimpleAssign a, (B.SimpleAssign, opb') ->
      let opbi = tuple_of_list1 opb' in
      tokenf a opbi >>= (fun a opbi ->
	return
	  (A.rewrap opa (A.SimpleAssign a), (B.SimpleAssign, [opbi])))
  | A.OpAssign oa, (B.OpAssign ob,opb') ->
    if equal_arithOp oa ob
    then
      let opbi = tuple_of_list1 opb' in
      tokenf oa opbi >>= (fun oa opbi_ ->
	return
          (A.rewrap opa (A.OpAssign oa), (B.OpAssign ob,[opbi])))
    else fail
  | A.MetaAssign (mv, c, keep, inherited), _ ->
    if check_assignOp_constraint opb c
    then begin
      let max_min _ =
        Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_assignOp opb) in
      X.envf keep inherited (mv,Ast_c.MetaAssignOpVal opb,max_min)
        (fun () -> X.distrf_assignOp mv opb)
      >>= (fun mv opb ->
            return (A.MetaAssign(mv,c,keep,inherited)+> A.rewrap opa,opb))
    end else fail
  | _ -> fail

and binaryOp opa opb =
  match (A.unwrap opa), opb with
    A.Arith oa, (B.Arith ob,opb') ->
      if equal_arithOp oa ob
      then
	let opbi = tuple_of_list1 opb' in
	tokenf oa opbi >>= (fun oa opbi ->
	  return
            (A.rewrap opa (A.Arith oa), (B.Arith ob,[opbi])))
      else fail
  | A.Logical oa, (B.Logical ob,opb') ->
      if equal_logicalOp oa ob
      then
	let opbi = tuple_of_list1 opb' in
	tokenf oa opbi >>= (fun oa opbi ->
	  return
            (A.rewrap opa (A.Logical oa), (B.Logical ob,[opbi])))
      else fail
  | A.MetaBinary (mv, c, keep, inherited), _ ->
    if check_binaryOp_constraint opb c
    then begin
      let max_min _ =
        Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_binaryOp opb) in
      X.envf keep inherited (mv,Ast_c.MetaBinaryOpVal opb,max_min)
        (fun () -> X.distrf_binaryOp mv opb)
      >>= (fun mv opb ->
            return (A.MetaBinary(mv,c,keep,inherited)+> A.rewrap opa,opb))
    end else fail
  | _ -> fail

and string_fragments eas ebs =
  let match_dots ea =
    match A.unwrap ea with
      A.Strdots(mcode) -> Some (mcode, None)
    |  _ -> None in
  let build_dots (mcode,_) = A.Strdots(mcode) in
  let match_comma ea = None in
  let build_comma _ = failwith "no commas" in
  let match_metalist ea =
    match A.unwrap ea with
      A.MetaFormatList(pct,ida,leninfo,keep,inherited) ->
        Some(ida,leninfo,keep,inherited,None)
    |  _ -> None in
  let build_metalist ea (ida,leninfo,keep,inherited) =
    match A.unwrap ea with
      A.MetaFormatList(pct,_,_,_,_) ->
	A.MetaFormatList(pct,ida,leninfo,keep,inherited)
    | _ -> failwith "not possible" in
  let mktermval v = Ast_c.MetaFragListVal v in
  let list_filter_function l =
    Some
      (List.filter
	 (function
	     B.FormatFragment _,_ -> true
	   | _ -> false)
	 l) in
  let special_cases ea eas ebs = None in
  list_matcher match_dots build_dots match_comma build_comma
    match_metalist build_metalist mktermval
    special_cases string_fragment X.distrf_fragments
    B.split_nocomma B.unsplit_nocomma
    Lib_parsing_c.ii_of_fragments list_filter_function eas ebs

and string_fragment ea (eb,ii) =
  X.all_bound (A.get_inherited ea) >&&>
  let wa x = A.rewrap ea x in
  match A.unwrap ea,eb with
    A.ConstantFragment(str1), B.ConstantFragment(str2)
      when A.unwrap_mcode str1 = str2 ->
      let ib1 = tuple_of_list1 ii in
      tokenf str1 ib1 >>= (fun str1 ib1 ->
	return
	  (A.ConstantFragment(str1) +> wa,
	   (B.ConstantFragment(str2),[ib1])))
  | A.FormatFragment(pct1,fmt1), B.FormatFragment(fmt2) ->
      let ib1 = tuple_of_list1 ii in
      tokenf pct1 ib1 >>= (fun pct1 ib1 ->
      string_format fmt1 fmt2 >>= (fun fmt1 fmt2 ->
	return
	  (A.FormatFragment(pct1,fmt1) +> wa,
	   (B.FormatFragment(fmt2), [ib1]))))
  | A.Strdots dots, eb -> failwith "not possible"
  | A.MetaFormatList(pct1,name1,lenname1,_,_), eb -> failwith "not possible"
  | _,_ -> fail

and string_format ea eb =
   let check_constraints constraints idb =
     match constraints with
       A.IdNoConstraint -> return ((),())
     | A.IdRegExpConstraint re ->
	 X.check_constraints satisfies_regexpconstraint re idb
	   (fun () -> return ((),()))
     | _ -> failwith "no nonid constraint for string format" in
  X.all_bound (A.get_inherited ea) >&&>
  let wa x = A.rewrap ea x in
  match A.unwrap ea,eb with
    A.ConstantFormat(str1), (B.ConstantFormat(str2),ii) ->
      let ib1 = tuple_of_list1 ii in
      if A.unwrap_mcode str1 = str2
      then
	tokenf str1 ib1 >>= (fun str1 ib1 ->
	  return
	    (A.ConstantFormat(str1) +> wa,
	     (B.ConstantFormat(str2),[ib1])))
      else fail
  | A.MetaFormat(ida,constraints,keep,inherited),(B.ConstantFormat(str2),ii) ->
      check_constraints constraints str2 >>=
      (fun () () ->
	let max_min _ =
	  Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_format eb) in
	X.envf keep inherited (ida,Ast_c.MetaFmtVal eb,max_min) (fun () ->
          X.distrf_format ida eb
            ) >>= (fun ida eb ->
              return (A.MetaFormat(ida,constraints,keep,inherited) +> wa,eb)))

(* ------------------------------------------------------------------------- *)

and exec_code_list eas ebs =
  let match_dots ea =
    match A.unwrap ea with
      A.ExecDots(mcode) -> Some (mcode, None)
    |  _ -> None in
  let build_dots (mcode,_) = A.ExecDots(mcode) in
  let match_comma ea = None in
  let build_comma _ = failwith "no commas" in
  let match_metalist ea = None in
  let build_metalist ea (ida,leninfo,keep,inherited) =
    failwith "no metalist" in
  let mktermval v = failwith "no metavariables" in
  let special_cases ea eas ebs = None in
  list_matcher match_dots build_dots match_comma build_comma
    match_metalist build_metalist mktermval
    special_cases exec_code X.distrf_exec_code_list
    B.split_nocomma B.unsplit_nocomma
    Lib_parsing_c.ii_of_exec_code_list (function x -> Some x) eas ebs

and exec_code ea (eb,ii) =
  X.all_bound (A.get_inherited ea) >&&>
  let wa x = A.rewrap ea x in
  match A.unwrap ea,eb with
    A.ExecEval(colon1,id1),B.ExecEval(id2) ->
      let colon2 = tuple_of_list1 ii in
      tokenf colon1 colon2 >>= (fun colon1 colon2 ->
      expression id1 id2 >>= (fun id1 id2 ->
	return(
	  A.ExecEval(colon1,id1) +> wa,
	  (B.ExecEval(id2),[colon2]))))
  | A.ExecToken(tok1),B.ExecToken ->
      let tok2 = tuple_of_list1 ii in
      tokenf tok1 tok2 >>= (fun tok1 tok2 ->
	return(
	  A.ExecToken(tok1) +> wa,
	  (B.ExecToken,[tok2])))
  | A.ExecDots(dots), eb -> failwith "not possible"
  | _,_ -> fail

(* ------------------------------------------------------------------------- *)
and (ident_cpp: info_ident -> (A.ident, B.name) matcher) =
 fun infoidb ida idb ->
   match idb with
   | B.RegularName (s, iis) ->
       let iis = tuple_of_list1 iis in
       ident infoidb ida (s, iis) >>= (fun ida (s,iis) ->
         return (
           ida,
           (B.RegularName (s, [iis]))
         ))
   | B.CppConcatenatedName _ | B.CppVariadicName _ |B.CppIdentBuilder _
       ->
	 (* This should be moved to the Id case of ident.  Metavariables
	 should be allowed to be bound to such variables.  But doing so
	 would require implementing an appropriate distr function *)
	 fail

and (ident: info_ident -> (A.ident, string * Ast_c.info) matcher) =
 fun infoidb ida ((idb, iib) as ib) -> (* (idb, iib) as ib *)
   let check_constraints constraints idb =
     match constraints with
       A.IdNoConstraint -> return ((),())
     | A.IdPosIdSet (str,meta) ->
	 X.check_constraints satisfies_iconstraint (str,meta) idb
	   (fun () -> return ((),()))
     | A.IdNegIdSet (str,meta) ->
	 X.check_constraints satisfies_niconstraint (str,meta) idb
	   (fun () -> return ((),()))
     | A.IdRegExpConstraint re ->
	 X.check_constraints satisfies_regexpconstraint re idb
	   (fun () -> return ((),())) in
  X.all_bound (A.get_inherited ida) >&&>
  match A.unwrap ida with
  | A.Id sa ->
      if (term sa) = idb then
      tokenf sa iib >>= (fun sa iib ->
        return (
          ((A.Id sa)) +> A.rewrap ida,
          (idb, iib)
        ))
      else fail

  | A.MetaId(mida,constraints,keep,inherited) ->
      check_constraints constraints idb >>=
      (fun () () ->
      let max_min _ = Lib_parsing_c.lin_col_by_pos [iib] in
      (* use drop_pos for ids so that the pos is not added a second time in
	 the call to tokenf *)
      X.envf keep inherited (A.drop_pos mida, Ast_c.MetaIdVal idb, max_min)
	(fun () ->
        tokenf mida iib >>= (fun mida iib ->
          return (
            ((A.MetaId (mida, constraints, keep, inherited)) +> A.rewrap ida,
            (idb, iib)
            )))
      ))

  | A.MetaFunc(mida,constraints,keep,inherited) ->
      let is_function _ =
	check_constraints constraints idb >>=
	(fun wrapper () ->
          let max_min _ = Lib_parsing_c.lin_col_by_pos [iib] in
          X.envf keep inherited (A.drop_pos mida,Ast_c.MetaFuncVal idb,max_min)
	    (fun () ->
            tokenf mida iib >>= (fun mida iib ->
              return (
                ((A.MetaFunc(mida,constraints,keep,inherited)))+>A.rewrap ida,
                (idb, iib)
              ))
          )) in
      (match infoidb with
      | LocalFunction | Function -> is_function()
      | DontKnow ->
	  failwith "MetaFunc, need more semantic info about id"
	  (* the following implementation could possibly be useful, if one
	     follows the convention that a macro is always in capital letters
	     and that a macro is not a function.
	  (if idb =~ "^[A-Z_][A-Z_0-9]*$" then fail else is_function())*)
      )

  | A.MetaLocalFunc(mida,constraints,keep,inherited) ->
      (match infoidb with
      | LocalFunction ->
	  check_constraints constraints idb >>=
	  (fun wrapper () ->
          let max_min _ = Lib_parsing_c.lin_col_by_pos [iib] in
          X.envf keep inherited
	    (A.drop_pos mida,Ast_c.MetaLocalFuncVal idb, max_min)
	    (fun () ->
            tokenf mida iib >>= (fun mida iib ->
              return (
                ((A.MetaLocalFunc(mida,constraints,keep,inherited)))
		   +> A.rewrap ida,
                (idb, iib)
              ))
          ))
      | Function -> fail
      | DontKnow -> failwith "MetaLocalFunc, need more semantic info about id"
      )

  | A.AsIdent(id,asid) ->
      ident infoidb id ib >>= (fun id ib ->
      ident infoidb asid ib >>= (fun asid ib ->
	return(
	  ((A.AsIdent(id,asid)) +> A.rewrap ida,
	   ib))))

  (* not clear why disj things are needed, after disjdistr? *)
  | A.DisjId ias ->
      failwith "DisjId should not arise"
(*
      ias +> List.fold_left (fun acc ia -> acc >|+|> (ident infoidb ia ib)) fail
*)

  | A.OptIdent _ -> failwith "not handling Opt for ident"

(* ------------------------------------------------------------------------- *)
and (arguments: sequence ->
  (A.expression list, Ast_c.argument Ast_c.wrap2 list) matcher) =
  fun seqstyle eas ebs ->
    match seqstyle with
    | Unordered -> failwith "not handling ooo"
    | Ordered ->
	arguments_bis eas (Ast_c.split_comma ebs) >>= (fun eas ebs_splitted ->
          return (eas, (Ast_c.unsplit_comma ebs_splitted))
	    )
(* because '...' can match nothing, need to take care when have
   * ', ...'   or '...,'  as in  f(..., X, Y, ...). It must match
   * f(1,2) for instance.
   * So I have added special cases such as (if startxs = []) and code
   * in the Ecomma matching rule.
   *
   * old: Must do some try, for instance when f(...,X,Y,...) have to
   * test the transfo for all the combinations    and if multiple transfo
   * possible ? pb ? => the type is to return a expression option ? use
   * some combinators to help ?
   * update: with the tag-SP approach, no more a problem.
*)

and arguments_bis = fun eas ebs ->
  let match_dots ea =
    match A.unwrap ea with
      A.Edots(mcode, optexpr) -> Some (mcode, optexpr)
    | _ -> None in
  let build_dots (mcode, optexpr) = A.Edots(mcode, optexpr) in
  let match_comma ea =
    match A.unwrap ea with
      A.EComma ia1 -> Some ia1
    | _ -> None in
  let build_comma ia1 = A.EComma ia1 in
  let match_metalist ea =
    match A.unwrap ea with
      A.MetaExprList(ida,leninfo,keep,inherited) ->
	Some(ida,leninfo,keep,inherited,None)
    | _ -> None in
  let build_metalist _ (ida,leninfo,keep,inherited) =
    A.MetaExprList(ida,leninfo,keep,inherited) in
  let mktermval v = Ast_c.MetaExprListVal v in
  let special_cases ea eas ebs = None in
  list_matcher match_dots build_dots match_comma build_comma
    match_metalist build_metalist mktermval
    special_cases argument X.distrf_args B.split_comma  B.unsplit_comma
    Lib_parsing_c.ii_of_args (function x -> Some x) eas ebs

and argument arga argb =
  X.all_bound (A.get_inherited arga) >&&>
  match A.unwrap arga, argb with
  | A.TypeExp tya,
    Right (B.ArgType {B.p_register=b,iib; p_namei=sopt;p_type=tyb}) ->
      if b || sopt <> None
      then
        (* failwith "the argument have a storage and ast_cocci does not have"*)
        fail
      else
        (* b = false and sopt = None *)
        fullType tya tyb >>= (fun tya tyb ->
          return (
            (A.TypeExp tya) +> A.rewrap arga,
            (Right (B.ArgType {B.p_register=(b,iib);
                               p_namei=sopt;
                               p_type=tyb;}))
        ))

  | A.TypeExp tya,  _                                  -> fail
  | _,              Right (B.ArgType _) -> fail
  | _, Left argb ->
      expression arga argb >>= (fun arga argb ->
        return (arga, Left argb)
      )
  | _, Right (B.ArgAction y) -> fail

and (ident_list :
  (A.ident list, Ast_c.name Ast_c.wrap2 list) matcher) =
  fun eas ebs ->
    ident_list_bis eas (Ast_c.split_comma ebs) >>= (fun eas ebs_splitted ->
          return (eas, (Ast_c.unsplit_comma ebs_splitted))
	    )

and ident_list_bis = fun eas ebs ->
  let match_dots ea = None in
  let build_dots (mcode, optexpr) = failwith "no dots in ident list" in
  let match_comma ea = None in
  let build_comma ia1 = failwith "no comma in ident list" in
  let match_metalist ea = None in
  let build_metalist _ (ida,leninfo,keep,inherited) =
    failwith "no metalist in ident list" in
  let mktermval v = failwith "no metalist in ident list" in
  let special_cases ea eas ebs = None in
  list_matcher match_dots build_dots match_comma build_comma
    match_metalist build_metalist mktermval
    special_cases (ident_cpp DontKnow) X.distrf_ident_list
    B.split_comma B.unsplit_comma
    Lib_parsing_c.ii_of_ident_list (function x -> Some x) eas ebs


(* ------------------------------------------------------------------------- *)
(* todo? facto code with argument ? *)
and (parameters: sequence ->
      (A.parameterTypeDef list, Ast_c.parameterType Ast_c.wrap2 list)
        matcher) =
 fun seqstyle eas ebs ->
  match seqstyle with
  | Unordered -> failwith "not handling ooo"
  | Ordered ->
      parameters_bis eas (Ast_c.split_comma ebs) >>= (fun eas ebs_splitted ->
        return (eas, (Ast_c.unsplit_comma ebs_splitted))
      )

and parameters_bis eas ebs =
  let match_dots ea =
    match A.unwrap ea with
      A.Pdots(mcode) -> Some (mcode, None)
    | _ -> None in
  let build_dots (mcode, _optexpr) = A.Pdots(mcode) in
  let match_comma ea =
    match A.unwrap ea with
      A.PComma ia1 -> Some ia1
    | _ -> None in
  let build_comma ia1 = A.PComma ia1 in
  let match_metalist ea =
    let rec loop acc p =
      match A.unwrap p with
	A.AsParam(p,e) -> loop (e :: acc) p
      | A.MetaParamList(ida,leninfo,keep,inherited) ->
	  Some ((ida,leninfo,keep,inherited),acc)
      | _ -> None in
    match loop [] ea with
      Some ((ida,leninfo,keep,inherited),ids) ->
	(match ids with
	  [] -> Some(ida,leninfo,keep,inherited,None)
	| _ ->
	    let extra vl max_min k =
	      let vl =
		Ast_c.MetaExprListVal
		     (List.map
			(function (v,i) ->
			  match v.Ast_c.p_namei with
			    Some name ->
			      (Left(Ast_c.mk_e (B.Ident name) Ast_c.noii),i)
			  | None -> failwith "no name in parameter list")
			vl) in
	      let rec loop = function
		  [] -> k ()
		| x::xs ->
		    (match A.unwrap x with
		      A.MetaExprList(ida,A.AnyListLen,keep,inherited) ->
			X.envf keep inherited
			  (ida, vl, max_min)
			  (fun () -> loop xs)
		    | A.MetaExprList _ ->
			failwith "length not supported"
		    | _ -> failwith "unexpected expression") in
	      loop ids in
	    Some(ida,leninfo,keep,inherited,Some extra))
    | None -> None in
  let rec build_metalist ea (ida,leninfo,keep,inherited) =
    match A.unwrap ea with
      A.MetaParamList _ ->
	A.MetaParamList(ida,leninfo,keep,inherited)
    | A.AsParam(p,e) ->
	A.AsParam(A.rewrap p (build_metalist p (ida,leninfo,keep,inherited)),
		  e)
    | _ -> failwith "not possible" in
  let mktermval v = Ast_c.MetaParamListVal v in
  let special_cases ea eas ebs =
    (* a case where one smpl parameter matches a list of C parameters *)
    match A.unwrap ea,ebs with
      A.VoidParam ta, ys ->
	Some
          (match eas, ebs with
          | [], [Left eb] ->
              let {B.p_register=(hasreg,iihasreg);
                    p_namei = idbopt;
                    p_type=tb; } = eb in

              if idbopt = None && not hasreg
              then
                match tb with
                | (qub, (B.BaseType B.Void,_)) ->
                    fullType ta tb >>= (fun ta tb ->
                      return (
                      [(A.VoidParam ta) +> A.rewrap ea],
                      [Left {B.p_register=(hasreg, iihasreg);
                              p_namei = idbopt;
                              p_type = tb;}]
			))
                | _ -> fail
              else fail
          | _ -> fail)
    | _ -> None in
  list_matcher match_dots build_dots match_comma build_comma
    match_metalist build_metalist mktermval
    special_cases parameter X.distrf_params
    B.split_comma B.unsplit_comma
    Lib_parsing_c.ii_of_params (function x -> Some x) eas ebs

(*
   let split_register_param = fun (hasreg, idb, ii_b_s) ->
   match hasreg, idb,  ii_b_s with
   | false, Some s, [i1] -> Left (s, [], i1)
   | true, Some s, [i1;i2] -> Left (s, [i1], i2)
   | _, None, ii -> Right ii
   | _ -> raise Impossible
*)


and parameter = fun parama paramb ->
  match A.unwrap parama, paramb with
    A.MetaParam (ida,keep,inherited), eb ->
      (* todo: use quaopt, hasreg ? *)
      let max_min _ =
	Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_param eb) in
      X.envf keep inherited (ida,Ast_c.MetaParamVal eb,max_min) (fun () ->
        X.distrf_param ida eb
          ) >>= (fun ida eb ->
            return (A.MetaParam(ida,keep,inherited)+> A.rewrap parama,eb))
  | A.Param (typa, idaopt), eb ->
      let {B.p_register = (hasreg,iihasreg);
	    p_namei = nameidbopt;
	    p_type = typb;} = paramb in

      fullType typa typb >>= (fun typa typb ->
	match idaopt, nameidbopt with
	| Some ida, Some nameidb ->
      (* todo: if minus on ida, should also minus the iihasreg ? *)
	    ident_cpp DontKnow ida nameidb >>= (fun ida nameidb ->
              return (
              A.Param (typa, Some ida)+> A.rewrap parama,
              {B.p_register = (hasreg, iihasreg);
		p_namei = Some (nameidb);
		p_type = typb}
		))

	| None, None ->
	    return (
            A.Param (typa, None)+> A.rewrap parama,
            {B.p_register=(hasreg,iihasreg);
              p_namei = None;
              p_type = typb;}
	      )
  (* why handle this case ? because of transform_proto ? we may not
   * have an ident in the proto.
   * If have some plus on ida ? do nothing about ida ?
   *)
 (* not anymore !!! now that julia is handling the proto.
  | _, Right iihasreg ->
      return (
        (idaopt, typa),
        ((hasreg, None, typb), iihasreg)
      )
 *)

	| Some _, None -> fail
	| None, Some _ -> fail)
  | A.OptParam _, _ ->
      failwith "not handling Opt for Param"
  | _ -> fail

(* ------------------------------------------------------------------------- *)
and (declaration: (A.mcodekind * bool * A.declaration,B.declaration) matcher) =
 fun (mckstart, allminus, decla) declb ->
  X.all_bound (A.get_inherited decla) >&&>
  match A.unwrap decla, declb with

  (* Un MetaDecl est introduit dans l'asttoctl pour sauter au dessus
   * de toutes les declarations qui sont au debut d'un fonction et
   * commencer le reste du match au premier statement. Alors, ca matche
   * n'importe quelle declaration. On n'a pas besoin d'ajouter
   * quoi que ce soit dans l'environnement. C'est une sorte de DDots.
   *
   * When the SP want to remove the whole function, the minus is not
   * on the MetaDecl but on the MetaRuleElem. So there should
   * be no transform of MetaDecl, just matching are allowed.
   *)

  | A.MetaDecl (ida,keep,inherited), _ ->
      let max_min _ =
	Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_decl declb) in
      X.envf keep inherited (ida, Ast_c.MetaDeclVal declb, max_min) (fun () ->
        X.distrf_decl ida declb
          ) >>= (fun ida declb ->
	    return ((mckstart, allminus,
		     (A.MetaDecl (ida, keep, inherited))+> A.rewrap decla),
		    declb))

  | A.AsDecl(dec,asdec), decb ->
      declaration (mckstart, allminus, dec) decb >>=
      (fun (mckstart, allminus, dec) decb ->
	let asmckstart = A.CONTEXT(A.NoPos,A.NOTHING) in
      declaration (asmckstart,false,asdec) decb >>= (fun (_,_,asdec) decb ->
	return(
	((mckstart, allminus,
	  (A.AsDecl(dec,asdec)) +> A.rewrap decla),
	 decb))))

  | _, (B.DeclList ([var], iiptvirgb::iifakestart::iisto)) ->
      onedecl allminus decla (var,iiptvirgb,iisto) >>=
      (fun decla (var,iiptvirgb,iisto)->
        X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart ->
          return (
            (mckstart, allminus, decla),
            (B.DeclList ([var], iiptvirgb::iifakestart::iisto))
          )))

  | _, (B.DeclList (xs, (iiptvirgb::iifakestart::iisto))) ->
      let indexify l =
	let rec loop n = function
	    [] -> []
	  | x::xs -> (n,x)::(loop (n+1) xs) in
	loop 0 l in
      let rec repln n vl cur = function
	  [] -> []
	| x::xs ->
	    if n = cur then vl :: xs else x :: (repln n vl (cur+1) xs) in
      let doit _ =
	(indexify xs) +> List.fold_left (fun acc (n,var) ->
	  (* consider all possible matches *)
          acc >||> (function tin -> (
            onedecl allminus decla (var, iiptvirgb, iisto) >>=
            (fun decla (var, iiptvirgb, iisto) ->
	      (* tokenf has to be after the onedecl, because ondecl
		 detects whether there is actually a match and the
		 tokenf should be done *)
              X.tokenf_mck mckstart iifakestart >>=
	      (fun mckstart iifakestart ->
                return (
                (mckstart, allminus, decla),
		    (* adjust the variable that was chosen *)
                (B.DeclList (repln n var 0 xs,
			     iiptvirgb::iifakestart::iisto))
                  )))) tin))
          fail in
      if !Flag.sgrep_mode2(*X.mode = PatternMode *) || A.get_safe_decl decla
      then doit()
      else
	begin
	  (* rather clunky... we only want to print the warning message if
	     there is a match *)
	  let firstii = iiptvirgb in
	  let contextified_decla =
	    let mcode (x,info,mc,pos) =
	      let newmc =
		match mc with
		  A.MINUS(pos,_,_,_)
		| A.CONTEXT(pos,_) -> A.CONTEXT(pos,A.NOTHING)
		| _ -> failwith "only minus/context expected in pattern" in
	      (x,info,newmc,pos) in
	    let donothing r k e = k e in
	    let v =
	      Visitor_ast.rebuilder
		mcode mcode mcode mcode mcode mcode mcode mcode mcode
		mcode mcode mcode mcode mcode
		donothing donothing donothing donothing donothing
		donothing donothing donothing donothing donothing donothing
		donothing donothing donothing donothing donothing donothing
		donothing donothing donothing donothing donothing in
	    v.Visitor_ast.rebuilder_declaration decla in

	  xs +> List.fold_left (fun acc var ->
	  (* consider all possible matches *)
            (function tin -> (
              onedecl allminus contextified_decla (var, iiptvirgb, iisto) >>=
              (fun _ _ ->
		pr2_once
		  (Printf.sprintf "%s: %d: %s"
		     (Ast_c.file_of_info firstii) (Ast_c.line_of_info firstii)
		     "More than one variable in the declaration, and so it cannot be transformed.  Check that there is no transformation on the type or the ;");
		fail)) tin))
	      fail
	end

  | A.MacroDecl (stoa,sa,lpa,eas,rpa,enda),
	B.MacroDecl ((stob,sb,ebs,true),ii) ->
      let (iisb, lpb, rpb, iiendb, iifakestart, iistob) =
        (match ii with
        | iisb::lpb::rpb::iiendb::iifakestart::iisto ->
            (iisb,lpb,rpb,iiendb, iifakestart,iisto)
        | _ -> raise (Impossible 26)
        ) in
        storage_optional_allminus allminus
          stoa ((stob, false), iistob) >>= (fun stoa ((stob, _), iistob) ->
        X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart ->
	ident DontKnow sa (sb, iisb) >>= (fun sa (sb, iisb) ->
        tokenf lpa lpb >>= (fun lpa lpb ->
        tokenf rpa rpb >>= (fun rpa rpb ->
        tokenf enda iiendb >>= (fun enda iiendb ->
        arguments (seqstyle eas) (A.unwrap eas) ebs >>= (fun easunwrap ebs ->
        let eas = A.rewrap eas easunwrap in

          return (
            (mckstart, allminus,
            (A.MacroDecl (stoa,sa,lpa,eas,rpa,enda)) +> A.rewrap decla),
            (B.MacroDecl ((stob,sb,ebs,true),
                         [iisb;lpb;rpb;iiendb;iifakestart] @ iistob))
          ))))))))

  | A.MacroDecl (None,sa,lpa,eas,rpa,enda),
      B.MacroDecl ((B.NoSto,sb,ebs,false),ii) ->
	(* This is for macrodecls with no semicolons, which come from
	   a parsing rule that deals with function prototypes with no
	   return type.  That parsing rule would have a conflict if there
	   were storage, so there is no point to treat the possibility of
	   storage here. *)
      X.optional_declarer_semicolon_flag (fun optional_declarer_semicolon ->
      match mcodekind enda, optional_declarer_semicolon with
	A.CONTEXT (_,A.NOTHING), true ->
	  let (iisb, lpb, rpb, iifakestart) =
            (match ii with
            | [iisb;lpb;rpb;iifakestart] ->
		(iisb,lpb,rpb,iifakestart)
            | _ -> raise (Impossible 27)) in
	    X.tokenf_mck mckstart iifakestart >>=
	    (fun mckstart iifakestart ->
	      ident DontKnow sa (sb, iisb) >>= (fun sa (sb, iisb) ->
	      tokenf lpa lpb >>= (fun lpa lpb ->
	      tokenf rpa rpb >>= (fun rpa rpb ->
	      arguments (seqstyle eas) (A.unwrap eas) ebs >>=
		(fun easunwrap ebs ->
		  let eas = A.rewrap eas easunwrap in

		  return (
		  (mckstart, allminus,
		   (A.MacroDecl (None,sa,lpa,eas,rpa,enda)) +> A.rewrap decla),
		  (B.MacroDecl ((B.NoSto,sb,ebs,false),
				[iisb;lpb;rpb;iifakestart]))
		  ))))))
      | _ -> fail)

  | A.MacroDeclInit (stoa,sa,lpa,eas,rpa,weqa,inia,enda),
      B.MacroDeclInit ((stob,sb,ebs,inib),ii) ->
      let (iisb, lpb, rpb, weqb, iiendb, iifakestart, iistob) =
        (match ii with
        |  iisb::lpb::rpb::weqb::iiendb::iifakestart::iisto ->
            (iisb,lpb,rpb,weqb,iiendb, iifakestart,iisto)
        |  _ -> raise (Impossible 28)
        ) in
        storage_optional_allminus allminus
          stoa ((stob, false), iistob) >>= (fun stoa ((stob, _), iistob) ->
        X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart ->
        ident DontKnow sa (sb, iisb) >>= (fun sa (sb, iisb) ->
        tokenf lpa lpb >>= (fun lpa lpb ->
        tokenf rpa rpb >>= (fun rpa rpb ->
        tokenf weqa weqb >>= (fun weqa weqb ->
        tokenf enda iiendb >>= (fun enda iiendb ->
        arguments (seqstyle eas) (A.unwrap eas) ebs >>= (fun easunwrap ebs ->
	initialiser inia inib >>= (fun inia inib ->
        let eas = A.rewrap eas easunwrap in

          return (
            (mckstart, allminus,
             (A.MacroDeclInit(stoa,sa,lpa,eas,rpa,weqa,inia,enda)) +>
	     A.rewrap decla),
            (B.MacroDeclInit ((stob,sb,ebs,inib),
                         [iisb;lpb;rpb;iiendb;iifakestart] @ iistob))
          ))))))))))


  | A.MacroDeclInit (stoa,sa,lpa,eas,rpa,weqa,inia,enda), _ -> fail

  | _, (B.MacroDecl _ |B.MacroDeclInit _ |B.DeclList _) -> fail


and annotated_decl decla declb =
  match A.unwrap decla with
    A.Ddots _ -> failwith "not possible"
  | A.DElem(mckstart, allminus, decl) ->
      declaration (mckstart, allminus, decl) declb >>=
      fun (mckstart, allminus, decl) declb ->
	return
	  (A.DElem(mckstart, allminus, decl) +> A.rewrap decla,
	   declb)

and onedecl = fun allminus decla (declb, iiptvirgb, iistob) ->
 X.all_bound (A.get_inherited decla) >&&>
 match A.unwrap decla, declb with

 (* kind of typedef iso, we must unfold, it's for the case
  * T { }; that we want to match against typedef struct { } xx_t;
  *)

 | A.TyDecl (tya0, ptvirga),
   ({B.v_namei = Some (nameidb, B.NoInit);
     B.v_type = typb0;
     B.v_storage = (B.StoTypedef, inl);
     B.v_local = local;
     B.v_attr = attrs;
     B.v_type_bis = typb0bis;
   }, iivirg) ->

   (match A.unwrap tya0, typb0 with
   | A.Type(allminus,cv1,tya1), ((qu,il),typb1) ->
       (* allminus doesn't seem useful here - nothing done with cv1 *)

     (match A.unwrap tya1, typb1 with
     | A.StructUnionDef(tya2, lba, declsa, rba),
      (B.StructUnion (sub, sbopt, declsb), ii) ->

       let (iisub, iisbopt, lbb, rbb) =
         match sbopt with
         | None ->
             let (iisub, lbb, rbb) = tuple_of_list3 ii in
             (iisub, [], lbb, rbb)
         | Some s ->
             pr2 (Printf.sprintf
              "warning: both a typedef (%s) and struct name introduction (%s)"
              (Ast_c.str_of_name nameidb) s
             );
             pr2 "warning: I will consider only the typedef";
             let (iisub, iisb, lbb, rbb) = tuple_of_list4 ii in
             (iisub, [iisb], lbb, rbb)
       in
       let structnameb =
         structdef_to_struct_name
           (Ast_c.nQ, (B.StructUnion (sub, sbopt, declsb), ii))
       in
       let fake_typeb =
         Ast_c.nQ,((B.TypeName (nameidb, Some
           (Lib_parsing_c.al_type structnameb))), [])
       in

       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb ->
       tokenf lba lbb >>= (fun lba lbb ->
       tokenf rba rbb >>= (fun rba rbb ->
       struct_fields (A.unwrap declsa) declsb >>= (fun undeclsa declsb ->
         let declsa = A.rewrap declsa undeclsa in

         (match A.unwrap tya2 with
         | A.Type(allminus, cv3, tya3) -> (* again allminus not used *)
           (match A.unwrap tya3 with
           | A.MetaType(ida,keep, inherited) ->

               fullType tya2 fake_typeb >>= (fun tya2 fake_typeb ->
		 let tya1 =
		   A.StructUnionDef(tya2,lba,declsa,rba)+> A.rewrap tya1 in
		 let tya0 = A.Type(allminus, cv1, tya1) +> A.rewrap tya0 in


		 let typb1 = B.StructUnion (sub,sbopt, declsb),
                   [iisub] @ iisbopt @ [lbb;rbb] in
		 let typb0 = ((qu, il), typb1) in

		 match fake_typeb with
		 | _nQ, ((B.TypeName (nameidb, _typ)),[]) ->

                     return (
                     (A.TyDecl (tya0, ptvirga)) +> A.rewrap decla,
                     (({B.v_namei = Some (nameidb, B.NoInit);
                        B.v_type = typb0;
                        B.v_storage = (B.StoTypedef, inl);
                        B.v_local = local;
                        B.v_attr = attrs;
                        B.v_type_bis = typb0bis;
                     },
                       iivirg),iiptvirgb,iistob)
                     )
		 | _ -> raise (Impossible 29)
             )

	   (* do we need EnumName here too? *)
           | A.StructUnionName(sua, sa) ->
             fullType tya2 structnameb >>= (fun tya2 structnameb ->

               let tya1 = A.StructUnionDef(tya2,lba,declsa,rba)+> A.rewrap tya1
               in
               let tya0 = A.Type(allminus, cv1, tya1) +> A.rewrap tya0 in

               match structnameb with
               | _nQ, (B.StructUnionName (sub, s), [iisub;iisbopt]) ->

                   let typb1 = B.StructUnion (sub,sbopt, declsb),
                     [iisub;iisbopt;lbb;rbb] in
                   let typb0 = ((qu, il), typb1) in

                   return (
                     (A.TyDecl (tya0, ptvirga)) +> A.rewrap decla,
                     (({B.v_namei = Some (nameidb, B.NoInit);
                        B.v_type = typb0;
                        B.v_storage = (B.StoTypedef, inl);
                        B.v_local = local;
                        B.v_attr = attrs;
                        B.v_type_bis = typb0bis;
                     },
                      iivirg),iiptvirgb,iistob)
                   )
               | _ -> raise (Impossible 30)
             )
           | _ -> raise (Impossible 31)
           )
         | _ -> fail
       )))))
     | _ -> fail
     )
   | _ -> fail
   )

   | A.UnInit (stoa, typa, ida, ptvirga),
     ({B.v_namei= Some (nameidb, _);B.v_storage= (B.StoTypedef,_);}, iivirg)
     -> fail

   | A.Init (stoa, typa, ida, eqa, inia, ptvirga),
     ({B.v_namei=Some(nameidb, _);B.v_storage=(B.StoTypedef,_);}, iivirg)
       -> fail



    (* could handle iso here but handled in standard.iso *)
   | A.UnInit (stoa, typa, ida, ptvirga),
     ({B.v_namei = Some (nameidb, B.NoInit);
       B.v_type = typb;
       B.v_storage = stob;
       B.v_local = local;
       B.v_attr = attrs;
       B.v_type_bis = typbbis;
     }, iivirg) ->
       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb ->
       fullType typa typb >>= (fun typa typb ->
       ident_cpp DontKnow ida nameidb >>= (fun ida nameidb ->
       storage_optional_allminus allminus stoa (stob, iistob) >>=
        (fun stoa (stob, iistob) ->
         return (
           (A.UnInit (stoa, typa, ida, ptvirga)) +>  A.rewrap decla,
           (({B.v_namei = Some (nameidb, B.NoInit);
              B.v_type = typb;
              B.v_storage = stob;
              B.v_local = local;
              B.v_attr = attrs;
              B.v_type_bis = typbbis;
           },iivirg),
	    iiptvirgb,iistob)
         )))))

   | A.Init (stoa, typa, ida, eqa, inia, ptvirga),
     ({B.v_namei = Some(nameidb, B.ValInit (iieqb, inib));
       B.v_type = typb;
       B.v_storage = stob;
       B.v_local = local;
       B.v_attr = attrs;
       B.v_type_bis = typbbis;
     },iivirg)
       ->
       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb ->
       tokenf eqa iieqb >>= (fun eqa iieqb ->
       fullType typa typb >>= (fun typa typb ->
       ident_cpp DontKnow ida nameidb >>= (fun ida nameidb ->
       storage_optional_allminus allminus stoa (stob, iistob) >>=
       (fun stoa (stob, iistob) ->
       initialiser inia inib >>= (fun inia inib ->
         return (
           (A.Init (stoa, typa, ida, eqa, inia, ptvirga)) +> A.rewrap decla,
           (({B.v_namei = Some(nameidb, B.ValInit (iieqb, inib));
              B.v_type = typb;
              B.v_storage = stob;
              B.v_local = local;
              B.v_attr = attrs;
              B.v_type_bis = typbbis;
           },iivirg),
           iiptvirgb,iistob)
         )))))))

   | A.Init (stoa, typa, ida, eqa, inia, ptvirga),
     ({B.v_namei = Some(nameidb, B.ConstrInit _);
       B.v_type = typb;
       B.v_storage = stob;
       B.v_local = local;
       B.v_attr = attrs;
       B.v_type_bis = typbbis;
     },iivirg)
       -> fail (* C++ constructor declaration not supported in SmPL *)

   | A.FunProto(fninfoa,ida,lpa,paramsa,va,rpa,sema),
     ({B.v_namei = Some (idb, B.NoInit);
       B.v_type =
	((({B.const = false; B.volatile = false},[]) as q),
	 (B.FunctionType(tyb, (paramsb, (isvaargs, iidotsb))), ii));
       B.v_storage = stob;
       B.v_local = local;
       B.v_attr = attrs;
       B.v_type_bis = typbbis;
     }, iivirg) ->
       (match (va,isvaargs) with
        | (None,false) -> return (va,(isvaargs, iidotsb))
        | (Some (commaa, dotsa), true) ->
           let (commab, dotsb) = tuple_of_list2 iidotsb in
           tokenf commaa commab >>= (fun commaa commab ->
           tokenf dotsa dotsb >>= (fun dotsa dotsb ->
           return (Some(commaa,dotsa), (true,[commab;dotsb]))
                                  ))
        | _ -> fail
       ) >>=
        (fun va (isvaargs, iidotsb) -> let (lpb, rpb) = tuple_of_list2 ii in
        tokenf lpa lpb >>= (fun lpa lpb ->
        tokenf rpa rpb >>= (fun rpa rpb ->
        tokenf sema iiptvirgb >>= (fun sema iiptvirgb ->
        ident_cpp DontKnow ida idb >>= (fun ida idb ->
	let (stoa,tya,inla,attras) = get_fninfo fninfoa in
        inline_optional_allminus allminus
          inla (stob, iistob) >>= (fun inla (stob, iistob) ->
        storage_optional_allminus allminus
          stoa (stob, iistob) >>= (fun stoa (stob, iistob) ->
        attribute_list attras attrs >>= (fun attras attrs ->
        fullType_optional_allminus allminus tya tyb >>= (fun tya tyb ->
	let fninfoa = put_fninfo stoa tya inla attras in
        parameters (seqstyle paramsa) (A.unwrap paramsa) paramsb >>=
          (fun paramsaunwrap paramsb ->
            let paramsa = A.rewrap paramsa paramsaunwrap in
            return (
              (A.FunProto(fninfoa,ida,lpa,paramsa,va,rpa,sema) +> A.rewrap decla,
	       (({B.v_namei = Some (idb, B.NoInit);
		  B.v_type =
		  (q,
		   (B.FunctionType(tyb, (paramsb, (isvaargs, iidotsb))),
		    [lpb; rpb]));
		  B.v_storage = stob;
		  B.v_local = local;
		  B.v_attr = attrs;
		  B.v_type_bis = typbbis;
		}, iivirg), iiptvirgb, iistob))))
	      )))))))))

   (* do iso-by-absence here ? allow typedecl and var ? *)
   | A.TyDecl (typa, ptvirga),
     ({B.v_namei = None; B.v_type = typb;
       B.v_storage = stob;
       B.v_local = local;
       B.v_attr = attrs;
       B.v_type_bis = typbbis;
     }, iivirg)  ->

       if stob = (B.NoSto, false)
       then
         tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb ->
         fullType typa typb >>= (fun typa typb ->
           return (
             (A.TyDecl (typa, ptvirga)) +> A.rewrap decla,
             (({B.v_namei = None;
                B.v_type = typb;
                B.v_storage = stob;
                B.v_local = local;
                B.v_attr = attrs;
                B.v_type_bis = typbbis;
             }, iivirg), iiptvirgb, iistob)
           )))
       else fail


   | A.Typedef (stoa, typa, ida, ptvirga),
     ({B.v_namei = Some (nameidb, B.NoInit);
       B.v_type = typb;
       B.v_storage = (B.StoTypedef,inline);
       B.v_local = local;
       B.v_attr = attrs;
       B.v_type_bis = typbbis;
     },iivirg) ->

       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb ->
       fullType typa typb >>= (fun typa typb ->
       (match iistob with
       | [iitypedef] ->
           tokenf stoa iitypedef >>= (fun stoa iitypedef ->
             return (stoa, [iitypedef])
           )
       | _ -> error iistob "weird, have both typedef and inline or nothing";
       ) >>= (fun stoa iistob ->
       (match A.unwrap ida with
       | A.MetaType(_,_,_) ->

           let fake_typeb =
             Ast_c.nQ, ((B.TypeName (nameidb, Ast_c.noTypedefDef())), [])
           in
           fullTypebis ida fake_typeb >>= (fun ida fake_typeb ->
             match fake_typeb with
             | _nQ, ((B.TypeName (nameidb, _typ)), []) ->
                 return (ida, nameidb)
             | _ -> raise (Impossible 32)
           )

       | A.TypeName sa ->
           (match nameidb with
           | B.RegularName (sb, iidb) ->
               let iidb1 = tuple_of_list1 iidb in

               if (term sa) = sb
               then
                 tokenf sa iidb1 >>= (fun sa iidb1 ->
                   return (
                     (A.TypeName sa) +> A.rewrap ida,
                     B.RegularName (sb, [iidb1])
                   ))
               else fail

           | B.CppConcatenatedName _ | B.CppVariadicName _ |B.CppIdentBuilder _
               -> raise Todo
           )

       | _ -> raise (Impossible 33)

       ) >>= (fun ida nameidb ->
         return (
           (A.Typedef (stoa, typa, ida, ptvirga)) +> A.rewrap decla,
           (({B.v_namei = Some (nameidb, B.NoInit);
              B.v_type = typb;
              B.v_storage = (B.StoTypedef,inline);
              B.v_local = local;
              B.v_attr = attrs;
              B.v_type_bis = typbbis;
           },
	     iivirg),
            iiptvirgb, iistob)
         )
       ))))


   | _, ({B.v_namei = None;}, _) ->
       (* old:   failwith "no variable in this declaration, weird" *)
      fail



   | A.DisjDecl declas, declb -> failwith "DisjDecl should not arise"
(*
      declas +> List.fold_left (fun acc decla ->
        acc >|+|>
            (* (declaration (mckstart, allminus, decla) declb) *)
            (onedecl allminus decla (declb,iiptvirgb, iistob))
      ) fail
*)

   | A.OptDecl _,    _ ->
       failwith "not handling Opt Decl"

   | _, ({B.v_namei=Some _}, _) ->
       fail

and get_fninfo fninfoa =
      (* fninfoa records the order in which the SP specified the various
	 information, but this isn't taken into account in the matching.
	 Could this be a problem for transformation? *)
  let stoa =
    match
      List.filter (function A.FStorage(s) -> true | _ -> false) fninfoa
    with [A.FStorage(s)] -> Some s | _ -> None in
  let tya =
    match List.filter (function A.FType(s) -> true | _ -> false) fninfoa
    with [A.FType(t)] -> Some t | _ -> None in

  let inla =
    match List.filter (function A.FInline(i) -> true | _ -> false) fninfoa
    with [A.FInline(i)] -> Some i | _ -> None in

  let attras =
    match List.filter (function A.FAttr(a) -> true | _ -> false) fninfoa
    with
      [] -> None | _ -> failwith "matching of attributes not supported"
	(* The following provides matching of one attribute against one
	   attribute.  But the problem is that in the C ast there are no
	   attributes in the attr field.  The attributes are all comments.
	   So there is nothing to match against. *)
	(*  [A.FAttr(a)] -> Some [A.FAttr(a)]
	| [] -> None
	| _ -> failwith "only one attr match allowed" *) in
  (stoa,tya,inla,attras)

and put_fninfo stoa tya inla attras =
  (match stoa  with Some st -> [A.FStorage st] | None -> []) @
    (match inla   with Some i -> [A.FInline i] | None -> []) @
    (match tya    with Some t -> [A.FType t] | None -> []) @
    (match attras with Some a -> a | None -> [])

(* ------------------------------------------------------------------------- *)

and (initialiser: (A.initialiser, Ast_c.initialiser) matcher) =  fun ia ib ->
    X.all_bound (A.get_inherited ia) >&&>
    match (A.unwrap ia,ib) with

    | (A.MetaInit(ida,keep,inherited), ib) ->
	let max_min _ =
	  Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_ini ib) in
	X.envf keep inherited (ida, Ast_c.MetaInitVal ib, max_min)
	  (fun () ->
	    X.distrf_ini ida ib >>= (fun ida ib ->
	      return (
	        A.MetaInit (ida,keep,inherited) +> A.rewrap ia,
	        ib
	     ))
	  )

    | A.AsInit(ini,asini), inib ->
	initialiser ini inib >>= (fun ini inib ->
	initialiser asini inib >>= (fun asini inib ->
	  return(
	  ((A.AsInit(ini,asini)) +> A.rewrap ia,
	   inib))))

    | (A.InitExpr expa, ib) ->
        (match A.unwrap expa, ib with
        | A.Edots (mcode, None), ib    ->
            X.distrf_ini (dots2metavar mcode) ib >>= (fun mcode ib ->
              return (
                A.InitExpr
                  (A.Edots (metavar2dots mcode, None) +> A.rewrap expa)
                    +>  A.rewrap ia,
                ib
               ))

        | A.Edots (_, Some expr), _    -> failwith "not handling when on Edots"

        | _, (B.InitExpr expb, ii) ->
            assert (ii = []);
            expression expa expb >>= (fun expa expb ->
              return (
                (A.InitExpr expa) +> A.rewrap ia,
                (B.InitExpr expb, ii)
              ))
        | _ -> fail
        )

    | (A.ArInitList (ia1, ias, ia2), (B.InitList ibs, ii)) ->
        (match ii with
	| ib1::ib2::iicommaopt ->
            tokenf ia1 ib1 >>= (fun ia1 ib1 ->
            tokenf ia2 ib2 >>= (fun ia2 ib2 ->
            ar_initialisers (A.unwrap ias) (ibs, iicommaopt) >>=
	      (fun iasunwrap (ibs,iicommaopt) ->
              return (
                (A.ArInitList (ia1, A.rewrap ias iasunwrap, ia2)) +> A.rewrap ia,
                (B.InitList ibs, ib1::ib2::iicommaopt)
              ))))

        | _ -> raise (Impossible 35)
        )

    | (A.StrInitList (allminus, ia1, ias, ia2, []), (B.InitList ibs, ii)) ->
        (match ii with
        | ib1::ib2::iicommaopt ->
            tokenf ia1 ib1 >>= (fun ia1 ib1 ->
            tokenf ia2 ib2 >>= (fun ia2 ib2 ->
            str_initialisers allminus ias (ibs, iicommaopt) >>=
	      (fun ias (ibs,iicommaopt) ->
              return (
                (A.StrInitList (allminus, ia1, ias, ia2, [])) +> A.rewrap ia,
                (B.InitList ibs, ib1::ib2::iicommaopt)
              ))))

        | _ -> raise (Impossible 36)
        )

    | (A.StrInitList (allminus, i1, ias, i2, whencode),
       (B.InitList ibs, _ii)) ->
        failwith "TODO: not handling whencode in initialisers"


    | (A.InitGccExt (designatorsa, ia2, inia),
      (B.InitDesignators (designatorsb, inib), ii2))->

        let iieq = tuple_of_list1 ii2 in

        tokenf ia2 iieq >>= (fun ia2 iieq ->
	designators designatorsa designatorsb >>=
	  (fun designatorsa designatorsb ->
        initialiser inia inib >>= (fun inia inib ->
          return (
            (A.InitGccExt (designatorsa, ia2, inia)) +> A.rewrap ia,
            (B.InitDesignators (designatorsb, inib), [iieq])
          ))))




    | (A.InitGccName (ida, ia1, inia), (B.InitFieldOld (idb, inib), ii)) ->
        (match ii with
        | [iidb;iicolon] ->
            ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) ->
            initialiser inia inib >>= (fun inia inib ->
            tokenf ia1 iicolon >>= (fun ia1 iicolon ->
              return (
                (A.InitGccName (ida, ia1, inia)) +> A.rewrap ia,
                (B.InitFieldOld (idb, inib), [iidb;iicolon])
              ))))
        | _ -> fail
        )



    | A.IComma(comma), _ ->
        raise (Impossible 37)

    | A.OptIni _,_ ->
      failwith "not handling Opt on initialisers"

    | _, (B.InitIndexOld (_, _), _) -> fail
    | _, (B.InitFieldOld (_, _), _) -> fail

    | _, ((B.InitDesignators (_, _)|B.InitList _|B.InitExpr _), _)
        -> fail

and designators dla dlb =
  match (dla,dlb) with
    ([],[]) -> return ([], [])
  | ([],_) | (_,[]) -> fail
  | (da::dla,db::dlb) ->
      designator da db >>= (fun da db ->
      designators dla dlb >>= (fun dla dlb ->
      return (da::dla, db::dlb)))

and designator da db =
  match (da,db) with
    (A.DesignatorField (ia1, ida), (B.DesignatorField idb,ii1)) ->

        let (iidot, iidb) = tuple_of_list2 ii1 in
        tokenf ia1 iidot >>= (fun ia1 iidot ->
        ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) ->
          return (
            A.DesignatorField (ia1, ida),
            (B.DesignatorField idb, [iidot;iidb])
          )))

  | (A.DesignatorIndex (ia1,ea,ia2), (B.DesignatorIndex eb, ii1)) ->

        let (ib1, ib2) = tuple_of_list2 ii1 in
        tokenf ia1 ib1 >>= (fun ia1 ib1 ->
        tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        expression ea eb >>= (fun ea eb ->
          return (
            A.DesignatorIndex (ia1,ea,ia2),
            (B.DesignatorIndex eb, [ib1;ib2])
          ))))

  | (A.DesignatorRange (ia1,e1a,ia2,e2a,ia3),
     (B.DesignatorRange (e1b, e2b), ii1)) ->

        let (ib1, ib2, ib3) = tuple_of_list3 ii1 in
        tokenf ia1 ib1 >>= (fun ia1 ib1 ->
        tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        tokenf ia3 ib3 >>= (fun ia3 ib3 ->
        expression e1a e1b >>= (fun e1a e1b ->
        expression e2a e2b >>= (fun e2a e2b ->
          return (
            A.DesignatorRange (ia1,e1a,ia2,e2a,ia3),
            (B.DesignatorRange (e1b, e2b), [ib1;ib2;ib3])
          ))))))
  | (_, ((B.DesignatorField _|B.DesignatorIndex _|B.DesignatorRange _), _)) ->
      fail

and str_initialisers = fun allminus ias (ibs, iicomma) ->
  let ias_unsplit = unsplit_icomma      ias in
  let ibs_split   = resplit_initialiser ibs iicomma in

  (* need unordered is to check if an expensive computation is useful, but if
     ias is null, then the computation is not expensive *)
  if ias = [] || need_unordered_initialisers ibs
  then
    initialisers_unordered2 allminus ias_unsplit ibs_split >>=
    (fun ias_unsplit ibs_split ->
      return (
      split_icomma ias_unsplit,
      unsplit_initialiser ibs_split))
  else fail

and ar_initialisers = fun ias (ibs, iicomma) ->
  (* this doesn't check need_unordered_initialisers because ... can be
     implemented as ordered, even if it matches unordered initializers *)
  let ibs = resplit_initialiser ibs iicomma in
  let ibs =
    List.concat
      (List.map (function (elem,comma) -> [Left elem; Right [comma]]) ibs) in
  initialisers_ordered2 ias ibs >>=
  (fun ias ibs_split ->
    let ibs,iicomma =
      match List.rev ibs_split with
	(Right comma)::rest -> (Ast_c.unsplit_comma (List.rev rest),comma)
      | (Left _)::_ -> (Ast_c.unsplit_comma ibs_split,[]) (* possible *)
      | [] -> ([],[]) in
    return (ias, (ibs,iicomma)))

and initialisers_ordered2 = fun ias ibs ->
  let match_dots ea =
    match A.unwrap ea with
      A.Idots(mcode, optexpr) -> Some (mcode, optexpr)
    |  _ -> None in
  let build_dots (mcode, optexpr) = A.Idots(mcode, optexpr) in
  let match_comma ea =
    match A.unwrap ea with
      A.IComma ia1 -> Some ia1
    |  _ -> None in
  let build_comma ia1 = A.IComma ia1 in
  let match_metalist ea =
    match A.unwrap ea with
      A.MetaInitList(ida,leninfo,keep,inherited) ->
	Some(ida,leninfo,keep,inherited,None)
    | _ -> None in
  let build_metalist _ (ida,leninfo,keep,inherited) =
    A.MetaInitList(ida,leninfo,keep,inherited) in
  let mktermval v = Ast_c.MetaInitListVal v in
  let special_cases ea eas ebs = None in
  let no_ii x = failwith "not possible" in
  list_matcher match_dots build_dots match_comma build_comma
    match_metalist build_metalist mktermval
    special_cases initialiser X.distrf_inis
    B.split_comma B.unsplit_comma no_ii
    (function x -> Some x) ias ibs

and initialisers_unordered2 = fun allminus ias ibs ->
  match ias, ibs with
  | [], ys ->
      if allminus
      then
	let rec loop = function
	    [] -> return ([],[])
	  | (ib,comma)::ibs ->
	      X.distrf_ini minusizer ib >>= (fun _ ib ->
		tokenf minusizer comma >>= (fun _ comma ->
		  loop ibs >>= (fun l ibs ->
		    return(l,(ib,comma)::ibs)))) in
	loop ibs
      else return ([], ys)
  | x::xs, ys ->
      let permut = Common.uncons_permut_lazy ys in
      permut +> List.fold_left (fun acc ((e, pos), rest) ->
        acc >||>
          (initialiser_comma x e
            >>= (fun x e ->
              let rest = Lazy.force rest in
              initialisers_unordered2 allminus xs rest >>= (fun xs rest ->
                return (
                  x::xs,
                  Common.insert_elem_pos (e, pos) rest
                ))))
      ) fail

and initialiser_comma (x,xcomma) (y, commay) =
  match A.unwrap xcomma with
    A.IComma commax ->
      tokenf commax commay >>= (fun commax commay ->
        initialiser x y >>= (fun x y ->
          return (
          (x, (A.IComma commax) +> A.rewrap xcomma),
          (y, commay))))
  | _ -> raise (Impossible 38) (* unsplit_iicomma wrong *)

(* ------------------------------------------------------------------------- *)
and (struct_fields: (A.annotated_decl list, B.field list) matcher) =
 fun eas ebs ->
  let match_dots ea =
    match A.unwrap ea with
      A.Ddots(mcode, optexpr) -> Some (mcode, optexpr)
    | _ -> None in
  let build_dots (mcode, optexpr) = A.Ddots(mcode, optexpr) in
  let match_comma ea = None in
  let build_comma ia1 = failwith "not possible" in
  let match_metalist ea =
    match A.unwrap ea with
      A.DElem(mckstart,allminus,d) ->
	(match A.unwrap d with
	  A.MetaFieldList(ida,leninfo,keep,inherited) ->
	    Some(ida,leninfo,keep,inherited,None)
	| _ -> None)
    | _ -> None in
  let build_metalist ea (ida,leninfo,keep,inherited) =
    match A.unwrap ea with
      A.DElem(mckstart,allminus,d) ->
	A.DElem(mckstart,allminus,
		(A.rewrap ea (A.MetaFieldList(ida,leninfo,keep,inherited))))
    | _ -> failwith "not possible" in
  let mktermval v =
    (* drop empty ii information, because nothing between elements *)
    let v = List.map Ast_c.unwrap v in
    Ast_c.MetaFieldListVal v in
  let special_cases ea eas ebs = None in
  let no_ii x = failwith "not possible" in
  let make_ebs ebs = List.map (function x -> Left x) ebs in
  let unmake_ebs ebs =
    List.map (function Left x -> x | Right x -> failwith "no right") ebs in
  let distrf mcode startxs =
    let startxs = unmake_ebs startxs in
    X.distrf_struct_fields mcode startxs >>=
    (fun mcode startxs -> return (mcode,make_ebs startxs)) in
  let filter_fields l =
    Some
      (List.filter
	 (function x ->
	   match Ast_c.unwrap x with
	     Ast_c.DeclarationField fld -> true
	   | Ast_c.EmptyField info -> true
	   | Ast_c.MacroDeclField decl -> true
	   | Ast_c.CppDirectiveStruct cpp -> false
	   | Ast_c.IfdefStruct ifdef -> false)
	 l) in
  list_matcher match_dots build_dots match_comma build_comma
    match_metalist build_metalist mktermval
    special_cases struct_field distrf
    B.split_comma B.unsplit_comma no_ii
    filter_fields eas (make_ebs ebs) >>=
  (fun eas ebs -> return (eas,unmake_ebs ebs))

and (struct_field: (A.annotated_decl, B.field) matcher) =
  fun fa fb ->
    match A.unwrap fa with
      A.Ddots _ -> failwith "dots should be treated otherwise"
    | A.DElem(mckstart,allminus,ifa) ->

	(match A.unwrap ifa,fb with
	| A.MetaField (ida,keep,inherited), _ ->
	    let max_min _ =
	      Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_field fb) in
	    X.envf keep inherited (ida, Ast_c.MetaFieldVal fb, max_min)
	      (fun () ->
		X.distrf_field ida fb
		  ) >>= (fun ida fb ->
		    return
		      (A.DElem
			 (mckstart,allminus,
			  (A.MetaField (ida, keep, inherited))+> A.rewrap ifa)
			 +> A.rewrap fa,
		       fb))
	| _,B.DeclarationField (B.FieldDeclList ([onevar,iivirg],iiptvirg)) ->

        (* no modif possible on iistartb; included for parallelism with
	   DeclList *)
	    let (iiptvirgb,iifakestart) = tuple_of_list2 iiptvirg in

	    assert (iivirg = []);
	    (match onevar with
	    | B.BitField (sopt, typb, _, expr) ->
		pr2_once "warning: bitfield not handled by ast_cocci";
		fail
	    | B.Simple (None, typb) ->
		pr2_once
		  "warning: unnamed struct field not handled by ast_cocci";
		fail
	    | B.Simple (Some nameidb, typb) ->
		X.tokenf_mck mckstart iifakestart >>=
		(fun mckstart iifakestart ->
		  (* build a declaration from a struct field *)
		  let (fake_var,iisto) = build_decl nameidb typb iivirg in
		  onedecl allminus ifa (fake_var,iiptvirgb,iisto) >>=
		  (fun ifa (fake_var,iiptvirgb,iisto) ->
		    let (onevar,iivirg) = unbuild_decl fake_var in

		    return (
		    (A.DElem(mckstart,allminus,ifa) +> A.rewrap fa),
		    ((B.DeclarationField
			(B.FieldDeclList([onevar, iivirg],
					 [iiptvirgb;iifakestart]))))))))

	| _,B.DeclarationField (B.FieldDeclList (xs,iiptvirg)) ->

	    let (iiptvirgb,iifakestart) = tuple_of_list2 iiptvirg in

	    let indexify l =
	      let rec loop n = function
		  [] -> []
		| x::xs -> (n,x)::(loop (n+1) xs) in
	      loop 0 l in
	    let rec repln n vl cur = function
		[] -> []
	      | x::xs ->
		  if n = cur then vl :: xs else x :: (repln n vl (cur+1) xs) in
	    let doit _ =
	      (indexify xs) +> List.fold_left (fun acc (n,(onevar,iivirg)) ->
          (* consider all possible matches *)
		acc >||>
		(X.tokenf_mck mckstart iifakestart >>=
		 (fun mckstart iifakestart ->
		   (match onevar with
		   | B.BitField (sopt, typb, _, expr) ->
		       pr2_once "warning: bitfield not handled by ast_cocci";
		       fail
		   | B.Simple (None, typb) ->
		       pr2_once
			 "warning: unnamed struct field not handled by ast_cocci";
		       fail
		   | B.Simple (Some nameidb, typb) ->

		       (* build a declaration from a struct field *)
		       let (fake_var,iisto) = build_decl nameidb typb iivirg in
		       onedecl allminus ifa (fake_var,iiptvirgb,iisto) >>=
		       (fun ifa (fake_var,iiptvirgb,iisto) ->
			 let (onevar,iivirg) = unbuild_decl fake_var in

			 return (
			 (A.DElem(mckstart,allminus,ifa) +> A.rewrap fa),
			 ((B.DeclarationField
			     (B.FieldDeclList
				(repln n (onevar,iivirg) 0 xs,
				 [iiptvirgb;iifakestart]))))))))))
		fail in
	    if !Flag.sgrep_mode2(*X.mode = PatternMode *) ||
  	       A.get_safe_decl ifa
	    then doit()
	    else
	      begin
	        (* unambitious version of the DeclList case... *)
		pr2_once "PB: More that one variable in decl. Have to split";
		fail
	      end

	| _,B.EmptyField _iifield ->
	    fail

	| A.MacroDecl (stoa,bsa,lpa,eas,rpa,enda),
	    B.MacroDeclField ((sb,ebs),ii) ->
	    raise Todo
	| _,B.MacroDeclField ((sb,ebs),ii) -> fail

	| _,B.CppDirectiveStruct directive -> fail
	| _,B.IfdefStruct directive -> fail)

and build_decl nameidb typb iivirg =
  let iisto = [] in
  let stob = B.NoSto, false in
  let fake_var =
    ({B.v_namei = Some (nameidb, B.NoInit);
       B.v_type = typb;
       B.v_storage = stob;
       B.v_local = Ast_c.NotLocalDecl;
       B.v_attr = Ast_c.noattr;
       B.v_type_bis = ref None;
              (* the struct field should also get expanded ? no it's not
               * important here, we will rematch very soon *)
     },
     iivirg) in
  (fake_var,iisto)

and unbuild_decl = function
    ({B.v_namei = Some (nameidb, B.NoInit);
       B.v_type = typb;
       B.v_storage = stob;
     }, iivirg) ->

       let onevar = B.Simple (Some nameidb, typb) in
       (onevar,iivirg)
  | _ -> raise (Impossible 40)

(* ---------------------------------------------------------------------- *)

and enum_fields = fun eas ebs ->
  let match_dots ea =
    match A.unwrap ea with
      A.Edots(mcode, optexpr) -> Some (mcode, optexpr)
    | _ -> None in
  let build_dots (mcode, optexpr) = A.Edots(mcode, optexpr) in
  let match_comma ea =
    match A.unwrap ea with
      A.EComma ia1 -> Some ia1
    | _ -> None in
  let build_comma ia1 = A.EComma ia1 in
  let match_metalist ea = None in
  let build_metalist _ (ida,leninfo,keep,inherited) =
    failwith "not possible" in
  let mktermval v = failwith "not possible" in
  let special_cases ea eas ebs = None in
  list_matcher match_dots build_dots match_comma build_comma
    match_metalist build_metalist mktermval
    special_cases enum_field X.distrf_enum_fields
    B.split_comma B.unsplit_comma
    Lib_parsing_c.ii_of_enum_fields (function x -> Some x) eas ebs

and enum_field ida idb =
  X.all_bound (A.get_inherited ida) >&&>
  match A.unwrap ida, idb with
    A.Ident(id),(nameidb,None) ->
      ident_cpp DontKnow id nameidb >>= (fun id nameidb ->
        return ((A.Ident id) +> A.rewrap ida, (nameidb,None)))
  | A.Ident(id),(nameidb,Some _) -> fail (* should we have an iso? *)
  | A.Assignment(ea1,opa,ea2,init),(nameidb,Some(opbi,eb2)) ->
      (match A.unwrap ea1 with
	A.Ident(id) ->
	  let assignOp opa0 opbi =
	    match A.unwrap opa0 with
              A.SimpleAssign oa ->
		tokenf oa opbi >>= fun oa opbi ->
		  return
                    (A.rewrap opa (A.SimpleAssign oa), opbi)
            | _ -> failwith "only simple assignment possible here" in
	  ident_cpp DontKnow id nameidb >>= (fun id nameidb ->
	  expression ea2 eb2 >>= (fun ea2 eb2 ->
          assignOp opa opbi >>= (fun opa opbi ->(* only one kind of assignop *)
	    return (
	    (A.Assignment((A.Ident(id)) +> A.rewrap ea1,opa,ea2,init)) +>
	    A.rewrap ida,
	    (nameidb,Some(opbi,eb2))))))
      |	_ -> failwith "not possible")
  | A.Assignment(ea1,opa,ea2,init),(nameidb,None) -> fail
  | _ -> failwith ("not possible: "^(Dumper.dump (A.unwrap ida)))

(* ------------------------------------------------------------------------- *)
and (fullType: (A.fullType, Ast_c.fullType) matcher) =
 fun typa typb ->
   X.optional_qualifier_flag (fun optional_qualifier ->
   X.all_bound (A.get_inherited typa) >&&>
   match A.unwrap typa, typb with
   | A.Type(allminus,cv,ty1), ((qu,il),ty2) ->

       if qu.B.const && qu.B.volatile
       then
	 pr2_once
	   ("warning: the type is both const & volatile but cocci " ^
            "does not handle that");

	(* Drop out the const/volatile part that has been matched.
         * This is because a SP can contain  const T v; in which case
         * later in match_t_t when we encounter a T, we must not add in
         * the environment the whole type.
         *)


       (match cv with
       (* "iso-by-absence" *)
       | None ->
           let do_stuff () =
             fullTypebis ty1 ((qu,il), ty2) >>= (fun ty1 ((qu,il), ty2) ->
             (if allminus
	     then minusize_list il
	     else return ((), il)
	     ) >>= (fun () il ->
	       return (
                 (A.Type(allminus, None, ty1)) +> A.rewrap typa,
                 ((qu,il), ty2)
               )))
           in
           (match optional_qualifier, qu.B.const || qu.B.volatile with
           | false, false -> do_stuff ()
           | false, true -> fail
           | true, false -> do_stuff ()
           | true, true ->
               if !FlagM.show_misc
               then pr2_once "USING optional_qualifier builtin isomorphism";
               do_stuff()
           )


       | Some x ->
          (* todo: can be __const__ ? can be const & volatile so
           * should filter instead ?
           *)
           (match term x, il with
           | A.Const, [i1] when qu.B.const ->

               tokenf x i1 >>= (fun x i1 ->
               fullTypebis ty1 (Ast_c.nQ,ty2) >>= (fun ty1 (_, ty2) ->
                 return (
                   (A.Type(allminus, Some x, ty1)) +> A.rewrap typa,
                   ((qu, [i1]), ty2)
                 )))

           | A.Volatile, [i1] when qu.B.volatile ->
               tokenf x i1 >>= (fun x i1 ->
               fullTypebis ty1 (Ast_c.nQ,ty2) >>= (fun ty1 (_, ty2) ->
                 return (
                   (A.Type(allminus, Some x, ty1)) +> A.rewrap typa,
                   ((qu, [i1]), ty2)
                 )))

           | _ -> fail
           )
       )

  | A.AsType(ty,asty), tyb ->
      fullType ty tyb >>= (fun ty tyb ->
      fullType asty tyb >>= (fun asty tyb ->
	return(
	  ((A.AsType(ty,asty)) +> A.rewrap typa,
	   tyb))))

  | A.DisjType typas, typb ->
      typas +>
      List.fold_left (fun acc typa -> acc >|+|> (fullType typa typb)) fail

   | A.OptType(_), _ -> failwith "not handling Opt on type"
   )


(*
 * Why not (A.typeC, Ast_c.typeC) matcher ?
 * because when there is MetaType, we want that T record the whole type,
 * including the qualifier, and so this type (and the new_il function in
 * preceding function).
*)

and (fullTypebis: (A.typeC, Ast_c.fullType) matcher) =
  fun ta tb ->
  X.all_bound (A.get_inherited ta) >&&>
  match A.unwrap ta, tb with

  (* cas general *)
  | A.MetaType(ida,keep, inherited),  typb ->
      let type_present =
	let (tyq, (ty, tyii)) = typb in
	match ty with
	  B.NoType -> false
	| _ -> true in
      let position_required_but_unavailable =
	match A.get_pos_var ida with
	  [] -> false
	| _ ->
	    let (tyq, (ty, tyii)) = typb in
	    let tyii =
	      match ty with
		B.TypeName(name,typ) ->
		  (* promoted typedef has ii information in the type name *)
		  let (_s, iis) = B.get_s_and_info_of_name name in
		  [iis]
	      |	_ -> tyii in
	    List.for_all Ast_c.is_fake tyii in
      if type_present && not position_required_but_unavailable
      then
	let max_min _ =
	  Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_type typb) in
	X.envf keep inherited (ida, B.MetaTypeVal typb, max_min) (fun () ->
          X.distrf_type ida typb >>= (fun ida typb ->
          return (
            A.MetaType(ida,keep, inherited) +> A.rewrap ta,
            typb)))
      else fail (* K&R, or macro, or C++? *)
  | unwrap, (qub, typb) ->
      typeC ta typb >>= (fun ta typb ->
        return (ta, (qub, typb))
      )

and simulate_signed ta basea stringsa signaopt tb baseb ii rebuilda =
      (* In ii there is a list, sometimes of length 1 or 2 or 3.
       * And even if in baseb we have a Signed Int, that does not mean
       * that ii is of length 2, cos Signed is the default, so if in signa
       * we have Signed explicitly ? we cannot "accrocher" this mcode to
       * something :( So for the moment when there is signed in cocci,
       * we force that there is a signed in c too (done in pattern.ml).
       *)
      let signbopt, iibaseb = split_signb_baseb_ii (baseb, ii) in


      (* handle some iso on type ? (cf complex C rule for possible implicit
	 casting) *)
      match basea, baseb with
      | A.VoidType,   B.Void
      | A.FloatType,  B.FloatType (B.CFloat)
      | A.DoubleType, B.FloatType (B.CDouble)
      |	A.SizeType,   B.SizeType
      |	A.SSizeType,  B.SSizeType
      |	A.PtrDiffType,B.PtrDiffType ->
           assert (signaopt = None);
	   let stringa = tuple_of_list1 stringsa in
           let (ibaseb) = tuple_of_list1 ii in
           tokenf stringa ibaseb >>= (fun stringa ibaseb ->
             return (
               (rebuilda ([stringa], signaopt)) +> A.rewrap ta,
               (B.BaseType baseb, [ibaseb])
             ))

      | A.CharType,  B.IntType B.CChar when signaopt = None ->
	  let stringa = tuple_of_list1 stringsa in
          let ibaseb = tuple_of_list1 ii in
           tokenf stringa ibaseb >>= (fun stringa ibaseb ->
             return (
               (rebuilda ([stringa], signaopt)) +> A.rewrap ta,
               (B.BaseType (B.IntType B.CChar), [ibaseb])
             ))

      | A.CharType,B.IntType (B.Si (_sign, B.CChar2)) when signaopt <> None ->
	  let stringa = tuple_of_list1 stringsa in
          let ibaseb = tuple_of_list1 iibaseb in
          sign signaopt signbopt >>= (fun signaopt iisignbopt ->
          tokenf stringa ibaseb >>= (fun stringa ibaseb ->
            return (
               (rebuilda ([stringa], signaopt)) +> A.rewrap ta,
               (B.BaseType (baseb), iisignbopt @ [ibaseb])
               )))

      | A.ShortType, B.IntType (B.Si (_, B.CShort))
      | A.IntType,   B.IntType (B.Si (_, B.CInt))
      | A.LongType,  B.IntType (B.Si (_, B.CLong))  ->
	  let stringa = tuple_of_list1 stringsa in
          (match iibaseb with
          | [] ->
              (* iso-by-presence ? *)
              (* when unsigned int in SP,  allow have just unsigned in C ? *)
              if mcode_contain_plus (mcodekind stringa)
              then fail
              else

                sign signaopt signbopt >>= (fun signaopt iisignbopt ->
                    return (
                      (rebuilda ([stringa], signaopt)) +> A.rewrap ta,
                      (B.BaseType (baseb), iisignbopt)
                    ))


          | [x;y] ->
              (*pr2_once
                "warning: long int or short int not handled by ast_cocci";*)
              fail

          | [ibaseb] ->
          sign signaopt signbopt >>= (fun signaopt iisignbopt ->
          tokenf stringa ibaseb >>= (fun stringa ibaseb ->
            return (
               (rebuilda ([stringa], signaopt)) +> A.rewrap ta,
               (B.BaseType (baseb), iisignbopt @ [ibaseb])
               )))
          | _ -> raise (Impossible 41)

          )

      | A.LongLongIntType, B.IntType (B.Si (_, B.CLongLong)) ->
	  let (string1a,string2a,string3a) = tuple_of_list3 stringsa in
          (match iibaseb with
            [ibase1b;ibase2b;ibase3b] ->
              sign signaopt signbopt >>= (fun signaopt iisignbopt ->
              tokenf string1a ibase1b >>= (fun base1a ibase1b ->
              tokenf string2a ibase2b >>= (fun base2a ibase2b ->
              tokenf string3a ibase3b >>= (fun base3a ibase3b ->
              return (
		(rebuilda ([base1a;base2a;base3a], signaopt)) +> A.rewrap ta,
		(B.BaseType (baseb), iisignbopt @ [ibase1b;ibase2b;ibase3b])
              )))))
	  | [ibase1b;ibase2b] -> fail (* int omitted *)
	  | [] -> fail (* should something be done in this case? *)
	  | _ -> raise (Impossible 42))


      | A.LongLongType, B.IntType (B.Si (_, B.CLongLong))
      | A.LongIntType,  B.IntType (B.Si (_, B.CLong))
      | A.ShortIntType, B.IntType (B.Si (_, B.CShort))
      | A.LongDoubleType, B.FloatType B.CLongDouble ->
	  let (string1a,string2a) = tuple_of_list2 stringsa in
          (match iibaseb with
            [ibase1b;ibase2b] ->
              sign signaopt signbopt >>= (fun signaopt iisignbopt ->
              tokenf string1a ibase1b >>= (fun base1a ibase1b ->
              tokenf string2a ibase2b >>= (fun base2a ibase2b ->
              return (
		(rebuilda ([base1a;base2a], signaopt)) +> A.rewrap ta,
		(B.BaseType (baseb), iisignbopt @ [ibase1b;ibase2b])
              ))))
	  | [ibase1b] -> fail (* short or long *)
	  | [ibase1b;ibase2b;ibase3b] -> fail (* long long case *)
	  | [] -> fail (* should something be done in this case? *)
	  | _ -> raise (Impossible 43))

      | _, (B.Void|B.FloatType _|B.IntType _
	    |B.SizeType|B.SSizeType|B.PtrDiffType) -> fail

and simulate_signed_meta ta basea signaopt tb baseb ii rebuilda =
      (* In ii there is a list, sometimes of length 1 or 2 or 3.
       * And even if in baseb we have a Signed Int, that does not mean
       * that ii is of length 2, cos Signed is the default, so if in signa
       * we have Signed explicitly ? we cannot "accrocher" this mcode to
       * something :( So for the moment when there is signed in cocci,
       * we force that there is a signed in c too (done in pattern.ml).
       *)
      let signbopt, iibaseb = split_signb_baseb_ii (baseb, ii) in

      let match_to_type rebaseb =
	sign signaopt signbopt >>= (fun signaopt iisignbopt ->
	let fta = A.rewrap basea (A.Type(false(*don't know*),None,basea)) in
	let ftb = Ast_c.nQ,(B.BaseType (rebaseb), iibaseb) in
	fullType fta ftb >>= (fun fta (_,tb) ->
	  (match A.unwrap fta,tb with
	    A.Type(_,_,basea), (B.BaseType baseb, ii) ->
	      return (
	      (rebuilda (basea, signaopt)) +> A.rewrap ta,
	      (B.BaseType (baseb), iisignbopt @ ii)
		)
	  | _ -> failwith "not possible"))) in

      (* handle some iso on type ? (cf complex C rule for possible implicit
	 casting) *)
      match baseb with
      | B.IntType (B.Si (_sign, B.CChar2)) ->
	  match_to_type (B.IntType B.CChar)

      | B.IntType (B.Si (_, ty)) ->
          (match iibaseb with
          | [] -> fail (* metavariable has to match something *)

          | _ -> match_to_type (B.IntType (B.Si (B.Signed, ty)))

          )

      | (B.Void|B.FloatType _|B.IntType _
         |B.SizeType|B.SSizeType|B.PtrDiffType) -> fail

and (typeC: (A.typeC, Ast_c.typeC) matcher) =
  fun ta tb ->
  match A.unwrap ta, tb with
    | A.BaseType (basea,stringsa), (B.BaseType baseb, ii) ->
	simulate_signed ta basea stringsa None tb baseb ii
	  (function (stringsa, signaopt) -> A.BaseType (basea,stringsa))
    | A.SignedT (signaopt, Some basea), (B.BaseType baseb, ii) ->
	(match A.unwrap basea with
	  A.BaseType (basea1,strings1) ->
	    simulate_signed ta basea1 strings1 (Some signaopt) tb baseb ii
	      (function (strings1, Some signaopt) ->
		A.SignedT
		  (signaopt,
		   Some (A.rewrap basea (A.BaseType (basea1,strings1))))
		| _ -> failwith "not possible")
	| A.MetaType(ida,keep,inherited) ->
	    simulate_signed_meta ta basea (Some signaopt) tb baseb ii
	      (function (basea, Some signaopt) ->
		A.SignedT(signaopt,Some basea)
		| _ -> failwith "not possible")
	| _ -> failwith "not possible")
    | A.SignedT (signa,None),   (B.BaseType baseb, ii) ->
        let signbopt, iibaseb = split_signb_baseb_ii (baseb, ii) in
        (match iibaseb, baseb with
        | [], B.IntType (B.Si (_sign, B.CInt)) ->
            sign (Some signa) signbopt >>= (fun signaopt iisignbopt ->
              match signaopt with
              | None -> raise (Impossible 45)
              | Some signa ->
                  return (
                    (A.SignedT (signa,None)) +> A.rewrap ta,
                    (B.BaseType baseb, iisignbopt)
                  )
            )
        | _ -> fail
        )



    (* todo? iso with array *)
    | A.Pointer (typa, iamult),            (B.Pointer typb, ii) ->
        let (ibmult) = tuple_of_list1 ii in
        fullType typa typb >>= (fun typa typb ->
        tokenf iamult ibmult >>= (fun iamult ibmult ->
          return (
            (A.Pointer (typa, iamult)) +> A.rewrap ta,
            (B.Pointer typb, [ibmult])
          )))

    | A.FunctionPointer(tya,lp1a,stara,rp1a,lp2a,paramsa,rp2a),
        (B.ParenType t1, ii) ->
        let (lp1b, rp1b) = tuple_of_list2 ii in
        let (qu1b, t1b) = t1 in
        (match t1b with
        | B.Pointer t2, ii ->
            let (starb) = tuple_of_list1 ii in
            let (qu2b, t2b) = t2 in
            (match t2b with
            | B.FunctionType (tyb, (paramsb, (isvaargs, iidotsb))), ii ->
                let (lp2b, rp2b) = tuple_of_list2 ii in

                if isvaargs
                then
		  pr2_once
		    ("Not handling well variable length arguments func. "^
		     "You have been warned");

                fullType tya tyb >>= (fun tya tyb ->
                tokenf lp1a lp1b >>= (fun lp1a lp1b ->
                tokenf rp1a rp1b >>= (fun rp1a rp1b ->
                tokenf lp2a lp2b >>= (fun lp2a lp2b ->
                tokenf rp2a rp2b >>= (fun rp2a rp2b ->
                tokenf stara starb >>= (fun stara starb ->
                parameters (seqstyle paramsa) (A.unwrap paramsa) paramsb >>=
                (fun paramsaunwrap paramsb ->
                  let paramsa = A.rewrap paramsa paramsaunwrap in

                  let t2 =
                    (qu2b,
                    (B.FunctionType (tyb, (paramsb, (isvaargs, iidotsb))),
                    [lp2b;rp2b]))
                  in
                  let t1 =
                    (qu1b,
                    (B.Pointer t2, [starb]))
                  in

                  return (
                    (A.FunctionPointer(tya,lp1a,stara,rp1a,lp2a,paramsa,rp2a))
                    +> A.rewrap ta,
                    (B.ParenType t1, [lp1b;rp1b])
                  )
                )))))))



            | _ -> fail
            )
        | _ -> fail
        )



    (* todo: handle the iso on optional size specification ? *)
    | A.Array (typa, ia1, eaopt, ia2), (B.Array (ebopt, typb), ii) ->
        let (ib1, ib2) = tuple_of_list2 ii in
        fullType typa typb >>= (fun typa typb ->
        eoption expression eaopt ebopt >>= (fun eaopt ebopt ->
        tokenf ia1 ib1 >>= (fun ia1 ib1 ->
        tokenf ia2 ib2 >>= (fun ia2 ib2 ->
          return (
            (A.Array (typa, ia1, eaopt, ia2)) +> A.rewrap ta,
            (B.Array (ebopt, typb), [ib1;ib2])
          )))))


    | A.Decimal(dec,lp,length,Some comma,Some precision,rp),
	(B.Decimal (len, Some prec), ii) ->
        let (ib1, ib2, ib3, ib4) = tuple_of_list4 ii in
        expression length len >>= (fun length len ->
        expression precision prec >>= (fun precision prec ->
        tokenf dec ib1   >>= (fun dec ib1 ->
        tokenf lp ib2    >>= (fun lp ib2 ->
        tokenf comma ib3 >>= (fun comma ib3 ->
        tokenf rp ib4    >>= (fun rp ib4 ->
          return (
            (A.Decimal(dec,lp,length,Some comma,Some precision,rp)) +>
	      A.rewrap ta,
            (B.Decimal (len, Some prec), [ib1;ib2;ib3;ib4])
          )))))))


    | A.Decimal(dec,lp,length,None,None,rp),
	(B.Decimal (len, None), ii) ->
        let (ib1, ib2, ib3) = tuple_of_list3 ii in
        expression length len >>= (fun length len ->
        tokenf dec ib1 >>= (fun dec ib1 ->
        tokenf lp ib2  >>= (fun lp ib2 ->
        tokenf rp ib3  >>= (fun rp ib3 ->
          return (
            (A.Decimal(dec,lp,length,None,None,rp)) +> A.rewrap ta,
            (B.Decimal (len,None), [ib1;ib2;ib3])
          )))))


     (* todo: could also match a Struct that has provided a name *)
     (* This is for the case where the SmPL code contains "struct x", without
	a definition.  In this case, the name field is always present.
        This case is also called from the case for A.StructUnionDef when
        a name is present in the C code. *)
    | A.StructUnionName(sua, Some sa), (B.StructUnionName (sub, sb), ii) ->
        (* sa is now an ident, not an mcode, old: ... && (term sa) = sb *)
        let (ib1, ib2) = tuple_of_list2 ii in
        if equal_structUnion  (term sua) sub
        then
          ident DontKnow sa (sb, ib2) >>= (fun sa (sb, ib2) ->
          tokenf sua ib1 >>= (fun sua ib1 ->
            return (
              (A.StructUnionName (sua, Some sa)) +> A.rewrap ta,
              (B.StructUnionName (sub, sb), [ib1;ib2])
              )))
        else fail


    | A.StructUnionDef(ty, lba, declsa, rba),
     (B.StructUnion (sub, sbopt, declsb), ii) ->

       let (ii_sub_sb, lbb, rbb) =
	 match ii with
	   [iisub; lbb; rbb] -> (Common.Left iisub,lbb,rbb)
	 | [iisub; iisb; lbb; rbb] -> (Common.Right (iisub,iisb),lbb,rbb)
	 | _ -> error ii "list of length 3 or 4 expected" in

       let process_type =
         match (sbopt,ii_sub_sb) with
           (None,Common.Left iisub) ->
	     (* the following doesn't reconstruct the complete SP code, just
		the part that matched *)
	     let rec loop s =
	       match A.unwrap s with
		 A.Type(allminus,None,ty) ->
		   (match A.unwrap ty with
		     A.StructUnionName(sua, None) ->
		       (match (term sua, sub) with
			 (A.Struct,B.Struct)
		       | (A.Union,B.Union) -> return ((),())
		       | _ -> fail) >>=
		       (fun _ _ ->
			 tokenf sua iisub >>= (fun sua iisub ->
			   let ty =
			     A.Type(allminus,None,
				    A.StructUnionName(sua, None) +> A.rewrap ty)
			       +> A.rewrap s in
			   return (ty,[iisub])))
		   | _ -> fail)
	       | A.DisjType(disjs) ->
		   disjs +>
		   List.fold_left (fun acc disj -> acc >|+|> (loop disj)) fail
	       | _ -> fail in
	     loop ty

         | (Some sb,Common.Right (iisub,iisb)) ->

             (* build a StructUnionName from a StructUnion *)
             let fake_su = B.nQ, (B.StructUnionName (sub, sb), [iisub;iisb]) in

             fullType ty fake_su >>= (fun ty fake_su ->
               match fake_su with
               | _nQ, (B.StructUnionName (sub, sb), [iisub;iisb]) ->
                   return (ty,  [iisub; iisb])
               | _ -> raise (Impossible 46))
	 | _ -> fail in

       process_type
	 >>= (fun ty ii_sub_sb ->

            tokenf lba lbb >>= (fun lba lbb ->
            tokenf rba rbb >>= (fun rba rbb ->
            struct_fields (A.unwrap declsa) declsb >>=(fun undeclsa declsb ->
              let declsa = A.rewrap declsa undeclsa in

              return (
                (A.StructUnionDef(ty, lba, declsa, rba)) +> A.rewrap ta,
                (B.StructUnion (sub, sbopt, declsb),ii_sub_sb@[lbb;rbb])
              )))))


   (* todo? handle isomorphisms ? because Unsigned Int can be match on a
    * uint in the C code. But some CEs consists in renaming some types,
    * so we don't want apply isomorphisms every time.
    *)
    | A.TypeName sa,  (B.TypeName (nameb, typb), noii) ->
        assert (noii = []);

        (match nameb with
        | B.RegularName (sb, iidb) ->
            let iidb1 = tuple_of_list1 iidb in

            if (term sa) = sb
            then
              tokenf sa iidb1 >>= (fun sa iidb1 ->
                return (
                  (A.TypeName sa) +> A.rewrap ta,
                  (B.TypeName (B.RegularName (sb, [iidb1]), typb), noii)
                   ))
               else fail

           | B.CppConcatenatedName _ | B.CppVariadicName _ |B.CppIdentBuilder _
               -> raise Todo
        )


    | _, (B.NoType, ii) -> fail
    | _, (B.TypeOfExpr e, ii) -> fail
    | _, (B.TypeOfType e, ii) -> fail

    | _, (B.ParenType e, ii) -> fail (* todo ?*)
    | A.EnumName(en,Some namea), (B.EnumName nameb, ii) ->
        let (ib1,ib2) = tuple_of_list2 ii in
	ident DontKnow namea (nameb, ib2) >>= (fun namea (nameb, ib2) ->
          tokenf en ib1 >>= (fun en ib1 ->
          return (
          (A.EnumName (en, Some namea)) +> A.rewrap ta,
          (B.EnumName nameb, [ib1;ib2])
          )))

    | A.EnumDef(ty, lba, idsa, rba),
	(B.Enum (sbopt, idsb), ii) ->

       let (ii_sub_sb, lbb, rbb, comma_opt) =
	 match ii with
	   [iisub; lbb; rbb; comma_opt] ->
	     (Common.Left iisub,lbb,rbb,comma_opt)
	 | [iisub; iisb; lbb; rbb; comma_opt] ->
	     (Common.Right (iisub,iisb),lbb,rbb,comma_opt)
	 | _ -> error ii "list of length 4 or 5 expected" in

       let process_type =
         match (sbopt,ii_sub_sb) with
           (None,Common.Left iisub) ->
	     (* the following doesn't reconstruct the complete SP code, just
		the part that matched *)
	     let rec loop s =
	       match A.unwrap s with
		 A.Type(allminus,None,ty) ->
		   (match A.unwrap ty with
		     A.EnumName(sua, None) ->
		       tokenf sua iisub >>= (fun sua iisub ->
			 let ty =
			   A.Type(allminus,None,A.EnumName(sua, None) +>
				  A.rewrap ty)
			     +> A.rewrap s in
			 return (ty,[iisub]))
		   | _ -> fail)
	       | A.DisjType(disjs) ->
		   disjs +>
		   List.fold_left (fun acc disj -> acc >|+|> (loop disj)) fail
	       | _ -> fail in
	     loop ty

         | (Some sb,Common.Right (iisub,iisb)) ->

             (* build an EnumName from an Enum *)
             let fake_su = B.nQ, (B.EnumName sb, [iisub;iisb]) in

             fullType ty fake_su >>= (fun ty fake_su ->
               match fake_su with
               | _nQ, (B.EnumName sb, [iisub;iisb]) ->
                   return (ty,  [iisub; iisb])
               | _ -> raise (Impossible 47))
	 | _ -> fail in

       process_type
	 >>= (fun ty ii_sub_sb ->

            tokenf lba lbb >>= (fun lba lbb ->
            tokenf rba rbb >>= (fun rba rbb ->
	      let idsb = resplit_initialiser idsb [comma_opt] in
	      let idsb =
		List.concat
		  (List.map
		     (function (elem,comma) -> [Left elem; Right [comma]])
		     idsb) in
            enum_fields (A.unwrap idsa) idsb >>= (fun unidsa idsb ->
              let idsa = A.rewrap idsa unidsa in
	      let idsb,iicomma =
		match List.rev idsb with
		  (Right comma)::rest ->
		    (Ast_c.unsplit_comma (List.rev rest),comma)
		| (Left _)::_ -> (Ast_c.unsplit_comma idsb,[]) (* possible *)
		| [] -> ([],[]) in
              return (
                (A.EnumDef(ty, lba, idsa, rba)) +> A.rewrap ta,
                (B.Enum (sbopt, idsb),ii_sub_sb@[lbb;rbb]@iicomma)
              ))
		)))

    | _, (B.Enum _, _) -> fail (* todo cocci ?*)

    | _,
     ((B.TypeName _ | B.StructUnionName (_, _) | B.EnumName _ |
      B.StructUnion (_, _, _) |
      B.FunctionType _ | B.Array (_, _) | B.Decimal(_, _) |
      B.Pointer _ | B.BaseType _),
     _)
     -> fail


(* todo: iso on sign, if not mentioned then free.  tochange?
 * but that require to know if signed int because explicit
 * signed int,  or because implicit signed int.
 *)

and sign signa signb =
  match signa, signb with
  | None, None -> return (None, [])
  | Some signa,  Some (signb, ib) ->
      if equal_sign (term signa) signb
      then tokenf signa ib >>= (fun signa ib ->
        return (Some signa, [ib])
      )
      else fail
  | _, _ -> fail


and minusize_list iixs =
  iixs +> List.fold_left (fun acc ii ->
    acc >>= (fun xs ys ->
    tokenf minusizer ii >>= (fun minus ii ->
      return (minus::xs, ii::ys)
    ))) (return ([],[]))
   >>= (fun _xsminys ys ->
     return ((), List.rev ys)
   )

and storage_optional_allminus allminus stoa (stob, iistob) =
  (* "iso-by-absence" for storage, and return type. *)
  X.optional_storage_flag (fun optional_storage ->
  match stoa, stob with
  | None, (stobis, inline) ->
      let do_minus () =
        if allminus
        then
          minusize_list iistob >>= (fun () iistob ->
            return (None, (stob, iistob))
          )
        else return (None, (stob, iistob))
      in

      (match optional_storage, stobis with
      | false, B.NoSto -> do_minus ()
      | false, _ -> fail
      | true, B.NoSto -> do_minus ()
      | true, _ ->
          if !FlagM.show_misc
          then pr2_once "USING optional_storage builtin isomorphism";
          do_minus()
      )

  | Some x, ((stobis, inline)) ->
      if equal_storage (term x) stobis
      then
	let rec loop acc = function
	    [] -> fail
	  | i1::iistob ->
	      let str = B.str_of_info i1 in
	      (match str with
		"static" | "extern" | "auto" | "register" ->
		  (* not very elegant, but tokenf doesn't know what token to
		     match with *)
		  tokenf x i1 >>= (fun x i1 ->
		    let rebuilt = (List.rev acc) @ i1 :: iistob in
		    return (Some x,  ((stobis, inline), rebuilt)))
	      |	_ -> loop (i1::acc) iistob) in
	loop [] iistob
      else fail
  )

and inline_optional_allminus allminus inla (stob, iistob) =
  (* "iso-by-absence" for storage, and return type. *)
  X.optional_storage_flag (fun optional_storage ->
  match inla, stob with
  | None, (stobis, inline) ->
      let do_minus () =
        if allminus
        then
          minusize_list iistob >>= (fun () iistob ->
            return (None, (stob, iistob))
          )
        else return (None, (stob, iistob))
      in

      if inline
      then
	if optional_storage
	then
	  begin
	    if !FlagM.show_misc
            then pr2_once "USING optional_storage builtin isomorphism";
            do_minus()
	  end
	else fail (* inline not in SP and present in C code *)
      else do_minus()

  | Some x, ((stobis, inline)) ->
      if inline
      then
	let rec loop acc = function
	    [] -> fail
	  | i1::iistob ->
	      let str = B.str_of_info i1 in
	      (match str with
		"inline" ->
		  (* not very elegant, but tokenf doesn't know what token to
		     match with *)
		  tokenf x i1 >>= (fun x i1 ->
		    let rebuilt = (List.rev acc) @ i1 :: iistob in
		    return (Some x,  ((stobis, inline), rebuilt)))
	      |	_ -> loop (i1::acc) iistob) in
	loop [] iistob
      else fail (* SP has inline, but the C code does not *)
  )

and fullType_optional_allminus allminus tya retb =
  match tya with
  | None ->
      if allminus
      then
        X.distrf_type minusizer retb >>= (fun _x retb ->
          return (None, retb)
        )

      else return (None, retb)
  | Some tya ->
      fullType tya retb >>= (fun tya retb ->
        return (Some tya, retb)
      )

(* Works for many attributes, but assumes order will be preserved.  Looks
for an exact match.  Actually the call site only allows a list of length
one to come through.  Makes no requirement if attributes not present. *)

(* The following is the intended version, on lists.  Unfortunately, this
   requires SmPL attributes to be wrapped.  Which they are not, for some
   reason. *)
(*
and attribute_list attras attrbs =
  X.optional_attributes_flag (fun optional_attributes ->
  match attras with
    None -> return (None, attrbs)
  | Some attras ->
      let match_dots ea = None in
      let build_dots (mcode, optexpr) = failwith "not possible" in
      let match_comma ea = None in
      let build_comma ia1 = failwith "not posible" in
      let match_metalist ea = None in
      let build_metalist _ (ida,leninfo,keep,inherited) =
	failwith "not possible" in
      let mktermval v = failwith "not possible" in
      let special_cases ea eas ebs = None in
      let no_ii x = failwith "not possible" in
      list_matcher match_dots build_dots match_comma build_comma
	match_metalist build_metalist mktermval
	special_cases attribute X.distrf_attrs
	B.split_nocomma B.unsplit_nocomma no_ii
	(function x -> Some x) attras attrbs >>=
      (fun attras attrbs -> return (Some attras, attrbs))) *)

(* The cheap hackish version.  No wrapping requires... *)

and attribute_list attras attrbs =
  X.optional_attributes_flag (fun optional_attributes ->
  match attras,attrbs with
    None, _ -> return (None, attrbs)
  | Some [attra], [attrb] ->
    attribute attra attrb >>= (fun attra attrb ->
      return (Some [attra], [attrb])
    )
  | Some [attra], attrb -> fail
  | _ -> failwith "only one attribute allowed in SmPL")

and attribute = fun ea eb ->
  match ea, eb with
    (A.FAttr attra), (B.Attribute attrb, ii)
      when (A.unwrap_mcode attra) = attrb ->
      let ib1 = tuple_of_list1 ii in
      tokenf attra ib1 >>= (fun attra ib1 ->
	return (
	  A.FAttr attra,
	  (B.Attribute attrb, [ib1])
        ))
  | _ -> fail

(*---------------------------------------------------------------------------*)

and compatible_base_type a signa b =
  let ok  = return ((),()) in

  match a, b with
  | Type_cocci.VoidType,    B.Void
  | Type_cocci.SizeType,    B.SizeType
  | Type_cocci.SSizeType,   B.SSizeType
  | Type_cocci.PtrDiffType, B.PtrDiffType ->
      assert (signa = None);
      ok
  | Type_cocci.CharType, B.IntType B.CChar when signa = None ->
      ok
  | Type_cocci.CharType, B.IntType (B.Si (signb, B.CChar2)) ->
      compatible_sign signa signb
  | Type_cocci.ShortType, B.IntType (B.Si (signb, B.CShort)) ->
      compatible_sign signa signb
  | Type_cocci.IntType, B.IntType (B.Si (signb, B.CInt)) ->
      compatible_sign signa signb
  | Type_cocci.LongType, B.IntType (B.Si (signb, B.CLong)) ->
      compatible_sign signa signb
  | Type_cocci.LongLongType, B.IntType (B.Si (signb, B.CLongLong)) ->
      compatible_sign signa signb
  | Type_cocci.FloatType, B.FloatType B.CFloat ->
      assert (signa = None);
      ok
  | Type_cocci.DoubleType, B.FloatType B.CDouble ->
      assert (signa = None);
      ok
  | _, B.FloatType B.CLongDouble ->
      pr2_once "no longdouble in cocci";
      fail
  | Type_cocci.BoolType, _ -> failwith "no booltype in C"

  | _, (B.Void|B.FloatType _|B.IntType _
        |B.SizeType|B.SSizeType|B.PtrDiffType) -> fail

and compatible_base_type_meta a signa qua b ii local =
  match a, b with
  | Type_cocci.MetaType(ida,keep,inherited),
    B.IntType (B.Si (signb, B.CChar2)) ->
      compatible_sign signa signb >>= fun _ _ ->
	let newb = ((qua, (B.BaseType (B.IntType B.CChar),ii)),local) in
	compatible_type a newb
  | Type_cocci.MetaType(ida,keep,inherited), B.IntType (B.Si (signb, ty)) ->
      compatible_sign signa signb >>= fun _ _ ->
	let newb =
	  ((qua, (B.BaseType (B.IntType (B.Si (B.Signed, ty))),ii)),local) in
	compatible_type a newb
  | _, B.FloatType B.CLongDouble ->
      pr2_once "no longdouble in cocci";
      fail

  | _, (B.Void|B.FloatType _|B.IntType _
        |B.SizeType|B.SSizeType|B.PtrDiffType) -> fail


and compatible_type a (b,local) =
  let ok  = return ((),()) in

  let rec loop = function
    | _, (qua, (B.NoType, _)) ->
	failwith "compatible_type: matching with NoType"
    | Type_cocci.BaseType a, (qua, (B.BaseType b,ii)) ->
	compatible_base_type a None b

    | Type_cocci.SignedT (signa,None), (qua, (B.BaseType b,ii)) ->
	compatible_base_type Type_cocci.IntType (Some signa) b

    | Type_cocci.SignedT (signa,Some ty), (qua, (B.BaseType b,ii)) ->
	(match ty with
	  Type_cocci.BaseType ty ->
	    compatible_base_type ty (Some signa) b
	| Type_cocci.MetaType(ida,keep,inherited) ->
	    compatible_base_type_meta ty (Some signa) qua b ii local
	| _ -> failwith "not possible")

    | Type_cocci.Pointer  a, (qub, (B.Pointer b, ii)) ->
	loop (a,b)
    | Type_cocci.FunctionPointer a, _ ->
	failwith
	  "TODO: function pointer type doesn't store enough information to determine compatibility"
    | Type_cocci.Array   a, (qub, (B.Array (eopt, b),ii)) ->
      (* no size info for cocci *)
	loop (a,b)
    | Type_cocci.Decimal(e1,e2), (qub, (B.Decimal(l,p),ii)) ->
	(match X.mode with
	  TransformMode -> ok (* nothing to do here *)
	| PatternMode ->
	    let tc2e = function
		Type_cocci.Name n ->
		  A.make_term(A.Ident(A.make_term(A.Id(A.make_mcode n))))
	      | Type_cocci.Num n ->
		  A.make_term(A.Constant(A.make_mcode(A.Int n)))
	      | Type_cocci.MV(mv,keep,inh) ->
		  A.make_term
		    (A.MetaExpr
		       (A.make_mcode mv,A.NoConstraint,keep,None,A.CONST,inh))
	      | _ -> failwith "unexpected name in decimal" in
      (* no size info for cocci *)
	    expression (tc2e e1) l >>=
	    (fun _ _ -> (* no transformation to record *)
	      match p with
		None -> failwith "not allowed in a type"
	      | Some p ->
		  expression (tc2e e2) p >>=
		  (fun _ _ -> (* no transformation to record *)
		    ok)))
    | Type_cocci.StructUnionName (sua, name),
	(qub, (B.StructUnionName (sub, sb),ii)) ->
	  if equal_structUnion_type_cocci sua sub
	  then structure_type_name name sb ii
	  else fail
    | Type_cocci.EnumName (name),
	(qub, (B.EnumName (sb),ii)) -> structure_type_name name sb ii
    | Type_cocci.TypeName sa, (qub, (B.TypeName (namesb, _typb),noii)) ->
        let sb = Ast_c.str_of_name namesb in
	if sa = sb
	then ok
	else fail

    | Type_cocci.ConstVol (qua, a),      (qub, b) ->
	if (fst qub).B.const && (fst qub).B.volatile
	then
	  begin
	    pr2_once ("warning: the type is both const & volatile but cocci " ^
                      "does not handle that");
            fail
	  end
	else
          if
            (match qua with
            | Type_cocci.Const -> (fst qub).B.const
            | Type_cocci.Volatile -> (fst qub).B.volatile
  	    )
          then loop (a,(Ast_c.nQ, b))
          else fail

    | Type_cocci.MetaType (ida,keep,inherited),     typb ->
	let max_min _ =
	  Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_type typb) in
	X.envf keep inherited (A.make_mcode ida, B.MetaTypeVal typb, max_min)
	  (fun () -> ok
        )

  (* subtil: must be after the MetaType case *)
    | a, (qub, (B.TypeName (_namesb, Some b), noii)) ->
      (* kind of typedef iso *)
	loop (a,b)

  (* for metavariables of type expression *^* *)
    | Type_cocci.Unknown , _ -> ok

    | (_,
      (_,
      ((
       B.TypeOfType _|B.TypeOfExpr _|B.ParenType _|
       B.EnumName _|B.StructUnion (_, _, _)|B.Enum (_, _)
      ),
      _))) -> fail

    | (_,
      (_,
      ((
       B.StructUnionName (_, _)|
       B.FunctionType _|
       B.Array (_, _)|B.Decimal (_, _)|B.Pointer _|B.TypeName _|
       B.BaseType _
      ),
      _))) -> fail
(*
and decimal_type_exp nm sb ii =
    match nm with
      Type_cocci.NoName -> failwith "unexpected NoName in decimal type"
    | Type_cocci.Name sa ->
	(match B.unwrap sb with
	  B.Ident name ->
	    let ida = A.make_term(A.Id(A.make_mcode n)) in
	    ident_cpp DontKnow ida nameidb >>= (fun ida nameidb -> ok)
	| _ -> fail)
    | Type_cocci.Num sa ->
    | Type_cocci.MV(ida,keep,inherited) ->
	(* degenerate version of MetaId, no transformation possible *)
        let (ib1, ib2) = tuple_of_list2 ii in
	let max_min _ = Lib_parsing_c.lin_col_by_pos [ib2] in
	let mida = A.make_mcode ida in
	X.envf keep inherited (mida, B.MetaIdVal sb, max_min)
	  (fun () -> ok)
*)
and structure_type_name nm sb ii =
    match nm with
      Type_cocci.NoName -> ok
    | Type_cocci.Name sa ->
	if sa = sb
	then ok
	else fail
    | Type_cocci.Num sa -> failwith "unexpected Num in structure type"
    | Type_cocci.MV(ida,keep,inherited) ->
	(* degenerate version of MetaId, no transformation possible *)
        let (ib1, ib2) = tuple_of_list2 ii in
	let max_min _ = Lib_parsing_c.lin_col_by_pos [ib2] in
	let mida = A.make_mcode ida in
	X.envf keep inherited (mida, B.MetaIdVal sb, max_min)
	  (fun () -> ok)

  in
  loop (a,b)

and compatible_sign signa signb =
  let ok  = return ((),()) in
  match signa, signb with
  | None, B.Signed
  | Some Type_cocci.Signed, B.Signed
  | Some Type_cocci.Unsigned, B.UnSigned
      -> ok
  | _ -> fail


and equal_structUnion_type_cocci a b =
  match a, b with
  | Type_cocci.Struct, B.Struct -> true
  | Type_cocci.Union,  B.Union -> true
  | _, (B.Struct | B.Union) -> false



(*---------------------------------------------------------------------------*)
and inc_file (a, before_after) (b, h_rel_pos) =

  let rec aux_inc (ass, bss) passed =
    match ass, bss with
    | [], [] -> true
    | [A.IncDots], _ ->
        let passed = List.rev passed in

        (match before_after, !h_rel_pos with
        | IncludeNothing, _ -> true
        | IncludeMcodeBefore, Some x ->
            List.mem passed (x.Ast_c.first_of)

        | IncludeMcodeAfter, Some x ->
            List.mem passed (x.Ast_c.last_of)

        (* no info, maybe cos of a #include <xx.h> that was already in a .h *)
        | _, None -> false
        )

    | (A.IncPath x)::xs, y::ys -> x = y && aux_inc (xs, ys) (x::passed)
    | _ -> failwith "IncDots not in last place or other pb"

  in

  match a, b with
  | A.Local ass, B.Local bss ->
      aux_inc (ass, bss) []
  | A.NonLocal ass, B.NonLocal bss ->
      aux_inc (ass, bss) []
  | _ -> false



(*---------------------------------------------------------------------------*)

and (define_params: sequence ->
  (A.define_param list, (string B.wrap) B.wrap2 list) matcher) =
 fun seqstyle eas ebs ->
  match seqstyle with
  | Unordered -> failwith "not handling ooo"
  | Ordered ->
      define_paramsbis eas (Ast_c.split_comma ebs) >>= (fun eas ebs_splitted ->
        return (eas, (Ast_c.unsplit_comma ebs_splitted))
      )

(* todo? facto code with argument and parameters ? *)
and define_paramsbis = fun eas ebs ->
  let match_dots ea =
    match A.unwrap ea with
      A.DPdots(mcode) -> Some (mcode, None)
    | _ -> None in
  let build_dots (mcode, _optexpr) = A.DPdots(mcode) in
  let match_comma ea =
    match A.unwrap ea with
      A.DPComma ia1 -> Some ia1
    | _ -> None in
  let build_comma ia1 = A.DPComma ia1 in
  let match_metalist ea = None in
  let build_metalist _ (ida,leninfo,keep,inherited) =
    failwith "not possible" in
  let mktermval v = failwith "not possible" in
  let special_cases ea eas ebs = None in
  let no_ii x = failwith "not possible" in
  list_matcher match_dots build_dots match_comma build_comma
    match_metalist build_metalist mktermval
    special_cases define_parameter X.distrf_define_params
    B.split_comma B.unsplit_comma no_ii
    (function x -> Some x) eas ebs

and define_parameter = fun parama paramb ->
  match A.unwrap parama, paramb with
    A.DParam ida, (idb, ii) ->
      let ib1 = tuple_of_list1 ii in
      ident DontKnow ida (idb, ib1) >>= (fun ida (idb, ib1) ->
        return ((A.DParam ida)+> A.rewrap parama,(idb, [ib1])))
  | A.OptDParam _, _ -> failwith "handling Opt for define parameters"
  | _ -> fail in

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* no global solution for positions here, because for a statement metavariable
we want a MetaStmtVal, and for the others, it's not clear what we want *)

(*
let rec (rule_elem_node: (A.rule_elem, F.node) matcher) =
 fun re node ->
*)
  let rewrap x =
    x >>= (fun a b -> return (A.rewrap re a, F.rewrap node b))
  in
  X.all_bound (A.get_inherited re) >&&>

  rewrap (
  match A.unwrap re, F.unwrap node with

  (* note: the order of the clauses is important. *)

  | _, F.Enter | _, F.Exit | _, F.ErrorExit -> fail2()

  (* the metaRuleElem contains just '-' information. We don't need to add
   * stuff in the environment. If we need stuff in environment, because
   * there is a + S somewhere, then this will be done via MetaStmt, not
   * via MetaRuleElem.
   * Can match TrueNode/FalseNode/... so must be placed before those cases.
   *)

  | A.MetaRuleElem(mcode,keep,inherited), unwrap_node ->
      let default = A.MetaRuleElem(mcode,keep,inherited), unwrap_node in
      (match unwrap_node with
      | F.CaseNode _
      | F.TrueNode _ | F.FalseNode | F.AfterNode _
      | F.LoopFallThroughNode  | F.FallThroughNode
      | F.InLoopNode ->
          if X.mode = PatternMode
          then return default
          else
            if mcode_contain_plus (mcodekind mcode)
            then failwith "try add stuff on fake node"
              (* minusize or contextize a fake node is ok *)
            else return default

      | F.EndStatement None ->
          if X.mode = PatternMode then return default
          else
              (* DEAD CODE NOW ? only useful in -no_cocci_vs_c_3 ?
                 if mcode_contain_plus (mcodekind mcode)
                 then
                 let fake_info = Ast_c.fakeInfo() in
                 distrf distrf_node (mcodekind mcode)
                 (F.EndStatement (Some fake_info))
                 else return unwrap_node
              *)
            raise Todo

      | F.EndStatement (Some i1) ->
          tokenf mcode i1 >>= (fun mcode i1 ->
            return (
              A.MetaRuleElem (mcode,keep, inherited),
              F.EndStatement (Some i1)
            ))

      | F.FunHeader _ ->
          if X.mode = PatternMode then return default
          else failwith "a MetaRuleElem can't transform a headfunc"
      | _n ->
          if X.mode = PatternMode then return default
          else
          X.distrf_node (generalize_mcode mcode) node >>= (fun mcode node ->
            return (
              A.MetaRuleElem(mcode,keep, inherited),
              F.unwrap node
            ))
      )


  (* rene cannot have found that a state containing a fake/exit/... should be
   * transformed
   * TODO: and F.Fake ?
   *)
  | _, F.EndStatement _ | _, F.CaseNode _
  | _, F.TrueNode _ | _, F.FalseNode | _, F.AfterNode _
  | _, F.FallThroughNode | _, F.LoopFallThroughNode
  | _, F.InLoopNode -> fail2()

  (* really ? diff between pattern.ml and transformation.ml *)
  | _, F.Fake -> fail2()


  (* cas general: a Meta can match everything. It matches only
   * "header"-statement. We transform only MetaRuleElem, not MetaStmt.
   * So can't have been called in transform.
   *)
  | A.MetaStmt (ida,keep,metainfoMaybeTodo,inherited),  F.Decl(_) -> fail

  | A.MetaStmt (ida,keep,metainfoMaybeTodo,inherited),  unwrap_node ->
      (* todo: should not happen in transform mode *)

      (match F.extract_fullstatement node with
      | Some stb ->
	    let max_min _ =
	      Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_stmt stb) in
            X.envf keep inherited
	      (ida, Ast_c.MetaStmtVal(stb,Ast_c.WITH_TYPES), max_min)
	      (fun () ->
              (* no need tag ida, we can't be called in transform-mode *)
		return (
		A.MetaStmt (ida, keep, metainfoMaybeTodo, inherited),
		unwrap_node
	      )
	    )
      | None -> fail
      )

  (* not me?: *)
  | A.MetaStmtList _, _ ->
      failwith "not handling MetaStmtList"

  | A.TopExp ea, F.DefineExpr eb  ->
      expression ea eb >>= (fun ea eb ->
        return (
          A.TopExp ea,
          F.DefineExpr eb
        ))

  | A.TopExp ea, F.DefineType eb  ->
      (match A.unwrap ea with
	A.TypeExp(ft) ->
	  fullType ft eb >>= (fun ft eb ->
            return (
              A.TopExp (A.rewrap ea (A.TypeExp(ft))),
              F.DefineType eb
            ))
      |	_ -> fail)



  (* It is important to put this case before the one that fails because
   * of the lack of the counter part of a C construct in SmPL (for instance
   * there is not yet a CaseRange in SmPL). Even if SmPL don't handle
   * yet certain constructs, those constructs may contain expression
   * that we still want and can transform.
   *)

  | A.Exp exp, nodeb ->
      (* kind of iso, initialisation vs affectation *)
      let node =
        match A.unwrap exp, nodeb with
        | A.Assignment (ea, op, eb, true), F.Decl decl ->
            initialisation_to_affectation decl +> F.rewrap node
        | _ -> node
      in


     (* Now keep fullstatement inside the control flow node,
      * so that can then get in a MetaStmtVar the fullstatement to later
      * pp back when the S is in a +. But that means that
      * Exp will match an Ifnode even if there is no such exp
      * inside the condition of the Ifnode (because the exp may
      * be deeper, in the then branch). So have to not visit
      * all inside a node anymore.
      *
      * update: j'ai choisi d'accrocher au noeud du CFG à la
      * fois le fullstatement et le partialstatement et appeler le
      * visiteur que sur le partialstatement.
      *)
      let expfn =
        match Ast_cocci.get_pos re with
        | None -> expression
        | Some pos ->
            (fun ea eb ->
              let (max,min) =
                Lib_parsing_c.max_min_by_pos (Lib_parsing_c.ii_of_expr eb) in
              let keep = Type_cocci.Unitary in
              let inherited = false in
	      let max_min _ = failwith "no pos" in
              X.envf keep inherited (pos, B.MetaPosVal (min,max), max_min)
		(fun () ->
                  expression ea eb
              )
            )
      in
      X.cocciExp expfn exp node >>= (fun exp node ->
        return (
          A.Exp exp,
          F.unwrap node
        )
      )

  | A.Ty ty, nodeb ->
      X.cocciTy fullType ty node >>= (fun ty node ->
        return (
          A.Ty ty,
          F.unwrap node
        )
      )

  | A.TopId id, nodeb ->
      X.cocciId (ident_cpp DontKnow) id node >>= (fun id node ->
        return (
          A.TopId id,
          F.unwrap node
        )
      )

  | A.TopInit init, nodeb ->
      X.cocciInit initialiser init node >>= (fun init node ->
        return (
          A.TopInit init,
          F.unwrap node
        )
      )


  | A.FunHeader (mckstart, allminus, fninfoa, ida, oparen, paramsa, va, cparen),
    F.FunHeader ({B.f_name = nameidb;
                  f_type = (retb, (paramsb, (isvaargs, iidotsb)));
                  f_storage = stob;
                  f_attr = attrs;
                  f_body = body;
                  f_old_c_style = oldstyle;
                  }, ii) ->
      assert (body = []);

      if oldstyle <> None
      then pr2 "OLD STYLE DECL NOT WELL SUPPORTED";

      let (stoa,tya,inla,attras) = get_fninfo fninfoa in

      (match ii with
      | ioparenb::icparenb::iifakestart::iistob ->

          (* maybe important to put ident as the first tokens to transform.
           * It's related to transform_proto. So don't change order
           * between the >>=.
           *)
          ident_cpp LocalFunction ida nameidb >>= (fun ida nameidb ->
          X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart ->
          tokenf oparen ioparenb >>= (fun oparen ioparenb ->
          tokenf cparen icparenb >>= (fun cparen icparenb ->
          parameters (seqstyle paramsa)
            (A.unwrap paramsa) paramsb >>=
            (fun paramsaunwrap paramsb ->
              let paramsa = A.rewrap paramsa paramsaunwrap in
          inline_optional_allminus allminus
            inla (stob, iistob) >>= (fun inla (stob, iistob) ->
          storage_optional_allminus allminus
            stoa (stob, iistob) >>= (fun stoa (stob, iistob) ->
          attribute_list attras attrs >>= (fun attras attrs ->
              (
                if isvaargs
                then
		  pr2_once
		    ("Not handling well variable length arguments func. "^
		     "You have been warned");
                if allminus
                then minusize_list iidotsb
                else return ((),iidotsb)
              ) >>= (fun () iidotsb ->

           fullType_optional_allminus allminus tya retb >>= (fun tya retb ->

             let fninfoa = put_fninfo stoa tya inla attras in

             return (
               A.FunHeader(mckstart,allminus,fninfoa,ida,oparen,
                          paramsa,va,cparen),
               F.FunHeader ({B.f_name = nameidb;
                             f_type = (retb, (paramsb, (isvaargs, iidotsb)));
                             f_storage = stob;
                             f_attr = attrs;
                             f_body = body;
                             f_old_c_style = oldstyle; (* TODO *)
                           },
                           ioparenb::icparenb::iifakestart::iistob)
                )
              ))))))))))
      | _ -> raise (Impossible 49)
      )

  | A.Decl decla, F.Decl declb ->
      annotated_decl decla declb >>=
       (fun decla declb ->
        return (
          A.Decl decla,
          F.Decl declb
        ))


  | A.SeqStart mcode, F.SeqStart (st, level, i1) ->
      tokenf mcode i1 >>= (fun mcode i1 ->
        return (
          A.SeqStart mcode,
          F.SeqStart (st, level, i1)
        ))

  | A.SeqEnd mcode, F.SeqEnd (level, i1) ->
      tokenf mcode i1 >>= (fun mcode i1 ->
        return (
          A.SeqEnd mcode,
          F.SeqEnd (level, i1)
          ))

  | A.ExprStatement (Some ea, ia1), F.ExprStatement (st, (Some eb, ii)) ->
      let ib1 = tuple_of_list1 ii in
      expression ea eb >>= (fun ea eb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
        return (
          A.ExprStatement (Some ea, ia1),
          F.ExprStatement (st, (Some eb, [ib1]))
        )
      ))

  | A.ExprStatement (None, ia1), F.ExprStatement (st, (None, [ib1])) ->
      (* ia1/ib1 represents a ; *)
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
        return (
          A.ExprStatement (None, ia1),
          F.ExprStatement (st, (None, [ib1]))
        )
      )

  | _, F.ExprStatement (st, (None, [])) ->
      (* This case occurs when we have eg nothing after a switch label
	 and so there is no semicolon.  Indeed, there is no match, so any
	 match against this fails. *)
      fail

  | A.IfHeader (ia1,ia2, ea, ia3), F.IfHeader (st, (eb,ii)) ->
      let (ib1, ib2, ib3) = tuple_of_list3 ii in
      expression ea eb >>= (fun ea eb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      tokenf ia3 ib3 >>= (fun ia3 ib3 ->
        return (
          A.IfHeader (ia1, ia2, ea, ia3),
          F.IfHeader (st, (eb,[ib1;ib2;ib3]))
        )))))

  | A.Else ia, F.Else ib ->
      tokenf ia ib >>= (fun ia ib ->
        return (A.Else ia, F.Else ib)
      )

  | A.WhileHeader (ia1, ia2, ea, ia3), F.WhileHeader (st, (eb, ii)) ->
      let (ib1, ib2, ib3) = tuple_of_list3 ii in
      expression ea eb >>= (fun ea eb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      tokenf ia3 ib3 >>= (fun ia3 ib3 ->
        return (
          A.WhileHeader (ia1, ia2, ea, ia3),
          F.WhileHeader (st, (eb, [ib1;ib2;ib3]))
        )))))

  | A.DoHeader ia, F.DoHeader (st, ib) ->
      tokenf ia ib >>= (fun ia ib ->
        return (
          A.DoHeader ia,
          F.DoHeader (st, ib)
        ))
  | A.WhileTail (ia1,ia2,ea,ia3,ia4), F.DoWhileTail (eb, ii) ->
      let (ib1, ib2, ib3, ib4) = tuple_of_list4 ii in
      expression ea eb >>= (fun ea eb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      tokenf ia3 ib3 >>= (fun ia3 ib3 ->
      tokenf ia4 ib4 >>= (fun ia4 ib4 ->
        return (
          A.WhileTail (ia1,ia2,ea,ia3,ia4),
          F.DoWhileTail (eb, [ib1;ib2;ib3;ib4])
        ))))))
  | A.IteratorHeader (ia1, ia2, eas, ia3), F.MacroIterHeader (st, ((s,ebs),ii))
      ->
      let (ib1, ib2, ib3) = tuple_of_list3 ii in

      ident DontKnow ia1 (s, ib1) >>= (fun ia1 (s, ib1) ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      tokenf ia3 ib3 >>= (fun ia3 ib3 ->
      arguments (seqstyle eas) (A.unwrap eas) ebs >>= (fun easunwrap ebs ->
       let eas = A.rewrap eas easunwrap in
       return (
         A.IteratorHeader (ia1, ia2, eas, ia3),
         F.MacroIterHeader (st, ((s,ebs), [ib1;ib2;ib3]))
       )))))



  | A.ForHeader (ia1, ia2, firsta, ea2opt, ia4, ea3opt, ia5),
    F.ForHeader (st, ((firstb, (eb2opt,ib4s), (eb3opt,ib4vide)), ii))
    ->
      assert (ib4vide = []);
      let (ib1, ib2, ib5) = tuple_of_list3 ii in
      let ib4 = tuple_of_list1 ib4s in

      (match (firsta,firstb) with
	(A.ForExp(ea1opt, ia3),B.ForExp(eb1opt,ib3s)) ->
	  let ib3 = tuple_of_list1 ib3s in
	  tokenf ia3 ib3 >>= (fun ia3 ib3 ->
	  eoption expression ea1opt eb1opt >>= (fun ea1opt eb1opt ->
	    return (A.ForExp(ea1opt, ia3),B.ForExp(eb1opt,[ib3]))))
      |	(A.ForDecl decla,B.ForDecl declb) ->
	  annotated_decl decla declb >>=
	  (fun decla declb ->
	    return (
            A.ForDecl decla,
            B.ForDecl declb
          ))
      |	_ -> fail)
	>>=
      (fun firsta firstb ->
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      tokenf ia4 ib4 >>= (fun ia4 ib4 ->
      tokenf ia5 ib5 >>= (fun ia5 ib5 ->
      eoption expression ea2opt eb2opt >>= (fun ea2opt eb2opt ->
      eoption expression ea3opt eb3opt >>= (fun ea3opt eb3opt ->
        return (
          A.ForHeader(ia1, ia2, firsta, ea2opt, ia4, ea3opt, ia5),
          F.ForHeader(st,((firstb,(eb2opt,[ib4]),(eb3opt,[])),[ib1;ib2;ib5]))
        ))))))))


  | A.SwitchHeader(ia1,ia2,ea,ia3), F.SwitchHeader (st, (eb,ii)) ->
      let (ib1, ib2, ib3) = tuple_of_list3 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      tokenf ia3 ib3 >>= (fun ia3 ib3 ->
      expression ea eb >>= (fun ea eb ->
        return (
          A.SwitchHeader(ia1,ia2,ea,ia3),
          F.SwitchHeader (st, (eb,[ib1;ib2;ib3]))
        )))))

  | A.Break (ia1, ia2), F.Break (st, ((),ii),fromswitch) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        return (
          A.Break (ia1, ia2),
          F.Break (st, ((),[ib1;ib2]), fromswitch)
        )))

  | A.Continue (ia1, ia2), F.Continue (st, ((),ii)) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        return (
          A.Continue (ia1, ia2),
          F.Continue (st, ((),[ib1;ib2]))
        )))

  | A.Return (ia1, ia2), F.Return (st, ((),ii)) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
        return (
          A.Return (ia1, ia2),
          F.Return (st, ((),[ib1;ib2]))
        )))

  | A.ReturnExpr (ia1, ea, ia2), F.ReturnExpr (st, (eb, ii)) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      expression ea eb >>= (fun ea eb ->
        return (
          A.ReturnExpr (ia1, ea, ia2),
          F.ReturnExpr (st, (eb, [ib1;ib2]))
        ))))


  | A.Include(incla,filea),
    F.Include {B.i_include = (fileb, ii);
               B.i_rel_pos = h_rel_pos;
               B.i_is_in_ifdef = inifdef;
               B.i_content = copt;
              } ->
      assert (copt = None);

      let include_requirment =
        match mcodekind incla, mcodekind filea with
        | A.CONTEXT (_, A.BEFORE _), _ ->
            IncludeMcodeBefore
        | _, A.CONTEXT (_, A.AFTER _) ->
            IncludeMcodeAfter
        | _ ->
            IncludeNothing
      in

      let (inclb, iifileb) = tuple_of_list2 ii in
      if inc_file (term filea, include_requirment) (fileb, h_rel_pos)
      then
        tokenf incla inclb >>= (fun incla inclb ->
        tokenf filea iifileb >>= (fun filea iifileb ->
          return (
            A.Include(incla, filea),
            F.Include {B.i_include = (fileb, [inclb;iifileb]);
                       B.i_rel_pos = h_rel_pos;
                       B.i_is_in_ifdef = inifdef;
                       B.i_content = copt;
            }
          )))
      else fail

  | A.Undef(undefa,ida), F.DefineHeader ((idb, ii), B.Undef) ->
      let (defineb, iidb, ieol) = tuple_of_list3 ii in
      ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) ->
      tokenf undefa defineb >>= (fun undefa defineb ->
        return (
	  A.Undef(undefa,ida),
          F.DefineHeader ((idb,[defineb;iidb;ieol]),B.Undef)
        ))
      )


  | A.DefineHeader(definea,ida,params), F.DefineHeader ((idb, ii), defkind) ->
      let (defineb, iidb, ieol) = tuple_of_list3 ii in
      ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) ->
      tokenf definea defineb >>= (fun definea defineb ->
      (match A.unwrap params, defkind with
      | A.NoParams, B.DefineVar ->
          return (
            A.NoParams +> A.rewrap params,
            B.DefineVar
          )
      | A.DParams(lpa,eas,rpa), (B.DefineFunc (ebs, ii)) ->
          let (lpb, rpb) = tuple_of_list2 ii in
          tokenf lpa lpb >>= (fun lpa lpb ->
          tokenf rpa rpb >>= (fun rpa rpb ->

          define_params (seqstyle eas) (A.unwrap eas) ebs >>=
            (fun easunwrap ebs ->
              let eas = A.rewrap eas easunwrap in
              return (
                A.DParams (lpa,eas,rpa) +> A.rewrap params,
                B.DefineFunc (ebs,[lpb;rpb])
                )
            )))
      | _ -> fail
      ) >>= (fun params defkind ->
        return (
          A.DefineHeader (definea, ida, params),
          F.DefineHeader ((idb,[defineb;iidb;ieol]),defkind)
        ))
      ))

  | A.Pragma(prga,ida,pragmainfoa), F.PragmaHeader ((idb, ii), pragmainfob) ->
      let (prgb, iidb, ieol) = tuple_of_list3 ii in
      ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) ->
      tokenf prga prgb >>= (fun prga prgb ->
      let wp x = A.rewrap pragmainfoa x  in
      (match A.unwrap pragmainfoa, pragmainfob with
	A.PragmaTuple(lp,eas,rp), B.PragmaTuple(ebs,iib) ->
	  let (ib1, ib2) = tuple_of_list2 iib in
	  tokenf lp ib1 >>= (fun lp ib1 ->
	  tokenf rp ib2 >>= (fun rp ib2 ->
	  arguments (seqstyle eas) (A.unwrap eas) ebs >>= (fun easunwrap ebs ->
          let eas = A.rewrap eas easunwrap in
	  return (
	    A.PragmaTuple(lp,eas,rp) +> wp,
	    B.PragmaTuple(ebs,[ib1; ib2])
	  ))))
      | A.PragmaIdList(idsa), B.PragmaIdList(idsb) ->
	  ident_list (A.unwrap idsa) idsb >>= (fun idsaunwrap idsb ->
          let idsa = A.rewrap idsa idsaunwrap in
	  return(
	    A.PragmaIdList(idsa) +> wp,
	    B.PragmaIdList(idsb)
	  ))
      | A.PragmaDots(mcode), _ ->
	  X.distrf_pragmainfo (dots2metavar mcode) pragmainfob >>=
	  (fun mcode pragmainfob ->
            return (
            A.PragmaDots(metavar2dots mcode) +> wp,
            pragmainfob
          ))
      |	_ -> fail
      ) >>= (fun pragmainfoa pragmainfob ->
        return (
	  A.Pragma(prga,ida,pragmainfoa),
	  F.PragmaHeader ((idb, [prgb; iidb; ieol]), pragmainfob)
        ))
      ))

  | A.Default(def,colon), F.Default (st, ((),ii)) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf def ib1 >>= (fun def ib1 ->
      tokenf colon ib2 >>= (fun colon ib2 ->
        return (
          A.Default(def,colon),
          F.Default (st, ((),[ib1;ib2]))
        )))



  | A.Case(case,ea,colon), F.Case (st, (eb,ii)) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf case ib1 >>= (fun case ib1 ->
      expression ea eb >>= (fun ea eb ->
      tokenf colon ib2 >>= (fun colon ib2 ->
        return (
          A.Case(case,ea,colon),
          F.Case (st, (eb,[ib1;ib2]))
        ))))

  (* only occurs in the predicates generated by asttomember *)
  | A.DisjRuleElem eas, _ ->
      (eas +>
      List.fold_left (fun acc ea -> acc >|+|> (rule_elem_node ea node)) fail)
	>>= (fun ea eb -> return (A.unwrap ea,F.unwrap eb))

  | _, F.ExprStatement (_, (None, ii)) -> fail (* happen ? *)

  | A.Label(id,dd), F.Label (st, nameb, ((),ii)) ->
      let (ib2) = tuple_of_list1 ii in
      ident_cpp DontKnow id nameb >>= (fun ida nameb ->
      tokenf dd ib2 >>= (fun dd ib2 ->
	return (
	A.Label (ida,dd),
	F.Label (st,nameb, ((),[ib2]))
      )))

  | A.Goto(goto,id,sem),          F.Goto (st,nameb, ((),ii))       ->
      let (ib1,ib3) = tuple_of_list2 ii in
      tokenf goto ib1 >>= (fun goto ib1 ->
      ident_cpp DontKnow id nameb >>= (fun id nameb ->
      tokenf sem ib3 >>= (fun sem ib3 ->
	return(
	    A.Goto(goto,id,sem),
            F.Goto (st,nameb, ((),[ib1;ib3]))
          ))))

  | A.Exec(exec,lang,code,sem), F.Exec(st,(code2,ii)) ->
      let (exec2,lang2,sem2) = tuple_of_list3 ii in
      tokenf exec exec2 >>= (fun exec exec2 ->
      tokenf lang lang2 >>= (fun lang lang2 ->
      tokenf sem sem2 >>= (fun sem sem2 ->
      exec_code_list (A.unwrap code) (B.split_nocomma code2) >>=
	(fun code_unwrap code2_splitted ->
	  let code = A.rewrap code code_unwrap in
	  let code2 = Ast_c.unsplit_nocomma code2_splitted in
	  return(
	    A.Exec(exec,lang,code,sem),
	    F.Exec(st,(code2,[exec2;lang2;sem2])))))))

  (* have not a counter part in coccinelle, for the moment *)
  (* todo?: print a warning at least ? *)
  | _, F.CaseRange _
  | _, F.Asm _
    -> fail2()
  | _, F.MacroTop _
    -> fail2()

  | _, (F.IfdefEndif _|F.IfdefElse _|F.IfdefHeader _)
    -> fail2 ()

  | _, F.IfdefIteHeader _
    -> fail2 ()

  | _,
    (F.MacroStmt (_, _)| F.DefineDoWhileZeroHeader _| F.EndNode|F.TopNode)
      -> fail
  | _,
    (F.Label (_, _, _)|F.Break (_, _, _)|F.Continue (_, _)|F.Default (_, _)|
    F.Case (_, _)|F.Include _|F.Goto _|F.ExprStatement _|F.Exec _|
    F.DefineType _|F.DefineExpr _|F.DefineTodo|
    F.DefineHeader (_, _)|F.PragmaHeader (_, _)|
    F.ReturnExpr (_, _)|F.Return (_, _)|
    F.MacroIterHeader (_, _)|
    F.SwitchHeader (_, _)|F.ForHeader (_, _)|F.DoWhileTail _|F.DoHeader (_, _)|
    F.WhileHeader (_, _)|F.Else _|F.IfHeader (_, _)|
    F.SeqEnd (_, _)|F.SeqStart (_, _, _)|
    F.Decl _|F.FunHeader _)
      -> fail


  )
end
