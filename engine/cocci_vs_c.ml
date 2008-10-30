(* Copyright (C) 2006, 2007, 2008 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 * 
 * This file was part of Coccinelle.
 *)

open Common

module A = Ast_cocci
module B = Ast_c

module F = Control_flow_c

module Flag = Flag_matcher

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type sequence = Ordered | Unordered

let seqstyle eas =      
   match A.unwrap eas with 
   | A.DOTS _ -> Ordered 
   | A.CIRCLES _ -> Unordered 
   | A.STARS _ -> failwith "not handling stars"

let (redots : 'a A.dots -> 'a list -> 'a A.dots)=fun eas easundots ->
  A.rewrap eas ( 
    match A.unwrap eas with 
    | A.DOTS _ -> A.DOTS easundots
    | A.CIRCLES _ -> A.CIRCLES easundots
    | A.STARS _ -> A.STARS easundots
  )


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
         -> false
   )

(* For the #include <linux/...> in the .cocci, need to find where is
 * the '+' attached to this element, to later find the first concrete
 * #include <linux/xxx.h> or last one in the serie of #includes in the
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
  | A.MINUS (_,[]) -> false
  | A.MINUS (_,x::xs) -> true
  | A.PLUS -> raise Impossible

let mcode_simple_minus = function
  | A.MINUS (_,[]) -> true
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
  {A.line = 0; column =0; A.strbef=[]; A.straft=[];},
  (A.MINUS(A.DontCarePos, [])),
  A.NoMetaPos

let generalize_mcode ia = 
  let (s1, i, mck, pos) = ia in
  let new_mck =
    match mck with
    | A.PLUS -> raise Impossible
    | A.CONTEXT (A.NoPos,x) -> 
	A.CONTEXT (A.DontCarePos,x)
    | A.MINUS   (A.NoPos,x) -> 
	A.MINUS   (A.DontCarePos,x)

    | A.CONTEXT ((A.FixPos _|A.DontCarePos), _) 
    | A.MINUS ((A.FixPos _|A.DontCarePos), _)
        ->
        raise Impossible
  in
  (s1, i, new_mck, pos)



(*---------------------------------------------------------------------------*)

(* 0x0 is equivalent to 0,  value format isomorphism *)
let equal_c_int s1 s2 = 
  try 
    int_of_string s1 = int_of_string s2
  with Failure("int_of_string") -> 
    s1 =$= s2



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
  | A.DeRef    , B.DeRef   -> true
  | A.UnPlus   , B.UnPlus  -> true
  | A.UnMinus  , B.UnMinus -> true
  | A.Tilde    , B.Tilde   -> true
  | A.Not      , B.Not     -> true
  | _, B.GetRefLabel -> false (* todo cocci? *)
  | _, (B.Not|B.Tilde|B.UnMinus|B.UnPlus|B.DeRef|B.GetRef) -> false
      


let equal_arithOp a b = 
  match a, b with
  | A.Plus     , B.Plus     -> true
  | A.Minus    , B.Minus    -> true
  | A.Mul      , B.Mul      -> true
  | A.Div      , B.Div      -> true
  | A.Mod      , B.Mod      -> true
  | A.DecLeft  , B.DecLeft  -> true
  | A.DecRight , B.DecRight -> true
  | A.And      , B.And      -> true
  | A.Or       , B.Or       -> true
  | A.Xor      , B.Xor      -> true
  | _, (B.Xor|B.Or|B.And|B.DecRight|B.DecLeft|B.Mod|B.Div|B.Mul|B.Minus|B.Plus)
      -> false

let equal_logicalOp a b = 
  match a, b with
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

let equal_assignOp a b = 
  match a, b with
  | A.SimpleAssign, B.SimpleAssign -> true
  | A.OpAssign a,   B.OpAssign b -> equal_arithOp a b
  | _, (B.OpAssign _|B.SimpleAssign) -> false

let equal_fixOp a b = 
  match a, b with
  | A.Dec, B.Dec -> true
  | A.Inc, B.Inc -> true
  | _, (B.Inc|B.Dec) -> false

let equal_binaryOp a b = 
  match a, b with
  | A.Arith a,    B.Arith b ->   equal_arithOp a b
  | A.Logical a,  B.Logical b -> equal_logicalOp a b
  | _, (B.Logical _ | B.Arith _) -> false

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
  | Ast_c.MetaIdVal a, Ast_c.MetaIdVal b -> a =$= b
  | Ast_c.MetaFuncVal a, Ast_c.MetaFuncVal b -> a =$= b
  | Ast_c.MetaLocalFuncVal a, Ast_c.MetaLocalFuncVal b -> 
      (* do something more ? *)
      a =$= b

  (* al_expr before comparing !!! and accept when they match.
   * Note that here we have Astc._expression, so it is a match
   * modulo isomorphism (there is no metavariable involved here,
   * just isomorphisms). => TODO call isomorphism_c_c instead of
   * =*=. Maybe would be easier to transform ast_c in ast_cocci
   * and call the iso engine of julia. *)
  | Ast_c.MetaExprVal a, Ast_c.MetaExprVal b -> 
      Lib_parsing_c.al_expr a =*= Lib_parsing_c.al_expr b
  | Ast_c.MetaExprListVal a, Ast_c.MetaExprListVal b -> 
      Lib_parsing_c.al_arguments a =*= Lib_parsing_c.al_arguments b

  | Ast_c.MetaStmtVal a, Ast_c.MetaStmtVal b -> 
      Lib_parsing_c.al_statement a =*= Lib_parsing_c.al_statement b
  | Ast_c.MetaTypeVal a, Ast_c.MetaTypeVal b -> 
      (* old: Lib_parsing_c.al_type a =*= Lib_parsing_c.al_type b *)
      C_vs_c.eq_type a b
        
  | Ast_c.MetaListlenVal a, Ast_c.MetaListlenVal b -> a =|= b

  | Ast_c.MetaParamVal a, Ast_c.MetaParamVal b -> 
      Lib_parsing_c.al_param a =*= Lib_parsing_c.al_param b
  | Ast_c.MetaParamListVal a, Ast_c.MetaParamListVal b -> 
      Lib_parsing_c.al_params a =*= Lib_parsing_c.al_params b

  | Ast_c.MetaPosVal (posa1,posa2), Ast_c.MetaPosVal (posb1,posb2) -> 
      Ast_cocci.equal_pos posa1 posb1 && Ast_cocci.equal_pos posa2 posb2
        
  | Ast_c.MetaPosValList l1, Ast_c.MetaPosValList l2 ->
      List.exists
	(function (fla,posa1,posa2) ->
	  List.exists
	    (function (flb,posb1,posb2) ->
	      fla = flb &&
	      Ast_c.equal_posl posa1 posb1 && Ast_c.equal_posl posa2 posb2)
            l2)
	l1

  | (B.MetaPosValList _|B.MetaListlenVal _|B.MetaPosVal _|B.MetaStmtVal _
      |B.MetaTypeVal _
      |B.MetaParamListVal _|B.MetaParamVal _|B.MetaExprListVal _
      |B.MetaExprVal _|B.MetaLocalFuncVal _|B.MetaFuncVal _|B.MetaIdVal _
    ), _
      -> raise Impossible


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
      (match sign, base, xs with
      | B.Signed, B.CChar2,   ["signed",i1;"char",i2] -> 
          Some (B.Signed, i1), [i2]
      | B.UnSigned, B.CChar2,   ["unsigned",i1;"char",i2] -> 
          Some (B.UnSigned, i1), [i2]

      | B.Signed, B.CShort, ["short",i1] -> 
          None, [i1]
      | B.Signed, B.CShort, ["signed",i1;"short",i2] -> 
          Some (B.Signed, i1), [i2]
      | B.UnSigned, B.CShort, ["unsigned",i1;"short",i2] -> 
          Some (B.UnSigned, i1), [i2]
      | B.Signed, B.CShort, ["short",i1;"int",i2] -> 
          None, [i1;i2]

      | B.Signed, B.CInt, ["int",i1] -> 
          None, [i1]
      | B.Signed, B.CInt, ["signed",i1;"int",i2] -> 
          Some (B.Signed, i1), [i2]
      | B.UnSigned, B.CInt, ["unsigned",i1;"int",i2] -> 
          Some (B.UnSigned, i1), [i2]

      | B.Signed, B.CInt, ["signed",i1;] -> 
          Some (B.Signed, i1), []
      | B.UnSigned, B.CInt, ["unsigned",i1;] -> 
          Some (B.UnSigned, i1), []

      | B.Signed, B.CLong, ["long",i1] -> 
          None, [i1]
      | B.Signed, B.CLong, ["long",i1;"int",i2] -> 
          None, [i1;i2]
      | B.Signed, B.CLong, ["signed",i1;"long",i2] -> 
          Some (B.Signed, i1), [i2]
      | B.UnSigned, B.CLong, ["unsigned",i1;"long",i2] -> 
          Some (B.UnSigned, i1), [i2]

      | B.Signed, B.CLongLong, ["long",i1;"long",i2] -> None, [i1;i2]
      | B.Signed, B.CLongLong, ["signed",i1;"long",i2;"long",i3] -> 
          Some (B.Signed, i1), [i2;i3]
      | B.UnSigned, B.CLongLong, ["unsigned",i1;"long",i2;"long",i3] -> 
          Some (B.UnSigned, i1), [i2;i3]


      | B.UnSigned, B.CShort, ["unsigned",i1;"short",i2; "int", i3] -> 
          Some (B.UnSigned, i1), [i2;i3]
          


      | _ -> failwith "strange type1, maybe because of weird order"
      )
  | _ -> failwith "strange type2, maybe because of weird order"

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
  | _, [] -> 
      failwith "shouldn't have a iicomma"
  | [iicomma], x::xs -> 
      let elems = List.map fst (x::xs) in
      let commas = List.map snd (x::xs) +> List.flatten in
      let commas = commas @ [iicomma] in
      zip elems commas 
  | _ -> raise Impossible



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
          
      | x -> raise Impossible
      )
  | _ -> raise Impossible

(*---------------------------------------------------------------------------*)
let initialisation_to_affectation decl = 
  match decl with
  | B.MacroDecl _ -> F.Decl decl
  | B.DeclList (xs, iis) -> 
      
      (* todo?: should not do that if the variable is an array cos
       *  will have x[] = , mais de toute facon ca sera pas un InitExp
       *)
      (match xs with
      | [] -> raise Impossible
      | [x] -> 
          let ({B.v_namei = var;
                B.v_type = returnType;
                B.v_storage = storage;
                B.v_local = local},
              iisep) = x in

          (match var with
          | Some ((s, ini),  iis::iini) -> 
              (match ini with
              | Some (B.InitExpr e, ii_empty2) -> 
		  let local =
		    match local with
		      Ast_c.NotLocalDecl -> Ast_c.NotLocalVar
		    | Ast_c.LocalDecl -> Ast_c.LocalVar (iis.Ast_c.pinfo) in
          
                  let typ =
		    ref (Some ((Lib_parsing_c.al_type returnType),local),
			       Ast_c.NotTest) in
                  let id = (B.Ident s, typ),[iis] in
                  F.DefineExpr
                    ((B.Assignment (id, B.SimpleAssign, e), 
                     Ast_c.noType()), iini)
              | _ -> F.Decl decl
              )
          | _ -> F.Decl decl
          )
      | x::xs -> 
          pr2_once "TODO: initialisation_to_affectation for multi vars";
          (* todo? do a fold_left and generate 'x = a, y = b' etc, use
           * the Sequence expression operator of C and make an 
           * ExprStatement from that.
           *)
          F.Decl decl
      )





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
    val distrf_node : 
      (A.meta_name A.mcode, Control_flow_c.node) matcher

    val distrf_define_params : 
      (A.meta_name A.mcode, (string Ast_c.wrap, Ast_c.il) either list)
      matcher

    val distrf_struct_fields : 
      (A.meta_name A.mcode, B.field list) matcher

    val distrf_cst : 
      (A.meta_name A.mcode, (B.constant, string) either B.wrap) matcher

    val cocciExp : 
      (A.expression, B.expression) matcher -> (A.expression, F.node) matcher

    val cocciExpExp : 
      (A.expression, B.expression) matcher ->
	(A.expression, B.expression) matcher

    val cocciTy : 
      (A.fullType, B.fullType) matcher -> (A.fullType, F.node) matcher

    val cocciInit : 
      (A.initialiser, B.initialiser) matcher -> (A.initialiser, F.node) matcher

    val envf :
      A.keep_binding -> A.inherited -> 
      A.meta_name A.mcode * Ast_c.metavar_binding_kind *
	  (unit -> Common.filename * Ast_c.posl * Ast_c.posl) ->
      (unit -> tin -> 'x tout) -> (tin -> 'x tout)

    val check_constraints :
      ('a, 'b) matcher -> 'a list -> 'b ->
	(unit -> tin -> 'x tout) -> (tin -> 'x tout)

    val all_bound : A.meta_name list -> (tin -> bool)

    val optional_storage_flag : (bool -> tin -> 'x tout) -> (tin -> 'x tout)
    val optional_qualifier_flag : (bool -> tin -> 'x tout) -> (tin -> 'x tout)
    val value_format_flag: (bool -> tin -> 'x tout) -> (tin -> 'x tout)


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
  | TransformMode -> raise Impossible


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
let dots2metavar (_,info,mcodekind,pos) = (("","..."),info,mcodekind,pos)
let metavar2dots (_,info,mcodekind,pos) = ("...",info,mcodekind,pos)

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

(*---------------------------------------------------------------------------*)
let rec (expression: (A.expression, Ast_c.expression) matcher) =
 fun ea eb -> 
  X.all_bound (A.get_inherited ea) >&&>
  let wa x = A.rewrap ea x  in
  match A.unwrap ea, eb with
  
  (* general case: a MetaExpr can match everything *)
  | A.MetaExpr (ida,constraints,keep,opttypa,form,inherited),
    (((expr, opttypb), ii) as expb) ->

      (* old: before have a MetaConst. Now we factorize and use 'form' to 
       * differentiate between different cases *)
      let rec matches_id = function
	  B.Ident(c) -> true
	| B.Cast(ty,e) -> matches_id (B.unwrap_expr e)
	| _ -> false in
      let form_ok =
	match (form,expr) with
	  (A.ANY,_) -> true
	| (A.CONST,e) ->
	    let rec matches = function
		B.Constant(c) -> true
              | B.Ident idb when idb =~ "^[A-Z_][A-Z_0-9]*$" -> 
		  pr2_once ("warning: I consider " ^ idb ^ " as a constant");
		  true
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
	  X.check_constraints expression constraints eb
            (fun () ->
	  let max_min _ =
	    Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_expr expb) in
	  X.envf keep inherited (ida, Ast_c.MetaExprVal expb, max_min)
	    (fun () -> 
	  X.distrf_e ida expb >>= (fun ida expb -> 
	     return (
	       A.MetaExpr (ida,constraints,keep,opttypa,form,inherited)+>
	           A.rewrap ea,
	       expb
	     ))
	  )))
      else fail
	  
  (* old: 
   * | A.MetaExpr(ida,false,opttypa,_inherited), expb ->
   *   D.distribute_mck (mcodekind ida) D.distribute_mck_e expb binding
   * 
   * but bug! because if have not tagged SP, then transform without doing
   * any checks. Hopefully now have tagged SP technique.
   *)
	  
	  
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
	
	
  | A.Ident ida,   ((B.Ident idb, typ),ii) ->
      let ib1 = tuple_of_list1 ii in
      ident DontKnow ida (idb, ib1) >>= (fun ida (idb, ib1) -> 
        return (
        ((A.Ident ida)) +> wa, 
        ((B.Ident idb, typ),[ib1])
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
      | A.Int x, B.Int y -> 
          X.value_format_flag (fun use_value_equivalence -> 
            if use_value_equivalence 
            then 
              if equal_c_int x y
              then do1()
              else fail
            else 
              if x =$= y
              then do1()
            else fail
          )
      | A.Char x, B.Char (y,_) when x =$= y  (* todo: use kind ? *)
          -> do1()
      | A.Float x, B.Float (y,_) when x =$= y (* todo: use floatType ? *)
          -> do1()

      | A.String sa, B.String (sb,_kind) when sa =$= sb ->
          (match ii with
          | [ib1] -> 
            tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
              return ( 
                ((A.Constant ia1)) +> wa, 
                ((B.Constant (ib), typ),[ib1])
              ))
          | _ -> fail (* multi string, not handled *)
          )

      | _, B.MultiString -> (* todo cocci? *) fail
      | _, (B.String _ | B.Float _ | B.Char _ | B.Int _) -> fail
      )


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
      arguments (seqstyle eas) (A.undots eas) ebs >>= (fun easundots ebs -> 
        let eas = redots eas easundots in
        return (
          ((A.FunCall (ea, ia1, eas, ia2)) +> wa,
          ((B.FunCall (eb, ebs),typ), [ib1;ib2])
        ))))))




  | A.Assignment (ea1, opa, ea2, simple),
      ((B.Assignment (eb1, opb, eb2), typ),ii) -> 
      let (opbi) = tuple_of_list1 ii in
      if equal_assignOp (term opa) opb 
      then
        expression ea1 eb1 >>= (fun ea1 eb1 -> 
        expression ea2 eb2 >>= (fun ea2 eb2 -> 
        tokenf opa opbi >>= (fun opa opbi -> 
          return (
            ((A.Assignment (ea1, opa, ea2, simple))) +> wa,
            ((B.Assignment (eb1, opb, eb2), typ), [opbi])
        ))))
      else fail

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
      let opbi = tuple_of_list1 ii in
      if equal_binaryOp (term opa) opb
      then 
        expression ea1 eb1 >>= (fun ea1 eb1 -> 
        expression ea2 eb2 >>= (fun ea2 eb2 -> 
        tokenf opa opbi >>= (fun opa opbi -> 
          return (
            ((A.Binary (ea1, opa, ea2))) +> wa,
            ((B.Binary (eb1, opb, eb2), typ),[opbi]
          )))))
      else fail

  | A.Nested (ea1, opa, ea2), eb -> 
      let rec loop eb =
	(if A.get_test_exp ea1 && not (Ast_c.is_test eb) then fail
	else expression ea1 eb) >|+|>
	(match eb with
	  ((B.Binary (eb1, opb, eb2), typ),ii)
	  when equal_binaryOp (term opa) opb ->
	    let opbi = tuple_of_list1 ii in
	    let left_to_right =
              (expression ea1 eb1 >>= (fun ea1 eb1 -> 
		expression ea2 eb2 >>= (fun ea2 eb2 -> 
		  tokenf opa opbi >>= (fun opa opbi -> 
		    return (
		    ((A.Nested (ea1, opa, ea2))) +> wa,
		    ((B.Binary (eb1, opb, eb2), typ),[opbi]
		       )))))) in
	    let right_to_left =
              (expression ea2 eb1 >>= (fun ea2 eb1 -> 
		expression ea1 eb2 >>= (fun ea1 eb2 -> 
		  tokenf opa opbi >>= (fun opa opbi -> 
		    return (
		    ((A.Nested (ea1, opa, ea2))) +> wa,
		    ((B.Binary (eb1, opb, eb2), typ),[opbi]
		       )))))) in
	    let in_left =
              (loop eb1 >>= (fun ea1 eb1 -> 
		expression ea2 eb2 >>= (fun ea2 eb2 -> 
		  tokenf opa opbi >>= (fun opa opbi -> 
		    return (
		    ((A.Nested (ea1, opa, ea2))) +> wa,
		    ((B.Binary (eb1, opb, eb2), typ),[opbi]
		       )))))) in
	    let in_right =
              (expression ea2 eb1 >>= (fun ea2 eb1 -> 
		loop eb2 >>= (fun ea1 eb2 -> 
		  tokenf opa opbi >>= (fun opa opbi -> 
		    return (
		    ((A.Nested (ea1, opa, ea2))) +> wa,
		    ((B.Binary (eb1, opb, eb2), typ),[opbi]
		       )))))) in
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
      let (ib1, ib2) = tuple_of_list2 ii in
      ident DontKnow ida (idb, ib2) >>= (fun ida (idb, ib2) -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      expression ea eb >>= (fun ea eb -> 
        return (
          ((A.RecordAccess (ea, ia1, ida))) +> wa,
          ((B.RecordAccess (eb, idb), typ), [ib1;ib2])
        ))))



  | A.RecordPtAccess (ea,ia1,ida),((B.RecordPtAccess (eb, idb), typ), ii) ->
      let (ib1, ib2) = tuple_of_list2 ii in
      ident DontKnow ida (idb, ib2) >>= (fun ida (idb, ib2) -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      expression ea eb >>= (fun ea eb -> 
        return (
          ((A.RecordPtAccess (ea, ia1, ida))) +> wa,
          ((B.RecordPtAccess (eb, idb), typ), [ib1;ib2])
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

  | A.NestExpr(exps,None,true), eb ->
      (match A.unwrap exps with
	A.DOTS [exp] ->
	  X.cocciExpExp expression exp eb >>= (fun exp eb -> 
            return (
            (A.NestExpr(A.rewrap exps (A.DOTS [exp]),None,true)) +> wa,
            eb
            )
	  )
      |	_ ->
	  failwith
	    "for nestexpr, only handling the case with dots and only one exp")

  | A.NestExpr _,     _ ->
      failwith "only handling multi and no when code in a nest expr"

  (* only in arg lists or in define body *)  
  | A.TypeExp _,    _ -> fail

  (* only in arg lists *)
  | A.MetaExprList _,    _   
  | A.EComma _,    _  
  | A.Ecircles _,    _ 
  | A.Estars _,    _   
      ->
	raise Impossible

  | A.DisjExpr eas, eb -> 
      eas +> List.fold_left (fun acc ea -> acc >|+|> (expression ea eb)) fail

  | A.UniqueExp _,_ | A.OptExp _,_ -> 
      failwith "not handling Opt/Unique/Multi on expr"

 (* Because of Exp cant put a raise Impossible; have to put a fail *)

 (* have not a counter part in coccinelle, for the moment *) 
  | _, ((B.Sequence _,_),_) 
  | _, ((B.StatementExpr _,_),_) 
  | _, ((B.Constructor _,_),_) 
    -> fail


  | _, 
     (((B.Cast (_, _)|B.ParenExpr _|B.SizeOfType _|B.SizeOfExpr _|
     B.RecordPtAccess (_, _)|
     B.RecordAccess (_, _)|B.ArrayAccess (_, _)|
     B.Binary (_, _, _)|B.Unary (_, _)|
     B.Infix (_, _)|B.Postfix (_, _)|
     B.Assignment (_, _, _)|B.CondExpr (_, _, _)|
     B.FunCall (_, _)|B.Constant _|B.Ident _),
     _),_)
       -> fail






(* ------------------------------------------------------------------------- *)
and (ident: info_ident -> (A.ident, string * Ast_c.info) matcher) = 
 fun infoidb ida ((idb, iib) as ib) -> 
  X.all_bound (A.get_inherited ida) >&&>
  match A.unwrap ida with
  | A.Id sa -> 
      if (term sa) =$= idb then
      tokenf sa iib >>= (fun sa iib -> 
        return (
          ((A.Id sa)) +> A.rewrap ida,
          (idb, iib)
        ))
      else fail


  | A.MetaId(mida,constraints,keep,inherited) -> 
      X.check_constraints (ident infoidb) constraints ib
        (fun () ->
      let max_min _ = Lib_parsing_c.lin_col_by_pos [iib] in
      (* use drop_pos for ids so that the pos is not added a second time in
	 the call to tokenf *)
      X.envf keep inherited (A.drop_pos mida, Ast_c.MetaIdVal (idb), max_min)
	(fun () -> 
        tokenf mida iib >>= (fun mida iib -> 
          return (
            ((A.MetaId (mida, constraints, keep, inherited)) +> A.rewrap ida,
            (idb, iib)
            )))
      ))

  | A.MetaFunc(mida,constraints,keep,inherited) -> 
      let is_function _ =
	X.check_constraints (ident infoidb) constraints ib
            (fun () ->
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
	  X.check_constraints (ident infoidb) constraints ib
            (fun () ->
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

  | A.OptIdent _ | A.UniqueIdent _ -> 
      failwith "not handling Opt/Unique for ident"



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
 * test the transfo for all the combinaitions    and if multiple transfo
 * possible ? pb ? => the type is to return a expression option ? use
 * some combinators to help ?
 * update: with the tag-SP approach, no more a problem.
 *)

and arguments_bis = fun eas ebs -> 
  match eas, ebs with
  | [], [] -> return ([], [])
  | [], eb::ebs -> fail
  | ea::eas, ebs -> 
      X.all_bound (A.get_inherited ea) >&&>
      (match A.unwrap ea, ebs with
      | A.Edots (mcode, optexpr), ys -> 
          (* todo: if optexpr, then a WHEN and so may have to filter yys *)
          if optexpr <> None then failwith "not handling when in argument";

          (* '...' can take more or less the beginnings of the arguments *)
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (

              (* allow '...', and maybe its associated ',' to match nothing.
               * for the associated ',' see below how we handle the EComma
               * to match nothing.
               *)
              (if startxs = []
              then
                if mcode_contain_plus (mcodekind mcode)
                then fail 
                  (* failwith "I have no token that I could accroche myself on" *)
                else return (dots2metavar mcode, [])
              else 
                (* subtil: we dont want the '...' to match until the
                 * comma. cf -test pb_params_iso. We would get at
                 * "already tagged" error.
                 * this is because both f (... x, ...) and f (..., x, ...)
                 * would match a  f(x,3)  with our "optional-comma" strategy.
                 *)
                  (match Common.last startxs with
                  | Right _ -> fail
                  | Left _ -> 
                      X.distrf_args (dots2metavar mcode) startxs
                  )
              )
              >>= (fun mcode startxs ->
		let mcode = metavar2dots mcode in
                arguments_bis eas endxs >>= (fun eas endxs -> 
                  return (
                    (A.Edots (mcode, optexpr) +> A.rewrap ea) ::eas,
                    startxs ++ endxs
                  )))
            )
          ) fail 

      | A.EComma ia1, Right ii::ebs -> 
          let ib1 = tuple_of_list1 ii in
          tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
            arguments_bis eas ebs >>= (fun eas ebs -> 
              return (
                (A.EComma ia1 +> A.rewrap ea)::eas,
                (Right [ib1])::ebs
              )
            ))
      | A.EComma ia1, ebs -> 
          (* allow ',' to maching nothing. optional comma trick *)
          if mcode_contain_plus (mcodekind ia1)
          then fail
          else arguments_bis eas ebs

      | A.MetaExprList(ida,leninfo,keep,inherited),ys ->
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (
              let ok =
                if startxs = []
                then
                  if mcode_contain_plus (mcodekind ida)
                  then false 
                    (* failwith "no token that I could accroche myself on" *)
                  else true
                else 
                  (match Common.last startxs with
                  | Right _ -> false
                  | Left _ -> true
                  )
              in
              if not ok
              then fail
              else 
                let startxs' = Ast_c.unsplit_comma startxs in
                let len = List.length  startxs' in

		(match leninfo with
		| Some (lenname,lenkeep,leninherited) ->
		    let max_min _ = failwith "no pos" in
                    X.envf lenkeep leninherited
                      (lenname, Ast_c.MetaListlenVal (len), max_min)
		| None -> function f -> f()
                )
                (fun () -> 
		  let max_min _ =
		    Lib_parsing_c.lin_col_by_pos
		      (Lib_parsing_c.ii_of_args startxs) in
                  X.envf keep inherited
                    (ida, Ast_c.MetaExprListVal startxs', max_min)
                (fun () -> 
		  if startxs = []
		  then return (ida, [])
                  else X.distrf_args ida (Ast_c.split_comma startxs')
                )
                >>= (fun ida startxs -> 
                  arguments_bis eas endxs >>= (fun eas endxs -> 
                    return (
                      (A.MetaExprList(ida,leninfo,keep,inherited))
		      +> A.rewrap ea::eas,
                      startxs ++ endxs
                    ))
                  )
                )
            )) fail 


      | _unwrapx, (Left eb)::ebs -> 
          argument ea eb >>= (fun ea eb -> 
          arguments_bis eas ebs >>= (fun eas ebs -> 
            return (ea::eas, Left eb::ebs)
          ))
      | _unwrapx, (Right y)::ys -> raise Impossible
      | _unwrapx, [] -> fail
      )
            
      
and argument arga argb = 
  X.all_bound (A.get_inherited arga) >&&>
   match A.unwrap arga, argb with
  | A.TypeExp tya,  Right (B.ArgType (((b, sopt, tyb), ii_b_s))) ->

      if b || sopt <> None
      then 
        (* failwith "the argument have a storage and ast_cocci does not have"*)
        fail
      else 
        fullType tya tyb >>= (fun tya tyb -> 
          return (
            (A.TypeExp tya) +> A.rewrap arga,
            (Right (B.ArgType (((b, sopt, tyb), ii_b_s))))
        ))

  | A.TypeExp tya,  _                                  -> fail
  | _,              Right (B.ArgType (tyb, sto_iisto)) -> fail
  | _, Left argb -> 
      expression arga argb >>= (fun arga argb -> 
        return (arga, Left argb)
      )
  | _, Right (B.ArgAction y) -> fail


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
  match eas, ebs with
  | [], [] -> return ([], [])
  | [], eb::ebs -> fail
  | ea::eas, ebs ->
      (* the management of positions is inlined into each case, because
	 sometimes there is a Param and sometimes a ParamList *)
      X.all_bound (A.get_inherited ea) >&&>
      (match A.unwrap ea, ebs with
      | A.Pdots (mcode), ys -> 

          (* '...' can take more or less the beginnings of the arguments *)
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (

              (if startxs = []
              then
                if mcode_contain_plus (mcodekind mcode)
                then fail 
                (* failwith "I have no token that I could accroche myself on"*)
                else return (dots2metavar mcode, [])
              else 
                (match Common.last startxs with
                | Right _ -> fail
                | Left _ -> 
                    X.distrf_params (dots2metavar mcode) startxs
                )
              ) >>= (fun mcode startxs ->
		let mcode = metavar2dots mcode in
                parameters_bis eas endxs >>= (fun eas endxs -> 
                  return (
                    (A.Pdots (mcode) +> A.rewrap ea) ::eas,
                    startxs ++ endxs
                  )))
            )
          ) fail 

      | A.PComma ia1, Right ii::ebs -> 
          let ib1 = tuple_of_list1 ii in
          tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
          parameters_bis eas ebs >>= (fun eas ebs -> 
            return (
              (A.PComma ia1 +> A.rewrap ea)::eas,
              (Right [ib1])::ebs
            )
          ))

      | A.PComma ia1, ebs -> 
          (* try optional comma trick *)
          if mcode_contain_plus (mcodekind ia1)
          then fail
          else parameters_bis eas ebs


      | A.MetaParamList(ida,leninfo,keep,inherited),ys->
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (
              let ok =
                if startxs = []
                then
                  if mcode_contain_plus (mcodekind ida)
                  then false 
               (* failwith "I have no token that I could accroche myself on" *)
                  else true
                else 
                  (match Common.last startxs with
                  | Right _ -> false
                  | Left _ -> true
                  )
              in
              if not ok
              then fail
              else 
                let startxs' = Ast_c.unsplit_comma startxs in
                let len = List.length  startxs' in

		(match leninfo with
		  Some (lenname,lenkeep,leninherited) ->
		    let max_min _ = failwith "no pos" in
                    X.envf lenkeep leninherited
		      (lenname, Ast_c.MetaListlenVal (len), max_min)
		| None -> function f -> f()
                )
	        (fun () ->
		  let max_min _ =
		    Lib_parsing_c.lin_col_by_pos
		      (Lib_parsing_c.ii_of_params startxs) in
                  X.envf keep inherited 
                    (ida, Ast_c.MetaParamListVal startxs', max_min)
                (fun () -> 
		  if startxs = []
		  then return (ida, [])
		  else X.distrf_params ida (Ast_c.split_comma startxs')
		) >>= (fun ida startxs -> 
                  parameters_bis eas endxs >>= (fun eas endxs -> 
                    return (
                      (A.MetaParamList(ida,leninfo,keep,inherited))
                        +> A.rewrap ea::eas,
                      startxs ++ endxs
		    ))
                )
                ))
          ) fail 


      | A.VoidParam ta, ys -> 
          (match eas, ebs with
          | [], [Left eb] -> 
              let ((hasreg, idbopt, tb), ii_b_s) = eb in
              if idbopt = None && null ii_b_s 
              then 
                match tb with 
                | (qub, (B.BaseType B.Void,_)) -> 
                    fullType ta tb >>= (fun ta tb -> 
                      return (
                        [(A.VoidParam ta) +> A.rewrap ea],
                        [Left ((hasreg, idbopt, tb), ii_b_s)]
                      ))
                | _ -> fail
              else fail
          | _ -> fail
          )

      | (A.OptParam _ | A.UniqueParam _), _ -> 
              failwith "handling Opt/Unique for Param"

      | A.Pcircles (_), ys -> raise Impossible (* in Ordered mode *)


      | A.MetaParam (ida,keep,inherited), (Left eb)::ebs -> 
          (* todo: use quaopt, hasreg ? *)
	  let max_min _ =
	    Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_param eb) in
          X.envf keep inherited (ida,Ast_c.MetaParamVal eb,max_min) (fun () -> 
            X.distrf_param ida eb
          ) >>= (fun ida eb -> 
              parameters_bis eas ebs >>= (fun eas ebs -> 
                return (
                  (A.MetaParam(ida,keep,inherited))+> A.rewrap ea::eas,
                  (Left eb)::ebs
                )))


      | A.Param (typa, idaopt), (Left eb)::ebs -> 
	  (*this should succeed if the C code has a name, and fail otherwise*)
          parameter (idaopt, typa) eb >>= (fun (idaopt, typa) eb -> 
          parameters_bis eas ebs >>= (fun eas ebs -> 
            return (
              (A.Param (typa, idaopt))+> A.rewrap ea :: eas,
              (Left eb)::ebs
            )))
          
      | _unwrapx, (Right y)::ys -> raise Impossible
      | _unwrapx, [] -> fail
      )
  




and parameter = fun (idaopt, typa)   ((hasreg, idbopt, typb), ii_b_s) ->
  fullType typa typb >>= (fun typa typb -> 
  match idaopt, Ast_c.split_register_param (hasreg, idbopt, ii_b_s) with
  | Some ida, Left (idb, iihasreg, iidb) -> 
      (* todo: if minus on ida, should also minus the iihasreg ? *)
      ident DontKnow ida (idb,iidb) >>= (fun ida (idb,iidb) -> 
        return (
          (Some ida, typa),
          ((hasreg, Some idb, typb), iihasreg++[iidb])
        ))
        
  | None, Right iihasreg -> 
      return (
        (None, typa),
        ((hasreg, None, typb), iihasreg)
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

  | Some _, Right _ -> fail
  | None, Left _ -> fail
  )




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

  | A.MetaDecl(ida,_keep,_inherited), _ -> (* keep ? inherited ? *)
      (* todo: should not happen in transform mode *)
      return ((mckstart, allminus, decla), declb)



  | _, (B.DeclList ([var], iiptvirgb::iifakestart::iisto)) -> 
      onedecl allminus decla (var,iiptvirgb,iisto) >>=
      (fun decla (var,iiptvirgb,iisto)->
        X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart -> 
          return (
            (mckstart, allminus, decla),
            (B.DeclList ([var], iiptvirgb::iifakestart::iisto))
          )))
        
  | _, (B.DeclList (xs, iiptvirgb::iifakestart::iisto)) -> 
      if X.mode = PatternMode
      then
        xs +> List.fold_left (fun acc var -> 
          acc >||> (
            X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart ->
              onedecl allminus decla (var, iiptvirgb, iisto) >>= 
                (fun decla (var, iiptvirgb, iisto) -> 
                  return (
                    (mckstart, allminus, decla),
                    (B.DeclList ([var], iiptvirgb::iifakestart::iisto))
                  )))))
          fail
      else 
        failwith "More that one variable in decl. Have to split to transform."

  | A.MacroDecl (sa,lpa,eas,rpa,enda), B.MacroDecl ((sb,ebs),ii) ->
      let (iisb, lpb, rpb, iiendb, iifakestart, iistob) = 
        (match ii with
        | iisb::lpb::rpb::iiendb::iifakestart::iisto -> 
            (iisb,lpb,rpb,iiendb, iifakestart,iisto)
        | _ -> raise Impossible
        ) in
      (if allminus 
      then minusize_list iistob
      else return ((), iistob)
      ) >>= (fun () iistob ->

        X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart -> 
	ident DontKnow sa (sb, iisb) >>= (fun sa (sb, iisb) ->
        tokenf lpa lpb >>= (fun lpa lpb -> 
        tokenf rpa rpb >>= (fun rpa rpb -> 
        tokenf enda iiendb >>= (fun enda iiendb -> 
        arguments (seqstyle eas) (A.undots eas) ebs >>= (fun easundots ebs -> 
        let eas = redots eas easundots in

          return (
            (mckstart, allminus, 
            (A.MacroDecl (sa,lpa,eas,rpa,enda)) +> A.rewrap decla), 
            (B.MacroDecl ((sb,ebs),
                         [iisb;lpb;rpb;iiendb;iifakestart] ++ iistob))
          ))))))))

  | _, (B.MacroDecl _ |B.DeclList _) -> fail



and onedecl = fun allminus decla (declb, iiptvirgb, iistob) -> 
 X.all_bound (A.get_inherited decla) >&&>
 match A.unwrap decla, declb with

 (* kind of typedef iso, we must unfold, it's for the case 
  * T { }; that we want to match against typedef struct { } xx_t;
  *)
 | A.TyDecl (tya0, ptvirga), 
   ({B.v_namei = Some ((idb, None),[iidb]);
     B.v_type = typb0;
     B.v_storage = (B.StoTypedef, inl);
     B.v_local = local; 
     B.v_attr = attrs;
   }, iivirg) ->

   (match A.unwrap tya0, typb0 with
   | A.Type(cv1,tya1), ((qu,il),typb1) ->

     (match A.unwrap tya1, typb1 with
     | A.StructUnionDef(tya2, lba, declsa, rba), 
      (B.StructUnion (sub, sbopt, declsb), ii) -> 

       let (iisub, iisbopt, lbb, rbb) = 
         match sbopt with
         | None -> 
             let (iisub, lbb, rbb) = tuple_of_list3 ii in
             (iisub, [], lbb, rbb)
         | Some s -> 
             pr2 (sprintf 
              "warning: both a typedef (%s) and struct name introduction (%s)"
              idb s
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
         Ast_c.nQ,((B.TypeName (idb, Some 
           (Lib_parsing_c.al_type structnameb))), [iidb]) 
       in

       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb -> 
       tokenf lba lbb >>= (fun lba lbb -> 
       tokenf rba rbb >>= (fun rba rbb -> 
       struct_fields (A.undots declsa) declsb >>=(fun undeclsa declsb ->
         let declsa = redots declsa undeclsa in

         (match A.unwrap tya2 with
         | A.Type(cv3, tya3) -> 
           (match A.unwrap tya3 with
           | A.MetaType(ida,keep, inherited) -> 

               fullType tya2 fake_typeb >>= (fun tya2 fake_typeb -> 
		 let tya1 =
		   A.StructUnionDef(tya2,lba,declsa,rba)+> A.rewrap tya1 in
		 let tya0 = A.Type(cv1, tya1) +> A.rewrap tya0 in
               
                   
		 let typb1 = B.StructUnion (sub,sbopt, declsb),
                   [iisub] @ iisbopt @ [lbb;rbb] in
		 let typb0 = ((qu, il), typb1) in
               
		 match fake_typeb with 
		 | _nQ, ((B.TypeName (idb,_typ)), [iidb]) -> 

                     return (
                     (A.TyDecl (tya0, ptvirga)) +> A.rewrap decla,
                     (({B.v_namei = Some ((idb, None),[iidb]);
                        B.v_type = typb0;
                        B.v_storage = (B.StoTypedef, inl);
                        B.v_local = local;
                        B.v_attr = attrs;
                     },
                       iivirg),iiptvirgb,iistob)
                     )
		 | _ -> raise Impossible    
             )

           | A.StructUnionName(sua, sa) -> 

             fullType tya2 structnameb >>= (fun tya2 structnameb -> 

               let tya1 = A.StructUnionDef(tya2,lba,declsa,rba)+> A.rewrap tya1
               in
               let tya0 = A.Type(cv1, tya1) +> A.rewrap tya0 in

               match structnameb with 
               | _nQ, (B.StructUnionName (sub, s), [iisub;iisbopt]) ->

                   let typb1 = B.StructUnion (sub,sbopt, declsb),
                     [iisub;iisbopt;lbb;rbb] in
                   let typb0 = ((qu, il), typb1) in
               
                   return (
                     (A.TyDecl (tya0, ptvirga)) +> A.rewrap decla,
                     (({B.v_namei = Some ((idb, None),[iidb]);
                        B.v_type = typb0;
                        B.v_storage = (B.StoTypedef, inl);
                        B.v_local = local;
                        B.v_attr = attrs;
                     },
                      iivirg),iiptvirgb,iistob)
                   )
               | _ -> raise Impossible    
             )
           | _ -> raise Impossible
           )
         | _ -> fail
       )))))
     | _ -> fail
     )
   | _ -> fail
   )
         
   | A.UnInit (stoa, typa, ida, ptvirga), 
     ({B.v_namei = Some ((idb, _),[iidb]);
       B.v_storage = (B.StoTypedef,_);
     }, iivirg) -> 
       fail

   | A.Init (stoa, typa, ida, eqa, inia, ptvirga), 
     ({B.v_namei = Some ((idb, _),[iidb]);
       B.v_storage = (B.StoTypedef,_);
     }, iivirg) -> 
       fail



    (* could handle iso here but handled in standard.iso *)
   | A.UnInit (stoa, typa, ida, ptvirga), 
     ({B.v_namei = Some ((idb, None),[iidb]);
       B.v_type = typb;
       B.v_storage = stob;
       B.v_local = local;
       B.v_attr = attrs;
     }, iivirg) -> 

       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb -> 
       fullType typa typb >>= (fun typa typb -> 
       ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
       storage_optional_allminus allminus stoa (stob, iistob) >>= 
        (fun stoa (stob, iistob) -> 
         return (
           (A.UnInit (stoa, typa, ida, ptvirga)) +>  A.rewrap decla,
           (({B.v_namei = Some ((idb,None),[iidb]);
              B.v_type = typb;
              B.v_storage = stob;
              B.v_local = local;
              B.v_attr = attrs;
           },iivirg),
	    iiptvirgb,iistob)
         )))))

   | A.Init (stoa, typa, ida, eqa, inia, ptvirga), 
     ({B.v_namei = Some((idb,Some inib),[iidb;iieqb]);
       B.v_type = typb;
       B.v_storage = stob;
       B.v_local = local;
       B.v_attr = attrs;
     },iivirg)
       ->
       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb -> 
       tokenf eqa iieqb >>= (fun eqa iieqb -> 
       fullType typa typb >>= (fun typa typb -> 
       ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
       storage_optional_allminus allminus stoa (stob, iistob) >>= 
       (fun stoa (stob, iistob) -> 
       initialiser inia inib >>= (fun inia inib -> 
         return (
           (A.Init (stoa, typa, ida, eqa, inia, ptvirga)) +> A.rewrap decla,
           (({B.v_namei = Some((idb,Some inib),[iidb;iieqb]);
              B.v_type = typb;
              B.v_storage = stob;
              B.v_local = local;
              B.v_attr = attrs;
           },iivirg),
           iiptvirgb,iistob)
         )))))))
           
   (* do iso-by-absence here ? allow typedecl and var ? *)
   | A.TyDecl (typa, ptvirga), 
     ({B.v_namei = None; B.v_type = typb; 
       B.v_storage = stob; 
       B.v_local = local;
       B.v_attr = attrs;
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
             }, iivirg), iiptvirgb, iistob)
           )))
       else fail


   | A.Typedef (stoa, typa, ida, ptvirga), 
     ({B.v_namei = Some ((idb, None),[iidb]);
       B.v_type = typb;
       B.v_storage = (B.StoTypedef,inline);
       B.v_local = local;
       B.v_attr = attrs;
     },iivirg) ->

       tokenf ptvirga iiptvirgb >>= (fun ptvirga iiptvirgb -> 
       fullType typa typb >>= (fun typa typb -> 
       (match iistob with
       | [iitypedef] -> 
           tokenf stoa iitypedef >>= (fun stoa iitypedef -> 
             return (stoa, [iitypedef])
           )
       | _ -> failwith "wierd, have both typedef and inline or nothing";
       ) >>= (fun stoa iistob -> 
       (match A.unwrap ida with
       | A.MetaType(_,_,_) -> 

           let fake_typeb = 
             Ast_c.nQ, ((B.TypeName (idb, Ast_c.noTypedefDef())), [iidb]) 
           in
           fullTypebis ida fake_typeb >>= (fun ida fake_typeb -> 
             match fake_typeb with
             | _nQ, ((B.TypeName (idb,_typ)), [iidb]) -> 
                 return (ida, (idb, iidb))
             | _ -> raise Impossible
           )

       | A.TypeName sa -> 
           if (term sa) =$= idb
           then 
             tokenf sa iidb >>= (fun sa iidb -> 
               return (
                 (A.TypeName sa) +> A.rewrap ida,
                 (idb, iidb)
               ))
             else fail
       | _ -> raise Impossible

       ) >>= (fun ida (idb, iidb) ->
         return (
           (A.Typedef (stoa, typa, ida, ptvirga)) +> A.rewrap decla,
           (({B.v_namei = Some ((idb, None),[iidb]);
              B.v_type = typb;
              B.v_storage = (B.StoTypedef,inline);
              B.v_local = local;
              B.v_attr = attrs;
           },
	     iivirg),
            iiptvirgb, iistob)
         )
       ))))
             
       
   | _, ({B.v_namei = None;}, _) -> 
       (* old:   failwith "no variable in this declaration, wierd" *)
       fail



  | A.DisjDecl declas, declb -> 
      declas +> List.fold_left (fun acc decla -> 
        acc >|+|> 
            (* (declaration (mckstart, allminus, decla) declb) *)
            (onedecl allminus decla (declb,iiptvirgb, iistob))
      ) fail


     
   (* only in struct type decls *)
   | A.Ddots(dots,whencode), _ ->
       raise Impossible
            
   | A.OptDecl _,    _ | A.UniqueDecl _,     _ -> 
       failwith "not handling Opt/Unique Decl"

   | _, ({B.v_namei=Some _}, _)
       -> fail




(* ------------------------------------------------------------------------- *)

and (initialiser: (A.initialiser, Ast_c.initialiser) matcher) =  fun ia ib -> 
    X.all_bound (A.get_inherited ia) >&&>
    match (A.unwrap ia,ib) with

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
            assert (null ii);
            expression expa expb >>= (fun expa expb -> 
              return (
                (A.InitExpr expa) +> A.rewrap ia,
                (B.InitExpr expb, ii)
              ))
        | _ -> fail
        )

    | (A.InitList (ia1, ias, ia2, []), (B.InitList ibs, ii)) -> 
        (match ii with 
        | ib1::ib2::iicommaopt -> 
            tokenf ia1 ib1 >>= (fun ia1 ib1 ->
            tokenf ia2 ib2 >>= (fun ia2 ib2 ->
            initialisers ias (ibs, iicommaopt) >>= (fun ias (ibs,iicommaopt) ->
              return (
                (A.InitList (ia1, ias, ia2, [])) +> A.rewrap ia,
                (B.InitList ibs, ib1::ib2::iicommaopt)
              ))))
              
        | _ -> raise Impossible
        )

    | (A.InitList (i1, ias, i2, whencode),(B.InitList ibs, _ii)) ->
        failwith "TODO: not handling whencode in initialisers"


    | (A.InitGccDotName (ia1, ida, ia2, inia), 
      (B.InitDesignators ([B.DesignatorField idb,ii1], inib), ii2))->

        let (iidot, iidb) = tuple_of_list2 ii1 in
        let iieq = tuple_of_list1 ii2 in

        tokenf ia1 iidot >>= (fun ia1 iidot -> 
        tokenf ia2 iieq >>= (fun ia2 iieq -> 
        ident DontKnow ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
        initialiser inia inib >>= (fun inia inib -> 
          return (
            (A.InitGccDotName (ia1, ida, ia2, inia)) +> A.rewrap ia,
            (B.InitDesignators 
                ([B.DesignatorField idb, [iidot;iidb]], inib), [iieq])
          )))))


    | (A.InitGccIndex (ia1,ea,ia2,ia3,inia), 
      (B.InitDesignators ([B.DesignatorIndex eb, ii1], inib), ii2)) -> 
        
        let (ib1, ib2) = tuple_of_list2 ii1 in
        let ib3 = tuple_of_list1 ii2 in
        tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
        expression ea eb >>= (fun ea eb -> 
        initialiser inia inib >>= (fun inia inib -> 
          return (
            (A.InitGccIndex (ia1,ea,ia2,ia3,inia)) +> A.rewrap ia,
            (B.InitDesignators 
                ([B.DesignatorIndex eb, [ib1;ib2]], inib), [ib3])
          ))))))


    | (A.InitGccRange (ia1,e1a,ia2,e2a,ia3,ia4,inia), 
      (B.InitDesignators ([B.DesignatorRange (e1b, e2b), ii1], inib), ii2)) -> 

        let (ib1, ib2, ib3) = tuple_of_list3 ii1 in
        let (ib4) = tuple_of_list1 ii2 in
        tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        tokenf ia3 ib3 >>= (fun ia3 ib3 -> 
        tokenf ia4 ib4 >>= (fun ia4 ib4 -> 
        expression e1a e1b >>= (fun e1a e1b -> 
        expression e2a e2b >>= (fun e2a e2b -> 
        initialiser inia inib >>= (fun inia inib -> 
          return (
            (A.InitGccRange (ia1,e1a,ia2,e2a,ia3,ia4,inia)) +> A.rewrap ia,
            (B.InitDesignators 
                ([B.DesignatorRange (e1b, e2b),[ib1;ib2;ib3]], inib), [ib4])
          ))))))))




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
        raise Impossible

    | A.UniqueIni _,_ | A.OptIni _,_ -> 
      failwith "not handling Opt/Unique on initialisers"

    | _, (B.InitIndexOld (_, _), _) -> fail 
    | _, (B.InitFieldOld (_, _), _) -> fail 

    | _, ((B.InitDesignators (_, _)|B.InitList _|B.InitExpr _), _)
        -> fail






and initialisers = fun ias (ibs, iicomma) ->
  let ias_unsplit = unsplit_icomma      ias in
  let ibs_split   = resplit_initialiser ibs iicomma in

  let f = 
    if need_unordered_initialisers ibs 
    then initialisers_unordered2
    else initialisers_ordered2
  in
  f ias_unsplit ibs_split >>= 
      (fun ias_unsplit ibs_split -> 
        return (
          split_icomma ias_unsplit,
          unsplit_initialiser ibs_split
        )
      )

(* todo: one day julia will reput a IDots *)
and initialisers_ordered2 = fun ias ibs -> 
  match ias, ibs with
  | [], [] -> return ([], [])
  | (x, xcomma)::xs, (y, commay)::ys -> 
      (match A.unwrap xcomma with
      | A.IComma commax -> 
          tokenf commax commay >>= (fun commax commay -> 
          initialiser x y >>= (fun x y -> 
          initialisers_ordered2 xs ys >>= (fun xs ys -> 
            return (
                    (x, (A.IComma commax) +> A.rewrap xcomma)::xs, 
                    (y, commay)::ys
            )
          )))
      | _ -> raise Impossible (* unsplit_iicomma wrong *)
      )
  | _ -> fail

          

and initialisers_unordered2 = fun ias ibs -> 

  match ias, ibs with
  | [], ys -> return ([], ys)
  | (x,xcomma)::xs, ys -> 

      let permut = Common.uncons_permut_lazy ys in
      permut +> List.fold_left (fun acc ((e, pos), rest) -> 
        acc >||> 
          (
            (match A.unwrap xcomma, e with 
            | A.IComma commax, (y, commay) -> 
                tokenf commax commay >>= (fun commax commay -> 
                initialiser x y >>= (fun x y -> 
                  return (
                    (x, (A.IComma commax) +> A.rewrap xcomma), 
                    (y, commay))
                )
                )
            | _ -> raise Impossible (* unsplit_iicomma wrong *)
            ) 
            >>= (fun x e -> 
              let rest = Lazy.force rest in
              initialisers_unordered2 xs rest >>= (fun xs rest -> 
                return (
                  x::xs,
                  Common.insert_elem_pos (e, pos) rest
                ))))
      ) fail
       

(* ------------------------------------------------------------------------- *)
and (struct_fields: (A.declaration list, B.field list) matcher) =
 fun eas ebs -> 
  match eas, ebs with
  | [], [] -> return ([], [])
  | [], eb::ebs -> fail
  | ea::eas, ebs -> 
      X.all_bound (A.get_inherited ea) >&&>
      (match A.unwrap ea, ebs with
      | A.Ddots (mcode, optwhen), ys -> 
          if optwhen <> None then failwith "not handling when in argument";

          (* '...' can take more or less the beginnings of the arguments *)
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (
              
              (if startxs = []
              then
                if mcode_contain_plus (mcodekind mcode)
                then fail 
                  (* failwith "I have no token that I could accroche myself on" *)
                else return (dots2metavar mcode, [])
              else 

                  X.distrf_struct_fields (dots2metavar mcode) startxs
              ) >>= (fun mcode startxs ->
		let mcode = metavar2dots mcode in
                struct_fields eas endxs >>= (fun eas endxs -> 
                  return (
                    (A.Ddots (mcode, optwhen) +> A.rewrap ea) ::eas,
                    startxs ++ endxs
                  )))
            )
          ) fail 
      | _unwrapx, eb::ebs -> 
          struct_field ea eb >>= (fun ea eb -> 
          struct_fields eas ebs >>= (fun eas ebs -> 
            return (ea::eas, eb::ebs)
          ))

      | _unwrapx, [] -> fail
      )

and (struct_field: (A.declaration, B.field) matcher) = fun fa fb -> 
  let (xfield, iifield) = fb in

  match xfield with 
  | B.DeclarationField (B.FieldDeclList (onefield_multivars,iiptvirg)) -> 

    let iiptvirgb = tuple_of_list1 iiptvirg in

    (match onefield_multivars with
    | [] -> raise Impossible
    | [onevar,iivirg] -> 
      assert (null iivirg);
      (match onevar with
      | B.BitField (sopt, typb, expr), ii -> 
          pr2_once "warning: bitfield not handled by ast_cocci";
          fail
      | B.Simple (None, typb), ii -> 
          pr2_once "warning: unamed struct field not handled by ast_cocci";
          fail
      | B.Simple (Some idb, typb), ii -> 
          let (iidb) = tuple_of_list1 ii in

          (* build a declaration from a struct field *)
          let allminus = false in
          let iisto = [] in
          let stob = B.NoSto, false in
          let fake_var = 
            ({B.v_namei = Some ((idb, None),[iidb]);
              B.v_type = typb;
              B.v_storage = stob;
              B.v_local = Ast_c.NotLocalDecl;
              B.v_attr = Ast_c.noattr;
            },
	     iivirg)            
          in
          onedecl allminus fa (fake_var,iiptvirgb,iisto) >>= 
            (fun fa (var,iiptvirgb,iisto) -> 

              match fake_var with
              | ({B.v_namei = Some ((idb, None),[iidb]);
                  B.v_type = typb;
                  B.v_storage = stob;
                }, iivirg) -> 
                  let onevar = B.Simple (Some idb, typb), [iidb] in
                  
                  return (
                    (fa),
                    ((B.DeclarationField 
                        (B.FieldDeclList ([onevar, iivirg], [iiptvirgb]))),
                    iifield)
                  )
              | _ -> raise Impossible
            )
      )

    | x::y::xs -> 
      pr2_once "PB: More that one variable in decl. Have to split";
      fail
    )
  | B.EmptyField -> 
      let _iiptvirgb = tuple_of_list1 iifield in
      fail

  | B.MacroStructDeclTodo -> fail
  | B.CppDirectiveStruct directive -> fail
  | B.IfdefStruct directive -> fail



(* ------------------------------------------------------------------------- *)
and (fullType: (A.fullType, Ast_c.fullType) matcher) = 
 fun typa typb -> 
   X.optional_qualifier_flag (fun optional_qualifier -> 
   X.all_bound (A.get_inherited typa) >&&>
   match A.unwrap typa, typb with
   | A.Type(cv,ty1), ((qu,il),ty2) ->

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
             fullTypebis ty1 ((qu,il), ty2) >>= (fun ty1 fullty2 -> 
               return (
                 (A.Type(None, ty1)) +> A.rewrap typa,
                 fullty2
               ))
           in
           (match optional_qualifier, qu.B.const || qu.B.volatile with
           | false, false -> do_stuff ()
           | false, true -> fail
           | true, false -> do_stuff ()
           | true, true -> 
               if !Flag.show_misc 
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
                   (A.Type(Some x, ty1)) +> A.rewrap typa,
                   ((qu, [i1]), ty2)
                 )))
               
           | A.Volatile, [i1] when qu.B.volatile -> 
               tokenf x i1 >>= (fun x i1 -> 
               fullTypebis ty1 (Ast_c.nQ,ty2) >>= (fun ty1 (_, ty2) -> 
                 return (
                   (A.Type(Some x, ty1)) +> A.rewrap typa,
                   ((qu, [i1]), ty2)
                 )))
               
           | _ -> fail
           )
       )

  | A.DisjType typas, typb -> 
      typas +>
      List.fold_left (fun acc typa -> acc >|+|> (fullType typa typb)) fail

   | A.OptType(_), _  | A.UniqueType(_), _
       -> failwith "not handling Opt/Unique on type"
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
      let max_min _ =
	Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_type typb) in
      X.envf keep inherited (ida, B.MetaTypeVal typb, max_min) (fun () -> 
        X.distrf_type ida typb >>= (fun ida typb -> 
          return (
            A.MetaType(ida,keep, inherited) +> A.rewrap ta,
            typb
          ))
      )
  | unwrap, (qub, typb) -> 
      typeC ta typb >>= (fun ta typb -> 
        return (ta, (qub, typb))
      )


and (typeC: (A.typeC, Ast_c.typeC) matcher) = 
  fun ta tb -> 
  match A.unwrap ta, tb with
  | A.BaseType (basea, signaopt), (B.BaseType baseb, ii) -> 
      (* In ii there is a list, sometimes of length 1 or 2 or 3.
       * And even if in baseb we have a Signed Int, that does not mean
       * that ii is of length 2, cos Signed is the default, so if in signa
       * we have Signed explicitely ? we cant "accrocher" this mcode to 
       * something :( So for the moment when there is signed in cocci,
       * we force that there is a signed in c too (done in pattern.ml).
       *)
      let signbopt, iibaseb = split_signb_baseb_ii (baseb, ii) in

        
      (* handle some iso on type ? (cf complex C rule for possible implicit
	 casting) *)
      (match term basea, baseb with
      | A.VoidType,  B.Void 
      | A.FloatType, B.FloatType (B.CFloat)
      | A.DoubleType, B.FloatType (B.CDouble) -> 
           assert (signaopt = None); 
           let (ibaseb) = tuple_of_list1 ii in 
           tokenf basea ibaseb >>= (fun basea ibaseb -> 
             return (
               (A.BaseType (basea, signaopt)) +> A.rewrap ta,
               (B.BaseType baseb, [ibaseb])
             ))
            
      | A.CharType,  B.IntType B.CChar when signaopt = None -> 
          let ibaseb = tuple_of_list1 ii in
           tokenf basea ibaseb >>= (fun basea ibaseb -> 
             return (
               (A.BaseType (basea, signaopt)) +> A.rewrap ta,
               (B.BaseType (B.IntType B.CChar), [ibaseb])
             ))
            
      | A.CharType,B.IntType (B.Si (_sign, B.CChar2)) when signaopt <> None -> 
          let ibaseb = tuple_of_list1 iibaseb in
          sign signaopt signbopt >>= (fun signaopt iisignbopt -> 
          tokenf basea ibaseb >>= (fun basea ibaseb -> 
            return (
               (A.BaseType (basea, signaopt)) +> A.rewrap ta,
               (B.BaseType (baseb), iisignbopt ++ [ibaseb])
               )))
          
      | A.ShortType, B.IntType (B.Si (_, B.CShort)) 
      | A.IntType,   B.IntType (B.Si (_, B.CInt))   
      | A.LongType,  B.IntType (B.Si (_, B.CLong))  ->
          (match iibaseb with 
          | [] -> 
              (* iso-by-presence ? *)
              (* when unsigned int in SP,  allow have just unsigned in C ? *)
              if mcode_contain_plus (mcodekind basea)
              then fail
              else 
                
                sign signaopt signbopt >>= (fun signaopt iisignbopt -> 
                    return (
                      (A.BaseType (basea, signaopt)) +> A.rewrap ta,
                      (B.BaseType (baseb), iisignbopt ++ [])
                    ))
              

          | [x;y] -> 
              pr2_once 
                "warning: long int or short int not handled by ast_cocci";
              fail

          | [ibaseb] -> 
          sign signaopt signbopt >>= (fun signaopt iisignbopt -> 
          tokenf basea ibaseb >>= (fun basea ibaseb -> 
            return (
               (A.BaseType (basea, signaopt)) +> A.rewrap ta,
               (B.BaseType (baseb), iisignbopt ++ [ibaseb])
               )))
          | _ -> raise Impossible

          )

            
      | _, B.IntType (B.Si (_, B.CLongLong)) 
      | _, B.FloatType B.CLongDouble 
          -> 
          pr2_once 
            "warning: long long or long double not handled by ast_cocci";
          fail

      | _, (B.Void|B.FloatType _|B.IntType _) -> fail

          
      )

    | A.ImplicitInt (signa),   (B.BaseType baseb, ii) -> 
        let signbopt, iibaseb = split_signb_baseb_ii (baseb, ii) in
        (match iibaseb, baseb with
        | [], B.IntType (B.Si (_sign, B.CInt)) -> 
            sign (Some signa) signbopt >>= (fun signaopt iisignbopt -> 
              match signaopt with
              | None -> raise Impossible
              | Some signa -> 
                  return (
                    (A.ImplicitInt (signa)) +> A.rewrap ta,
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

    | A.FunctionType(allminus,tyaopt,lpa,paramsa,rpa), 
      (B.FunctionType(tyb, (paramsb, (isvaargs, iidotsb))), ii) -> 

        let (lpb, rpb) = tuple_of_list2 ii in
        if isvaargs 
        then
          pr2_once
	    ("Not handling well variable length arguments func. "^
             "You have been warned");
        tokenf lpa lpb >>= (fun lpa lpb -> 
        tokenf rpa rpb >>= (fun rpa rpb -> 
        fullType_optional_allminus allminus tyaopt tyb >>= (fun tyaopt tyb -> 
        parameters (seqstyle paramsa) (A.undots paramsa) paramsb >>=
          (fun paramsaundots paramsb -> 
            let paramsa = redots paramsa paramsaundots in
            return (
              (A.FunctionType(allminus,tyaopt,lpa,paramsa,rpa) +> A.rewrap ta,
              (B.FunctionType(tyb, (paramsb, (isvaargs, iidotsb))), [lpb;rpb])
              )
            )))))
            
        

          

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
                parameters (seqstyle paramsa) (A.undots paramsa) paramsb >>=
                (fun paramsaundots paramsb -> 
                  let paramsa = redots paramsa paramsaundots in

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
        
        

    (* todo: handle the iso on optionnal size specifification ? *)
    | A.Array (typa, ia1, eaopt, ia2), (B.Array (ebopt, typb), ii) -> 
        let (ib1, ib2) = tuple_of_list2 ii in
        fullType typa typb >>= (fun typa typb -> 
        option expression eaopt ebopt >>= (fun eaopt ebopt -> 
        tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
          return (
            (A.Array (typa, ia1, eaopt, ia2)) +> A.rewrap ta,
            (B.Array (ebopt, typb), [ib1;ib2])
          )))))


     (* todo: could also match a Struct that has provided a name *)
     (* This is for the case where the SmPL code contains "struct x", without
	a definition.  In this case, the name field is always present.
        This case is also called from the case for A.StructUnionDef when
        a name is present in the C code. *)
    | A.StructUnionName(sua, Some sa), (B.StructUnionName (sub, sb), ii) -> 
        (* sa is now an ident, not an mcode, old: ... && (term sa) =$= sb *)
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
	 | _ -> failwith "list of length 3 or 4 expected" in

       let process_type =
         match (sbopt,ii_sub_sb) with
           (None,Common.Left iisub) ->
	     (* the following doesn't reconstruct the complete SP code, just
		the part that matched *)
	     let rec loop s =
	       match A.unwrap s with
		 A.Type(None,ty) ->
		   (match A.unwrap ty with
		     A.StructUnionName(sua, None) ->
		       tokenf sua iisub >>= (fun sua iisub ->
			 let ty =
			   A.Type(None,
				  A.StructUnionName(sua, None) +> A.rewrap ty)
			     +> A.rewrap s in
			 return (ty,[iisub]))
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
               | _ -> raise Impossible)
	 | _ -> fail in

       process_type
	 >>= (fun ty ii_sub_sb -> 

            tokenf lba lbb >>= (fun lba lbb -> 
            tokenf rba rbb >>= (fun rba rbb -> 
            struct_fields (A.undots declsa) declsb >>=(fun undeclsa declsb ->
              let declsa = redots declsa undeclsa in

              return (
                (A.StructUnionDef(ty, lba, declsa, rba)) +> A.rewrap ta,
                (B.StructUnion (sub, sbopt, declsb),ii_sub_sb@[lbb;rbb])
              )))))


   (* todo? handle isomorphisms ? because Unsigned Int can be match on a 
    * uint in the C code. But some CEs consists in renaming some types,
    * so we don't want apply isomorphisms every time. 
    *) 
    | A.TypeName sa,  (B.TypeName (sb,typb), ii) ->
        let (isb) = tuple_of_list1 ii in
        if (term sa) =$= sb
        then 
          tokenf sa isb >>= (fun sa isb -> 
          return (
            (A.TypeName sa) +> A.rewrap ta,
            (B.TypeName (sb,typb), [isb])
          ))
        else fail

    | _, (B.TypeOfExpr e, ii) -> fail
    | _, (B.TypeOfType e, ii) -> fail

    | _, (B.ParenType e, ii) -> fail (* todo ?*)
    | _, (B.EnumName _, _) -> fail (* todo cocci ?*)
    | _, (B.Enum _, _) -> fail (* todo cocci ?*)

    | _,
     ((B.TypeName (_, _)|B.StructUnionName (_, _)|
      B.StructUnion (_, _, _)|
      B.FunctionType _|B.Array (_, _)|B.Pointer _|
      B.BaseType _),
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
          if !Flag.show_misc 
          then pr2_once "USING optional_storage builtin isomorphism";
          do_minus()
      )

  | Some x, ((stobis, inline)) -> 
      if equal_storage (term x) stobis
      then 
        match iistob with
        | [i1] ->
           tokenf x i1 >>= (fun x i1 -> 
             return (Some x,  ((stobis, inline), [i1]))
           )
       (* or if have inline ? have to do a split_storage_inline a la 
        * split_signb_baseb_ii *)
        | _ -> raise Impossible 
      else fail
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



(*---------------------------------------------------------------------------*)
and compatible_type a (b,_local) = 
  let ok  = return ((),()) in

  let rec loop = function
    | Type_cocci.BaseType (a, signa), (qua, (B.BaseType b,ii)) -> 
	(match a, b with
	| Type_cocci.VoidType, B.Void -> 
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
	| _, B.IntType (B.Si (signb, B.CLongLong)) -> 
            pr2_once "no longlong in cocci";
            fail
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


        | _, (B.Void|B.FloatType _|B.IntType _) -> fail
      )



    | Type_cocci.Pointer  a, (qub, (B.Pointer b, ii)) -> 
	loop (a,b)
    | Type_cocci.FunctionPointer a, _ ->
	failwith
	  "TODO: function pointer type doesn't store enough information to determine compatability"
    | Type_cocci.Array   a, (qub, (B.Array (eopt, b),ii)) ->
      (* no size info for cocci *)
	loop (a,b)
    | Type_cocci.StructUnionName (sua, _, sa),
	(qub, (B.StructUnionName (sub, sb),ii)) -> 
	  if equal_structUnion_type_cocci sua sub && sa = sb
	  then ok
	  else fail

    | Type_cocci.TypeName sa, (qub, (B.TypeName (sb,_typb), ii)) -> 
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
    | a, (qub, (B.TypeName (sb,Some b), ii)) -> 
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
       B.Array (_, _)|B.Pointer _|B.TypeName _|
       B.BaseType _
      ),
      _))) -> fail


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
  match eas, ebs with
  | [], [] -> return ([], [])
  | [], eb::ebs -> fail
  | ea::eas, ebs -> 
      X.all_bound (A.get_inherited ea) >&&>
      (match A.unwrap ea, ebs with
      | A.DPdots (mcode), ys -> 

          (* '...' can take more or less the beginnings of the arguments *)
          let startendxs = Common.zip (Common.inits ys) (Common.tails ys) in
          startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
            acc >||> (

              (if startxs = []
              then
                if mcode_contain_plus (mcodekind mcode)
                then fail 
                  (* failwith "I have no token that I could accroche myself on" *)
                else return (dots2metavar mcode, [])
              else 
                (match Common.last startxs with
                | Right _ -> fail
                | Left _ -> 
                    X.distrf_define_params (dots2metavar mcode) startxs
                )
              ) >>= (fun mcode startxs ->
		let mcode = metavar2dots mcode in
                define_paramsbis eas endxs >>= (fun eas endxs -> 
                  return (
                    (A.DPdots (mcode) +> A.rewrap ea) ::eas,
                    startxs ++ endxs
                  )))
              )
            ) fail 

      | A.DPComma ia1, Right ii::ebs -> 
          let ib1 = tuple_of_list1 ii in
          tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
          define_paramsbis eas ebs >>= (fun eas ebs -> 
            return (
              (A.DPComma ia1 +> A.rewrap ea)::eas,
              (Right [ib1])::ebs
            )
          ))

      | A.DPComma ia1, ebs -> 
          if mcode_contain_plus (mcodekind ia1)
          then fail
          else 
            (define_paramsbis eas ebs) (* try optional comma trick *)

      | (A.OptDParam _ | A.UniqueDParam _), _ -> 
              failwith "handling Opt/Unique for define parameters"

      | A.DPcircles (_), ys -> raise Impossible (* in Ordered mode *)

      | A.DParam ida, (Left (idb, ii))::ebs -> 
          let ib1 = tuple_of_list1 ii in
          ident DontKnow ida (idb, ib1) >>= (fun ida (idb, ib1) -> 
          define_paramsbis eas ebs >>= (fun eas ebs -> 
            return (
              (A.DParam ida)+> A.rewrap ea :: eas,
              (Left (idb, [ib1]))::ebs
            )))
          
      | _unwrapx, (Right y)::ys -> raise Impossible
      | _unwrapx, [] -> fail
      )
  


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* no global solution for positions here, because for a statement metavariable
we want a MetaStmtVal, and for the others, it's not clear what we want *)

let rec (rule_elem_node: (A.rule_elem, Control_flow_c.node) matcher) = 
 fun re node -> 
  let rewrap x = 
    x >>= (fun a b -> return (A.rewrap re a, F.rewrap node b))
  in
  X.all_bound (A.get_inherited re) >&&>

  rewrap (
  match A.unwrap re, F.unwrap node with

  (* note: the order of the clauses is important. *)

  | _, F.Enter | _, F.Exit | _, F.ErrorExit -> fail2()

  (* the metaRuleElem contains just '-' information. We dont need to add
   * stuff in the environment. If we need stuff in environment, because
   * there is a + S somewhere, then this will be done via MetaStmt, not
   * via MetaRuleElem. 
   * Can match TrueNode/FalseNode/... so must be placed before those cases.
   *)

  | A.MetaRuleElem(mcode,keep,inherited), unwrap_node -> 
      let default = A.MetaRuleElem(mcode,keep,inherited), unwrap_node in
      (match unwrap_node with
      | F.CaseNode _
      | F.TrueNode | F.FalseNode | F.AfterNode | F.FallThroughNode 
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


  (* rene cant have found that a state containing a fake/exit/... should be 
   * transformed 
   * TODO: and F.Fake ?
   *)
  | _, F.EndStatement _ | _, F.CaseNode _
  | _, F.TrueNode | _, F.FalseNode | _, F.AfterNode | _, F.FallThroughNode
  | _, F.InLoopNode
    -> fail2()

  (* really ? diff between pattern.ml and transformation.ml *)
  | _, F.Fake -> fail2()


  (* cas general: a Meta can match everything. It matches only
   * "header"-statement. We transform only MetaRuleElem, not MetaStmt.
   * So can't have been called in transform. 
   *)
  | A.MetaStmt (ida,keep,metainfoMaybeTodo,inherited),  F.Decl(_) -> fail

  | A.MetaStmt (ida,keep,metainfoMaybeTodo,inherited),  unwrap_node -> 
      (* todo: should not happen in transform mode *)

      (match Control_flow_c.extract_fullstatement node with
      | Some stb -> 
	    let max_min _ =
	      Lib_parsing_c.lin_col_by_pos (Lib_parsing_c.ii_of_stmt stb) in
            X.envf keep inherited (ida, Ast_c.MetaStmtVal stb, max_min)
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

  | A.TopInit init, nodeb -> 
      X.cocciInit initialiser init node >>= (fun init node -> 
        return (
          A.TopInit init,
          F.unwrap node
        )
      )


  | A.FunHeader (mckstart, allminus, fninfoa, ida, oparen, paramsa, cparen),
    F.FunHeader ({B.f_name = idb;
                  f_type = (retb, (paramsb, (isvaargs, iidotsb)));
                  f_storage = stob;
                  f_attr = attrs;
                  f_body = body;
                  }, ii) -> 
      assert (null body);

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

      (match List.filter (function A.FInline(i) -> true | _ -> false) fninfoa
      with [A.FInline(i)] -> failwith "not checking inline" | _ -> ());

      (match List.filter (function A.FAttr(a) -> true | _ -> false) fninfoa
      with [A.FAttr(a)] -> failwith "not checking attributes" | _ -> ());

      (match ii with
      | iidb::ioparenb::icparenb::iifakestart::iistob -> 

          (* maybe important to put ident as the first tokens to transform.
           * It's related to transform_proto. So don't change order
           * between the >>=.
           *)
          ident LocalFunction ida (idb, iidb) >>= (fun ida (idb, iidb) -> 
          X.tokenf_mck mckstart iifakestart >>= (fun mckstart iifakestart -> 
          tokenf oparen ioparenb >>= (fun oparen ioparenb ->
          tokenf cparen icparenb >>= (fun cparen icparenb ->
          parameters (seqstyle paramsa) 
            (A.undots paramsa) paramsb >>=
            (fun paramsaundots paramsb -> 
              let paramsa = redots paramsa paramsaundots in
          storage_optional_allminus allminus 
            stoa (stob, iistob) >>= (fun stoa (stob, iistob) -> 
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

             let fninfoa = 
               (match stoa with Some st -> [A.FStorage st] | None -> []) ++
               (match tya  with Some t -> [A.FType t] | None -> [])

             in

             return (
               A.FunHeader(mckstart,allminus,fninfoa,ida,oparen,
                          paramsa,cparen),
               F.FunHeader ({B.f_name = idb;
                             f_type = (retb, (paramsb, (isvaargs, iidotsb)));
                             f_storage = stob;
                             f_attr = attrs;
                             f_body = body;
                           },
                           iidb::ioparenb::icparenb::iifakestart::iistob)
                )
              ))))))))
      | _ -> raise Impossible
      )






  | A.Decl (mckstart,allminus,decla), F.Decl declb -> 
      declaration (mckstart,allminus,decla) declb >>= 
       (fun (mckstart,allminus,decla) declb -> 
        return (
          A.Decl (mckstart,allminus,decla),
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

  | A.ExprStatement (ea, ia1), F.ExprStatement (st, (Some eb, ii)) -> 
      let ib1 = tuple_of_list1 ii in 
      expression ea eb >>= (fun ea eb -> 
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
        return (
          A.ExprStatement (ea, ia1),
          F.ExprStatement (st, (Some eb, [ib1]))
        )
      ))


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
      arguments (seqstyle eas) (A.undots eas) ebs >>= (fun easundots ebs -> 
       let eas = redots eas easundots in
       return (
         A.IteratorHeader (ia1, ia2, eas, ia3), 
         F.MacroIterHeader (st, ((s,ebs), [ib1;ib2;ib3]))
       )))))

     

  | A.ForHeader (ia1, ia2, ea1opt, ia3, ea2opt, ia4, ea3opt, ia5), 
    F.ForHeader (st, (((eb1opt,ib3s), (eb2opt,ib4s), (eb3opt,ib4vide)), ii))
    -> 
      assert (null ib4vide);
      let (ib1, ib2, ib5) = tuple_of_list3 ii in
      let ib3 = tuple_of_list1 ib3s in
      let ib4 = tuple_of_list1 ib4s in
      
      tokenf ia1 ib1 >>= (fun ia1 ib1 ->
      tokenf ia2 ib2 >>= (fun ia2 ib2 ->
      tokenf ia3 ib3 >>= (fun ia3 ib3 ->
      tokenf ia4 ib4 >>= (fun ia4 ib4 ->
      tokenf ia5 ib5 >>= (fun ia5 ib5 ->
      option expression ea1opt eb1opt >>= (fun ea1opt eb1opt ->
      option expression ea2opt eb2opt >>= (fun ea2opt eb2opt ->
      option expression ea3opt eb3opt >>= (fun ea3opt eb3opt ->
        return (
          A.ForHeader (ia1, ia2, ea1opt, ia3, ea2opt, ia4, ea3opt, ia5),
          F.ForHeader (st, (((eb1opt,[ib3]), (eb2opt,[ib4]), (eb3opt,[])),
                           [ib1;ib2;ib5]))

        )))))))))


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
      
  | A.Break (ia1, ia2), F.Break (st, ((),ii)) -> 
      let (ib1, ib2) = tuple_of_list2 ii in
      tokenf ia1 ib1 >>= (fun ia1 ib1 -> 
      tokenf ia2 ib2 >>= (fun ia2 ib2 -> 
        return (
          A.Break (ia1, ia2), 
          F.Break (st, ((),[ib1;ib2]))
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

          define_params (seqstyle eas) (A.undots eas) ebs >>= 
            (fun easundots ebs -> 
              let eas = redots eas easundots in
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

  | A.Label(id,dd), F.Label (st,(s,ii)) ->
      let (ib1,ib2) = tuple_of_list2 ii in
      let (string_of_id,rebuild) =
	match A.unwrap id with
	  A.Id(s) -> (s,function s -> A.rewrap id (A.Id(s)))
	| _ -> failwith "labels with metavariables not supported" in
      if (term string_of_id) =$= s
      then
	tokenf string_of_id ib1 >>= (fun string_of_id ib1 ->
	tokenf dd ib2 >>= (fun dd ib2 ->
	  return (
	    A.Label(rebuild string_of_id,dd),
	    F.Label (st,(s,[ib1;ib2]))
	  )))
      else fail

  | A.Goto(goto,id,sem),          F.Goto (st,(s,ii))       ->
      let (ib1,ib2,ib3) = tuple_of_list3 ii in
      tokenf goto ib1 >>= (fun goto ib1 ->
      ident DontKnow id (s, ib2) >>= (fun id (s, ib2) ->
      tokenf sem ib3 >>= (fun sem ib3 ->
	return(
	    A.Goto(goto,id,sem),
            F.Goto (st,(s,[ib1;ib2;ib3]))
          ))))

  (* have not a counter part in coccinelle, for the moment *)
  (* todo?: print a warning at least ? *)
  | _, F.CaseRange _  
  | _, F.Asm _
  | _, F.MacroTop _
    -> fail2()

  | _, (F.IfdefEndif _|F.IfdefElse _|F.IfdefHeader _)
    -> fail2 ()

  | _, 
    (F.MacroStmt (_, _)| F.DefineDoWhileZeroHeader _| F.EndNode|F.TopNode)
      -> fail
  | _, 
    (F.Label (_, _)|F.Break (_, _)|F.Continue (_, _)|F.Default (_, _)|
    F.Case (_, _)|F.Include _|F.Goto _|F.ExprStatement _|
    F.DefineType _|F.DefineExpr _|
    F.DefineHeader (_, _)|F.ReturnExpr (_, _)|F.Return (_, _)|F.MacroIterHeader (_, _)|
    F.SwitchHeader (_, _)|F.ForHeader (_, _)|F.DoWhileTail _|F.DoHeader (_, _)|
    F.WhileHeader (_, _)|F.Else _|F.IfHeader (_, _)|
    F.SeqEnd (_, _)|F.SeqStart (_, _, _)|
    F.Decl _|F.FunHeader _)
      -> fail


  )
end

