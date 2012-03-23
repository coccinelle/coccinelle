(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

(* A pretty-printer for [IL]. *)

open IL
open Printf

module Make (X : sig

  (* This is the channel that is being written to. *)

  val f: out_channel

  (* If [raw_stretch_action] is set, then we print the semantic actions
     as they are found into the original source code. *)
  val raw_stretch_action: bool

  (* This controls the way we print Objective Caml stretches (types and
     semantic actions). We either surround them with #line directives
     (for better error reports if the generated code is ill - typed) or
     don't (for better readability). The value is either [None] -- do
     not provide #line directives -- or [Some filename] -- do provide
     them. [filename] is the name of the file that is being written
     to. *)

  val locate_stretches: string option

end) = struct

(* ------------------------------------------------------------------------- *)
(* Dealing with newlines and indentation. *)

let maxindent =
  120

let whitespace =
  String.make maxindent ' '

let indentation =
  ref 0

let line =
  ref 1

(* [rawnl] is, in principle, the only place where writing a newline
   character to the output channel is permitted. This ensures that the
   line counter remains correct. But see also [stretch] and [typ0]. *)

let rawnl f =
  incr line;
  output_char f '\n'

let nl f =
  rawnl f;
  output f whitespace 0 !indentation

let indent ofs producer f x =
  let old_indentation = !indentation in
  let new_indentation = old_indentation + ofs in
  if new_indentation <= maxindent then
    indentation := new_indentation;
  nl f;
  producer f x;
  indentation := old_indentation

(* This produces a #line directive. *)

let sharp f line file =
  fprintf f "%t# %d \"%s\"%t" rawnl line file rawnl

(* ------------------------------------------------------------------------- *)
(* Printers of atomic elements. *)

let nothing f =
  ()

let space f =
  output_char f ' '

let comma f =
  output_string f ", "

let seminl f =
  output_char f ';';
  nl f

let times f =
  output_string f " * "

let letrec f =
  output_string f "let rec "

let letnonrec f =
  output_string f "let "

let keytyp f =
  output_string f "type "

let exc f =
  output_string f "exception "

let et f =
  output_string f "and "

let var f x =
  output_string f x

let bar f =
  output_string f " | "

(* ------------------------------------------------------------------------- *)
(* List printers. *)

let rec list elem sep f = function
  | [] ->
      ()
  | e :: es ->
      fprintf f "%t%a%a" sep elem e (list elem sep) es

let rec typeparams p0 p1 f = function
  | [] ->
      ()
  | [ param ] ->
      fprintf f "%a " p0 param
  | param :: params ->
      fprintf f "(%a%a) " p1 param (list p1 comma) params

(* ------------------------------------------------------------------------- *)
(* Expression printer. *)

(* We use symbolic constants that stand for subsets of the
   expression constructors. We do not use numeric levels
   to stand for subsets, because our subsets do not form
   a linear inclusion chain. *)

type subset =
  | All
  | AllButSeq
  | AllButFunTryMatch
  | AllButFunTryMatchSeq
  | AllButLetFunTryMatch
  | AllButLetFunTryMatchSeq
  | AllButIfThen
  | AllButIfThenSeq
  | OnlyAppOrAtom
  | OnlyAtom

(* This computes the intersection of a subset with the
   constraint "should not be a sequence". *)

let andNotSeq = function
  | All
  | AllButSeq ->
      AllButSeq
  | AllButFunTryMatch
  | AllButFunTryMatchSeq ->
      AllButFunTryMatchSeq
  | AllButLetFunTryMatch
  | AllButLetFunTryMatchSeq ->
      AllButLetFunTryMatchSeq
  | AllButIfThen
  | AllButIfThenSeq ->
      AllButIfThenSeq
  | OnlyAppOrAtom ->
      OnlyAppOrAtom
  | OnlyAtom ->
      OnlyAtom

(* This defines the semantics of subsets by relating
   expressions with subsets. *)

let rec member e k =
  match e with
  | EComment _
  | EPatComment _ ->
      true
  | EFun _
  | ETry _
  | EMatch _ ->
      begin
	match k with
	| AllButFunTryMatch
	| AllButFunTryMatchSeq
	| AllButLetFunTryMatch
	| AllButLetFunTryMatchSeq
	| OnlyAppOrAtom
	| OnlyAtom ->
	    false
	| _ ->
	    true
      end
  | ELet ([], e) ->
      member e k
  | ELet ((PUnit, _) :: _, _) ->
      begin
	match k with
	| AllButSeq
	| AllButFunTryMatchSeq
	| AllButLetFunTryMatchSeq
	| AllButIfThenSeq
	| OnlyAppOrAtom
	| OnlyAtom ->
	    false
	| _ ->
	    true
      end
  | ELet (_ :: _, _) ->
      begin
	match k with
	| AllButLetFunTryMatch
	| AllButLetFunTryMatchSeq
	| OnlyAppOrAtom
	| OnlyAtom ->
	    false
	| _ ->
	    true
      end
  | EIfThen _ ->
      begin
	match k with
	| AllButIfThen
	| AllButIfThenSeq
	| OnlyAppOrAtom
	| OnlyAtom ->
	    false
	| _ ->
	    true
      end
  | EApp (_, _ :: _)
  | EData (_, _ :: _)
  | EMagic _
  | ERepr _
  | ERaise _ ->
      begin
	match k with
	| OnlyAtom ->
	    false
	| _ ->
	    true
      end
  | ERecordWrite _
  | EIfThenElse _ ->
      begin
	match k with
	| OnlyAppOrAtom
	| OnlyAtom ->
	    false
	| _ ->
	    true
      end
  | EVar _
  | ETextual _
  | EApp (_, [])
  | EData (_, [])
  | ETuple _
  | EAnnot _
  | ERecord _
  | ERecordAccess (_, _)
  | EIntConst _
  | EStringConst _
  | EUnit
  | EArray _
  | EArrayAccess (_, _) ->
      true


let rec exprlet k pes f e2 =
  match pes with
  | [] ->
      exprk k f e2
  | (PUnit, e1) :: pes ->
      fprintf f "%a%t%a" (exprk AllButLetFunTryMatch) e1 seminl (exprlet k pes) e2
  | (PVar id1, EAnnot (e1, ts1)) :: pes ->
      (* TEMPORARY current ocaml does not support type schemes here; drop quantifiers, if any *)
      fprintf f "let %s : %a = %a in%t%a" id1 typ ts1.body (* scheme ts1 *) expr e1 nl (exprlet k pes) e2
  | (PVar id1, EFun (ps1, e1)) :: pes ->
      fprintf f "let %s%a = %a in%t%t%a"
	id1 (list pat0 space) ps1 (indent 2 expr) e1 nl nl (exprlet k pes) e2
  | (p1, (ELet _ as e1)) :: pes ->
      fprintf f "let %a =%a%tin%t%a" pat p1 (indent 2 expr) e1 nl nl (exprlet k pes) e2
  | (p1, e1) :: pes ->
      fprintf f "let %a = %a in%t%a" pat p1 expr e1 nl (exprlet k pes) e2

and atom f e =
  exprk OnlyAtom f e

and app f e =
  exprk OnlyAppOrAtom f e

and expr f e =
  exprk All f e

and exprk k f e =
  if member e k then
    match e with
    | EComment (c, e) ->
	if Settings.comment then
	  fprintf f "(* %s *)%t%a" c nl (exprk k) e
	else
	  exprk k f e
    | EPatComment (s, p, e) ->
	if Settings.comment then
	  fprintf f "(* %s%a *)%t%a" s pat p nl (exprk k) e
	else
	  exprk k f e
    | ELet (pes, e2) ->
	exprlet k pes f e2
    | ERecordWrite (e1, field, e2) ->
	fprintf f "%a.%s <- %a" atom e1 field (exprk (andNotSeq k)) e2
    | EMatch (e, []) ->
	assert false
    | EMatch (e, brs) ->
	fprintf f "match %a with%a" expr e (branches k) brs
    | ETry (_, []) ->
	assert false
    | ETry (e, brs) ->
	fprintf f "try%a%twith%a" (indent 2 expr) e nl (branches k) brs
    | EIfThen (e1, e2) ->
	fprintf f "if %a then%a" expr e1 (indent 2 (exprk (andNotSeq k))) e2
    | EIfThenElse (e0, e1, e2) ->
	fprintf f "if %a then%a%telse%a"
          expr e0 (indent 2 (exprk AllButIfThenSeq)) e1 nl (indent 2 (exprk (andNotSeq k))) e2
    | EFun (ps, e) ->
	fprintf f "fun%a ->%a" (list pat0 space) ps (indent 2 (exprk k)) e
    | EApp (EVar op, [ e1; e2 ])
      when op.[0] = '(' && op.[String.length op - 1] = ')' ->
	let op = String.sub op 1 (String.length op - 2) in
	fprintf f "%a %s %a" app e1 op app e2
    | EApp (e, args) ->
	fprintf f "%a%a" app e (list atom space) args
    | ERaise e ->
	fprintf f "raise %a" atom e
    | EMagic e ->
	fprintf f "Obj.magic %a" atom e
    | ERepr e ->
	fprintf f "Obj.repr %a" atom e
    | EData (d, []) ->
	var f d
    | EData (d, [ arg ]) ->
	fprintf f "%s %a" d atom arg
    | EData (d, arg :: args) ->
	fprintf f "%s (%a%a)" d app arg (list app comma) args
    | EVar v ->
	var f v
    | ETextual action ->
	stretch (X.raw_stretch_action) f action
    | EUnit ->
	fprintf f "()"
    | EIntConst k ->
	if k >= 0 then
	  fprintf f "%d" k
	else
	  fprintf f "(%d)" k
    | EStringConst s ->
	fprintf f "\"%s\"" (String.escaped s)
    | ETuple [] ->
	assert false
    | ETuple [ e ] ->
	atom f e
    | ETuple (e :: es) ->
	fprintf f "(%a%a)" app e (list app comma) es
    | EAnnot (e, s) ->
	(* TEMPORARY current ocaml does not support type schemes here; drop quantifiers, if any *)
	fprintf f "(%a : %a)" app e typ s.body (* should be scheme s *)
    | ERecordAccess (e, field) ->
	fprintf f "%a.%s" atom e field
    | ERecord fs ->
	fprintf f "{%a}" (indent 2 (list field nothing)) fs
    | EArray fs ->
	fprintf f "[|%a|]" (indent 2 (list array_field nothing)) fs
    | EArrayAccess (e, i) ->
	fprintf f "%a.(%a)" atom e expr i
  else
    fprintf f "(%a)" expr e

and stretch raw f stretch =
  let content = stretch.Stretch.stretch_content
  and raw_content = stretch.Stretch.stretch_raw_content in
  match X.locate_stretches with
  | Some basename ->
      sharp f stretch.Stretch.stretch_linenum stretch.Stretch.stretch_filename;
      output_string f content;
      line := !line + stretch.Stretch.stretch_linecount;
      sharp f (!line + 2) basename;
      output f whitespace 0 !indentation
  | None ->
      output_string f (if raw then raw_content else content)

and branches k f = function
  | [] ->
      ()
  | [ br ] ->
      fprintf f "%t| %a" nl (branch k) br
  | br :: brs ->
      fprintf f "%t| %a%a" nl (branch AllButFunTryMatch) br (branches k) brs

and branch k f br =
  fprintf f "%a ->%a" pat br.branchpat (indent 4 (exprk k)) br.branchbody

and field f (label, e) =
  fprintf f "%s = %a%t" label app e seminl

and fpats f fps =
  list fpat nothing f fps

and fpat f = function
  | (_, PWildcard) ->
      () (* in a record pattern, fields can be omitted *)
  | (label, p) ->
      fprintf f "%s = %a%t" label pat p seminl

and array_field f e =
  fprintf f "%a%t" app e seminl

and pat0 f = function
  | PUnit ->
      fprintf f "()"
  | PWildcard ->
      fprintf f "_"
  | PVar x ->
      var f x
  | PData (d, []) ->
      var f d
  | PTuple [] ->
      assert false
  | PTuple [ p ] ->
      pat0 f p
  | PTuple (p :: ps) ->
      fprintf f "(%a%a)" pat1 p (list pat1 comma) ps
  | PAnnot (p, t) ->
      fprintf f "(%a : %a)" pat p typ t
  | PRecord fps ->
      fprintf f "{%a}" (indent 2 fpats) fps
  | p ->
      fprintf f "(%a)" pat p

and pat1 f = function
  | PData (d, [ arg ]) ->
      fprintf f "%s %a" d pat0 arg
  | PData (d, arg :: args) ->
      fprintf f "%s (%a%a)" d pat1 arg (list pat1 comma) args
  | PTuple [ p ] ->
      pat1 f p
  | p ->
      pat0 f p

and pat2 f = function
  | POr [] ->
      assert false
  | POr (p :: ps) ->
      fprintf f "%a%a" pat2 p (list pat2 bar) ps
  | PTuple [ p ] ->
      pat2 f p
  | p ->
      pat1 f p

and pat f p =
  pat2 f p

and typevar f v =
  fprintf f "'%s" v

and typ0 f = function
  | TypTextual (Stretch.Declared ocamltype) ->
      (* Parentheses are necessary to avoid confusion between 1 - ary
	 data constructor with n arguments and n - ary data constructor. *)
      fprintf f "(%a)" (stretch true) ocamltype
  | TypTextual (Stretch.Inferred t) ->
      line := !line + LineCount.count 0 (Lexing.from_string t);
      fprintf f "(%s)" t
  | TypVar v ->
      typevar f v
  | TypApp (t, params) ->
      fprintf f "%a%s" (typeparams typ0 typ) params t
  | t ->
      fprintf f "(%a)" typ t

and typ1 f = function
  | TypTuple [] ->
      assert false
  | TypTuple [ t ] ->
      typ1 f t
  | TypTuple (t :: ts) ->
      fprintf f "%a%a" typ0 t (list typ0 times) ts
  | t ->
      typ0 f t

and typ2 f = function
  | TypArrow (t1, t2) ->
      fprintf f "%a -> %a" typ1 t1 typ2 t2
  | t ->
      typ1 f t

and typ f =
  typ2 f

and scheme f scheme =
  match scheme.quantifiers with
  | [] ->
      typ f scheme.body
  | qs ->
      fprintf f "%a. %a" (list typevar space) qs typ scheme.body

(* ------------------------------------------------------------------------- *)
(* Toplevel definition printer. *)

let datavalparams f = function
  | [] ->
      ()
  | valparam :: valparams ->
      fprintf f " of %a%a" typ valparam (list typ times) valparams

let datatypeparams f = function
  | None ->
      ()
  | Some typs ->
      fprintf f "(* %a*)" (list typ space) typs (* TEMPORARY not great *)

let datadef f def =
  fprintf f "  | %s%a%a" def.dataname datavalparams def.datavalparams datatypeparams def.datatypeparams

let fielddef f def =
  fprintf f "  %s%s: %a"
    (if def.modifiable then "mutable " else "")
    def.fieldname
    scheme def.fieldtype

let typerhs f = function
  | TDefRecord [] ->
      assert false
  | TDefRecord (field :: fields) ->
      fprintf f " = {%t%a%a%t}" nl fielddef field (list fielddef seminl) fields nl
  | TDefSum [] ->
      ()
  | TDefSum defs ->
      fprintf f " = %a" (list datadef nl) defs
  | TAbbrev t ->
      fprintf f " = %a" typ t

let typeconstraint f = function
  | None ->
      ()
  | Some (t1, t2) ->
      fprintf f "%tconstraint %a = %a" nl typ t1 typ t2

let typedef f def =
  fprintf f "%a%s%a%a%t%t"
    (typeparams typevar typevar) def.typeparams
    def.typename
    typerhs def.typerhs
    typeconstraint def.typeconstraint
    nl nl

let rec pdefs pdef sep1 sep2 f = function
  | [] ->
      ()
  | def :: defs ->
      fprintf f "%t%a%a" sep1 pdef def (pdefs pdef sep2 sep2) defs

let valdef f = function
  | { valpat = PVar id; valval = EAnnot (e, ts) } ->
      (* TEMPORARY current ocaml does not support type schemes here; drop quantifiers, if any *)
      fprintf f "%s : %a =%a%t%t" id typ ts.body (* scheme ts *) (indent 2 expr) e nl nl
  | { valpat = p; valval = e } ->
      fprintf f "%a =%a%t%t" pat p (indent 2 expr) e nl nl

let valdefs =
  pdefs valdef letrec et

let nonrecvaldefs =
  pdefs valdef letnonrec letnonrec

let typedefs =
  pdefs typedef keytyp et

let directive f = function
  | DirOpen s ->
      fprintf f "open %s%t%t" s nl nl
  | DirInclude s ->
      fprintf f "include %s%t%t" s nl nl

let directives =
  pdefs directive nothing nothing

let excdef f def =
  match def.exceq with
  | None ->
      fprintf f "%s%t%t" def.excname nl nl
  | Some s ->
      fprintf f "%s = %s%t%t" def.excname s nl nl

let excdefs =
  pdefs excdef exc exc

let functorparams intf body b f params =
  match params with
  | [] ->
      fprintf f "%a%!" body b
  | _ ->
      fprintf f "module Make%a%t%s%t%a%t%tend%t%!"
	(list (stretch false) nl) params
	nl (if intf then ": sig" else "= struct") nl
	(indent 2 body) b
	nl nl nl

let structure f p =
  fprintf f "struct%aend" (
    indent 2 (fun f p ->
      fprintf f "%t%a%a%a"
	nl
	excdefs p.struct_excdefs
	typedefs p.struct_typedefs
	nonrecvaldefs p.struct_nonrecvaldefs
    )
  ) p


let rec modexpr f = function
  | MVar x ->
      fprintf f "%s" x
  | MStruct s ->
      structure f s
  | MApp (e1, e2) ->
      fprintf f "%a (%a)" modexpr e1 modexpr e2

let moduledef f d =
  fprintf f "module %s = %a%t%t" d.modulename modexpr d.modulerhs nl nl

let program f p =
  fprintf f "%a%a"
    excdefs p.excdefs
    typedefs p.typedefs;
  List.iter (stretch false f) p.prologue;
  fprintf f "%a%a%a"
    nonrecvaldefs p.nonrecvaldefs
    (list moduledef nothing) p.moduledefs
    valdefs p.valdefs;
  List.iter (output_string f) p.postlogue

let valdecl f (x, ts) =
  fprintf f "val %s: %a" x typ ts.body

let interface f i =
  fprintf f "%a%a%a%!" excdefs i.excdecls typedefs i.typedecls (list valdecl nl) i.valdecls

let program p =
  functorparams false program p X.f p.paramdefs

let interface i =
  functorparams true interface i X.f i.paramdecls

let expr e =
  expr X.f e

end
