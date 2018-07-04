(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

{
open Parser_cocci_menhir
module D = Data
module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module P = Parse_aux
module FC = Flag_parsing_cocci
exception Lexical of string
let tok = Lexing.lexeme

let line = ref 1
let file = ref ""
let logical_line = ref 0

(* ---------------------------------------------------------------------- *)
(* control codes *)

(* Defined in data.ml
type line_type = MINUS | OPTMINUS | PLUS | CONTEXT | OPT
*)

let current_line_type = ref (D.CONTEXT,!line,!logical_line)

let prev_plus = ref false
let line_start = ref 0 (* offset of the beginning of the line *)
let get_current_line_type lexbuf =
  let (c,l,ll) = !current_line_type in
  let lex_start = Lexing.lexeme_start lexbuf in
  let preceeding_spaces =
    if !line_start < 0 then 0 else lex_start - !line_start in
  (*line_start := -1;*)
  prev_plus := (c = D.PLUS) || (c = D.PLUSPLUS);
  (c,l,ll,ll,lex_start,preceeding_spaces,[],[],[],"")
let current_line_started = ref false
let col_zero = ref true

let contextify (c,l,ll,lle,lex_start,preceeding_spaces,bef,aft,pos,ws) =
  (D.CONTEXT,l,ll,lle,lex_start,preceeding_spaces,bef,aft,pos,ws)

let reset_line lexbuf =
  line := !line + 1;
  current_line_type := (D.CONTEXT,!line,!logical_line);
  current_line_started := false;
  (if not(!Data.in_meta || !Data.in_rule_name) then col_zero := true);
  line_start := Lexing.lexeme_start lexbuf + 1

let started_line = ref (-1)

let start_line seen_char =
  current_line_started := true;
  col_zero := false;
  (if seen_char && not(!line = !started_line)
  then
    begin
      started_line := !line;
      logical_line := !logical_line + 1
    end)

let pass_zero _ = col_zero := false

let lexerr s1 s2 = raise (Lexical (Printf.sprintf "%s%s" s1 s2))

let opt_reverse_token token =
  if !FC.interpret_inverted
  then match token with
         D.MINUS        -> D.PLUSPLUS  (* maybe too liberal *)
       | D.OPTMINUS     -> lexerr "cannot invert token ?- (an optional minus line), which is needed for reversing the patch" ""
       | D.PLUS         -> D.MINUS
       | D.PLUSPLUS     -> D.MINUS (* may not be sufficient *)
       | _              -> token
  else token

let add_current_line_type x =
  match (opt_reverse_token x,!current_line_type) with
    (D.MINUS,(D.CONTEXT,ln,lln))  ->
      current_line_type := (D.MINUS,ln,lln)
  | (D.MINUS,(D.OPT,ln,lln))      ->
      current_line_type := (D.OPTMINUS,ln,lln)
  | (D.PLUS,(D.CONTEXT,ln,lln))   ->
      current_line_type := (D.PLUS,ln,lln)
  | (D.PLUSPLUS,(D.CONTEXT,ln,lln))   ->
      current_line_type := (D.PLUSPLUS,ln,lln)
  | (D.OPT,(D.CONTEXT,ln,lln))    ->
      current_line_type := (D.OPT,ln,lln)
  | _ -> lexerr "invalid control character combination" ""

let check_minus_context_linetype s =
  match !current_line_type with
    (D.PLUS,_,_) | (D.PLUSPLUS,_,_) -> lexerr "invalid in a + context: " s
  | _ -> ()

let check_context_linetype s =
  match !current_line_type with
    (D.CONTEXT,_,_) -> ()
  | _ -> lexerr "invalid in a nonempty context: " s

let check_plus_linetype s =
  match !current_line_type with
    (D.PLUS,_,_) | (D.PLUSPLUS,_,_) -> ()
  | _ -> lexerr "invalid in a non + context: " s

let check_arity_context_linetype s =
  match !current_line_type with
    (D.CONTEXT,_,_) | (D.PLUS,_,_) | (D.PLUSPLUS,_,_)
  | (D.OPT,_,_) -> ()
  | _ -> lexerr "invalid in a nonempty context: " s

let check_comment s =
  if not !current_line_started
  then lexerr "+ expected at the beginning of the line" s

let process_include start finish str =
  (match !current_line_type with
    (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
      (try
	let _ = Str.search_forward (Str.regexp "\\.\\.\\.") str start in
	lexerr "... not allowed in + include" ""
      with Not_found -> ())
  | _ -> ());
  String.sub str (start + 1) (finish - start - 1)

(* ---------------------------------------------------------------------- *)
type pm = PATCH | MATCH | UNKNOWN

let pm = ref UNKNOWN

let patch_or_match = function
    PATCH ->
      if not !D.ignore_patch_or_match
      then
	(match !pm with
	  MATCH ->
	    lexerr "- or + not allowed in the first column for a match" ""
	| PATCH -> ()
	| UNKNOWN -> Flag.sgrep_mode2 := false; pm := PATCH)
  | MATCH ->
      if not !D.ignore_patch_or_match
      then
	(match !pm with
	  PATCH -> lexerr "* not allowed in the first column for a patch" ""
	| MATCH -> ()
	| UNKNOWN -> Flag.sgrep_mode2 := true; pm := MATCH)
  | _ -> failwith "unexpected argument"

(* ---------------------------------------------------------------------- *)
(* identifiers, including metavariables *)

let metavariables = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let all_metavariables =
  (Hashtbl.create(100) : (string,(string * (D.clt -> token)) list) Hashtbl.t)

let type_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let attr_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let declarer_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let iterator_names = (Hashtbl.create(100) : (string, D.clt -> token) Hashtbl.t)

let symbol_names = (Hashtbl.create(15) : (string, D.clt -> token) Hashtbl.t)

let rule_names = (Hashtbl.create(100) : (string, unit) Hashtbl.t)

let check_var s linetype =
  let fail _ =
    if (!Data.in_prolog || !Data.in_rule_name) &&
      Str.string_match (Str.regexp "<.*>") s 0
    then TPathIsoFile s
    else
      try (Hashtbl.find metavariables s) linetype
      with Not_found ->
	(try (Hashtbl.find type_names s) linetype
	with Not_found ->
	  (try (Hashtbl.find attr_names s) linetype
	  with Not_found ->
	    (try (Hashtbl.find declarer_names s) linetype
	    with Not_found ->
	      (try (Hashtbl.find iterator_names s) linetype
	      with Not_found ->
		(try (Hashtbl.find symbol_names s) linetype
		with Not_found ->
                  TIdent (s,linetype)))))) in
  if !Data.in_meta || !Data.in_rule_name
  then (try Hashtbl.find rule_names s; TRuleName s with Not_found -> fail())
  else fail()

let id_tokens lexbuf =
  let s = tok lexbuf in
  let linetype = get_current_line_type lexbuf in
  let in_rule_name = !Data.in_rule_name in
  let in_meta = !Data.in_meta && not !Data.saw_struct in
  let in_iso = !Data.in_iso in
  let in_prolog = !Data.in_prolog in
  (if s = "identifer" && in_meta
  then Common.pr2 "Warning: should identifer be identifier?");
  match s with
    "metavariable" when in_meta -> check_arity_context_linetype s; TMetavariable
  | "identifier" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TIdentifier
  | "type" when in_meta ->       check_arity_context_linetype s; TType
  | "parameter" when in_meta ->  check_arity_context_linetype s; TParameter
  | "operator" when in_meta ->   check_arity_context_linetype s; TOperator
  | "binary" when in_meta ->   check_arity_context_linetype s; TBinary
  | "assignment" when in_meta ->   check_arity_context_linetype s; TAssignment
  | "constant"  when in_meta ->  check_arity_context_linetype s; TConstant
  | "generated" when in_rule_name && not (!Flag.make_hrule = None) ->
      check_arity_context_linetype s; TGenerated
  | "expression" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TExpression
  | "declaration" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TDeclaration
  | "field" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TField
  | "initialiser" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TInitialiser
  | "initializer" when in_meta || in_rule_name ->
      check_arity_context_linetype s; TInitialiser
  | "idexpression" when in_meta ->
      check_arity_context_linetype s; TIdExpression
  | "statement" when in_meta ->  check_arity_context_linetype s; TStatement
  | "function"  when in_meta ->  check_arity_context_linetype s; TFunction
  | "local" when in_meta ->      check_arity_context_linetype s; TLocal
  | "global" when in_meta ->     check_arity_context_linetype s; TGlobal
  | "list" when in_meta ->       check_arity_context_linetype s; Tlist
  | "fresh" when in_meta ->      check_arity_context_linetype s; TFresh
  | "typedef" when in_meta ->    check_arity_context_linetype s; TTypedef
  | "attribute" when in_meta ->  check_arity_context_linetype s; TAttribute
  | "declarer" when in_meta ->   check_arity_context_linetype s; TDeclarer
  | "iterator" when in_meta ->   check_arity_context_linetype s; TIterator
  | "name" when in_meta ->       check_arity_context_linetype s; TName
  | "position" when in_meta ->   check_arity_context_linetype s; TPosition
  | "format" when in_meta ->     check_arity_context_linetype s; TFormat
  | "analysis" when in_meta ->   check_arity_context_linetype s; TAnalysis
  | "any" when in_meta ->        check_arity_context_linetype s; TPosAny
  | "pure" when in_meta && in_iso ->
      check_arity_context_linetype s; TPure
  | "context" when in_meta && in_iso ->
      check_arity_context_linetype s; TContext
  | "error" when in_meta ->      check_arity_context_linetype s; TError
  | "words" when in_meta ->      check_context_linetype s; TWords
  | "symbol" when in_meta ->     check_arity_context_linetype s; TSymbol

  | "using" when in_rule_name || in_prolog ->  check_context_linetype s; TUsing
  | "virtual" when in_prolog || in_rule_name || in_meta ->
      (* don't want to allow virtual as a rule name *)
      check_context_linetype s; TVirtual
  | "merge" when in_prolog || in_rule_name || in_meta ->
      check_context_linetype s; TMerge
  | "disable" when in_rule_name ->  check_context_linetype s; TDisable
  | "extends" when in_rule_name -> check_context_linetype s; TExtends
  | "depends" when in_rule_name -> check_context_linetype s; TDepends
  | "on" when in_rule_name      -> check_context_linetype s; TOn
  | "ever" when in_rule_name    -> check_context_linetype s; TEver
  | "never" when in_rule_name   -> check_context_linetype s; TNever
  | "file" when in_rule_name    -> check_context_linetype s; TFile
  | "in" when in_rule_name      -> check_context_linetype s; TIn
  (* exists and forall for when are reparsed in parse_cocci.ml *)
  | "exists" when in_rule_name  -> check_context_linetype s; TExists
  | "forall" when in_rule_name  -> check_context_linetype s; TForall
  | "script" when (in_rule_name || in_meta) ->
      check_context_linetype s; TScript (!file, !line)
  | "initialize" when in_rule_name -> check_context_linetype s; TInitialize
  | "finalize" when in_rule_name   -> check_context_linetype s; TFinalize

  | "char" ->       Tchar     linetype
  | "short" ->      Tshort    linetype
  | "int" ->        Tint      linetype
  | "double" ->     Tdouble   linetype
  | "float" ->      Tfloat    linetype
  | "long" ->       Tlong     linetype
  | "void" ->       Tvoid     linetype
  | "size_t" ->     Tsize_t   linetype
  | "ssize_t" ->    Tssize_t  linetype
  | "ptrdiff_t" ->  Tptrdiff_t linetype
  (* in_meta is only for the first keyword; drop it now to allow any type
     name *)
  | "struct" ->     Data.saw_struct := true; Tstruct   linetype
  | "union" ->      Data.saw_struct := true; Tunion    linetype
  | "enum" ->       Data.saw_struct := true; Tenum     linetype
  | "unsigned" ->   Tunsigned linetype
  | "signed" ->     Tsigned   linetype
  | "decimal" when !Flag.ibm -> Tdecimal linetype
  | "EXEC" when !Flag.ibm -> Texec linetype

  | "auto"  ->      Tauto     linetype
  | "register" ->   Tregister linetype
  | "extern" ->     Textern   linetype
  | "static" ->     Tstatic   linetype
  | "inline" ->     Tinline   linetype
  | "typedef" ->    Ttypedef  linetype

  | "const" ->      Tconst    linetype
  | "volatile" ->   Tvolatile linetype

  | "if" ->         TIf       linetype
  | "else" ->       TElse     linetype
  | "while" ->      TWhile    linetype
  | "do" ->         TDo       linetype
  | "for" ->        TFor      linetype
  | "switch" ->     TSwitch   linetype
  | "case" ->       TCase     linetype
  | "default" ->    TDefault  linetype
  | "return" ->     TReturn   linetype
  | "break" ->      TBreak    linetype
  | "continue" ->   TContinue linetype
  | "goto" ->       TGoto     linetype

  | "sizeof" ->     TSizeof   linetype
  | "typeof" ->     TTypeof   linetype

  | "Expression"       when !Data.in_iso -> TIsoExpression
  | "ArgExpression"    when !Data.in_iso -> TIsoArgExpression
  | "TestExpression"   when !Data.in_iso -> TIsoTestExpression
  | "ToTestExpression" when !Data.in_iso -> TIsoToTestExpression
  | "Statement"        when !Data.in_iso -> TIsoStatement
  | "Declaration"      when !Data.in_iso -> TIsoDeclaration
  | "Type"             when !Data.in_iso -> TIsoType
  | "TopLevel"         when !Data.in_iso -> TIsoTopLevel

  | "_" when !Data.in_meta -> TUnderscore

  | s -> check_var s linetype

let mkassign op lexbuf =
  TOpAssign (op, (get_current_line_type lexbuf))

let spinit _ = (* per semantic patch *)
  Hashtbl.clear Data.non_local_script_constraints

let init _ = (* per file, first .cocci then iso *)
  line := 1;
  logical_line := 0;
  prev_plus := false;
  line_start := 0;
  current_line_started := false;
  current_line_type := (D.CONTEXT,0,0);
  col_zero := true;
  pm := UNKNOWN;
  Data.in_rule_name := false;
  Data.in_meta := false;
  Data.in_prolog := false;
  Data.saw_struct := false;
  Data.inheritable_positions := [];
  Hashtbl.clear all_metavariables;
  Hashtbl.clear Data.all_metadecls;
  Hashtbl.clear metavariables;
  Hashtbl.clear type_names;
  Hashtbl.clear rule_names;
  Hashtbl.clear iterator_names;
  Hashtbl.clear declarer_names;
  Hashtbl.clear symbol_names;
  let get_name (_,x) = x in
  Data.add_meta_meta :=
    (fun name cstr pure ->
      let fn clt = TMeta(name,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_id_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaId(name,constraints,Ast.NoVal,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_virt_id_meta_found :=
    (fun name vl ->
      let fn clt = TIdent(vl,clt) in
      Hashtbl.replace metavariables name fn);
  Data.add_virt_id_meta_not_found :=
    (fun name pure ->
      let fn clt = TMetaId(name,Ast.CstrTrue,Ast.NoVal,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_fresh_id_meta :=
    (fun name seed ->
      let fn clt = TMetaId(name,Ast.CstrTrue,seed,Ast0.Impure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_type_meta :=
    (fun name cstr pure ->
      let fn clt = TMetaType(name,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_init_meta :=
    (fun name cstr pure ->
      let fn clt = TMetaInit(name,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_initlist_meta :=
    (fun name lenname cstr pure ->
      let fn clt = TMetaInitList(name,lenname,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_param_meta :=
    (fun name cstr pure ->
      let fn clt = TMetaParam(name,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_paramlist_meta :=
    (fun name lenname cstr pure ->
      let fn clt = TMetaParamList(name,lenname,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_const_meta :=
    (fun tyopt name constraints pure ->
      let fn clt = TMetaConst(name,constraints,pure,tyopt,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_err_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaErr(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_exp_meta :=
    (fun tyopt name constraints pure bitfield ->
      let fn clt = TMetaExp(name,constraints,pure,tyopt,clt,bitfield) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_idexp_meta :=
    (fun tyopt name constraints pure ->
      let fn clt = TMetaIdExp(name,constraints,pure,tyopt,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_local_idexp_meta :=
    (fun tyopt name constraints pure ->
      let fn clt = TMetaLocalIdExp(name,constraints,pure,tyopt,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_global_idexp_meta :=
    (fun tyopt name constraints pure ->
      let fn clt = TMetaGlobalIdExp(name,constraints,pure,tyopt,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_explist_meta :=
    (fun name lenname cstr pure ->
      let fn clt = TMetaExpList(name,lenname,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_decl_meta :=
    (fun name cstr pure ->
      let fn clt = TMetaDecl(name,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_field_meta :=
    (fun name cstr pure ->
      let fn clt = TMetaField(name,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_field_list_meta :=
    (fun name lenname cstr pure ->
      let fn clt = TMetaFieldList(name,lenname,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_fmt_meta :=
    (function name -> function iconstraints ->
      let fn clt = failwith "format metavariable only allowed in a string" in
      Data.format_metavariables :=
	(get_name name,(name,iconstraints)) :: !Data.format_metavariables;
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_fmtlist_meta :=
    (fun name constraints lenname ->
      let fn clt =
	failwith "format list metavariable only allowed in a string" in
      Data.format_list_metavariables :=
	(get_name name,(name,lenname,constraints))
	:: !Data.format_list_metavariables;
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_stm_meta :=
    (fun name cstr pure ->
      let fn clt = TMetaStm(name,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_stmlist_meta :=
    (fun name lenname cstr pure ->
      let fn clt = TMetaStmList(name,lenname,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_dparamlist_meta :=
    (fun name lenname cstr pure ->
      let fn clt = TMetaDParamList(name,lenname,cstr,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_func_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaFunc(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_local_func_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaLocalFunc(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_iterator_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaIterator(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_declarer_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaDeclarer(name,constraints,pure,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_pos_meta :=
    (fun name constraints any ->
      let fn ((d,ln,_,_,_,_,_,_,_,_) as clt) =
	(if d = Data.PLUS
	then
	  failwith
	    (Printf.sprintf "%d: positions only allowed in minus code" ln));
	TMetaPos(name,constraints,any,clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_assignOp_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaAssignOp (name, constraints, pure, clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_binaryOp_meta :=
    (fun name constraints pure ->
      let fn clt = TMetaBinaryOp (name, constraints, pure, clt) in
      Hashtbl.replace metavariables (get_name name) fn);
  Data.add_type_name :=
    (function name ->
      let fn clt = TTypeId(name,clt) in
      Hashtbl.replace type_names name fn);
  Data.add_attribute :=
    (function name ->
      let fn clt =
	match clt with
	  ((Data.PLUS | Data.PLUSPLUS),_,_,_,_,_,_,_,_,_) ->
	    TDirective (Ast.Space name, clt)
	| _ -> Tattr (name, clt) in
      Hashtbl.replace attr_names name fn);
  Data.add_declarer_name :=
    (function name ->
      let fn clt = TDeclarerId(name,clt) in
      Hashtbl.replace declarer_names name fn);
  Data.add_iterator_name :=
    (function name ->
      let fn clt = TIteratorId(name,clt) in
      Hashtbl.replace iterator_names name fn);
  Data.add_symbol_meta :=
    (function name ->
      let fn clt = TSymId (name,clt) in
      Hashtbl.replace symbol_names name fn);
  Data.init_rule := (function _ -> Hashtbl.clear metavariables);
  Data.install_bindings :=
    (function parent ->
      List.iter (function (name,fn) -> Hashtbl.add metavariables name fn)
	(Hashtbl.find all_metavariables parent))

let post_init _ =
  Stdcompat.Hashtbl.reset all_metavariables;
  Stdcompat.Hashtbl.reset Data.all_metadecls;
  Stdcompat.Hashtbl.reset metavariables;
  Stdcompat.Hashtbl.reset type_names;
  Stdcompat.Hashtbl.reset rule_names;
  Stdcompat.Hashtbl.reset iterator_names;
  Stdcompat.Hashtbl.reset declarer_names;
  Stdcompat.Hashtbl.reset symbol_names

(* initialization for each cocci rule *)
let reinit _ =
  Data.format_metavariables := [];
  Data.format_list_metavariables := []

(* the following is needed to properly tokenize include files.  Because an
include file is included after seeing a @, so current_line_started is true.
Current_line_started is not important for parsing the name of a rule, so we
don't have to reset this value to true after parsing an included file. *)
let include_init _ =
  current_line_started := false

let drop_spaces s =
  let len = String.length s in
  let rec loop n =
    if n = len
    then n
    else
      if List.mem (String.get s n) [' ';'\t']
      then loop (n+1)
      else n in
  let start = loop 0 in
  String.sub s start (len - start)
}

(* ---------------------------------------------------------------------- *)
(* tokens *)

let letter = ['A'-'Z' 'a'-'z' '_']
let digit  = ['0'-'9']

let dec = ['0'-'9']
let oct = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let decimal = ('0' | (['1'-'9'] dec*))
let octal   = ['0']        oct+
let hexa    = ("0x" |"0X") hex+

let pent   = dec+
let pfract = dec+
let sign = ['-' '+']
let exp  = ['e''E'] sign? dec+
let real = pent exp | ((pent? '.' pfract | pent '.' pfract? ) exp?)


rule token = parse
  | [' ' '\t']* ['\n' '\r' '\011' '\012']
    { let cls = !current_line_started in
      if not cls
      then
	begin
	  match !current_line_type with
	    (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	      (* increment the logical line even though nothing seen *)
	      start_line true;
	      let info = get_current_line_type lexbuf in
	      reset_line lexbuf;
	      TDirective (Ast.Noindent "", info)
	  | _ -> reset_line lexbuf; token lexbuf
	end
      else (reset_line lexbuf; token lexbuf) }

  | ([' ' '\t'  ]+ as w) { (* collect whitespaces only when inside a rule *)
    start_line false;
    if !Data.in_rule_name || !Data.in_prolog || !Data.in_iso
    then token lexbuf
    else TWhitespace w }

  | [' ' '\t'  ]* (("//" [^ '\n']*) as after) {
    match !current_line_type with
      (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	let str =
	  if !current_line_started
	  then (tok lexbuf)
	  else after in
	start_line true;
	TDirective (Ast.Indent str, get_current_line_type lexbuf)
    | _ -> start_line false; token lexbuf }

  | [' ' '\t'  ]* (("#!" [^ '\n']*) as after) {
     check_context_linetype after;
     start_line false; token lexbuf }

  | "__attribute__"
   { match !current_line_type with
      (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
	start_line true;
	TAttr_ (get_current_line_type lexbuf)
    | _ -> failwith "attributes only allowed in + code" }

  | "@@" { start_line true; TArobArob }
  | "@"  { pass_zero();
	   if !Data.in_rule_name || not !current_line_started
	   then (start_line true; TArob)
	   else (check_minus_context_linetype "@";
		 TPArob (get_current_line_type lexbuf)) }

  | "WHEN" | "when"
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TWhen (get_current_line_type lexbuf) }

  | "..."
      { start_line true; check_minus_context_linetype (tok lexbuf);
	TEllipsis (get_current_line_type lexbuf) }
  | "<..." { start_line true; check_context_linetype (tok lexbuf);
	     TOEllipsis (get_current_line_type lexbuf) }
  | "...>" { start_line true; check_context_linetype (tok lexbuf);
	     TCEllipsis (get_current_line_type lexbuf) }
  | "<+..." { start_line true; check_minus_context_linetype (tok lexbuf);
	     TPOEllipsis (get_current_line_type lexbuf) }
  | "...+>" { start_line true; check_minus_context_linetype (tok lexbuf);
	     TPCEllipsis (get_current_line_type lexbuf) }
  | "-" { pass_zero();
	  if !current_line_started
	  then (start_line true; TMinus (get_current_line_type lexbuf))
          else (patch_or_match PATCH;
		add_current_line_type D.MINUS; token lexbuf) }
  | "+" { pass_zero();
	  if !current_line_started
	  then (start_line true; TPlus (get_current_line_type lexbuf))
          else (patch_or_match PATCH;
		add_current_line_type D.PLUS; token lexbuf) }
  | "?" { pass_zero();
	  if !current_line_started
	  then (start_line true; TWhy (get_current_line_type lexbuf))
          else (add_current_line_type D.OPT; token lexbuf) }
  | "!" { start_line true; TBang (get_current_line_type lexbuf) }
  | "(" { if not !col_zero
	  then (start_line true; TOPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TOPar0 ("(",get_current_line_type lexbuf))}
  | "\\(" { start_line true;
	    TOPar0 ("\\(",contextify(get_current_line_type lexbuf)) }
  | "|" { if not (!col_zero)
	  then (start_line true; TOr(get_current_line_type lexbuf))
          else (start_line true;
		check_context_linetype (tok lexbuf);
		TMid0 ("|",get_current_line_type lexbuf))}
  | "\\|" { start_line true;
	    TMid0 ("\\|",contextify(get_current_line_type lexbuf)) }
  | ")" { if not !col_zero
	  then (start_line true; TCPar (get_current_line_type lexbuf))
          else
            (start_line true; check_context_linetype (tok lexbuf);
	     TCPar0 (")",get_current_line_type lexbuf))}
  | "\\)" { start_line true;
	    TCPar0 ("\\)",contextify(get_current_line_type lexbuf)) }

  | '[' { start_line true; TOCro (get_current_line_type lexbuf)   }
  | ']' { start_line true; TCCro (get_current_line_type lexbuf)   }
  | '{' { start_line true; TOBrace (get_current_line_type lexbuf) }
  | '}' { start_line true; TCBrace (get_current_line_type lexbuf) }

  | "->"           { start_line true; TPtrOp (get_current_line_type lexbuf)  }
  | '.'            { start_line true; TDot (get_current_line_type lexbuf)    }
  | ','            { start_line true; TComma (get_current_line_type lexbuf)  }
  | "......"   { start_line true; TVAEllipsis (get_current_line_type lexbuf) }
  | ";"            { start_line true; TPtVirg (get_current_line_type lexbuf) }


  | '*'            { pass_zero();
		     if !current_line_started
		     then
		       (start_line true; TMul (get_current_line_type lexbuf))
		     else
		       (patch_or_match MATCH;
			add_current_line_type D.MINUS; token lexbuf) }
  | '/'            { start_line true;
		     TDmOp (Ast.Div,get_current_line_type lexbuf) }
  | "<?"            { start_line true;
		     TDmOp (Ast.Min,get_current_line_type lexbuf) }
  | ">?"            { start_line true;
		     TDmOp (Ast.Max,get_current_line_type lexbuf) }
  | '%'            { start_line true;
		     TDmOp (Ast.Mod,get_current_line_type lexbuf) }
  | '~'            { start_line true;  TTilde (get_current_line_type lexbuf) }

  | "++"           { pass_zero();
 		     if !current_line_started
 		     then
 		       (start_line true; TInc (get_current_line_type lexbuf))
 		     else (patch_or_match PATCH;
 			   add_current_line_type D.PLUSPLUS; token lexbuf) }
  | "--"           { start_line true;  TDec (get_current_line_type lexbuf) }

  | "="            { start_line true; TEq (get_current_line_type lexbuf) }

  | "-="           { start_line true; mkassign Ast.Minus lexbuf }
  | "+="           { start_line true; mkassign Ast.Plus lexbuf }

  | "*="           { start_line true; mkassign Ast.Mul lexbuf }
  | "/="           { start_line true; mkassign Ast.Div lexbuf }
  | "%="           { start_line true; mkassign Ast.Mod lexbuf }

  | "&="           { start_line true; mkassign Ast.And lexbuf }
  | "|="           { start_line true; mkassign Ast.Or lexbuf }
  | "^="           { start_line true; mkassign Ast.Xor lexbuf }
  | ">?="           { start_line true; mkassign Ast.Max lexbuf }
  | "<?="           { start_line true; mkassign Ast.Min lexbuf }

  | "<<="          { start_line true; mkassign Ast.DecLeft lexbuf }
  | ">>="          { start_line true; mkassign Ast.DecRight lexbuf }

  | ":"            { start_line true; TDotDot (get_current_line_type lexbuf) }

  | "=="           { start_line true; TEqEq    (get_current_line_type lexbuf) }
  | "!="           { start_line true; TNotEq   (get_current_line_type lexbuf) }
  | ">="           { start_line true;
		     TLogOp(Ast.SupEq,get_current_line_type lexbuf) }
  | "<="           { start_line true;
		     TLogOp(Ast.InfEq,get_current_line_type lexbuf) }
  | "<"            { start_line true;
		     TLogOp(Ast.Inf,get_current_line_type lexbuf) }
  | ">"            { start_line true;
		     TLogOp(Ast.Sup,get_current_line_type lexbuf) }

  | "&&"           { start_line true; TAndLog (get_current_line_type lexbuf) }
  | "||"           { start_line true; TOrLog  (get_current_line_type lexbuf) }

  | ">>"           { start_line true;
		     TShROp(Ast.DecRight,get_current_line_type lexbuf) }
  | "<<"           { start_line true;
		     TShLOp(Ast.DecLeft,get_current_line_type lexbuf) }

  | "&"            { if not (!col_zero)
                     then (start_line true; TAnd(get_current_line_type lexbuf))
                     else (start_line true;
	                   check_context_linetype (tok lexbuf);
		           TAnd0 ("&",get_current_line_type lexbuf))}
  | "\\&"          { start_line true;
	             TAnd0 ("\\&",contextify(get_current_line_type lexbuf)) }

  | "^"            { start_line true; TXor(get_current_line_type lexbuf) }

  | "##"            { start_line true; TCppConcatOp }
  | (("#" [' ' '\t']*  "undef" ([' ' '\t']+ as wss)) as def)
    ((letter (letter |digit)*) as ident)
      { start_line true;
	let (arity,line,lline,llend,offset,col,strbef,straft,pos,ws) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	(* -1 in the code below because the ident is not at the line start *)
	TUndef
	  (lt,
	   check_var ident
	     (arity,line,lline,llend,offset+off,col+off,[],[],[],wss)) }
  | (( ("#" [' ' '\t']*  "define" ([' ' '\t']+ as wss))) as def)
    ( (letter (letter |digit)*) as ident)
      { start_line true;
	let (arity,line,lline,llend,offset,col,strbef,straft,pos,ws) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	(* -1 in the code below because the ident is not at the line start *)
	TDefine
	  (lt,
	   check_var ident
	     (arity,line,lline,llend,offset+off,col+off,[],[],[],wss)) }
  | (( ("#" [' ' '\t']*  "define" ([' ' '\t']+ as wss))) as def)
    ( (letter (letter | digit)*) as ident)
    '('
      { start_line true;
	let (arity,line,lline,llend,offset,col,strbef,straft,pos,ws) as lt =
	  get_current_line_type lexbuf in
	let off = String.length def in
	TDefineParam
        (lt,
	 check_var ident
	   (* why pos here but not above? *)
	   (arity,line,lline,llend,offset+off,col+off,strbef,straft,pos,wss),
	 offset + off + (String.length ident),
	 col + off + (String.length ident)) }
  | ("#" [' ' '\t']*  "pragma")
      { start_line true; TPragma(get_current_line_type lexbuf) }

    (* For the unparser: in TIncludeL and TIncludeNL, the whitespace after
     * #include is not preserved, because we have nowhere to put it. *)
  | "#" [' ' '\t']* "include" [' ' '\t']* '\"' [^ '\"']+ '\"'
      { TIncludeL
	  (let str = tok lexbuf in
	  let start = String.index str '\"' in
	  let finish = String.rindex str '\"' in
	  start_line true;
	  (process_include start finish str, get_current_line_type lexbuf)) }
  | "#" [' ' '\t']* "include" [' ' '\t']* '<' [^ '>']+ '>'
      { TIncludeNL
	  (let str = tok lexbuf in
	  let start = String.index str '<' in
	  let finish = String.rindex str '>' in
	  start_line true;
	  (process_include start finish str,get_current_line_type lexbuf)) }
  | "#" [' ' '\t']* "include" [' ' '\t']* "..."
      { check_minus_context_linetype (tok lexbuf);
	start_line true;
	TIncludeAny("...",get_current_line_type lexbuf) }
  | "#" [' ' '\t']* "include" (* changes not supported *)
      { check_context_linetype (tok lexbuf);
	start_line true;
	TInclude(get_current_line_type lexbuf) }
  | "#" [' ' '\t']* "if" [^'\n']*
  | "#" [' ' '\t']* "ifdef" [^'\n']*
  | "#" [' ' '\t']* "ifndef" [^'\n']*
  | "#" [' ' '\t']* "else" [^'\n']*
  | "#" [' ' '\t']* "elif" [^'\n']*
  | "#" [' ' '\t']* "endif" [^'\n']*
  | "#" [' ' '\t']* "error" [^'\n']*
  | "#" [' ' '\t']* "line" [^'\n']*
      { start_line true; check_plus_linetype (tok lexbuf);
	TDirective (Ast.Noindent(tok lexbuf), get_current_line_type lexbuf) }

  | "\\" ('\n' | "\r\n")
      { start_line true;
	let res = TCppEscapedNewline (get_current_line_type lexbuf) in
	reset_line lexbuf;
	res }

  | "/*"
      {
       match !current_line_type with
        (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
        start_line true;
	(* second argument to TDirective is not quite right, because
	   it represents only the first token of the comment, but that
	   should be good enough *)
	TDirective (Ast.Indent("/*"^(comment check_comment lexbuf)),
		 get_current_line_type lexbuf)
      |	_ -> let _ = comment (fun _ -> ()) lexbuf in token lexbuf }
  | "---" [^'\n']*
      { (if !current_line_started
      then lexerr "--- must be at the beginning of the line" "");
	start_line true;
	TMinusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) }
  | "+++" [^'\n']*
      { (if !current_line_started
      then lexerr "+++ must be at the beginning of the line" "");
	start_line true;
	TPlusFile
	  (let str = tok lexbuf in
	  (drop_spaces(String.sub str 3 (String.length str - 3)),
	   (get_current_line_type lexbuf))) }

  | letter (letter | digit)*
      { start_line true; id_tokens lexbuf }

      (* christia: testing *)
  | (letter | '$') (letter | digit | '$') *
      { start_line true; id_tokens lexbuf }

  | (letter | '$') (letter | digit | '$') *
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?
    ("::~" (letter | '$') (letter | digit | '$') *
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?) +

      {
	start_line true;
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf
      }
  | ((letter | '$') (letter | digit | '$') * )
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>')

      {
	start_line true;
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf
      }

  | (((letter | '$') (letter | digit | '$') * ))
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?
    "::" (((letter | '$') (letter | digit | '$') * ))
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?
    ("::" ((letter | '$') (letter | digit | '$') * )
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?) *

      {
	start_line true;
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf
      }

   | "::" ((letter | '$') (letter | digit | '$') * )
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?
    ("::" ((letter | '$') (letter | digit | '$') * )
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?) *
      {
	start_line true;
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf
      }
       (* christia: end *)


  | "'" { start_line true;
	  TChar(char lexbuf,get_current_line_type lexbuf) }
  | '\"' { start_line true;
	  TString(string lexbuf,(get_current_line_type lexbuf)) }
  | (real as x)    { start_line true;
		     TFloat(x,(get_current_line_type lexbuf)) }
  | ((( decimal | hexa | octal)
      ( ['u' 'U']
      | ['l' 'L']
      | (['l' 'L'] ['u' 'U'])
      | (['u' 'U'] ['l' 'L'])
      | (['u' 'U'] ['l' 'L'] ['l' 'L'])
      | (['l' 'L'] ['l' 'L'])
      )?
    ) as x) { start_line true; TInt(x,(get_current_line_type lexbuf)) }

  | (decimal ['d' 'D']) as x
      { if !Flag.ibm
      then
	begin
	  start_line true;
	  let len = string_of_int(String.length x - 1) in
          TDecimalCst(x,len,"0",(get_current_line_type lexbuf))
	end
      else failwith "unrecognized constant modifier d/D" }

  | "<=>"          { TIso }
  | "=>"           { TRightIso }

  | eof            { EOF }

  | _ { lexerr "unrecognised symbol, in token rule: " (tok lexbuf) }

(* This is for both SmPL and script metavariable declarations *)
and metavariable_decl_token = parse
  | [' ' '\t']* ['\n' '\r' '\011' '\012']
    { reset_line lexbuf; metavariable_decl_token lexbuf }

  | [' ' '\t'  ]+ {
    start_line false; metavariable_decl_token lexbuf }

  | [' ' '\t'  ]* ("//" [^ '\n']*) {
    start_line false; metavariable_decl_token lexbuf }

  | "@@" { start_line true; TArobArob }

  | "=~" { start_line true; TTildeEq (get_current_line_type lexbuf) }
  | "!~" { start_line true; TTildeExclEq (get_current_line_type lexbuf) }
  | "="  { start_line true; TEq (get_current_line_type lexbuf) }
  | "(" { start_line true; TOPar (get_current_line_type lexbuf) }
  | ")" { start_line true; TCPar (get_current_line_type lexbuf) }

  | '[' { start_line true; TOCro (get_current_line_type lexbuf)   }
  | ']' { start_line true; TCCro (get_current_line_type lexbuf)   }
  | '{' { start_line true; TOBrace (get_current_line_type lexbuf) }
  | '}' { start_line true; TCBrace (get_current_line_type lexbuf) }

  | "->"           { start_line true; TPtrOp (get_current_line_type lexbuf)  }
  | '.'            { start_line true; TDot (get_current_line_type lexbuf)    }
  | ','            { start_line true; TComma (get_current_line_type lexbuf)  }
  | ";"            { start_line true;
		     TMPtVirg (* works better with tokens_all *) }
  | "<<"           { start_line true;
		     TShLOp(Ast.DecLeft,get_current_line_type lexbuf) }

  | '*'            { pass_zero();
		     if !current_line_started
		     then
		       (start_line true; TMul (get_current_line_type lexbuf))
		     else
		       (patch_or_match MATCH;
			add_current_line_type D.MINUS;
			metavariable_decl_token lexbuf) }

  | "=="           { start_line true; TEqEq    (get_current_line_type lexbuf) }
  | "!="           { start_line true; TNotEq   (get_current_line_type lexbuf) }
  | "<="           { start_line true; TSub     (get_current_line_type lexbuf) }
  | "+" { (start_line true; TPlus (get_current_line_type lexbuf)) }
  | "-" { (start_line true; TMinus (get_current_line_type lexbuf)) }
  | "/" { start_line true; TDmOp (Ast.Div,get_current_line_type lexbuf) }
  | "%" { start_line true; TDmOp (Ast.Mod,get_current_line_type lexbuf) }
  | ">>" { start_line true; TShROp(Ast.DecRight,get_current_line_type lexbuf) }
  | "&" { start_line true; TAnd (get_current_line_type lexbuf) }
  | "|" {  (start_line true; TOr(get_current_line_type lexbuf)) }
  | "^" { start_line true; TXor(get_current_line_type lexbuf) }
  | ">=" { start_line true; TLogOp(Ast.SupEq,get_current_line_type lexbuf) }
  | "<" { start_line true; TLogOp(Ast.Inf,get_current_line_type lexbuf) }
  | ">" { start_line true; TLogOp(Ast.Sup,get_current_line_type lexbuf) }
  | "&&" { start_line true; TAndLog (get_current_line_type lexbuf) }
  | "||" { start_line true; TOrLog (get_current_line_type lexbuf) }
  | ":"  { start_line true; TDotDot (get_current_line_type lexbuf) }
  | "-="           { start_line true; mkassign Ast.Minus lexbuf }
  | "+="           { start_line true; mkassign Ast.Plus lexbuf }
  | "*="           { start_line true; mkassign Ast.Mul lexbuf }
  | "/="           { start_line true; mkassign Ast.Div lexbuf }
  | "%="           { start_line true; mkassign Ast.Mod lexbuf }
  | "&="           { start_line true; mkassign Ast.And lexbuf }
  | "|="           { start_line true; mkassign Ast.Or lexbuf }
  | "^="           { start_line true; mkassign Ast.Xor lexbuf }
  | "<<="          { start_line true; mkassign Ast.DecLeft lexbuf }
  | ">>="          { start_line true; mkassign Ast.DecRight lexbuf }
  | "/*"
      {match !current_line_type with
        (D.PLUS,_,_) | (D.PLUSPLUS,_,_) ->
        start_line true;
	(* second argument to TDirective is not quite right, because
	   it represents only the first token of the comment, but that
	   should be good enough *)
	TDirective (Ast.Indent("/*"^(comment check_comment lexbuf)),
		 get_current_line_type lexbuf)
      |	_ -> let _ = comment (fun _ -> ()) lexbuf in
	     metavariable_decl_token lexbuf }

  | "##"            { start_line true; TCppConcatOp (* for fresh vars *) }

  | letter (letter | digit)*
      { start_line true; id_tokens lexbuf }

      (* christia: testing *)
  | (letter | '$') (letter | digit | '$') *
      { start_line true; id_tokens lexbuf }

  | (letter | '$') (letter | digit | '$') *
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?
    ("::~" (letter | '$') (letter | digit | '$') *
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?) +

      { start_line true;
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf
      }
  | ((letter | '$') (letter | digit | '$') * )
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>')

      { start_line true;
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "< and > not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf
      }

  | (((letter | '$') (letter | digit | '$') * ))
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?
    "::" (((letter | '$') (letter | digit | '$') * ))
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?
    ("::" ((letter | '$') (letter | digit | '$') * )
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?) *

      { start_line true;
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf
      }

   | "::" ((letter | '$') (letter | digit | '$') * )
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?
    ("::" ((letter | '$') (letter | digit | '$') * )
      ('<' (letter | '$' | '~') (letter | digit | '$' | '~') * '>') ?) *
      { start_line true;
	if not !Flag.c_plus_plus
	then
	  Common.pr2_once
	    "~ and :: not allowed in C identifiers, try -c++ option";
	id_tokens lexbuf
      }
       (* christia: end *)


  | "'" { start_line true;
	  TChar(char lexbuf,get_current_line_type lexbuf) }
  | '\"' { start_line true;
	  TString(string lexbuf,(get_current_line_type lexbuf)) }
  | (real as x)    { Printf.printf "36\n"; start_line true;
		     TFloat(x,(get_current_line_type lexbuf)) }
  | ((( decimal | hexa | octal)
      ( ['u' 'U']
      | ['l' 'L']
      | (['l' 'L'] ['u' 'U'])
      | (['u' 'U'] ['l' 'L'])
      | (['u' 'U'] ['l' 'L'] ['l' 'L'])
      | (['l' 'L'] ['l' 'L'])
      )?
    ) as x) { start_line true; TInt(x,(get_current_line_type lexbuf)) }

  | (decimal ['d' 'D']) as x
      { if !Flag.ibm
      then
	begin
	  start_line true;
	  let len = string_of_int(String.length x - 1) in
          TDecimalCst(x,len,"0",(get_current_line_type lexbuf))
	end
      else failwith "unrecognized constant modifier d/D" }

  | _ { lexerr
	 "metavariables: unrecognised symbol in metavariable_decl_token rule: "
	 (tok lexbuf) }


and char = parse
  | (_ as x)                           { String.make 1 x ^ restchars lexbuf }
  (* todo?: as for octal, do exception  beyond radix exception ? *)
  | (("\\" (oct | oct oct | oct oct oct)) as x     ) { x ^ restchars lexbuf }
  (* this rule must be after the one with octal, lex try first longest
   * and when \7  we want an octal, not an exn.
   *)
  | (("\\x" ((hex | hex hex))) as x           )      { x ^ restchars lexbuf }
  | (("\\" (_ as v))           as x           )
	{
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '\"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ ->
              Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
	  );
          x ^ restchars lexbuf
	}
  | _
      { Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      }

and restchars = parse
  | "'"                                { "" }
  | (_ as x)                           { String.make 1 x ^ restchars lexbuf }
  (* todo?: as for octal, do exception  beyond radix exception ? *)
  | (("\\" (oct | oct oct | oct oct oct)) as x     ) { x ^ restchars lexbuf }
  (* this rule must be after the one with octal, lex try first longest
   * and when \7  we want an octal, not an exn.
   *)
  | (("\\x" ((hex | hex hex))) as x           )      { x ^ restchars lexbuf }
  | (("\\" (_ as v))           as x           )
	{
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '\"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ ->
              Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
	  );
          x ^ restchars lexbuf
	}
  | _
      { Common.pr2 ("LEXER: unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      }

and string  = parse
  | '\"'                                       { "" }
  | ['\n' '\r' '\011' '\012'] as x
    { line := !line + 1; (Printf.sprintf "%c" x) ^ string lexbuf }
  | (_ as x)                   { Common.string_of_char x ^ string lexbuf }
  | ("\\" (oct | oct oct | oct oct oct)) as x { x ^ string lexbuf }
  | ("\\x" (hex | hex hex)) as x              { x ^ string lexbuf }
  | ("\\" (_ as v)) as x
       {
         (match v with
	    | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
	    | 'f' -> () | 'a' -> ()
	    | '\\' -> () | '?'  -> () | '\'' -> ()  | '\"' -> ()
	    | 'e' -> ()
	    | '\n' -> ()
	    | '(' -> () | '|' -> () | ')' -> ()
	    | _ -> lexerr "unrecognised symbol:" (tok lexbuf)
	 );
          x ^ string lexbuf
       }
  | _ { lexerr "unrecognised symbol: " (tok lexbuf) }

and comment check_comment = parse
  | "*/" { let s = tok lexbuf in check_comment s; start_line true; s }
  | ['\n' '\r' '\011' '\012']
      { let s = tok lexbuf in
        (* even blank line should have a + *)
        check_comment s;
        reset_line lexbuf; s ^ comment check_comment lexbuf }
  | "+" { pass_zero();
	  if !current_line_started
	  then (start_line true;
		let s = tok lexbuf in s^(comment check_comment lexbuf))
	  else (start_line true; comment check_comment lexbuf) }
  (* noteopti: *)
  | [^ '*']
      { let s = tok lexbuf in
        check_comment s; start_line true; s ^ comment check_comment lexbuf }
  | [ '*']
      { let s = tok lexbuf in
        check_comment s; start_line true; s ^ comment check_comment lexbuf }
  | _
      { start_line true; let s = tok lexbuf in
        Common.pr2 ("LEXER: unrecognised symbol in comment:"^s);
        s ^ comment check_comment lexbuf
      }
