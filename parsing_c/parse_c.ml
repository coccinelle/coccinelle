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

module TH = Token_helpers
module LP = Lexer_parser
module IC = Includes_cache

module Stat = Parsing_stat

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_err, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_parsing

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lexbuf_to_strpos lexbuf     =
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)

let token_to_strpos tok =
  (TH.str_of_tok tok, TH.pos_of_tok tok)


let mk_info_item2 filename toks =
  let buf = Buffer.create 100 in
  let s =
    (* old: get_slice_file filename (line1, line2) *)
    begin
      toks +> List.iter (fun tok ->
        match TH.pinfo_of_tok tok with
        | Ast_c.OriginTok _ ->
            Buffer.add_string buf (TH.str_of_tok tok)
        | Ast_c.AbstractLineTok _ ->
            raise (Impossible 79)
        | _ -> ()
      );
      Buffer.contents buf
    end
  in
  (s, toks)

let mk_info_item a b =
  Common.profile_code "C parsing.mk_info_item"
    (fun () -> mk_info_item2 a b)


let info_same_line line xs =
  xs +> List.filter (fun info -> Ast_c.line_of_info info = line)


(* move in cpp_token_c ? *)
let is_define_passed passed =
  let xs = passed +> List.rev +> List.filter TH.is_not_comment in
  if List.length xs >= 2
  then
    (match Common.head_middle_tail xs with
    | Parser_c.TDefine _, _, Parser_c.TDefEOL _ ->
        true
    | _ -> false
    )
  else begin
    pr2_err "WEIRD: length list of error recovery tokens < 2 ";
    false
  end


(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)

let error_msg_tok tok =
  let file = TH.file_of_tok tok in
  if !Flag_parsing_c.verbose_parsing
  then Common.error_message file (token_to_strpos tok)
  else ("error in " ^ file  ^ "; set verbose_parsing for more info")

type line_restriction = Included of int * int | Excluded of int * int

type parse_error_function = int -> Parser_c.token list -> (int * int) ->
    string array -> int -> unit

let parse_error_function : parse_error_function option ref = ref None

let set_parse_error_function f =
   parse_error_function := Some f

let default_parse_error_function : parse_error_function =
  fun line_error _tokens (start_line, end_line) filelines pass ->
    begin
      pr2 ("badcount: " ^ string_of_int (end_line - start_line));

      for i = start_line to end_line do
	let line = filelines.(i) in

	if i = line_error
	then  pr2 ("BAD:!!!!!" ^ " " ^ line)
	else  pr2 ("bad:" ^ " " ^      line)
      done
    end

let print_bad line_error tokens (start_line, end_line) filelines pass =
  let func =
    match !parse_error_function with
      | Some f -> f
      | None -> default_parse_error_function in
  func line_error tokens (start_line, end_line) filelines pass

(*****************************************************************************)
(* Stats on what was passed/commentized  *)
(*****************************************************************************)

let commentized xs = xs +> Common.tail_map_filter (function
  | Parser_c.TCommentCpp (cppkind, ii) ->
      let s = Ast_c.str_of_info ii in
      let legal_passing =
        match !Flag_parsing_c.filter_passed_level with
        | 0 -> false
        | 1 ->
            List.mem cppkind [Token_c.CppAttr]
            ||
            (s =~ "__.*")
        | 2 ->
            List.mem cppkind [Token_c.CppAttr;Token_c.CppPassingNormal]
            ||
            (s =~ "__.*")
        | 3 ->
	    (match cppkind with
	      Token_c.CppAttr | Token_c.CppPassingNormal
            | Token_c.CppDirective | Token_c.CppIfDirective _ -> true
	    | _ -> false)
            ||
            (s =~ "__.*")
        | 4 ->
            List.mem cppkind
	      [Token_c.CppAttr;Token_c.CppPassingNormal;Token_c.CppMacro]
            ||
            (s =~ "__.*")
        | 5 ->
	    (match cppkind with
	      Token_c.CppAttr | Token_c.CppPassingNormal
            | Token_c.CppDirective | Token_c.CppIfDirective _
            | Token_c.CppMacro -> true
	    | _ -> false)
            ||
            (s =~ "__.*")
        | _ -> failwith "not valid level passing number"
      in
      if legal_passing then None else Some (ii.Ast_c.pinfo)

        (*
        | Ast_c.CppOther ->
            (match s with
            | s when s =~ "KERN_.*" -> None
            | s when s =~ "__.*" -> None
            | _ ->
                Some (ii.Ast_c.pinfo)
            )
        *)

  | Parser_c.TCommentMisc ii
  | Parser_c.TAction ii
    ->
      Some (ii.Ast_c.pinfo)
  | _ ->
      None
 )

let count_lines_commentized xs =
  let line = ref (-1) in
  let count = ref 0 in
  begin
    commentized xs +>
    List.iter
      (function
	  Ast_c.OriginTok pinfo | Ast_c.ExpandedTok (_,(pinfo,_)) ->
	    let newline = pinfo.Common.line in
	    if newline <> !line
	    then begin
              line := newline;
              incr count
	    end
	| _ -> ());
    !count
  end

let print_commentized xs =
  let line = ref (-1) in
  begin
    let ys = commentized xs in
    ys +>
    List.iter
      (function
	  Ast_c.OriginTok pinfo | Ast_c.ExpandedTok (_,(pinfo,_)) ->
	    let newline = pinfo.Common.line in
	    let s = pinfo.Common.str in
	    let s = Str.global_substitute
		(Str.regexp "\n") (fun s -> "") s
	    in
	    if newline = !line
	    then pr2_no_nl (s ^ " ")
	    else begin
	      let str =
		Printf.sprintf "%s:%d: passed:"
		  pinfo.Common.file newline in
              if !line = -1
              then pr2_no_nl str
              else pr2_no_nl ("\n"^str);
              line := newline;
              pr2_no_nl (s ^ " ");
	    end
	| _ -> ());
    if ys<>[] then pr2 "";
  end

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* called by parse_print_error_heuristic *)
let tokens2 file =
  let is_abstract_line_tok tok =
    let ii = TH.info_of_tok tok in
    match ii.Ast_c.pinfo with
      | Ast_c.AbstractLineTok _ -> true
      | _ -> false
  in
 Common.with_open_infile file (fun chan ->
  let lexbuf = Lexing.from_channel chan in
  let curp = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file } in
  let lexbuf = { lexbuf with Lexing.lex_curr_p = curp } in
  try
    let rec tokens_aux acc =
      let tok = Lexer_c.token lexbuf in
      if is_abstract_line_tok tok then failwith "should not occur";
      if TH.is_eof tok
      then List.rev (tok::acc)
      else tokens_aux (tok::acc)
    in
    tokens_aux []
  with
    | Lexer_c.Lexical s ->
        failwith ("lexical error " ^ s ^ "\n =" ^
                  (Common.error_message file (lexbuf_to_strpos lexbuf)))
    | e -> raise e
 )

let time_lexing ?(profile=true) a =
  if profile
  then Common.profile_code_exclusif "LEXING" (fun () -> tokens2 a)
  else tokens2 a
let tokens ?profile a = time_lexing ?profile a

let tokens_of_string string infos =
  let lexbuf = Lexing.from_string string in
  (match infos with
    None -> ()
  | Some pos ->
      lexbuf.Lexing.lex_abs_pos <- pos.Lexing.pos_cnum;
      lexbuf.Lexing.lex_curr_p <- pos);
  try
    let rec tokens_s_aux () =
      let tok = Lexer_c.token lexbuf in
      if TH.is_eof tok
      then [tok]
      else tok::(tokens_s_aux ())
    in
    tokens_s_aux ()
  with
    | Lexer_c.Lexical s -> failwith ("lexical error " ^ s ^ "\n =" )
    | e -> raise e


(*****************************************************************************)
(* Parsing, but very basic, no more used *)
(*****************************************************************************)

(*
 * !!!Those function use refs, and are not reentrant !!! so take care.
 * It use globals defined in Lexer_parser.
 *
 * update: because now lexer return comments tokens, those functions
 * may not work anymore.
 *)

let parse file =
  let lexbuf = Lexing.from_channel (open_in file) in
  let result = Parser_c.main Lexer_c.token lexbuf in
  result

(*****************************************************************************)
(* Parsing subelements, useful to debug parser *)
(*****************************************************************************)

(*
 * !!!Those function use refs, and are not reentrant !!! so take care.
 * It use globals defined in Lexer_parser.
 *)

(** Converts occurrences of the identifier ["defined"] in a token stream,
 * into the CPP defined operator [Tdefined].
 *
 * @author Iago Abal
 *)
let fix_cpp_defined_operator =
  List.map (function
    | Parser_c.TIdent("defined",info) -> Parser_c.Tdefined(info)
    | x                               -> x
    )

(* old:
 *   let parse_gen parsefunc s =
 *     let lexbuf = Lexing.from_string s in
 *     let result = parsefunc Lexer_c.token lexbuf in
 *     result
 *)

let parse_gen ~cpp ~tos parsefunc infos s =
  let toks = tokens_of_string s infos +> List.filter TH.is_not_comment in
  let toks' =
    if cpp
    (* We have fix_tokens_define that relaces \\\n by [TCommentSpace]
     * within #define and other CPP directives, but
     * a) it's not clear to me where [TCommentSpace] gets removed;
     * b) TH.filter_out_escaped_newline is simple but enough.
     * /Iago
     *)
    then fix_cpp_defined_operator (TH.filter_out_escaped_newline toks)
    else toks
  in

  (* Why use this lexing scheme ? Why not classically give lexer func
   * to parser ? Because I now keep comments in lexer. Could
   * just do a simple wrapper that when comment ask again for a token,
   * but maybe simpler to use cur_tok technique.
   *)
  let all_tokens = ref toks' in
  let cur_tok    = ref (List.hd !all_tokens) in

  let type_start = ref tos in

  let lexer_function =
    (fun _ ->
      if TH.is_eof !cur_tok
      then (pr2_err "LEXER: ALREADY AT END"; !cur_tok)
      else
        let v = Common.pop2 all_tokens in
        let v = match v with
        | Parser_c.TIdent (s, ii) ->
            if (* an id at the start of a type must be a type name *)
              (LP.is_typedef s || !type_start) &&
              not (!Flag_parsing_c.disable_add_typedef)
	    then Parser_c.TypedefIdent (s, ii)
            else Parser_c.TIdent (s, ii)
        | x -> x in
	type_start := false;
        cur_tok := v;
        !cur_tok
    )
  in
  let lexbuf_fake = Lexing.from_function (fun buf n -> raise (Impossible 80)) in
  let result = parsefunc lexer_function lexbuf_fake in
  result

(* Please DO NOT remove this code, even though most of it is not used *)
let type_of_string s     = parse_gen ~cpp:false ~tos:true Parser_c.type_name None s
let statement_of_string  = parse_gen ~cpp:false ~tos:false Parser_c.statement None
let expression_of_string = parse_gen ~cpp:false ~tos:false Parser_c.expr None
let cpp_expression_of_string = parse_gen ~cpp:true ~tos:false Parser_c.expr

(* ex: statement_of_string "(struct us_data* )psh->hostdata = NULL;" *)





(*****************************************************************************)
(* Parsing default define macros, usually in a standard.h file *)
(*****************************************************************************)

let extract_macros2 file =
  Common.save_excursion Flag_parsing_c.verbose_lexing (fun () ->
    Flag_parsing_c.verbose_lexing := false;
    let toks = tokens ~profile:false file in
    let toks = Parsing_hacks.fix_tokens_define toks in
    Cpp_token_c.extract_macros toks
  )

let extract_macros a =
  Common.profile_code_exclusif "HACK" (fun () -> extract_macros2 a)


(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)


(* The use of local refs (remaining_tokens, passed_tokens, ...) makes
 * possible error recovery. Indeed, they allow to skip some tokens and
 * still be able to call again the ocamlyacc parser. It is ugly code
 * because we cannot modify ocamllex and ocamlyacc. As we want some
 * extended lexing tricks, we have to use such refs.
 *
 * Those refs are now also used for my lalr(k) technique. Indeed They
 * store the futur and previous tokens that were parsed, and so
 * provide enough context information for powerful lex trick.
 *
 * - passed_tokens_last_ckp stores the passed tokens since last
 *   checkpoint. Used for NotParsedCorrectly and also to build the
 *   info_item attached to each program_element.
 * - passed_tokens_clean is used for lookahead, in fact for lookback.
 * - remaining_tokens_clean is used for lookahead. Now remaining_tokens
 *   contain some comments and so would make pattern matching difficult
 *   in lookahead. Hence this variable. We would like also to get rid
 *   of cpp instruction because sometimes a cpp instruction is between
 *   two tokens and makes a pattern matching fail. But lookahead also
 *   transform some cpp instruction (in comment) so can't remove them.
 *
 * So remaining_tokens, passed_tokens_last_ckp contain comment-tokens,
 * whereas passed_tokens_clean and remaining_tokens_clean does not contain
 * comment-tokens.
 *
 * Normally we have:
 * toks = (reverse passed_tok) ++ cur_tok ++ remaining_tokens
 *    after the call to pop2.
 * toks = (reverse passed_tok) ++ remaining_tokens
 *     at the and of the lexer_function call.
 * At the very beginning, cur_tok and remaining_tokens overlap, but not after.
 * At the end of lexer_function call,  cur_tok  overlap  with passed_tok.
 *
 * convention: I use "tr"  for "tokens refs"
 *
 * I now also need this lexing trick because the lexer return comment
 * tokens.
 *)

type tokens_state = {
  mutable rest :         Parser_c.token list;
  mutable rest_clean :   Parser_c.token list;
  mutable current :      Parser_c.token;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed :       Parser_c.token list;
  mutable passed_clean : Parser_c.token list;
}

let mk_tokens_state toks =
  {
    rest       = toks;
    rest_clean = (toks +> List.filter TH.is_not_comment);
    current    = (List.hd toks);
    passed = [];
    passed_clean = [];
  }



let clone_tokens_state tr =
  { rest = tr.rest;
    rest_clean = tr.rest_clean;
    current = tr.current;
    passed = tr.passed;
    passed_clean = tr.passed_clean;
  }
let copy_tokens_state ~src ~dst =
  dst.rest <- src.rest;
  dst.rest_clean <- src.rest_clean;
  dst.current <- src.current;
  dst.passed <- src.passed;
  dst.passed_clean <-  src.passed_clean;
  ()

(* todo? agglomerate the x##b ? *)
let rec filter_noise n xs =
  match n, xs with
  | _, [] -> []
  | 0, xs -> xs
  | n, x::xs ->
      (match x with
      | Parser_c.TMacroAttr _ ->
          filter_noise (n-1) xs
      | _ ->
          x::filter_noise (n-1) xs
      )

let clean_for_lookahead xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | x::xs ->
      x::filter_noise 10 xs

(* drops the first complete #define/#undefine - comment like *)
let extend_passed_clean v xs =
  let rec loop = function
      [] -> []
    | (Parser_c.TDefine _| Parser_c.TUndef _) :: rest -> rest
    | x::xs -> loop xs in
  match v with
    Parser_c.TDefEOL _ ->  loop xs
  | v -> v :: xs

(* Hacked lex. This function use refs passed by parse_print_error_heuristic
 * tr means token refs.
 *)
let in_exec = ref false

let rec lexer_function ~pass tr = fun lexbuf ->
  match tr.rest with
  | [] -> pr2_err "ALREADY AT END"; tr.current
  | v::xs ->
    tr.rest <- xs;
    tr.current <- v;

    if !Flag_parsing_c.debug_lexer then Common.pr2_gen v;

    if TH.is_comment v
    then begin
      tr.passed <- v::tr.passed;
      lexer_function ~pass tr lexbuf
    end
    else begin
      let x = List.hd tr.rest_clean  in
      tr.rest_clean <- List.tl tr.rest_clean;
      assert (x = v);

      (* ignore exec code *)
      (match v with
	Parser_c.Texec _ -> in_exec := true
      |	Parser_c.TPtVirg _ -> if !in_exec then in_exec := false
      |	_ -> ());

      let check_for_drop v counter msg =
        if not (LP.current_context () = LP.InTopLevel) &&
          (!Flag_parsing_c.cpp_directive_passing || (pass >= 2))
        then begin
          incr counter;
          pr2_once (msg^": inside function, I treat it as comment");
          let v' =
	    Parser_c.TCommentCpp (Token_c.CppDirective,TH.info_of_tok v)
          in
          tr.passed <- v'::tr.passed;
          tr.rest       <- Parsing_hacks.comment_until_defeol tr.rest;
          tr.rest_clean <- Parsing_hacks.drop_until_defeol tr.rest_clean;
          lexer_function ~pass tr lexbuf
        end
        else begin
          tr.passed <- v::tr.passed;
          tr.passed_clean <- extend_passed_clean v tr.passed_clean;
          v
        end in

      (match v with

      (* fix_define1.
       *
       * Why not in parsing_hacks lookahead and do passing like
       * I do for some ifdef directives ? Because here I also need to
       * generate some tokens sometimes and so I need access to the
       * tr.passed, tr.rest, etc.
       *)
      | Parser_c.TDefine (tok) -> check_for_drop v Stat.nDefinePassing "CPP-DEFINE"

      | Parser_c.TUndef (tok) -> check_for_drop v Stat.nUndefPassing "CPP-UNDEF"

      | Parser_c.TInclude (includes, filename, inifdef, info) ->
          if not (LP.current_context () = LP.InTopLevel)  &&
            (!Flag_parsing_c.cpp_directive_passing || (pass >= 2))
          then begin
            incr Stat.nIncludePassing;
            pr2_once ("CPP-INCLUDE: inside function, I treat it as comment");
            let v = Parser_c.TCommentCpp(Token_c.CppDirective, info) in
            tr.passed <- v::tr.passed;
            lexer_function ~pass tr lexbuf
          end
          else begin
            let (v,new_tokens) =
              Parsing_hacks.tokens_include(info, includes, filename, inifdef) in
            let new_tokens_clean =
              new_tokens +> List.filter TH.is_not_comment  in

            tr.passed <- v::tr.passed;
            tr.passed_clean <- extend_passed_clean v tr.passed_clean;
            tr.rest <- new_tokens @ tr.rest;
            tr.rest_clean <- new_tokens_clean @ tr.rest_clean;
            v
          end

      | Parser_c.TPragma(prag) -> check_for_drop v Stat.nPragmaPassing "CPP-PRAGMA"

      | _ ->

          (* typedef_fix1 *)
          let v = match v with
            | Parser_c.TIdent (s, ii) ->
                if LP.is_typedef s &&
                  not (!Flag_parsing_c.disable_add_typedef) &&
                  pass = 1
                then Parser_c.TypedefIdent (s, ii)
                else Parser_c.TIdent (s, ii)
            | x -> x
          in

          let passed_before = filter_noise 10 tr.passed_clean in

          let v =
	    if !in_exec
	    then v
	    else
	      let other _ =
		Parsing_hacks.lookahead ~pass
		  (clean_for_lookahead (v::tr.rest_clean))
		  passed_before in
	      match v with
		Parser_c.TIdent(s,ii) ->
		  let res =
		    try Some(Data.get_special_name s)
		    with _ -> None in
		  (match res with
		    Some Data.Attr -> Parser_c.TMacroAttr(s,ii)
		  | Some Data.AttrArgs -> Parser_c.TMacroAttrArgs(s,ii)
		  | Some Data.Declarer -> Parser_c.TMacroDecl(s, ii)
		  | Some Data.Iterator -> Parser_c.TMacroIterator(s, ii)
		  | Some Data.CommaInit ->
		      tr.rest <- Parsing_hacks.fix_comma_init ii tr.rest;
		      tr.rest_clean <- Parsing_hacks.fix_comma_init ii tr.rest_clean;
		      v
		  | _ -> other())
	      | _ -> other() in

          tr.passed <- v::tr.passed;

          (* the lookahead may have changed the status of the token and
           * consider it as a comment, for instance some #include are
           * turned into comments, hence this code. *)
          match v with
          | Parser_c.TCommentCpp _ -> lexer_function ~pass tr lexbuf
          | v ->
              tr.passed_clean <- extend_passed_clean v tr.passed_clean;
              v
      )
    end


let max_pass = 4

let get_one_elem ~pass tr =

  if not (LP.is_enabled_typedef()) && !Flag_parsing_c.debug_typedef
  then pr2_err "TYPEDEF:_handle_typedef=false. Not normal if don't come from exn";

  (* normally have to do that only when come from an exception in which
   * case the dt() may not have been done
   * TODO but if was in scoped scope ? have to let only the last scope
   * so need do a LP.lexer_reset_typedef ();
   *)
  LP.enable_typedef();
  LP._lexer_hint := (LP.default_hint ());
  LP.save_typedef_state();

  tr.passed <- [];

  let lexbuf_fake = Lexing.from_function (fun buf n -> raise (Impossible 81)) in

  (try
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Common.profile_code_exclusif "YACC" (fun () ->
	Left (Parser_c.celem (lexer_function ~pass tr) lexbuf_fake)
      )
    with e ->
      LP.restore_typedef_state();

      (* must keep here, before the code that adjusts the tr fields *)
      let line_error = TH.line_of_tok tr.current in
      let col_error = TH.col_of_tok tr.current in

      let passed_before_error = tr.passed in
      let current = tr.current in
      (*  error recovery, go to next synchro point *)
      let (passed', rest') =
	Parsing_recovery_c.find_next_synchro ~next:tr.rest ~already_passed:tr.passed in
      tr.rest <- rest';
      tr.passed <- passed';

      tr.current <- List.hd passed';
      tr.passed_clean <- [];           (* enough ? *)
      (* with error recovery, rest and rest_clean may not be in sync *)
      tr.rest_clean <- (tr.rest +> List.filter TH.is_not_comment);


      let info_of_bads = Common.map_eff_rev TH.info_of_tok tr.passed in
      Right (info_of_bads, line_error, col_error,
            tr.passed, passed_before_error,
            current, e, pass)
  )



(* Macro problem recovery *)
(* used by the multi-pass error recovery expand-on-demand *)
(*
val candidate_macros_in_passed:
  defs: (string, define_def) Hashtbl.t ->
  Parser_c.token list -> (string * define_def) list
*)

let candidate_macros_in_passed2 ~defs passed  =
  let res = ref [] in
  let res2 = ref [] in

  passed +> List.iter (function
  | Parser_c.TIdent (s,_)
   (* bugfix: may have to undo some inferred things *)
  | Parser_c.TMacroIterator (s,_)
  | Parser_c.TMacroDecl (s,_)
  | Parser_c.TMacroString (s,_)
  | Parser_c.TMacroStmt (s,_)
  | Parser_c.TMacroAttr (s,_)
  | Parser_c.TMacroIdentBuilder (s,_)
  | Parser_c.TypedefIdent (s,_)
    ->
      (match Common.hfind_option s defs with
      | Some def ->
          if s ==~ Parsing_hacks.regexp_macro
          then
            (* pr2 (spf "candidate: %s" s); *)
            Common.push2 (s, def) res
          else
            Common.push2 (s, def) res2
        | None -> ()
        )

  | _ -> ()
  );
  if !res = []
  then !res2
  else !res

let candidate_macros_in_passed ~defs b =
  Common.profile_code "MACRO management" (fun () ->
    candidate_macros_in_passed2 ~defs b)





let find_optional_macro_to_expand2 ~defs pos toks =

  let defs = Common.hash_of_list defs in

  let is_hintbody = function
    | (_, _, Cpp_token_c.DefineHintBody _) -> true
    | _ -> false
  in


  let toks = toks +> Common.tail_map (function

    (* special cases to undo *)
    | Parser_c.TMacroIterator (s, ii) ->
        if Hashtbl.mem defs s
        then Parser_c.TIdent (s, ii)
        else Parser_c.TMacroIterator (s, ii)

    | Parser_c.TMacroDecl (s, ii) ->
        if try is_hintbody (Hashtbl.find defs s) with Not_found -> false
        then Parser_c.TIdent (s, ii)
        else Parser_c.TMacroDecl (s, ii)

    | Parser_c.TMacroString (s, ii) ->
        if try is_hintbody (Hashtbl.find defs s) with Not_found -> false
        then Parser_c.TIdent (s, ii)
        else Parser_c.TMacroString (s, ii)

    | Parser_c.TMacroStmt (s, ii) ->
        if try is_hintbody (Hashtbl.find defs s) with Not_found -> false
        then Parser_c.TIdent (s, ii)
        else Parser_c.TMacroStmt (s, ii)

    | Parser_c.TMacroAttr (s, ii) ->
        if try is_hintbody (Hashtbl.find defs s) with Not_found -> false
        then Parser_c.TIdent (s, ii)
        else Parser_c.TMacroAttr (s, ii)

    | Parser_c.TMacroIdentBuilder (s, ii) ->
        if try is_hintbody (Hashtbl.find defs s) with Not_found -> false
        then Parser_c.TIdent (s, ii)
        else Parser_c.TMacroIdentBuilder (s, ii)

    | Parser_c.TypedefIdent (s, ii) ->
        if Hashtbl.mem defs s
        then Parser_c.TIdent (s, ii)
        else Parser_c.TypedefIdent (s, ii)

    | x -> x
  ) in

  let tokens = toks in
  Parsing_hacks.fix_tokens_cpp ~macro_defs:defs pos tokens

  (* just calling apply_macro_defs and having a specialized version
   * of the code in fix_tokens_cpp is not enough as some work such
   * as the passing of the body of attribute in Parsing_hacks.find_macro_paren
   * will not get the chance to be run on the new expanded tokens.
   * Hence even if it's expensive, it's currently better to
   * just call directly fix_tokens_cpp again here.

  let tokens2 = ref (tokens +> Common.acc_map TV.mk_token_extended) in
  let cleaner = !tokens2 +> Parsing_hacks.filter_cpp_stuff in
  let paren_grouped = TV.mk_parenthised  cleaner in
  Cpp_token_c.apply_macro_defs
    ~msg_apply_known_macro:(fun s -> pr2 (spf "APPLYING: %s" s))
    ~msg_apply_known_macro_hint:(fun s -> pr2 "hint")
    defs pos paren_grouped;
  (* because the before field is used by apply_macro_defs *)
  tokens2 := TV.rebuild_tokens_extented !tokens2;
  Parsing_hacks.insert_virtual_positions
    (!tokens2 +> Common.acc_map (fun x -> x.TV.tok))
  *)
let find_optional_macro_to_expand ~defs pos a =
    Common.profile_code "MACRO management" (fun () ->
      find_optional_macro_to_expand2 ~defs pos a)

(*****************************************************************************)
(* Parsing #if guards *)
(*****************************************************************************)

(** Traverses the syntax tree parsing #if guard strings
  * with a given parsing function.
  *
  * NOTE that whenever the parsing fails, we keep the ifdef_guard unchanged.
  *
  * @author Iago Abal
  *)
let parse_ifdef_guard_visitor
    (parse: Lexing.position option -> string -> Ast_c.expression)
    :Visitor_c.visitor_c_s =
  let v_ifdef_guard = function
      (* Gif_str <string> --parse--> Gif <expression> *)
    | Ast_c.Gif_str (pos,input) ->
        begin
          try Ast_c.Gif (parse (Some pos) input) with
          | Parsing.Parse_error ->
              pr2 ("Unable to parse #if condition: " ^ input);
              Ast_c.Gif_str (pos,input)
        end
    | x                   -> x
  in
  let v_ifdefkind = function
    | Ast_c.Ifdef       ifguard -> Ast_c.Ifdef       (v_ifdef_guard ifguard)
    | Ast_c.IfdefElseif ifguard -> Ast_c.IfdefElseif (v_ifdef_guard ifguard)
    | x                         -> x
  in
  { Visitor_c.default_visitor_c_s with
      Visitor_c.kifdefdirective_s = fun (k,bigf) d ->
        match d with
       | Ast_c.IfdefDirective ((ifkind,tag), ii) ->
           let ifkind' = v_ifdefkind ifkind in
           Ast_c.IfdefDirective ((ifkind',tag), ii)
  }

(** Traverses the syntax tree parsing #if guard strings with [Parse_c.expr].
  *
  * Known issue: [Parse_c.expr] is invoked through [expression_of_string],
  * which does not handle backslash-newlines #if guards. Those guards will
  * be kept unparsed. Possible solution would be to run [fix_tokens_define]
  * on the token stream before parsing.
  *
  * @author Iago Abal
  *)
let parse_ifdef_guards : Ast_c.program -> Ast_c.program =
  Visitor_c.vk_program_s (parse_ifdef_guard_visitor cpp_expression_of_string)



(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let (_defs : (string, Cpp_token_c.define_def) Hashtbl.t ref)  =
  ref (Hashtbl.create 101)

let (_defs_builtins : (string, Cpp_token_c.define_def) Hashtbl.t ref)  =
  ref (Hashtbl.create 101)


(* can not be put in parsing_hack, cos then mutually recursive problem as
 * we also want to parse the standard.h file.
 *)
let init_defs_macros std_h =
  if not (Common.lfile_exists std_h)
  then pr2 ("warning: Can't find default macro file: " ^ std_h)
  else begin
    pr2 ("init_defs: " ^ std_h);
    _defs := Common.hash_of_list (extract_macros std_h);
  end

let init_defs_builtins file_h =
  if not (Common.lfile_exists file_h)
  then pr2 ("warning: Can't find macro file: " ^ file_h)
  else begin
    pr2 ("init_defs_builtins: " ^ file_h);
    _defs_builtins :=
      Common.hash_of_list (extract_macros file_h);
  end



type info_item =  string * Parser_c.token list

type program2 = toplevel2 list
   and extended_program2 = toplevel2 list *
      (string, Lexer_parser.identkind) Common.scoped_h_env (* type defs *) *
      (string, Cpp_token_c.define_def) Hashtbl.t (* macro defs *)
   and toplevel2 = Ast_c.toplevel * info_item

type 'a generic_parse_info = {
  filename : string;
  ranges : line_restriction list option;
  parse_trees : 'a; (* program2 or extended_program2 *)
  statistics : Parsing_stat.parsing_stat;
}

type parse_info = program2 generic_parse_info

type extended_parse_info = extended_program2 generic_parse_info

let program_of_program2 xs =
  xs +> List.map fst

let with_program2 f program2 =
  program2
  +> Common.unzip
  +> (fun (program, infos) ->
    f program, infos
  )
  +> Common.uncurry Common.zip

let with_program2_unit f program2 =
  program2
  +> Common.unzip
  +> (fun (program, infos) ->
    f program
  )


(* note: as now we go in 2 passes, there is first all the error message of
 * the lexer, and then the error of the parser. It is not anymore
 * interwinded.
 *
 * !!!This function use refs, and is not reentrant !!! so take care.
 * It use globals defined in Lexer_parser and also the _defs global
 * in parsing_hack.ml.
 *
 * This function uses internally some semi globals in the
 * tokens_stat record and parsing_stat record.
 *)

module StringSet : Set.S with type elt = string = Set.Make(String)

module StringMap : Map.S with type key = string = Map.Make(String)

let header_cache = Common.create_bounded_cache 0(*disabled 300*) ("",None)

let tree_stack = ref []
let seen_files = ref []

let normalize file =
  let file =
    String.concat "/" (Str.split (Str.regexp_string "/./") file) in
  let pieces = Str.split_delim (Str.regexp "/") file in
  let rec loop prev = function
      [] -> String.concat "/" (List.rev prev)
    | ".."::rest ->
	(match prev with
	  ".."::xs -> loop (".."::prev) rest
	| x::xs -> loop xs rest
	| _ -> loop (".."::prev) rest)
    | x::rest -> loop (x::prev) rest in
  loop [] pieces

let rec _parse_print_error_heuristic2 saved_typedefs saved_macros
  parse_strings cache file use_header_cache =
  let file = normalize file in
  if List.mem file !seen_files
  then None (* Inclusion loop, not re-parsing *)
  else begin
    seen_files := file :: !seen_files;
    let cached_result =
      if false && use_header_cache
      then
	try
	  (match Common.find_bounded_cache header_cache file with
	    None -> failwith "Not possible"
	  | Some (cached,cached_includes) ->
	      List.iter
		(function incl ->
		  handle_include file incl
		    (fun nonlocal header_filename ->
		      _parse_print_error_heuristic2
			saved_typedefs saved_macros parse_strings
			nonlocal header_filename use_header_cache))
		cached_includes;
	      Some cached)
	with Not_found -> None
      else None in
    match cached_result with
      | None ->
        let result =
          _parse_print_error_heuristic2bis saved_typedefs saved_macros
            parse_strings file use_header_cache in
        (if false && use_header_cache && cache
	then
	  let my_includes =
	    let my_includes = ref [] in
	    let cpp (k,bigf) directive =
	      match directive with
		Ast_c.Include incl -> my_includes := incl :: !my_includes
	      | _ -> () in
	    let bigf =
	      { Visitor_c.default_visitor_c with
		Visitor_c.kcppdirective = cpp } in
	    let (pgm,ty,defs) = result.parse_trees in
	    Visitor_c.vk_program bigf (List.map fst pgm);
	    !my_includes in
	  Common.extend_bounded_cache header_cache file
	    (Some (result,my_includes)));
        tree_stack := result :: !tree_stack;
        Some result
      | Some result ->
        tree_stack := result :: !tree_stack;
        Some result
  end

and handle_include file wrapped_incl k =
    let incl = Ast_c.unwrap wrapped_incl.Ast_c.i_include in
    let parsing_style = Includes.get_parsing_style () in
    let f = Includes.resolve file parsing_style incl in
    if Includes.should_parse parsing_style file incl
    then
      match f with
      | Some header_filename when Common.lfile_exists header_filename ->
          if not (!Includes.include_headers_for_types) ||
	     not (IC.has_been_parsed header_filename)
          then
            begin
              IC.add_to_parsed_files header_filename;
              (if !Flag_parsing_c.verbose_includes
              then pr2 ("including "^header_filename));
              let nonlocal =
                match incl with Ast_c.NonLocal _ -> true | _ -> false in
              let res = k nonlocal header_filename in
              match res with
                None -> ()
              | Some x ->
                  let pt = x.parse_trees in
                  let (p, _, _) = pt in
                  with_program2_unit
                    (IC.extract_names header_filename)
                    p
            end;
          IC.add_to_dependency_graph file header_filename;
      | _ -> ()

and _parse_print_error_heuristic2bis saved_typedefs saved_macros
  parse_strings file use_header_cache =
  let stat = Parsing_stat.default_stat file in

  (* -------------------------------------------------- *)
  (* call lexer and get all the tokens *)
  (* -------------------------------------------------- *)

  LP.lexer_reset_typedef saved_typedefs;
  Parsing_hacks.ifdef_paren_cnt := 0;

  let toks_orig = tokens file in
  let toks = Parsing_hacks.fix_tokens_define toks_orig in
  (* expand macros on demand trick, preparation phase *)
  let macros =
    Common.profile_code "MACRO mgmt prep 1" (fun () ->
      let macros =
	match saved_macros with None -> Hashtbl.copy !_defs | Some h -> h in
      (* include also builtins as some macros may generate some builtins too
       * like __decl_spec or __stdcall
       *)
      !_defs_builtins +> Hashtbl.iter (fun s def ->
        Hashtbl.replace macros   s def;
      );
      macros
    )
  in
  Common.profile_code "MACRO mgmt prep 2" (fun () ->
    let local_macros = Cpp_token_c.extract_macros toks in
    local_macros +> List.iter (fun (s, def) ->
      Hashtbl.replace macros   s def;
    );
  );

  let toks = if !Flag_parsing_c.exts_ITU
                then Parsing_hacks.fix_tokens_ifdef toks
                else toks
    in
  let toks =
    Parsing_hacks.fix_tokens_cpp ~macro_defs:!_defs_builtins [] toks in
  let toks =
    if parse_strings
    then Parsing_hacks.fix_tokens_strings toks
    else toks in
  let toks =
    if !Flag.c_plus_plus <> Flag.Off
    then Parsing_hacks.convert_templates toks
    else toks in

  (* List.iter
       (fun t -> Printf.eprintf "tok: %s --- %s\n" (TH.str_of_tok t) (TH.string_of_token t))
       toks; *)

  let tr = mk_tokens_state toks in

  let rec loop acc tr =

    (* todo?: I am not sure that it represents current_line, cos maybe
     * tr.current partipated in the previous parsing phase, so maybe tr.current
     * is not the first token of the next parsing phase. Same with checkpoint2.
     * It would be better to record when we have a } or ; in parser.mly,
     *  cos we know that they are the last symbols of external_declaration2.
     *
     * bugfix: may not be equal to 'file' as after macro expansions we can
     * start to parse a new entity from the body of a macro, for instance
     * when parsing a define_machine() body, cf standard.h
     *)
    let checkpoint = TH.line_of_tok tr.current in
    let checkpoint_file = TH.file_of_tok tr.current in
    Ast_c.reset_nonpos();

    (* call the parser *)
    let elem =
      let pass1 =
        Common.profile_code "Parsing: 1st pass" (fun () ->
          get_one_elem ~pass:1 tr
        ) in
      match pass1 with
      | Left e ->
        begin
          match e with
            | Ast_c.CppTop(Ast_c.Include incl) ->
		handle_include file incl
		  (fun nonlocal header_filename ->
		    _parse_print_error_heuristic2
		      saved_typedefs saved_macros parse_strings
		      nonlocal header_filename use_header_cache)
            | _ -> ()
        end; Left e
      | Right (info,line_err, _, passed, passed_before_error, cur, exn, _) ->
          if !Flag_parsing_c.disable_multi_pass
          then pass1
          else begin
            Common.profile_code "Parsing: multi pass" (fun () ->

            pr2_err "parsing pass2: try again";
            let toks = List.rev passed @ tr.rest in
            let new_tr = mk_tokens_state toks in
            copy_tokens_state ~src:new_tr ~dst:tr;
            let passx = get_one_elem ~pass:2 tr in

            (match passx with
            | Left e -> passx
            | Right (info,line_err,col_err,passed,_,cur,exn,_) ->
                let candidates =
                  candidate_macros_in_passed ~defs:macros passed
                in

                if is_define_passed passed
                then passx
                else begin
                  (* todo factorize code *)

                  pr2_err "parsing pass3: try again";
                  let toks = List.rev passed @ tr.rest in
                  let toks' =
                    find_optional_macro_to_expand ~defs:candidates
		      [(line_err,col_err)] toks in
                  let new_tr = mk_tokens_state toks' in
                  copy_tokens_state ~src:new_tr ~dst:tr;
                  let passx = get_one_elem ~pass:3 tr in

                  (match passx with
                  | Left e -> passx
                  | Right(info,le1,ce1,passed,passed_before_error,cur,exn,_) ->
		      if candidates = [] && line_err = le1 && col_err = ce1
		      then passx (* nothing changed, so don't try again *)
		      else
			begin
			  pr2_err "parsing pass4: try again";

			  let candidates =
                            candidate_macros_in_passed
                              ~defs:macros passed
			  in

			  let toks = List.rev passed @ tr.rest in
			  let toks' =
			    find_optional_macro_to_expand ~defs:candidates
			      (Common.nub [(line_err,col_err);(le1,ce1)])
			      toks in
			  let new_tr = mk_tokens_state toks' in
			  copy_tokens_state ~src:new_tr ~dst:tr;
			  let passx = get_one_elem ~pass:4 tr in
			  passx
			end
                  )
                 end
            )
            )
          end
    in


    (* again not sure if checkpoint2 corresponds to end of bad region *)
    let checkpoint2 = TH.line_of_tok tr.current in (* <> line_error *)
    let checkpoint2_file = TH.file_of_tok tr.current in

    let diffline =
      if (checkpoint_file = checkpoint2_file) && (checkpoint_file = file)
      then (checkpoint2 - checkpoint)
      else 0
        (* TODO? so if error come in middle of something ? where the
         * start token was from original file but synchro found in body
         * of macro ? then can have wrong number of lines stat.
         * Maybe simpler just to look at tr.passed and count
         * the lines in the token from the correct file ?
         *)
    in
    let info = mk_info_item file (List.rev tr.passed) in

    (* some stat updates *)
    stat.Stat.commentized <-
      stat.Stat.commentized + count_lines_commentized (snd info);

    let elem =
      match elem with
      | Left e ->
          stat.Stat.correct <- stat.Stat.correct + diffline;
          e
      | Right (info_of_bads, line_error, col_error, toks_of_bads,
              passed_before_error, cur, exn, pass) ->

          let was_define = is_define_passed tr.passed in

          if was_define && !Flag_parsing_c.filter_msg_define_error
          then ()
          else begin

            (match exn with
            | Lexer_c.Lexical _
            | Parsing.Parse_error
            | Semantic_c.Semantic _ -> ()
            | e -> raise e
            );

            if !Flag_parsing_c.show_parsing_error
            then begin
              (match exn with
              (* Lexical is not anymore launched I think *)
              | Lexer_c.Lexical s ->
                  pr2 ("lexical error " ^s^ "\n =" ^ error_msg_tok cur)
              | Parsing.Parse_error ->
                  pr2 ("parse error \n = " ^ error_msg_tok cur)
              | Semantic_c.Semantic (s, i) ->
                  pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok cur)
              | e -> raise (Impossible 82)
              );
              (* bugfix: *)
              if (checkpoint_file = checkpoint2_file) &&
                checkpoint_file = file
              then
                let filelines =
                  try Common.cat_array file with _ -> raise (Flag.UnreadableFile file) in
		print_bad line_error passed_before_error
		  (checkpoint, checkpoint2) filelines pass
              else pr2 "PB: bad: but on tokens not from original file"
            end;


            let pbline =
              toks_of_bads
              +> Common.filter (TH.is_same_line_or_close line_error)
              +> Common.filter TH.is_ident_like
            in
            let error_info =
              (pbline +> List.map TH.str_of_tok), line_error
            in
            stat.Stat.problematic_lines <-
              error_info::stat.Stat.problematic_lines;

          end;

          if was_define && !Flag_parsing_c.filter_define_error
          then stat.Stat.correct <- stat.Stat.correct + diffline
          else stat.Stat.bad     <- stat.Stat.bad     + diffline;

          Ast_c.NotParsedCorrectly info_of_bads
    in

    (match elem with
    | Ast_c.FinalDef x -> List.rev ((Ast_c.FinalDef x, info)::acc)
    | xs -> loop ((xs, info) :: acc) tr (* recurse *)
    )
  in
  let v = loop [] tr in
  let v = with_program2 Parsing_consistency_c.consistency_checking v in
  with_program2_unit Danger.add_danger v;
  let v =
    if !Flag_parsing_c.ifdef_to_if
       then with_program2 Parsing_hacks.cpp_ifdef_statementize v
       else v
  in
  (* We parse #if guards when --ifdef-to-if is enabled, mostly because
   * I don't see a need (yet) to have yet-another flag. Right now, there
   * is also little interest in parsing #if guards without --ifdef-to-if.
   * Review this decision in the future!
   * / Iago
   *)
  let v =
    if !Flag_parsing_c.ifdef_to_if
       then with_program2 parse_ifdef_guards v
       else v
  in
  let v =
    let new_td = ref (Common.clone_scoped_h_env !LP._typedef) in
    Common.clean_scope_h new_td;
    (v, !new_td, macros) in
  { filename = file; ranges = None; parse_trees = v; statistics = stat }

let parse_print_error_heuristic2 saved_typedefs saved_macros
  parse_strings cache file use_header_cache =
  tree_stack := [];
  seen_files := [];
  ignore
    (_parse_print_error_heuristic2 saved_typedefs saved_macros
      parse_strings cache file use_header_cache);
  match !tree_stack with
    | [] -> assert false
    | tree::trees -> tree_stack := []; (tree, List.rev trees)

let time_total_parsing a b c d e =
  Common.profile_code "TOTAL"
    (fun () -> parse_print_error_heuristic2 a b c d e)

let parse_print_error_heuristic a b c d e =
  Common.profile_code "C parsing" (fun () -> time_total_parsing a b c d e)


(* alias *)
let parse_c_and_cpp parse_strings cache a =
  let v =
    (* no saved typedefs or macros, so can't use header cache *)
    fst (parse_print_error_heuristic None None parse_strings cache a false) in
  let (c, _, _) = v.parse_trees in
  (c, v.statistics)

let parse_c_and_cpp_keep_typedefs td macs parse_strings cache a =
  parse_print_error_heuristic td macs parse_strings cache a true

(*****************************************************************************)
(* Same but faster cos memoize stuff *)
(*****************************************************************************)
let parse_cache typedefs parse_strings cache file has_changes =
  (* Normally, if there are no changes to headers, we should not have to parse
     then again.  The problem is that headers have effects, eg on inferred
     typedefs, and we aren't storing and then reapplying those effects.
     So we have to reparse the header files again, at great runtime cost, to
     get those effects.  It also seems that when a header file is retrieved
     from the cache, then there is no runtime processing, eg matching, of at
     least the header files that it includes.  That is, for recursive includes,
     need not only the AST of the header file itself, but also of what it
     includes, and at least the latter is not coming in the cached case. *)
  if not !Flag_parsing_c.use_cache
  then
    parse_print_error_heuristic typedefs None parse_strings cache file
      (not has_changes || !Includes.include_headers_for_types)
  else
  let _ = pr2_once "TOFIX: use_cache is not sensitive to changes in the considered macros, include files, etc" in
  let need_no_changed_files =
    (* should use Sys.argv.(0), would be safer. *)

    [
      (* TOFIX
      Config.path ^ "/parsing_c/c_parser.cma";
      (* we may also depend now on the semantic patch because
         the SP may use macro and so we will disable some of the
         macro expansions from standard.h.
      *)
      !Config.std_h;
      *)
    ] in
  let need_no_changed_variables =
    (* could add some of the flags of flag_parsing_c.ml *)
    [] in
  Common.cache_computation_robust_in_dir
    !Flag_parsing_c.cache_prefix file ".ast_raw"
    (need_no_changed_files, need_no_changed_variables) ".depend_raw"
    (fun () ->
      (* check whether to clear the cache *)
      (match (!Flag_parsing_c.cache_limit,!Flag_parsing_c.cache_prefix) with
	(None,_) | (_,None) -> ()
      |	(Some limit,Some prefix) ->
	  let count =
	    Common.cmd_to_list
	      (Printf.sprintf "test -e %s && find %s -name \"*_raw\" | wc -l"
		 prefix prefix) in
	  match count with
	    [c] ->
	      if int_of_string c >= limit
	      then
		let _ =
		  Sys.command
		    (Printf.sprintf
		       "find %s -name \"*_raw\" -exec /bin/rm {} \\;"
		       prefix) in
		()
	  | _ -> ());
      (* recompute *)
      parse_print_error_heuristic None None true cache file false)



(*****************************************************************************)
(* Some special cases *)
(*****************************************************************************)

(* Please DO NOT remove this code, even though it is not used *)
let no_format s =
  try let _ = Str.search_forward (Str.regexp_string "%") s 0 in false
  with Not_found -> true

(* no point to parse strings in these cases. never applied to a format string *)
let (cstatement_of_string: string -> Ast_c.statement) = fun s ->
  assert (no_format s);
  let tmpfile = Common.new_temp_file "cocci_stmt_of_s" "c" in
  Common.write_file ~file:tmpfile ("void main() { \n" ^ s ^ "\n}");
  let program = fst (parse_c_and_cpp false false tmpfile) in
  program +> Common.find_some (fun (e,_) ->
    match e with
    | Ast_c.Definition ({Ast_c.f_body = [Ast_c.StmtElem st]},_) -> Some st
    | _ -> None
  )

let (cexpression_of_string: string -> Ast_c.expression) = fun s ->
  assert (no_format s);
  let tmpfile = Common.new_temp_file "cocci_expr_of_s" "c" in
  Common.write_file ~file:tmpfile ("void main() { \n" ^ s ^ ";\n}");
  let program = fst (parse_c_and_cpp false false tmpfile) in
  program +> Common.find_some (fun (e,_) ->
    match e with
    | Ast_c.Definition ({Ast_c.f_body = compound},_) ->
        (match compound with
        | [Ast_c.StmtElem st] ->
            (match Ast_c.unwrap_st st with
            | Ast_c.ExprStatement (Some e) -> Some e
            | _ -> None
            )
        | _ -> None
        )
    | _ -> None
  )
