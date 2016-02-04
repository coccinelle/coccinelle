(* Yoann Padioleau
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2006, 2007 Ecole des Mines de Nantes
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


type compare_result =
  | Correct
  | Pb of string
  | PbOnlyInNotParsedCorrectly of string


(*****************************************************************************)
(* Normalise before comparing *)
(*****************************************************************************)

(* List taken from CVS manual, 'Keyword substitution' chapter. Note
 * that I do not put "Log" because it is used only in comment, and it
 * is not enough to substituate until the end of the line. *)
let cvs_keyword_list = [
  "Id";"Date"; "Revision"; (* the common one *)
  "FreeBSD";"Heimdal";"KAME";"NetBSD";"OpenBSD";"OpenLDAP";"RuOBSD";
  "SourceForge";
  "Name";"Author";"CVSHeader";"Header";"Locker";"RCSfile";"Source";"State";
  "Rev";
]

(* Can also have just dollarIDdollar but it is only when you have not
 * yet committed the file. After the commit it would be a dollarIddollar:.
 * If reput Id:, do not join the regexp!! otherwise CVS will modify it :)
 *)
let cvs_keyword_regexp = Str.regexp
  ("\\$\\([A-Za-z_]+\\):[^\\$]*\\$")


let cvs_compute_newstr s =
  Str.global_substitute cvs_keyword_regexp (fun _s ->
    let substr = Str.matched_string s in
    assert (substr ==~ cvs_keyword_regexp); (* use its side-effect *)
    let tag = matched1 substr in

    if not (List.mem tag cvs_keyword_list)
    then pr2_once ("unknown CVS keyword: " ^ tag);

    "CVS_MAGIC_STRING"
  ) s




(* todo: get rid of the type for expressions  ? *)
let normal_form_program xs =
  let bigf = { Visitor_c.default_visitor_c_s with

    Visitor_c.kini_s = (fun (k,bigf) ini ->
      match ini with
      | InitList xs, [i1;i2;iicommaopt] ->
          k (InitList xs, [i1;i2])
      | _ -> k ini
    );

    Visitor_c.kexpr_s = (fun (k,bigf) e ->
      match e with
      (* todo: should also do something for multistrings *)
      |	 (Constant (String (s,kind)), typ), [ii]
          when Common.string_match_substring cvs_keyword_regexp s ->
          let newstr = cvs_compute_newstr s in
          (Constant (String (newstr,kind)), typ), [rewrap_str newstr ii]
      |	 _ -> k e
    );

    Visitor_c.kfragment_s = (fun (k,bigf) e ->
      match e with
      (* todo: should also do something for multistrings *)
      | (ConstantFragment s), [ii]
          when Common.string_match_substring cvs_keyword_regexp s ->
          let newstr = cvs_compute_newstr s in
	  (ConstantFragment newstr), [rewrap_str newstr ii]
      | _ -> k e
    );

    Visitor_c.ktoplevel_s = (fun (k,bigf) p ->
      match p with
      | CppTop (Define _) ->
          raise Todo
          (*
          let (i1, i2, i3) = Common.tuple_of_list3 ii in
          if Common.string_match_substring cvs_keyword_regexp body
          then
            let newstr = cvs_compute_newstr body in
            Define ((s, newstr), [i1;i2;rewrap_str newstr i3])
          else p
          *)
      | _ -> k p
    );

(*
    Visitor_c.kinfo_s = (fun (k,bigf) i ->
      let s = Ast_c.get_str_of_info i in
      if Common.string_match_substring cvs_keyword_regexp s
      then
        let newstr = cvs_compute_newstr s in
        rewrap_str newstr i
      else i
    );
*)

  }
  in
  xs +> List.map (fun p -> Visitor_c.vk_toplevel_s  bigf p)






let normal_form_token adjust_cvs x =
  let x' =
    match x with
    | Parser_c.TString ((s, kind),i1) -> Parser_c.TString (("",kind), i1)
    | x -> x
  in
  x' +> Token_helpers.visitor_info_of_tok (fun info ->
    let info = Ast_c.al_info 0 info in
    let str = Ast_c.str_of_info info in
    if adjust_cvs && Common.string_match_substring cvs_keyword_regexp str
    then
      let newstr = cvs_compute_newstr str in
      rewrap_str newstr info
    else info
  )


(*****************************************************************************)
(* Compare at Ast level *)
(*****************************************************************************)

(* Note that I do a (simple) astdiff to know if there is a difference, but
 * then I use diff to print the differences. So sometimes you have to dig
 * a little to find really where the real difference (one not involving
 * just spacing difference) was.
 * Note also that the astdiff is not very accurate. As I skip comments,
 * macro definitions, those are not in the Ast and if there is a diff
 * between 2 files regarding macro def, then I will not be able to report it :(
 * update: I now put the toplevel #define at least in the Ast.
 * update: You can use token_compare for more precise diff.
 *
 * todo?: finer grain astdiff, better report, more precise.
 *
 * todo: do iso between if() S and  if() { S }
 *)
(* I have removed this function as it was not used anywhere.
 *     let compare_ast filename1 filename2  = ...
 * /Iago
 *)


(*****************************************************************************)
(* Compare at token level *)
(*****************************************************************************)

(* Because I now commentize more in parsing, with parsing_hacks,
 * compare_ast may say that 2 programs are equal whereas they are not.
 * Here I compare token, and so have still the TCommentCpp and TCommentMisc
 * so at least detect such differences.
 *
 * Moreover compare_ast is not very precise in his report when it
 * detects a difference. So token_diff is better.
 *
 * I do token_diff but I use programCelement2, so that
 * I know if I am in a "notparsable" zone. The tokens are
 * in (snd programCelement2).
 *
 * Faire aussi un compare_token qui se moque des TCommentMisc,
 * TCommentCPP et TIfdef ? Normalement si fait ca retrouvera
 * les meme resultats que compare_ast.
 *
 *)


(* Pass only "true" comments, don't pass TCommentMisc and TCommentCpp *)
let is_normal_space_or_comment to_expected = function
  | Parser_c.TCommentSpace _
  | Parser_c.TCommentNewline _

(*  | Parser_c.TComma _ *) (* UGLY, because of gcc_opt_comma isomorphism  *)
      -> true
  | Parser_c.TComment _ -> to_expected (* only ignore in compare to expected *)
  | _ -> false


(* convetion: compare_token generated_file  expected_res
 * because when there is a notparsablezone in generated_file, I
 * don't issue a PbOnlyInNotParsedCorrectly
 *)
let do_compare_token adjust_cvs to_expected filename1 filename2 =

  let rec loop xs ys =
    match xs, ys with
    | [], [] -> None

    (* UGLY, because of gcc_opt_comma isomorphism  *)
    | (Parser_c.TComma _::Parser_c.TCBrace _::xs),  (Parser_c.TCBrace _::ys) ->
        loop xs ys
    | (Parser_c.TCBrace _::xs), (Parser_c.TComma _::Parser_c.TCBrace _::ys) ->
        loop xs ys

    | [], x::xs ->
        Some "not same number of tokens inside C elements"
    | x::xs, [] ->
        Some "not same number of tokens inside C elements"

    | x::xs, y::ys ->
        let x' = normal_form_token adjust_cvs x in
        let y' = normal_form_token adjust_cvs y in
        if x' =*= y'
        then loop xs ys
        else
          let str1, pos1 =
            Token_helpers.str_of_tok x, Token_helpers.pos_of_tok x in
          let str2, pos2 =
            Token_helpers.str_of_tok y, Token_helpers.pos_of_tok y in
          Some ("diff token: " ^ str1 ^" VS " ^ str2 ^ "\n" ^
                   Common.error_message filename1 (str1, pos1) ^ "\n" ^
                   Common.error_message filename2 (str2, pos2) ^ "\n"
          )

  in
  let final_loop xs ys =
    loop
      (xs +>
       List.filter (fun x -> not (is_normal_space_or_comment to_expected x)))
      (ys +>
       List.filter (fun x -> not (is_normal_space_or_comment to_expected x)))
  in

  (*
    let toks1 = Parse_c.tokens filename1 in
    let toks2 = Parse_c.tokens filename2 in
    loop toks1 toks2 in
  *)

  let (c1, _stat) = Parse_c.parse_c_and_cpp false filename1 in
  let (c2, _stat) = Parse_c.parse_c_and_cpp false filename2 in

  let res =
    if List.length c1 <> List.length c2
    then Pb "not same number of entities (func, decl, ...)"
    else
        zip c1 c2 +> Common.fold_k (fun acc ((a,infoa),(b,infob)) k ->
          match a, b with
          | NotParsedCorrectly a, NotParsedCorrectly b ->
              (match final_loop (snd infoa) (snd infob) with
              | None -> k acc
              | Some s -> PbOnlyInNotParsedCorrectly s
              )

          | NotParsedCorrectly a, _ ->
              Pb "PB parsing only in generated-file"
          | _, NotParsedCorrectly b ->
              PbOnlyInNotParsedCorrectly "PB parsing only in expected-file"
          | _, _ ->
              (match final_loop (snd infoa) (snd infob) with
              | None  -> k acc
              | Some s -> Pb s
              )
        ) (fun acc -> acc)
          (Correct)
  in

  let xs =
    match !Flag_parsing_c.diff_lines with
      None ->
	Common.cmd_to_list ("diff -u -b -B "^filename1^ " "  ^ filename2)
    | Some n ->
	Common.cmd_to_list ("diff -U "^n^" -b -B "^filename1^" "^filename2) in

  (* get rid of the --- and +++ lines *)
  let xs =
    if null xs
    then xs
    else Common.drop 2 xs
  in

  if null xs && (res <> Correct)
  then failwith
    "Impossible: How can diff be null and have not Correct in compare_c?";

  res, xs

let compare_token = do_compare_token true true


(*****************************************************************************)

(* compare to a res file *)
let compare_default = do_compare_token true true

(* compare to the source of the transformation *)
let compare_to_original = do_compare_token false false


let compare_result_to_string (correct, diffxs) =
  match correct with
  | Correct ->
      "seems correct" ^ "\n"
  | Pb s ->
      ("seems incorrect: " ^ s) ^ "\n" ^
        "diff (result(-) vs expected_result(+)) = " ^ "\n" ^
        (diffxs +> Common.join "\n") ^ "\n"
  | PbOnlyInNotParsedCorrectly s ->
      "seems incorrect, but only because of code that was not parsable" ^ "\n"^
        ("explanation:" ^ s) ^ "\n" ^
        "diff (result(-) vs expected_result(+)) = " ^ "\n" ^
        (diffxs +> Common.join "\n") ^ "\n"


let compare_result_to_bool correct =
  correct =*= Correct
