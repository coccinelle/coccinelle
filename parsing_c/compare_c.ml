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
  | PbKnown of string


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

let normal_form_token adjust_cvs x =
  let x' =
    match x with
    | Parser_c.TString ((s, kind),i1) -> Parser_c.TString (("",kind), i1)
    | Parser_c.TIfdef (_,_,i) -> Parser_c.TIfdef (Gnone,ref None,i)
    | Parser_c.TIfdefelif (_,_,i) -> Parser_c.TIfdefelif (Gnone,ref None,i)
    | Parser_c.TEndif (_,i) -> Parser_c.TEndif (ref None,i)
    | Parser_c.TIfdefelse (_,i) -> Parser_c.TIfdefelse (ref None,i)
    | Parser_c.TIfdefBool (b,_,i) -> Parser_c.TIfdefBool (b,ref None,i)
    | Parser_c.TIfdefMisc (b,_,i) -> Parser_c.TIfdefMisc (b,ref None,i)
    | Parser_c.TIfdefVersion (b,_,i) -> Parser_c.TIfdefVersion (b,ref None,i)
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

let get_diff filename1 filename2 bs =
  let com =
    match !Flag_parsing_c.diff_lines with
      None -> Printf.sprintf "diff -u %s %s %s" bs filename1 filename2
    | Some n ->
	Printf.sprintf "diff -U %s %s %s %s" n bs filename1 filename2 in
  let xs = Common.cmd_to_list com in

  (* get rid of the --- and +++ lines *)
  if xs=[]
  then xs
  else Common.drop 2 xs

(* convention: compare_token generated_file  expected_res
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
        if x' = y'
        then loop xs ys
        else
          let str1, pos1 =
            Token_helpers.str_of_tok x, Token_helpers.pos_of_tok x in
          let str2, pos2 =
            Token_helpers.str_of_tok y, Token_helpers.pos_of_tok y in
	  (*
	  let str1 = str1 ^ " - " ^ (Token_helpers.string_of_token x) in
	  let str2 = str2 ^ " - " ^ (Token_helpers.string_of_token y) in
	  *)
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

  let do_parse filename other =
    if Filename.dirname filename = "/tmp"
    then
      (* hack to make include paths similar for generated and original files *)
      let pth = !Includes.include_path in
      let dir = Filename.dirname other in
      let ps = Includes.get_parsing_style() in
      (match ps with
	Includes.Parse_local_includes ->
	  Includes.set_parsing_style Includes.Parse_all_includes;
	  Includes.include_path := [dir]
      | _ -> Includes.include_path := dir :: pth);
      let (c2, _stat) = Parse_c.parse_c_and_cpp false false filename in
      Includes.set_parsing_style ps;
      Includes.include_path := pth;
      c2
    else fst(Parse_c.parse_c_and_cpp false false filename) in

  let c1 = do_parse filename1 filename2 in
  let c2 = do_parse filename2 filename1 in

  let res =
    let r = Str.regexp ".*_failure\\.c" in
    let is_known_failure = (Str.string_match r filename1 0) in
    if List.length c1 <> List.length c2 && not is_known_failure
    then Pb "not same number of entities (func, decl, ...)"
    else
        zip c1 c2 +> Common.fold_k (fun acc ((a,infoa),(b,infob)) k ->
          match a, b with
          | NotParsedCorrectly a, NotParsedCorrectly b ->
              (match final_loop (snd infoa) (snd infob) with
              | None -> k acc
              | Some s when not is_known_failure -> PbOnlyInNotParsedCorrectly s
              | Some s -> PbKnown s
              )

          | NotParsedCorrectly a, _ when not is_known_failure ->
              Pb "PB parsing only in generated-file"
          | NotParsedCorrectly a, _ when is_known_failure ->
              PbKnown "PB parsing only in generated-file"
          | _, NotParsedCorrectly b when not is_known_failure ->
              PbOnlyInNotParsedCorrectly "PB parsing only in expected-file"
          | _, NotParsedCorrectly b when is_known_failure ->
              PbKnown "PB parsing only in expected-file"
          | _, _ ->
              (match final_loop (snd infoa) (snd infob) with
              | None  -> k acc
              | Some s when not is_known_failure -> Pb s
              | Some s -> PbKnown s
              )
        ) (fun acc -> acc)
          (Correct)
  in

  let xs = get_diff filename1 filename2 "-b -B" in

  (if xs=[] && (res <> Correct)
  then Printf.eprintf "%s %s"
    "Impossible: How can diff be null and have not Correct in compare_c?"
      (Dumper.dump res))

;

  res, xs

let compare_token = do_compare_token true true


(*****************************************************************************)

(* compare to a res file *)
let compare_default = do_compare_token true true

(* compare to the source of the transformation *)
let compare_to_original = do_compare_token false false

let exact_compare file1 file2 =
  match get_diff file1 file2 "" with
    [] -> (Correct, [])
  | res -> (Pb "files differ", res)

let compare_result_to_string (correct, diffxs) =
  match correct with
  | Correct ->
      "seems correct" ^ "\n"
  | Pb s ->
      ("seems incorrect: " ^ s) ^ "\n" ^
        "diff (result(-) vs expected_result(+)) = " ^ "\n" ^
        (diffxs +> String.concat "\n") ^ "\n"
  | PbOnlyInNotParsedCorrectly s ->
      "seems incorrect, but only because of code that was not parsable" ^ "\n"^
        ("explanation:" ^ s) ^ "\n" ^
        "diff (result(-) vs expected_result(+)) = " ^ "\n" ^
        (diffxs +> String.concat "\n") ^ "\n"
  | PbKnown s ->
      ("seems incorrect, which was expected: " ^ s) ^ "\n" ^
        "diff (result(-) vs expected_result(+)) = " ^ "\n" ^
        (diffxs +> String.concat "\n") ^ "\n"



let compare_result_to_bool correct =
  correct = Correct
