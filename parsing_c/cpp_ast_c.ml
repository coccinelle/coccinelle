(* Yoann Padioleau
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2008, 2009 University of Urbana Champaign
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

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once =
  Common.mk_pr2_wrappers Flag_parsing_c.verbose_cpp_ast
let pr2_debug,pr2_debug_once =
  Common.mk_pr2_wrappers Flag_parsing_c.debug_cpp_ast

(*****************************************************************************)
(* Cpp Ast Manipulations *)
(*****************************************************************************)

(*
 * cpp-include-expander-builtin.
 *
 * alternative1: parse and call cpp tour a tour. So let cpp work at
 *  the token level. That's what most tools do.
 * alternative2: apply cpp at the very end. Process that go through ast
 *  and do the stuff such as #include,  macro expand,
 *  ifdef but on the ast!
 *
 * But need keep those info in ast at least, even bad
 * macro for instance, and for parse error region ? maybe can
 * get another chance ?
 * I think it's better to do the cpp-include-expander in a different step
 * rather than embedding it in the parser. The parser is already too complex.
 * Also keep with the tradition to try to parse as-is.
 *
 * todo? but maybe could discover new info that could help reparse
 * the ParseError in original file. Try again parsing it by
 * putting it in a minifile ?
 *
 *
 * todo? maybe can do some pass that work at the ifdef level and for instance
 * try to paren them, so have in Ast some stuff that are not
 * present at parsing time but that can then be constructed after
 * some processing (a little bit like my type for expression filler,
 * or position info filler, or include relative position filler).
 *
 * ??add such info about what was done somewhere ? could build new
 * ??ast each time but too tedious (maybe need delta-programming!)
 *
 * todo? maybe change cpp_ast_c to go deeper on local "" ?
 *
 *
 * TODO: macro expand,
 * TODO: handle ifdef
 *
 *
 *
 * cpp_ifdef_statementize: again better to separate concern and in parser
 *  just add the directives in a flat way (IfdefStmt) and later do more
 *  processing and transform them in a tree with some IfdefStmt2.
 *)



(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

type cpp_option =
  | I of Common.dirname
  | D of string * string option



let i_of_cpp_options xs =
  xs +> Common.map_filter (function
  | I f -> Some f
  | D _ -> None
  )

let cpp_option_of_cmdline (xs, ys) =
  (xs +> List.map (fun s -> I s)) ++
  (ys +> List.map (fun s ->
    if s =~ "\\([A-Z][A-Z0-9_]*\\)=\\(.*\\)"
    then
      let (def, value) = matched2 s in
      D (def, Some value)
    else
      D (s, None)
  ))

(*****************************************************************************)
(* Debug *)
(*****************************************************************************)
let (show_cpp_i_opts: string list -> unit) = fun xs ->
  if not (null xs) then begin
    pr2 "-I";
    xs +> List.iter pr2
  end


let (show_cpp_d_opts: string list -> unit) = fun xs ->
  if not (null xs) then begin
    pr2 "-D";
    xs +> List.iter pr2
  end

(* ---------------------------------------------------------------------- *)
let trace_cpp_process depth mark inc_file =
  pr2_debug (spf "%s>%s %s"
          (Common.repeat "-" depth +> Common.join "")
          mark
          (s_of_inc_file_bis inc_file));
  ()



(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)


let _hcandidates = Hashtbl.create 101

let init_adjust_candidate_header_files dir =
  let ext = "[h]" in
  let files = Common.files_of_dir_or_files ext [dir] in

  files +> List.iter (fun file ->
    let base = Filename.basename file in
    pr2_debug file;
    Hashtbl.add _hcandidates base file;
  );
  ()



(* may return a list of match ? *)
let find_header_file1 cppopts dirname inc_file =
  match inc_file with
  | Local f ->
      let finalfile =
        Filename.concat dirname (Ast_c.s_of_inc_file inc_file) in
      if Sys.file_exists finalfile
      then [finalfile]
      else []
  | NonLocal f ->
      i_of_cpp_options cppopts +> Common.map_filter (fun dirname ->
        let finalfile =
          Filename.concat dirname (Ast_c.s_of_inc_file inc_file) in
        if Sys.file_exists finalfile
        then Some finalfile
        else None
      )
  | Weird s ->
      pr2 ("CPPAST: weird include not handled:" ^ s);
      []

(* todo? can try find most precise ? first just use basename but
 * then maybe look if have also some dir in common ?
 *)
let find_header_file2 inc_file =
  match inc_file with
  | Local f
  | NonLocal f ->
      let s = (Ast_c.s_of_inc_file inc_file) in
      let base = Filename.basename s in

      let res = Hashtbl.find_all _hcandidates base in
      (match res with
      | [file] ->
          pr2_debug ("CPPAST: find header in other dir: " ^ file);
          res
      | [] ->
          []
      | x::y::xs -> res
      )
  | Weird s ->
      []


let find_header_file cppopts dirname inc_file =
  let res1 = find_header_file1 cppopts dirname inc_file in
  match res1 with
  | [file] -> res1
  | [] -> find_header_file2 inc_file
  | x::y::xs -> res1




(* ---------------------------------------------------------------------- *)
let _headers_hash = Hashtbl.create 101

(* On freebsd ocaml is trashing, use up to 1.6Go of memory and then
 * building the database_c takes ages.
 *
 * So just limit with following threshold to avoid this trashing, simple.
 *
 * On netbsd, got a Out_of_memory exn on this file;
 * /home/pad/software-os-src2/netbsd/dev/microcode/cyclades-z/
 * even if the cache is small. That's because huge single
 * ast element and probably the ast marshalling fail.
 *)
let default_threshold_cache_nb_files = 200

let parse_c_and_cpp_cache
  ?(threshold_cache_nb_files= default_threshold_cache_nb_files) file =

  if Hashtbl.length _headers_hash > threshold_cache_nb_files
  then Hashtbl.clear _headers_hash;

  Common.memoized _headers_hash file (fun () ->
    Parse_c.parse_c_and_cpp file
  )



(*****************************************************************************)
(* Main entry *)
(*****************************************************************************)


let (cpp_expand_include2:
 ?depth_limit:int option ->
 ?threshold_cache_nb_files:int ->
 cpp_option list -> Common.dirname -> Ast_c.program -> Ast_c.program) =
 fun ?(depth_limit=None) ?threshold_cache_nb_files iops dirname ast ->

  if !Flag_parsing_c.debug_cpp_ast
  then pr2_xxxxxxxxxxxxxxxxx();

  let already_included = ref [] in

  let rec aux stack dirname ast =
    let depth = List.length stack in

    ast +> Visitor_c.vk_program_s { Visitor_c.default_visitor_c_s with
      Visitor_c.kcppdirective_s = (fun (k, bigf) cpp ->
        match cpp with
        | Include {i_include = (inc_file, ii);
                   i_rel_pos = h_rel_pos;
                   i_is_in_ifdef = b;
                   i_content = copt;
                   }
          ->
          (match depth_limit with
          | Some limit when depth >= limit -> cpp
          | _ ->

            (match find_header_file iops dirname inc_file with
            | [file] ->
                if List.mem file !already_included
                then begin
                  (* pr2 ("already included: " ^ file); *)
                  trace_cpp_process depth "*" inc_file;
                  k cpp
                end else begin
                  trace_cpp_process depth "" inc_file;
                  Common.push2 file already_included;
                  (* CONFIG *)
                  Flag_parsing_c.verbose_parsing := false;
                  Flag_parsing_c.verbose_lexing := false;
                  let (ast2, _stat) =
                    parse_c_and_cpp_cache ?threshold_cache_nb_files file
                  in

                  let ast = Parse_c.program_of_program2 ast2 in
                  let dirname' = Filename.dirname file in

                  (* recurse *)
                  let ast' = aux (file::stack) dirname' ast in

                  Include {i_include = (inc_file, ii);
                           i_rel_pos = h_rel_pos;
                           i_is_in_ifdef = b;
                           i_content = Some (file, ast');
                  }
                end
            | [] ->
                trace_cpp_process depth "!!" inc_file;
                pr2 "CPPAST: file not found";
                k cpp
            | x::y::zs ->
                trace_cpp_process depth "!!" inc_file;
                pr2 "CPPAST: too much candidates";
                k cpp
            )
          )
        | _ -> k cpp
      );
    }
  in
  aux [] dirname ast


let cpp_expand_include ?depth_limit ?threshold_cache_nb_files a b c =
  Common.profile_code "cpp_expand_include"
   (fun () -> cpp_expand_include2 ?depth_limit ?threshold_cache_nb_files a b c)

(*
let unparse_showing_include_content ?
*)


(*****************************************************************************)
(* Ifdef-statementize *)
(*****************************************************************************)


let is_ifdef_and_same_tag tag x =
  match x with
  | IfdefStmt (IfdefDirective ((_, tag2),_)) ->
      tag =*= tag2
  | StmtElem _ | CppDirectiveStmt _ -> false
  | IfdefStmt2 _ -> raise (Impossible 77)



(* What if I skipped in the parser only some of the ifdef elements
 * of the same tag. Once I passed one, I should pass all of them and so
 * at least should detect here that one tag is not "valid". Maybe in the parser
 * can return or marked some tags as "partially_passed_ifdef_tag".
 * Maybe could do in ast_c a MatchingTag of int * bool ref (* one_was_passed *)
 * where the ref will be shared by the ifdefs with the same matching tag
 * indice. Or simply count  the number of directives with the same tag and
 * put this information in the tag. Hence the total_with_this_tag below.
 *)
let should_ifdefize (tag,ii) ifdefs_directives xxs =
  let IfdefTag (_tag, total_with_this_tag) = tag in

  if total_with_this_tag <> List.length ifdefs_directives
  then begin
    let strloc = Ast_c.strloc_of_info (List.hd ii) in
    pr2 (spf "CPPASTC: can not ifdefize ifdef at %s" strloc);
    pr2 "CPPASTC: some of its directives were passed";
    false
  end else
    (* todo? put more condition ? dont ifdefize declaration ? *)
    true





(* return a triple, (ifdefs directive * grouped xs * remaining sequencable)
 * XXX1 XXX2 elsif YYY1 else ZZZ1 endif WWW1 WWW2
 * => [elsif, else, endif], [XXX1 XXX2; YYY1; ZZZ1], [WWW1 WWW2]
 *)
let group_ifdef tag xs =
  let (xxs, xs) = group_by_post (is_ifdef_and_same_tag tag) xs in

  xxs +> List.map snd +> List.map (fun x ->
    match x with
    | IfdefStmt y -> y
    | StmtElem _ | CppDirectiveStmt _ | IfdefStmt2 _ -> raise (Impossible 78)
  ),
  xxs +> List.map fst,
  xs


let rec cpp_ifdef_statementize ast =
  Visitor_c.vk_program_s { Visitor_c.default_visitor_c_s with
    Visitor_c.kstatementseq_list_s = (fun (k, bigf) xs ->
      let rec aux xs =
        match xs with
        | [] -> []
        | stseq::xs ->
            (match stseq with
            | StmtElem st ->
                Visitor_c.vk_statement_sequencable_s bigf stseq::aux xs
            | CppDirectiveStmt directive ->
                Visitor_c.vk_statement_sequencable_s bigf stseq::aux xs
            | IfdefStmt ifdef ->
                (match ifdef with
                | IfdefDirective ((Ifdef,tag),ii) ->

                    let (restifdefs, xxs, xs') = group_ifdef tag xs in
                    if should_ifdefize (tag,ii) (ifdef::restifdefs) xxs
                    then
                      let res = IfdefStmt2 (ifdef::restifdefs, xxs) in
                      Visitor_c.vk_statement_sequencable_s bigf res::aux xs'
                    else
                      Visitor_c.vk_statement_sequencable_s bigf stseq::aux xs

                | IfdefDirective (((IfdefElseif|IfdefElse|IfdefEndif),b),ii) ->
                    pr2 "weird: first directive is not a ifdef";
                    (* maybe not weird, just that should_ifdefize
                     * returned false *)
                    Visitor_c.vk_statement_sequencable_s bigf stseq::aux xs
                )

            | IfdefStmt2 (ifdef, xxs) ->
                failwith "already applied cpp_ifdef_statementize"
            )
      in
      aux xs
    );
  } ast


(*****************************************************************************)
(* Macro *)
(*****************************************************************************)

let (cpp_expand_macro_expr:
  Ast_c.define_kind -> Ast_c.argument Ast_c.wrap2 list ->
  Ast_c.expression option) =
 fun defkind args ->
   raise Todo
