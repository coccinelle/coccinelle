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

(* if do .mli:
val print_parsing_stat_list: parsing_stat list -> unit
*)

(*****************************************************************************)
(* Stat *)
(*****************************************************************************)
type parsing_stat = {
    filename: filename;
    mutable have_timeout: bool;

    mutable correct: int;
    mutable bad: int;

    mutable commentized: int; (* by our cpp commentizer *)

    (* if want to know exactly what was passed through, uncomment:
     *
     * mutable passing_through_lines: int;
     *
     * it differs from bad by starting from the error to
     * the synchro point instead of starting from start of
     * function to end of function.
     *)

    mutable problematic_lines:
      (string list (* ident in error line *) * int (* line_error *)) list;

  }

let default_stat file =  {
    filename = file;
    have_timeout = false;
    correct = 0; bad = 0;
    commentized = 0;
    problematic_lines = [];
  }

(* todo: stat per dir ?  give in terms of func_or_decl numbers:
 * nbfunc_or_decl pbs / nbfunc_or_decl total ?/
 *
 * note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les
 * valeurs alors on parsera correctement tout le fichier et pourtant y'aura
 * aucune def  et donc aucune couverture en fait.
 * ==> TODO evaluer les parties non parsé ?
 *)

let print_parsing_stat_list ?(verbose=false) = fun statxs ->
  let total = List.length statxs in
  let perfect =
    statxs
      +> List.filter (function
          {have_timeout = false; bad = 0} -> true | _ -> false)
      +> List.length
  in

  if verbose then begin
  pr "\n\n\n---------------------------------------------------------------";
  pr "pbs with files:";
  statxs
    +> List.filter (function
      | {have_timeout = true} -> true
      | {bad = n} when n > 0 -> true
      | _ -> false)
    +> List.iter (function
        {filename = file; have_timeout = timeout; bad = n} ->
          pr (file ^ "  " ^ (if timeout then "TIMEOUT" else i_to_s n));
        );

  pr "\n\n\n";
  pr "files with lots of tokens passed/commentized:";
  let threshold_passed = 100 in
  statxs
    +> List.filter (function
      | {commentized = n} when n > threshold_passed -> true
      | _ -> false)
    +> List.iter (function
        {filename = file; commentized = n} ->
          pr (file ^ "  " ^ (i_to_s n));
        );

  pr "\n\n\n---------------------------------------------------------------";
  end;

  pr (
  (sprintf "NB total files = %d; " total) ^
  (sprintf "perfect = %d; " perfect) ^
  (sprintf "pbs = %d; "     (statxs +> List.filter (function
      {have_timeout = b; bad = n} when n > 0 -> true | _ -> false)
                               +> List.length)) ^
  (sprintf "timeout = %d; " (statxs +> List.filter (function
      {have_timeout = true; bad = n} -> true | _ -> false)
                               +> List.length)) ^
  (sprintf "=========> %d" ((100 * perfect) / total)) ^ "%"

  );
  let good = statxs +> List.fold_left (fun acc {correct = x} -> acc+x) 0 in
  let bad  = statxs +> List.fold_left (fun acc {bad = x} -> acc+x) 0  in
  let passed = statxs +> List.fold_left (fun acc {commentized = x} -> acc+x) 0
  in
  let gf, badf = float_of_int good, float_of_int bad in
  let passedf = float_of_int passed in
  pr (
  (sprintf "nb good = %d,  nb passed = %d " good passed) ^
  (sprintf "=========> %f"  (100.0 *. (passedf /. gf)) ^ "% passed")
   );
  pr (
  (sprintf "nb good = %d,  nb bad = %d " good bad) ^
  (sprintf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "% good"
   )
  )

(*****************************************************************************)
(* Recurring error diagnostic *)
(*****************************************************************************)
(* asked/inspired by reviewer of CC'09 *)

let lines_around_error_line ~context (file, line) =
  let arr = Common.cat_array file in

  let startl = max 0 (line - context) in
  let endl   = min (Array.length arr) (line + context) in
  let res = ref [] in

  for i = startl to endl -1 do
    Common.push2 arr.(i) res
  done;
  List.rev !res



let print_recurring_problematic_tokens xs =
  let h = Hashtbl.create 101 in
  xs +> List.iter (fun x ->
    let file = x.filename in
    x.problematic_lines +> List.iter (fun (xs, line_error) ->
      xs +> List.iter (fun s ->
        Common.hupdate_default s
          (fun (old, example)  -> old + 1, example)
          (fun() -> 0, (file, line_error)) h;
      )));
  pr2_xxxxxxxxxxxxxxxxx();
  pr2 ("maybe 10 most problematic tokens");
  pr2_xxxxxxxxxxxxxxxxx();
  Common.hash_to_list h
  +> List.sort (fun (k1,(v1,_)) (k2,(v2,_)) -> compare v2 v1)
  +> Common.take_safe 10
  +> List.iter (fun (k,(i, (file_ex, line_ex))) ->
    pr2 (spf "%s: present in %d parsing errors" k i);
    pr2 ("example: ");
    let lines = lines_around_error_line ~context:2 (file_ex, line_ex) in
    lines +> List.iter (fun s -> pr2 ("       " ^ s));

  );
  pr2_xxxxxxxxxxxxxxxxx();
  ()




(*****************************************************************************)
(* Stat *)
(*****************************************************************************)

(* Those variables were written for CC09, to evaluate the need for
 * some of our heuristics and extensions.
 *
 * coupling: if you add a new var, modify also assoc_stat_number below
 *)

let nTypedefInfer = ref 0

let nIncludeGrammar = ref 0
let nIncludeHack = ref 0

let nIteratorGrammar = ref 0
let nIteratorHeuristic = ref 0

let nMacroTopDecl = ref 0
let nMacroStructDecl = ref 0
let nMacroDecl = ref 0
let nMacroStmt = ref 0
let nMacroString = ref 0
let nMacroHigherOrder = ref 0 (* actions *)
let nMacrohigherTypeGrammar = ref 0
let nMacroAttribute = ref 0

let nIfdefTop = ref 0
let nIfdefStmt = ref 0
let nIfdefStruct = ref 0
let nIfdefInitializer = ref 0
(* nIfdefExpr, nIfdefType *)

let nIfdefFunheader = ref 0

let nIfdefExprPassing = ref 0
let nIfdefPassing = ref 0

let nIncludePassing = ref 0
let nUndefPassing = ref 0
let nDefinePassing = ref 0

let nIfdefZero = ref 0
let nIfdefVersion = ref 0



let nGccTypeof = ref 0
let nGccLongLong = ref 0
let nGccAsm = ref 0
let nGccInline = ref 0
let nGccAttribute = ref 0
let nGccCaseRange = ref 0
let nGccMixDecl = ref 0
let nGccDesignator = ref 0
let nGccStmtExpr = ref 0
let nGccConstructor = ref 0
let nGccEmptyStruct = ref 0
let nGccNestedFunc = ref 0

let nGccMisc = ref 0



let nDefineHack = ref 0

let nDefineConstant = ref 0
let nDefineStmt = ref 0
let nDefineExpr = ref 0
(* both below require some heuristic support *)
let nDefineWhile0 = ref 0
let nDefineInit = ref 0

let nDefineOther = ref 0

let nUndef = ref 0
let nPragmaAndCo = ref 0

(* let nDirectiveTop = ref 0 *)
let nDirectiveStmt = ref 0
let nDirectiveStruct = ref 0
let nDirectiveInitializer = ref 0


(* from standard.h *)
let nMacroHint = ref 0
let nMacroExpand = ref 0

let nNotParsedCorrectly = ref 0

let assoc_stat_number =
  [
    "nTypedefInfer", nTypedefInfer;

    "nIteratorHeuristic", nIteratorHeuristic;

    "nMacroTopDecl", nMacroTopDecl;
    "nMacroStructDecl", nMacroStructDecl;
    "nMacroDecl", nMacroDecl;
    "nMacroStmt", nMacroStmt;
    "nMacroString", nMacroString;
    "nMacroHigherOrder", nMacroHigherOrder;
    "nMacroAttribute", nMacroAttribute;

    "nMacrohigherTypeGrammar", nMacrohigherTypeGrammar;

    "nIfdefTop", nIfdefTop;
    "nIfdefStmt", nIfdefStmt;
    "nIfdefStruct", nIfdefStruct;
    "nIfdefInitializer", nIfdefInitializer;

    "nIfdefFunheader", nIfdefFunheader;
    "nIfdefZero", nIfdefZero;
    "nIfdefVersion", nIfdefVersion;
    "nIfdefExprPassing", nIfdefExprPassing;
    "nIfdefPassing", nIfdefPassing;

    "nIncludePassing", nIncludePassing;
    "nDefinePassing", nDefinePassing;
    "nUndefPassing", nUndefPassing;

    "nMacroExpand", nMacroExpand;
    "nMacroHint", nMacroHint;


    "nGccTypeof", nGccTypeof;
    "nGccLongLong", nGccLongLong;
    "nGccAsm", nGccAsm;
    "nGccInline", nGccInline;
    "nGccAttribute", nGccAttribute;
    "nGccCaseRange", nGccCaseRange;
    "nGccMixDecl", nGccMixDecl;
    "nGccDesignator", nGccDesignator;
    "nGccStmtExpr", nGccStmtExpr;
    "nGccConstructor", nGccConstructor;
    "nGccEmptyStruct", nGccEmptyStruct;
    "nGccNestedFunc", nGccNestedFunc;

    "nGccMisc", nGccMisc;


    "nDefineHack", nDefineHack;

    "nDefineConstant", nDefineConstant;
    "nDefineStmt", nDefineStmt;
    "nDefineExpr", nDefineExpr;
    "nDefineInit", nDefineInit;
    "nDefineOther", nDefineOther;

    "nUndef", nUndef;
    "nPragmaAndCo", nPragmaAndCo;

    "nDirectiveStmt", nDirectiveStmt;
    "nDirectiveStruct", nDirectiveStruct;
    "nDirectiveInitializer", nDirectiveInitializer;

    "nNotParsedCorrectly", nNotParsedCorrectly;


    (* less *)
    "nIncludeGrammar", nIncludeGrammar;
    "nIncludeHack", nIncludeHack;

    "nIteratorGrammar", nIteratorGrammar;
  ]

let print_stat_numbers () =
  assoc_stat_number +> List.iter (fun (k, vref) ->
    pr2 (spf "%-30s -> %d" k !vref);
  )
