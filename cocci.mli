(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common

(* full_engine takes (coccifile, isofile) and cfiles in parameters and
 * returns a list associating to the input cfiles, and maybe header
 * files that was also required to be modified, the files containing the
 * result (in general files in /tmp).
 * pre_engine does the compilation of the SmPL code and runs any initially
 * scripts
 * post_engine runs any finally scripts
 *
 * This function uses memoisation internally, which is useful when
 * using -dir to not redo twice the same work. So take care!
 *)
type cocci_info
type constant_info =
    string list option (*grep tokens*) *
      string list option (*glimpse tokens*) *
      (Str.regexp * Str.regexp list * string list)
      option (*coccigrep/gitgrep tokens*) *
      Get_constants2.combine option
type merge_vars = string array list * string array list
val union_merge_vars : merge_vars -> merge_vars -> merge_vars
val pre_engine : (filename * filename) -> cocci_info * constant_info
val worth_trying : filename list -> constant_info -> bool
val full_engine :
  cocci_info -> filename list -> (filename * filename option) list * merge_vars
val post_engine : cocci_info -> merge_vars -> unit
val has_finalize : cocci_info -> bool

(* because of the #include "toto.c" and also because we may associate the
 * same C file to multiple drivers because they share code, we can
 * modify multiple times the same file when use -dir. This check
 * remove duplicates and check that the modification are consistent
 * among the different drivers.
 *)
val check_duplicate_modif :
 (filename * filename option) list -> (filename * filename option) list

(* provides memoization *)
val sp_of_file :
  filename (* coccifile *)  -> filename option (* isofile *) ->
  Ast_cocci.metavar list list * Ast_cocci.rule list *
      Ast_cocci.meta_name list list list *
      (Ast_cocci.meta_name list * Ast_cocci.meta_name list) list list * (*pos*)
      (Ast_cocci.meta_name list list list (*used after list*) *
	 (*fresh used after list*)
	 Ast_cocci.meta_name list list list *
	 (*fresh used after list seeds*)
	 Ast_cocci.meta_name list list list) *
      Ast_cocci.meta_name list list list * constant_info *
      bool (* format information needed for strings? *)

val normalize_path : string -> string
