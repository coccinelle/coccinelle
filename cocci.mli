(*
 * Copyright 2012-2015, Inria
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


# 0 "./cocci.mli"
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
      (Str.regexp * Str.regexp list) option (*coccigrep tokens*) *
      Get_constants2.combine option
val pre_engine : (filename * filename) -> cocci_info * constant_info
val worth_trying : filename list -> constant_info -> bool
val full_engine :
  cocci_info -> filename list -> (filename * filename option) list
val post_engine : cocci_info -> unit
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
      Ast_cocci.meta_name list list list *
      (Ast_cocci.meta_name list list list (*used after list*) *
	 (*fresh used after list*)
	 Ast_cocci.meta_name list list list *
	 (*fresh used after list seeds*)
	 Ast_cocci.meta_name list list list) *
      Ast_cocci.meta_name list list list *
      (string list option (*grep tokens*) *
	 string list option (*glimpse tokens*) *
	 (Str.regexp * Str.regexp list) option (*coccigrep tokens*) *
	 Get_constants2.combine option) *
      bool (* format information needed for strings? *)

val normalize_path : string -> string
