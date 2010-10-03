(* File: path.mli

    Copyright (C) 2005-

      Jane Street Holding, LLC
      Author: Markus Mottl
      email: mmottl\@janestcapital.com
      WWW: http://www.janestcapital.com/ocaml

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* WL YM for MM: Some other functions that would be useful: get_opt, insert_record_field,
   remove_record_field *)

(** Path: Module for Substitutions within S-expressions *)

(** {6 Types} *)

(** Type of substitution elements *)
type el =
  | Pos of int  (** [Pos n] denotes [n]th element in a tuple *)
  | Match of string * int
      (** [Match (tag, n)] denotes [n]th argument of sum matching [tag] *)
  | Rec of string  (** [Rec name] denotes the record field having [name] *)

(** Type of substitution paths *)
type t = el list


(** {6 High-level functions} *)

val parse : string -> t
(** [parse str] @return a substitution path represented by string [str].

    Syntax:

      "." ->
        separates path elements; must be present at start of string.

      "\[4\]" ->
        specifies the 4th element in a tuple.

      "some_tag\[4\]" ->
        tries to match [some_tag], then denotes its 4th argument.

      "name" ->
        denotes record field having [name]

    Example from test code:

      ".t.x.B[1]" -> choose record field with name [t], then subfield
      [x].  Match this value against [B], and denote its first argument.
*)

val get : ?path : t -> ?str : string -> Sexp.t -> Sexp.t
(** [get ?path ?str sexp] if [path] is provided, use it as path.
    Otherwise, if [str] is provided, parse it as a path.  If neither
    is provided, assume an empty path.  @return the sub-expression from
    S-expression [sexp] denoted by the path. *)

val replace : ?path : t -> ?str : string -> Sexp.t -> subst : Sexp.t -> Sexp.t
(** [replace ?path ?str sexp ~subst] like [get], but does not extract
    a sub-expression but substitutes it with [subst].  @return resulting
    S-expression.  *)

val replace_no_path : str : string -> Sexp.t -> subst : Sexp.t -> Sexp.t
(** [replace_no_path ~str sexp ~subst] like [replace], but does not take
    optional arguments.  [str] must be specified. *)

val subst_path : Sexp.t -> t -> (Sexp.t -> Sexp.t) * Sexp.t
(** [subst_path sexp path] @return the tuple [(subst, sub)], where [subst]
    is a function that returns an S-expression in which the subexpression
    denoted by [path] in [sexp] has been substituted by its argument.
    [sub] is the denoted subexpression.  Note that [subst sub = sexp]. *)


(** {6 Low-level functions} *)

val extract_pos : int -> Sexp.t -> (Sexp.t option -> Sexp.t) * Sexp.t
(** [extract_pos n sexp] @return the tuple [(subst, sub)], where [subst]
    is a function that returns an S-expression in which the subexpression
    denoted at position [n] in [sexp], which must be a list, has been
    substituted by [value] if the optional argument is [Some value], or
    removes the denoted subexpression if the optional argument is [None].
    [sub] is the denoted subexpression.  Note that [subst (Some sub) =
    sexp]. *)

val extract_match :
  string -> int -> Sexp.t -> (Sexp.t option -> Sexp.t) * Sexp.t
(** [extract_match tag n sexp] @return the tuple [(subst, sub)], where
    [subst] is a function that returns an S-expression in which the
    subexpression denoted by matching [tag] and taking its [n]th argument
    in [sexp] has been substituted by [value] if the argument is [Some
    value], or removes the denoted subexpression if the optional argument
    is [None].  [sub] is the denoted subexpression.  Note that [subst
    (Some sub) = sexp]. *)

val extract_rec : string -> Sexp.t -> (Sexp.t -> Sexp.t) * Sexp.t
(** [extract_rec name sexp] @return the tuple [(subst, sub)], where
    [subst] is a function that returns an S-expression in which the
    subexpression denoted by matching field name [name] in [sexp] has
    been substituted by its argument.  [sub] is the denoted subexpression.
    Note that [subst (Some sub) = sexp]. *)
