(* File: conv.mli

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

(** Conv: Utility Module for S-expression Conversions *)

open Bigarray

(** Dummy definition for "optional" options *)
type 'a sexp_option = 'a option


(** {6 Conversion of OCaml-values to S-expressions} *)

val default_string_of_float : (float -> string) ref
(** [default_string_of_float] reference to the default function used
    to convert floats to strings.

    Initially set to [fun n -> sprintf "%.20G" n].
*)

val write_old_option_format : bool ref
(** [write_old_option_format] reference for the default option format
    used to write option values.  If set to [true], the old-style option
    format will be used, the new-style one otherwise.

    Initially set to [true].
*)

val read_old_option_format : bool ref
(** [read_old_option_format] reference for the default option format
    used to read option values.  [Of_sexp_error] will be raised with
    old-style option values if this reference is set to [false].
    Reading new-style option values is always supported.

    Initially set to [true].
*)

val sexp_of_unit : unit -> Sexp.t
(** [sexp_of_unit ()] converts a value of type [unit] to an S-expression. *)

val sexp_of_bool : bool -> Sexp.t
(** [sexp_of_bool b] converts the value [x] of type [bool] to an
    S-expression. *)

val sexp_of_string : string -> Sexp.t
(** [sexp_of_bool str] converts the value [str] of type [string] to an
    S-expression. *)

val sexp_of_char : char -> Sexp.t
(** [sexp_of_char c] converts the value [c] of type [char] to an
    S-expression. *)

val sexp_of_int : int -> Sexp.t
(** [sexp_of_int n] converts the value [n] of type [int] to an
    S-expression. *)

val sexp_of_float : float -> Sexp.t
(** [sexp_of_float n] converts the value [n] of type [float] to an
    S-expression. *)

val sexp_of_int32 : int32 -> Sexp.t
(** [sexp_of_int32 n] converts the value [n] of type [int32] to an
    S-expression. *)

val sexp_of_int64 : int64 -> Sexp.t
(** [sexp_of_int64 n] converts the value [n] of type [int64] to an
    S-expression. *)

val sexp_of_nativeint : nativeint -> Sexp.t
(** [sexp_of_nativeint n] converts the value [n] of type [nativeint] to an
    S-expression. *)

val sexp_of_big_int : Big_int.big_int -> Sexp.t
(** [sexp_of_big_int n] converts the value [n] of type [Big_int.big_int]
    to an S-expression. *)

val sexp_of_nat : Nat.nat -> Sexp.t
(** [sexp_of_nat n] converts the value [n] of type [Nat.nat] to an
    S-expression. *)

val sexp_of_num : Num.num -> Sexp.t
(** [sexp_of_num n] converts the value [n] of type [Num.num] to an
    S-expression. *)

val sexp_of_ratio : Ratio.ratio -> Sexp.t
(** [sexp_of_ratio n] converts the value [n] of type [Ratio.ratio] to an
    S-expression. *)

val sexp_of_ref : ('a -> 'b) -> 'a ref -> 'b
(** [sexp_of_ref conv r] converts the value [r] of type ['a ref] to
    an S-expression.  Uses [conv] to convert values of type ['a] to an
    S-expression. *)

val sexp_of_lazy : ('a -> 'b) -> 'a lazy_t -> 'b
(** [sexp_of_ref conv l] converts the value [l] of type ['a lazy_t] to
    an S-expression.  Uses [conv] to convert values of type ['a] to an
    S-expression. *)

val sexp_of_option : ('a -> Sexp.t) -> 'a option -> Sexp.t
(** [sexp_of_option conv opt] converts the value [opt] of type ['a
    option] to an S-expression.  Uses [conv] to convert values of type
    ['a] to an S-expression. *)

val sexp_of_pair : ('a -> Sexp.t) -> ('b -> Sexp.t) -> 'a * 'b -> Sexp.t
(** [sexp_of_pair conv1 conv2 pair] converts a pair to an S-expression.
    It uses its first argument to convert the first element of the pair,
    and its second argument to convert the second element of the pair. *)

val sexp_of_triple :
  ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> 'a * 'b * 'c -> Sexp.t
(** [sexp_of_triple conv1 conv2 conv3 triple] converts a triple to
    an S-expression using [conv1], [conv2], and [conv3] to convert its
    elements. *)

val sexp_of_list : ('a -> Sexp.t) -> 'a list -> Sexp.t
(** [sexp_of_list conv lst] converts the value [lst] of type ['a
    list] to an S-expression.  Uses [conv] to convert values of type
    ['a] to an S-expression. *)

val sexp_of_array : ('a -> Sexp.t) -> 'a array -> Sexp.t
(** [sexp_of_array conv ar] converts the value [ar] of type ['a
    array] to an S-expression.  Uses [conv] to convert values of type
    ['a] to an S-expression. *)

val sexp_of_hashtbl :
  ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) Hashtbl.t -> Sexp.t
(** [sexp_of_hashtbl conv_key conv_value htbl] converts the value [htbl]
    of type [('a, 'b) Hashtbl.t] to an S-expression.  Uses [conv_key]
    to convert the hashtable keys of type ['a], and [conv_value] to
    convert hashtable values of type ['b] to S-expressions. *)

val sexp_of_float32_vec :
  (float, float32_elt, fortran_layout) Array1.t -> Sexp.t
(** [sexp_of_float32_vec vec] converts the one-dimensional bigarray
    [vec] of 32-bit floats in Fortran-layout to an S-expression. *)

val sexp_of_float64_vec :
  (float, float64_elt, fortran_layout) Array1.t -> Sexp.t
(** [sexp_of_float64_vec vec] converts the one-dimensional bigarray
    [vec] of 64-bit floats in Fortran-layout to an S-expression. *)

val sexp_of_vec : (float, float64_elt, fortran_layout) Array1.t -> Sexp.t
(** [sexp_of_vec vec] same as {!Conv.sexp_of_float64_vec}. *)

val sexp_of_float32_mat :
  (float, float32_elt, fortran_layout) Array2.t -> Sexp.t
(** [sexp_of_float32_mat mat] converts the two-dimensional bigarray
    [mat] of 32-bit floats in Fortran-layout to an S-expression. *)

val sexp_of_float64_mat :
  (float, float64_elt, fortran_layout) Array2.t -> Sexp.t
(** [sexp_of_float64_mat mat] converts the two-dimensional bigarray
    [mat] of 64-bit floats in Fortran-layout to an S-expression. *)

val sexp_of_mat : (float, float64_elt, fortran_layout) Array2.t -> Sexp.t
(** [sexp_of_mat mat] same as {!Conv.sexp_of_float64_mat}. *)

val sexp_of_abstr : 'a -> Sexp.t
(** [sexp_of_abstr x] converts the value [x] of abstract type to an
    S-expression. *)

val sexp_of_fun : ('a -> 'b) -> Sexp.t
(** [sexp_of_fun f] converts the value [f] of function type to an
    S-expression. *)

(** Type used for declaring existing types as opaque to sexp converters,
    i.e. even if there is a defined converter for ['a], it will not
    be called.  This is useful to e.g. suppress printing of extremely
    large sub-structures for purely informational, human-readable output.
*)
type 'a opaque = 'a

val sexp_of_opaque : ('a -> Sexp.t) -> 'a opaque -> Sexp.t
(** [sexp_of_opaque _ _] converts opaque OCaml-values to S-expressions. *)

val string_of__of__sexp_of : ('a -> Sexp.t) -> 'a -> string
(** [string_of__of__sexp_of conv x] converts the OCaml-value [x] to
    an S-expression represented as a string by using conversion function
    [conv]. *)


(** {6 Conversion of S-expressions to OCaml-values} *)

exception Of_sexp_error of string * Sexp.t
(** [Of_sexp_error (reason, sexp)] the exception raised when an
    S-expression could not be successfully converted to an OCaml-value. *)

val record_check_extra_fields : bool ref
(** [record_check_extra_fields] checks for extra (= unknown) fields
    in record S-expressions. *)

val of_sexp_error : string -> Sexp.t -> 'a
(** [of_sexp_error reason sexp] @raise the exception [Of_sexp_error
    (reason, sexp)]. *)

val unit_of_sexp : Sexp.t -> unit
(** [unit_of_sexp sexp] converts S-expression [sexp] to a value of type
    [unit]. *)

val bool_of_sexp : Sexp.t -> bool
(** [bool_of_sexp sexp] converts S-expression [sexp] to a value of type
    [bool]. *)

val string_of_sexp : Sexp.t -> string
(** [string_of_sexp sexp] converts S-expression [sexp] to a value of type
    [string]. *)

val char_of_sexp : Sexp.t -> char
(** [char_of_sexp sexp] converts S-expression [sexp] to a value of type
    [char]. *)

val int_of_sexp : Sexp.t -> int
(** [int_of_sexp sexp] converts S-expression [sexp] to a value of type
    [int]. *)

val float_of_sexp : Sexp.t -> float
(** [float_of_sexp sexp] converts S-expression [sexp] to a value of type
    [float]. *)

val int32_of_sexp : Sexp.t -> int32
(** [int32_of_sexp sexp] converts S-expression [sexp] to a value of type
    [int32]. *)

val int64_of_sexp : Sexp.t -> int64
(** [int64_of_sexp sexp] converts S-expression [sexp] to a value of type
    [int64]. *)

val nativeint_of_sexp : Sexp.t -> nativeint
(** [nativeint_of_sexp sexp] converts S-expression [sexp] to a value
    of type [nativeint]. *)

val big_int_of_sexp : Sexp.t -> Big_int.big_int
(** [big_int_of_sexp sexp] converts S-expression [sexp] to a value
    of type [Big_int.big_int]. *)

val nat_of_sexp : Sexp.t -> Nat.nat
(** [nat_of_sexp sexp] converts S-expression [sexp] to a value
    of type [Nat.nat]. *)

val num_of_sexp : Sexp.t -> Num.num
(** [num_of_sexp sexp] converts S-expression [sexp] to a value
    of type [Nat.num]. *)

val ratio_of_sexp : Sexp.t -> Ratio.ratio
(** [ratio_of_sexp sexp] converts S-expression [sexp] to a value
    of type [Nat.ratio]. *)

val ref_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a ref
(** [ref_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a ref] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)

val lazy_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a lazy_t
(** [lazy_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a lazy_t] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)

val option_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a option
(** [option_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a option] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)

val pair_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> 'a * 'b
(** [pair_of_sexp conv1 conv2 sexp] converts S-expression [sexp] to a pair
    of type ['a * 'b] using conversion functions [conv1] and [conv2],
    which convert S-expressions to values of type ['a] and ['b]
    respectively. *)

val triple_of_sexp :
  (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> 'a * 'b * 'c
(** [triple_of_sexp conv1 conv2 conv3 sexp] converts S-expression [sexp]
    to a triple of type ['a * 'b * 'c] using conversion functions [conv1],
    [conv2], and [conv3], which convert S-expressions to values of type
    ['a], ['b], and ['c] respectively. *)

val list_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a list
(** [list_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a list] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)

val array_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a array
(** [array_of_sexp conv sexp] converts S-expression [sexp] to a value
    of type ['a array] using conversion function [conv], which converts
    an S-expression to a value of type ['a]. *)

val hashtbl_of_sexp :
  (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) Hashtbl.t
(** [hashtbl_of_sexp conv_key conv_value sexp] converts S-expression
    [sexp] to a value of type [('a, 'b) Hashtbl.t] using conversion
    function [conv_key], which converts an S-expression to hashtable
    key of type ['a], and function [conv_value], which converts an
    S-expression to hashtable value of type ['b]. *)

val float32_vec_of_sexp :
  Sexp.t -> (float, float32_elt, fortran_layout) Array1.t
(** [float32_vec_of_sexp sexp] converts S-expression [sexp] to a
    one-dimensional bigarray of 32-bit floats in Fortran-layout. *)

val float64_vec_of_sexp :
  Sexp.t -> (float, float64_elt, fortran_layout) Array1.t
(** [float64_vec_of_sexp sexp] converts S-expression [sexp] to a
    one-dimensional bigarray of 64-bit floats in Fortran-layout. *)

val vec_of_sexp : Sexp.t -> (float, float64_elt, fortran_layout) Array1.t
(** [vec_of_sexp sexp] same as {!float64_vec_of_sexp}. *)

val float32_mat_of_sexp :
  Sexp.t -> (float, float32_elt, fortran_layout) Array2.t
(** [float32_mat_of_sexp sexp] converts S-expression [sexp] to a
    two-dimensional bigarray of 32-bit floats in Fortran-layout. *)

val float64_mat_of_sexp :
  Sexp.t -> (float, float64_elt, fortran_layout) Array2.t
(** [float64_mat_of_sexp sexp] converts S-expression [sexp] to a
    two-dimensional bigarray of 64-bit floats in Fortran-layout. *)

val mat_of_sexp : Sexp.t -> (float, float64_elt, fortran_layout) Array2.t
(** [mat_of_sexp sexp] same as {!Conv.float64_mat_of_sexp}. *)

val fun_of_sexp : Sexp.t -> ('a -> 'b)
(** [fun_of_sexp sexp] @raise a conversion error when attempting to
    convert an S-expression to a function. *)

val of_string__of__of_sexp : (Sexp.t -> 'a) -> string -> 'a
(** [of_string__of__of_sexp conv str] converts the S-expression [str]
    represented as a string to an OCaml-value by using conversion function
    [conv]. *)
