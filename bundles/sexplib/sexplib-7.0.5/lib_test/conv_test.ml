(******************************************************************************
 *                             Sexplib                                        *
 *                                                                            *
 * Copyright (C) 2005- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *    Author: Markus Mottl                                                    *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

(** Conv_test: module for testing automated S-expression conversions and
    path substitutions *)

open Format

open Sexplib
open Sexp
open Conv

module Exc_test : sig
  exception Test_exc of (string * int) with sexp
end = struct
  exception Test_exc of (string * int) with sexp
end

(* Test each character. *)
let check_string s =
  let s' =
    match (Sexp.of_string (Sexp.to_string (Sexp.Atom s))) with
    | Sexp.Atom s -> s
    | _ -> assert false
  in
  assert (s = s')

let () =
  for i = 0 to 255 do
    check_string (String.make 1 (Char.chr i))
  done

(* Test user specified conversion *)

type my_float = float

let sexp_of_my_float n = Atom (sprintf "%.4f" n)

let my_float_of_sexp = function
  | Atom str -> float_of_string str
  | _ -> failwith "my_float_of_sexp: atom expected"


(* Test simple sum of products *)

type foo = A | B of int * float
with sexp


(* Test polymorphic variants and deep module paths *)

module M = struct
  module N = struct
    type ('a, 'b) variant = [ `X of ('a, 'b) variant | `Y of 'a * 'b ]
    with sexp
    type test = [ `Test ]
    with sexp
  end
end

type 'a variant =
  [ M.N.test | `V1 of [ `Z | ('a, string) M.N.variant ] option | `V2 ]
with sexp

(* Test empty types *)

type empty with sexp

(* Test variance annotations *)

module type S = sig
  type +'a t with sexp
end

(* Test labeled arguments in functions *)

type labeled = string -> foo : unit -> ?bar : int -> float -> float with sexp

let f str ~foo ?(bar = 3) n = float_of_string str +. n +. float bar

let labeled_sexp : Sexp.t = sexp_of_labeled f
let labeled : labeled lazy_t = lazy (labeled_of_sexp (labeled_sexp : Sexp.t))

type rec_labeled = { a : (foo : unit -> unit) } with sexp_of

(* Test recursive types *)

(* Test polymorphic record fields *)

type 'x poly =
  {
    p : 'a 'b. 'a list;
    maybe_t : 'x t option;
  }

(* Test records *)

and 'a t =
  {
    x : foo;
    a : 'a variant;
    foo : int;
    bar : (my_float * string) list option;
    sexp_option : int sexp_option;
    sexp_list : int sexp_list;
    sexp_bool : sexp_bool;
    poly : 'a poly;
  }
with sexp

type v = { t : int t }

(* Test manifest types *)
type u = v = { t : int t }
with sexp

(* Test types involving exceptions *)
type exn_test = int * exn
with sexp_of

(* Test function types *)
type fun_test = int -> unit with sexp_of

open Path

let main () =
  let make_t a =
    {
      x = B (42, 3.1);
      a = a;
      foo = 3;
      bar = Some [(3.1, "foo")];
      sexp_option = None;
      sexp_list = [];
      sexp_bool = true;
      poly =
        {
          p = [];
          maybe_t = None;
        };
    }
  in
  let v = `B (5, 5) in
  let v_sexp = <:sexp_of<[ `A | `B of int * int ] >> v in
  assert (<:of_sexp< [ `A | `B of int * int ] >> v_sexp = v);
  let u = { t = make_t (`V1 (Some (`X (`Y (7, "bla"))))) } in
  let u_sexp = sexp_of_u u in
  printf "Original:      %a@\n@." pp u_sexp;
  let u' = u_of_sexp u_sexp in
  assert (u = u');
  let foo_sexp = Sexp.of_string "A" in
  let _foo = foo_of_sexp foo_sexp in

  let path_str = ".[0].[1]" in
  let path = Path.parse path_str in
  let subst, el = subst_path u_sexp path in
  printf "Pos(%s):       %a -> SUBST1@\n" path_str pp el;
  let dumb_sexp = subst (Atom "SUBST1") in
  printf "Pos(%s):    %a@\n@\n" path_str pp dumb_sexp;

  let path_str = ".t.x.B[1]" in
  let path = Path.parse path_str in
  let subst, el = subst_path u_sexp path in
  printf "Record(%s):    %a -> SUBST2@\n" path_str pp el;

  let u_sexp = subst (Atom "SUBST2") in
  printf "Record(%s): %a@\n@\n" path_str pp u_sexp;

  printf "SUCCESS!!!@."

let () =
  try main (); raise (Exc_test.Test_exc ("expected exception", 42)) with
  | exc -> eprintf "Exception: %s@." (Sexp.to_string_hum (sexp_of_exn exc))
