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

let register exc exc_name =
  Conv.Exn_converter.add_auto exc (fun _exc -> Sexp.Atom exc_name)

let magic_field repr n = Obj.magic (Obj.field repr n)

let register1 make_exc exc_name
      sexp_of_arg1 =
  let exc =
    make_exc
      (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1;
    ])

let register2 make_exc exc_name
      sexp_of_arg1 sexp_of_arg2 =
  let exc =
    make_exc
      (Obj.magic 0) (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    let sexp2 = sexp_of_arg2 (magic_field repr 2) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1; sexp2;
    ])

let register3 make_exc exc_name
      sexp_of_arg1 sexp_of_arg2 sexp_of_arg3 =
  let exc =
    make_exc
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    let sexp2 = sexp_of_arg2 (magic_field repr 2) in
    let sexp3 = sexp_of_arg3 (magic_field repr 3) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1; sexp2; sexp3;
    ])

let register4 make_exc exc_name
      sexp_of_arg1 sexp_of_arg2 sexp_of_arg3 sexp_of_arg4 =
  let exc =
    make_exc
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    let sexp2 = sexp_of_arg2 (magic_field repr 2) in
    let sexp3 = sexp_of_arg3 (magic_field repr 3) in
    let sexp4 = sexp_of_arg4 (magic_field repr 4) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1; sexp2; sexp3; sexp4;
    ])

let register5 make_exc exc_name
      sexp_of_arg1 sexp_of_arg2 sexp_of_arg3 sexp_of_arg4 sexp_of_arg5 =
  let exc =
    make_exc
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    let sexp2 = sexp_of_arg2 (magic_field repr 2) in
    let sexp3 = sexp_of_arg3 (magic_field repr 3) in
    let sexp4 = sexp_of_arg4 (magic_field repr 4) in
    let sexp5 = sexp_of_arg5 (magic_field repr 5) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1; sexp2; sexp3; sexp4; sexp5;
    ])

let register6 make_exc exc_name
      sexp_of_arg1 sexp_of_arg2 sexp_of_arg3 sexp_of_arg4 sexp_of_arg5
      sexp_of_arg6 =
  let exc =
    make_exc
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
      (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    let sexp2 = sexp_of_arg2 (magic_field repr 2) in
    let sexp3 = sexp_of_arg3 (magic_field repr 3) in
    let sexp4 = sexp_of_arg4 (magic_field repr 4) in
    let sexp5 = sexp_of_arg5 (magic_field repr 5) in
    let sexp6 = sexp_of_arg6 (magic_field repr 6) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1; sexp2; sexp3; sexp4; sexp5; sexp6;
    ])

let register7 make_exc exc_name
      sexp_of_arg1 sexp_of_arg2 sexp_of_arg3 sexp_of_arg4 sexp_of_arg5
      sexp_of_arg6 sexp_of_arg7 =
  let exc =
    make_exc
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
      (Obj.magic 0) (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    let sexp2 = sexp_of_arg2 (magic_field repr 2) in
    let sexp3 = sexp_of_arg3 (magic_field repr 3) in
    let sexp4 = sexp_of_arg4 (magic_field repr 4) in
    let sexp5 = sexp_of_arg5 (magic_field repr 5) in
    let sexp6 = sexp_of_arg6 (magic_field repr 6) in
    let sexp7 = sexp_of_arg7 (magic_field repr 7) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1; sexp2; sexp3; sexp4; sexp5; sexp6; sexp7;
    ])

let register8 make_exc exc_name
      sexp_of_arg1 sexp_of_arg2 sexp_of_arg3 sexp_of_arg4 sexp_of_arg5
      sexp_of_arg6 sexp_of_arg7 sexp_of_arg8 =
  let exc =
    make_exc
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    let sexp2 = sexp_of_arg2 (magic_field repr 2) in
    let sexp3 = sexp_of_arg3 (magic_field repr 3) in
    let sexp4 = sexp_of_arg4 (magic_field repr 4) in
    let sexp5 = sexp_of_arg5 (magic_field repr 5) in
    let sexp6 = sexp_of_arg6 (magic_field repr 6) in
    let sexp7 = sexp_of_arg7 (magic_field repr 7) in
    let sexp8 = sexp_of_arg8 (magic_field repr 8) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1; sexp2; sexp3; sexp4; sexp5; sexp6; sexp7; sexp8;
    ])

let register9 make_exc exc_name
      sexp_of_arg1 sexp_of_arg2 sexp_of_arg3 sexp_of_arg4 sexp_of_arg5
      sexp_of_arg6 sexp_of_arg7 sexp_of_arg8 sexp_of_arg9 =
  let exc =
    make_exc
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    let sexp2 = sexp_of_arg2 (magic_field repr 2) in
    let sexp3 = sexp_of_arg3 (magic_field repr 3) in
    let sexp4 = sexp_of_arg4 (magic_field repr 4) in
    let sexp5 = sexp_of_arg5 (magic_field repr 5) in
    let sexp6 = sexp_of_arg6 (magic_field repr 6) in
    let sexp7 = sexp_of_arg7 (magic_field repr 7) in
    let sexp8 = sexp_of_arg8 (magic_field repr 8) in
    let sexp9 = sexp_of_arg9 (magic_field repr 9) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1; sexp2; sexp3; sexp4; sexp5; sexp6; sexp7; sexp8; sexp9;
    ])

let register10 make_exc exc_name
      sexp_of_arg1 sexp_of_arg2 sexp_of_arg3 sexp_of_arg4 sexp_of_arg5
      sexp_of_arg6 sexp_of_arg7 sexp_of_arg8 sexp_of_arg9 sexp_of_arg10 =
  let exc =
    make_exc
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
      (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0) (Obj.magic 0)
  in
  Conv.Exn_converter.add_auto exc (fun exc ->
    let repr = Obj.repr exc in
    let sexp1 = sexp_of_arg1 (magic_field repr 1) in
    let sexp2 = sexp_of_arg2 (magic_field repr 2) in
    let sexp3 = sexp_of_arg3 (magic_field repr 3) in
    let sexp4 = sexp_of_arg4 (magic_field repr 4) in
    let sexp5 = sexp_of_arg5 (magic_field repr 5) in
    let sexp6 = sexp_of_arg6 (magic_field repr 6) in
    let sexp7 = sexp_of_arg7 (magic_field repr 7) in
    let sexp8 = sexp_of_arg8 (magic_field repr 8) in
    let sexp9 = sexp_of_arg9 (magic_field repr 9) in
    let sexp10 = sexp_of_arg10 (magic_field repr 10) in
    Sexp.List [
      Sexp.Atom exc_name;
      sexp1; sexp2; sexp3; sexp4; sexp5; sexp6; sexp7; sexp8; sexp9; sexp10;
    ])
