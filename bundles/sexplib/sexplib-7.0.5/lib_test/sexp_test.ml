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

(** Sexp_test: Module for Testing S-expression I/O.

    Example invocation: "sexp_test < test.sexp"
*)

open Format
open Sexplib
open Sexp

(*
let input_sexps ic =
  let lexbuf = Lexing.from_channel ic in
  scan_sexps lexbuf
*)

let () =
  let orig_sexps = input_sexps stdin in

  let hum_file = "/tmp/__hum.sexp" in
  let hum_oc = open_out hum_file in
  let hum_ppf = formatter_of_out_channel hum_oc in
  List.iter
    (fun sexp -> fprintf hum_ppf "%a@\n" Sexp.pp_hum sexp)
    orig_sexps;
  pp_print_flush hum_ppf ();
  close_out hum_oc;

  let mach_file = "/tmp/__mach.sexp" in
  let mach_oc = open_out mach_file in
  List.iter
    (fun sexp -> Printf.fprintf mach_oc "%a\n" Sexp.output_mach sexp)
    orig_sexps;
  close_out mach_oc;

  let hum_ic = open_in hum_file in
  let hum_sexps = input_sexps hum_ic in
  close_in hum_ic;
  assert (hum_sexps = orig_sexps);
  Sys.remove hum_file;

  let mach_ic = open_in mach_file in
  let mach_sexps = input_sexps mach_ic in
  close_in mach_ic;
  assert (mach_sexps = orig_sexps);
  Sys.remove mach_file;

  printf "Parsing S-expressions: SUCCESS!!!@."
