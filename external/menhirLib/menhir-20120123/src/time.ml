(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

let channel =
  stderr

open Unix
open Printf

let clock =
  ref (times())

let tick msg =
  if Settings.timings then
    let times1 = !clock in
    let times2 = times() in
    fprintf channel "%s: %.02fs\n"
      msg
      (times2.tms_utime -. times1.tms_utime);
    clock := times()

type chrono =
    float ref

let fresh () =
  ref 0.

let chrono (chrono : float ref) (task : unit -> 'a) : 'a =
  if Settings.timings then begin
    let times1 = times() in
    let result = task() in
    let times2 = times() in
    chrono := !chrono +. times2.tms_utime -. times1.tms_utime;
    result
  end
  else
    task()

let display (chrono : float ref) msg =
  if Settings.timings then
    fprintf channel "%s: %.02fs\n"
      msg
      !chrono

