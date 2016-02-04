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

(* Call [tick msg] to stop timing a task and start timing the next
   task.  A message is displayed. The message includes [msg] as well
   as timing information.  The very first task is deemed to begin when
   this module is initialized. *)

val tick: string -> unit

(* Another timing method, with separate chronometers; useful for more
   precise profiling. *)

type chrono

val fresh: unit -> chrono

val chrono: chrono -> (unit -> 'a) -> 'a

val display: chrono -> string -> unit

