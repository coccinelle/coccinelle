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

(** This module implements a very simple notion of ``mark''. A mark is
    really a reference cell (without content). Creating a new mark
    requires allocating a new cell, and comparing marks requires
    comparing pointers. *)

type t =
    unit ref

let fresh =
  ref

let same =
  (==)

let none =
  fresh()

