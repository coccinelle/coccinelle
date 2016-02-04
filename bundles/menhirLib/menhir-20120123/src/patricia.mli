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

(* This is an implementation of Patricia trees, following Chris Okasaki's paper at the 1998 ML Workshop in Baltimore.
   Both big-endian and little-endian trees are provided. Both sets and maps are implemented on top of Patricia
   trees. *)

module Little : GMap.S with type key = int

module Big : GMap.S with type key = int

