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

include Map.Make (String)

let cardinal s =
  fold (fun _ _ x -> x + 1) s 0 

let filter pred map =
  fold (fun key value map ->
	  if pred key value then 
	    add key value map
	  else 
	    map) map empty

let restrict domain map =
  filter (fun k _ -> StringSet.mem k domain) map

let domain map = 
  fold (fun key _ acu -> StringSet.add key acu) map StringSet.empty
