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

open Positions
open Syntax

let current_token_precedence =
  let c = ref 0 in
  fun pos1 pos2 ->
    incr c;
    PrecedenceLevel (Error.get_filemark (), !c, pos1, pos2)
      
let current_reduce_precedence =
  let c = ref 0 in
  fun () ->
    incr c;
    PrecedenceLevel (Error.get_filemark (), !c, Lexing.dummy_pos, Lexing.dummy_pos)

module IdSet = Set.Make (struct
  type t = identifier located
  let compare id1 id2 =
    compare (value id1) (value id2)
end)

let defined_identifiers ((ido, _) : producer) accu =
  Option.fold IdSet.add ido accu

let defined_identifiers (producers : producer list) =
  List.fold_right defined_identifiers producers IdSet.empty

let check_production_group right_hand_sides pos1 pos2 action =

  begin
    match right_hand_sides with
    | [] ->
	assert false
    | ((producers : producer list), _, _, _) :: right_hand_sides ->
	let ids = defined_identifiers producers in
	List.iter (fun (producers, _, _, _) ->
	  let ids' = defined_identifiers producers in
	  try
	    let id =
	      IdSet.choose (IdSet.union
				  (IdSet.diff ids ids')
				  (IdSet.diff ids' ids))
	    in
	    Error.error [Positions.position id]
	      "Two productions that share a semantic action must define\n\
	       exactly the same identifiers."
	  with Not_found ->
	    ()
	  ) right_hand_sides
  end;
  begin
    if List.length right_hand_sides > 1 && Action.use_dollar action then
      Error.signal (Positions.two pos1 pos2)
	"A semantic action that is shared between several productions must\n\
 	 not use the $i notation -- semantic values must be named instead."
  end

let override pos o1 o2 =
  match o1, o2 with
  | Some _, Some _ ->
      Error.signal [ pos ] "This production carries two %prec declarations.";
      o2
  | None, Some _ ->
      o2
  | _, None ->
      o1
