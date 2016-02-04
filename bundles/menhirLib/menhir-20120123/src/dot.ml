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

open Printf

(* ------------------------------------------------------------------------- *)

(* Type definitions. *)

type size =
    float * float (* in inches *)

type orientation =
  | Portrait
  | Landscape

type rankdir =
  | LeftToRight
  | TopToBottom

type ratio =
  | Compress
  | Fill
  | Auto

type style =

    (* Both nodes and edges. *)

  | Solid
  | Dashed
  | Dotted
  | Bold
  | Invisible

    (* Nodes only. *)

  | Filled
  | Diagonals
  | Rounded

(* ------------------------------------------------------------------------- *)

(* Basic printers. *)

let print_style = function
  | None ->
      ""
  | Some style ->
      let style =
	match style with
	| Solid ->
	    "solid"
	| Dashed ->
	    "dashed"
	| Dotted ->
	    "dotted"
	| Bold ->
	    "bold"
	| Invisible ->
	    "invis"
	| Filled ->
	    "filled"
	| Diagonals ->
	    "diagonals"
	| Rounded ->
	    "rounded"
      in
      sprintf ", style = %s" style

(* ------------------------------------------------------------------------- *)

(* The graph printer. *)

module Print (G : sig

  type vertex

  val name: vertex -> string

  val successors: (?style:style -> label:string -> vertex -> unit) -> vertex -> unit

  val iter: (?style:style -> label:string -> vertex -> unit) -> unit

end) = struct

  let print
      ?(directed = true)
      ?size
      ?(orientation = Landscape)
      ?(rankdir = LeftToRight)
      ?(ratio = Compress)
      (f : out_channel)
      =

    fprintf f "%s G {\n" (if directed then "digraph" else "graph");
    Option.iter (fun (hsize, vsize) ->
      fprintf f "size=\"%f, %f\";\n" hsize vsize
    ) size;
    begin match orientation with
      | Portrait ->
	  fprintf f "orientation = portrait;\n"
      | Landscape ->
	  fprintf f "orientation = landscape;\n"
    end;
    begin match rankdir with
      | LeftToRight ->
	  fprintf f "rankdir = LR;\n"
      | TopToBottom ->
	  fprintf f "rankdir = TB;\n"
    end;
    begin match ratio with
      | Compress ->
	  fprintf f "ratio = compress;\n"
      | Fill ->
	  fprintf f "ratio = fill;\n"
      | Auto ->
	  fprintf f "ratio = auto;\n"
    end;

    G.iter (fun ?style ~label vertex ->
      fprintf f "%s [ label=\"%s\"%s ] ;\n"
	(G.name vertex)
	label
	(print_style style)
    );

    G.iter (fun ?style ~label source ->
      G.successors (fun ?style ~label destination ->
	fprintf f "%s %s %s [ label=\"%s\"%s ] ;\n"
	  (G.name source)
	  (if directed then "->" else "--")
	  (G.name destination)
	  label
	  (print_style style)
      ) source
    );

    fprintf f "\n}\n"

end

