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

(* $Id $*)
(* This module parses ocaml version and confronts it with a user-provided
   version. *)

(* According to OCaml's manual, the Sys.ocaml_version value follows the
   regexp [version_regexp].
*)
let mnum = "\\([0-9]+\\)"

(* version = major.minor[.patchlevel][+additional-info]. *)
let version_regexp = 
  Str.regexp 
    (Printf.sprintf "%s\\.%s\\(\\.%s\\)?\\(\\+\\(.+\\)\\)?" mnum mnum mnum)

let must field = function
  | None -> failwith (Printf.sprintf "\"%s\" field is undefined." field)
  | Some s -> s

let as_int s = 
  try
    int_of_string s
  with Failure _ ->
    Printf.eprintf "Invalid number '%s'\n" s;
    exit 1

let parse_version version =
  let get i = 
    try
      Some (Str.matched_group i version)
    with Not_found ->
      None
  in
    if Str.string_match version_regexp version 0 then (
      as_int (must "major" (get 1)), 
      as_int (must "minor" (get 2)), 
      get 4, get 6
    ) else
      begin
	Printf.eprintf "Failed to retrieve ocaml version.\n";
	exit 1
      end

(* The user can compare its version with three different orderings:
   - eq means major and minor numbers are equal ;
   - eq-strict means that even the patchlevel and the additional information
     are equal ;
   - lt means that ocaml version is older that the user-provided version ;
   - gt means that ocaml version is newer that the user-provided version. *)
let eq, eq_strict, gt, lt = ref false, ref false, ref false, ref false

let verbose = ref false

let options = Arg.align
  [
    "--eq", Arg.Set eq, " Is the version equal to <version> ?";
    "--eq-strict", Arg.Set eq_strict, 
    " Is the version strictly equal to <version> ? \
      (taking into account patchlevel and additional information)";
    "--gt", Arg.Set gt, " Is the version newer than <version> ? (default)";
    "--lt", Arg.Set lt, " Is the version older than <version> ?";
    "--verbose", Arg.Set verbose, " Show version."
  ]
  
let usage = "check-ocaml-version [options] <version>\n"

let version = ref None

let set_version s = 
  version := Some s

let _ =
  Arg.parse options set_version usage

let compare, compare_str, strict = 
  match !eq, !gt, !lt with
    | true, false, false -> ( = ) , "", !eq_strict
    | false, true, false -> ( >= ), "or greater ", false
    | false, false, true -> ( <= ), "or lesser ", false
    | false, false, false -> (Printf.printf "%s\n%!" Sys.ocaml_version; exit 1)
    | _ -> failwith "(eq|gt|lt) flags must be used independently"

let compare_version (major, minor, p, a) (major', minor', p', a') =
  if major = major' then 
    if minor = minor' then
      if strict then
	(p = p') && (a = a')
      else true
    else compare minor minor'
  else 
    compare major major' 

let _ =

  match !version with
    | None ->
	Printf.printf "%s\n%!" Sys.ocaml_version

    | Some version ->
	let ov = parse_version Sys.ocaml_version 
	and uv = parse_version version in
	if compare_version ov uv then begin
	  if !verbose then
	    Printf.printf "Version %s is OK.\n%!" Sys.ocaml_version;
	  exit 0
	end
	else begin
	  if !verbose then
	    Printf.printf "%s is NOT OK: version %s %swas required.%!\n" Sys.ocaml_version version compare_str;
	  exit 1
	end

