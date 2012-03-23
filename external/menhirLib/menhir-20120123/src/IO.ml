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

(* Input-output utilities. *)

(* ------------------------------------------------------------------------- *)
(* [exhaust channel] reads all of the data that's available on [channel]. *)

let chunk_size =
  2048

let exhaust channel =
  let buffer = Buffer.create chunk_size in
  let chunk = String.create chunk_size in
  let rec loop () =
    let length = input channel chunk 0 chunk_size in
    if length = 0 then
      Buffer.contents buffer
    else begin
      Buffer.add_substring buffer chunk 0 length;
      loop()
    end
  in
  loop()

(* ------------------------------------------------------------------------- *)
(* [invoke command] invokes an external command (which expects no
   input) and returns its output, if the command succeeds. It returns
   [None] if the command fails. *)

let invoke command =
  let ic = Unix.open_process_in command in
  let result = exhaust ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 ->
      Some result
  | _ ->
      None

(* ------------------------------------------------------------------------- *)
(* [winvoke writers command cleaners] invokes each of the [writer]
   functions, invokes the command [command], and runs each of the
   [cleaner] functions. Then, it either returns the command's output,
   if the command succeeded, or exits, otherwise. *)

let winvoke writers command cleaners =
  let call action =
    action ()
  in
  List.iter call writers;
  let output = invoke command in
  List.iter call cleaners;

  (* Stop if the command failed. Otherwise, return its output. *)

  match output with
  | None ->
      (* Presumably, the command printed an error message for us. *)
      exit 1
  | Some output ->
      output

