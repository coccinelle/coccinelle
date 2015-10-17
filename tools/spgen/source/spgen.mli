(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Driver module for spgen.
 *
 * Options:
 *  - config: the name of the file to draw user input from
 *  - output: the name of the file to save the generated file (default: stdout)
 *  - interactive: if true, draw user input interactively
 *  - default: if true, generate without user input (using default values)
 *  - hide: if true, do not output the generated file
 *)

type options

val make_options :
  ?config:string ->
  ?output:string ->
  ?interactive:bool ->
  ?default:bool ->
  ?hide:bool ->
  string -> (* filename of cocci file to generate *)
  options

val run : options -> unit
