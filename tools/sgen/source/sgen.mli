(* Driver module for sgen.
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
