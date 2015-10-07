(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val include_headers_for_types : bool ref

val is_header : string -> bool
(**
 * [is_header filename] returns (true] if filename ends with ".h"
 *)

type parsing_style =
  | Parse_no_includes
  | Parse_normal_includes
  | Parse_all_includes
  | Parse_really_all_includes

val string_of_parsing_style : parsing_style -> string
val get_parsing_style : unit -> parsing_style
val set_parsing_style : parsing_style -> unit
val is_parsing_style_set : unit -> bool
val parse_all_includes : parsing_style -> bool

val include_path : string list ref

val relax_include_path : bool ref
(** if true then when have a #include "../../xx.h", we look also for xx.h in
 * current directory. This is because of how works extract_c_and_res
 *)

val extra_includes : string list ref

val interpret_include_path : string list -> string option

val should_parse : parsing_style -> string -> Ast_c.inc_file -> bool
(**
 * [should_parse ps fn incl] returns [true] if the header reerred to
 * by the [incl] directive contained in [fn] should be parsed.
 *)

val resolve : string -> parsing_style -> Ast_c.inc_file -> string option
(**
 * [reslove f opt inc] determines whether [inc] included by [f]
 * exists and should be parsed according to [opt].
 * If so, returns its name. Returns [None] otherwise.
 *)
