(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

val cache_threshold : int

val elem_threshold : int

val include_headers_for_types : bool ref

type include_options =
    I_UNSPECIFIED | I_NO_INCLUDES | I_NORMAL_INCLUDES
  | I_ALL_INCLUDES | I_REALLY_ALL_INCLUDES

val include_options : include_options ref

val include_path : string list ref

val relax_include_path : bool ref
(** if true then when have a #include "../../xx.h", we look also for xx.h in
 * current directory. This is because of how works extract_c_and_res
 *)

val extra_includes : string list ref
