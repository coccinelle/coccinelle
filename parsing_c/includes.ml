(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

let cache_threshold = 500 (** caching of header file information *)

let elem_threshold = 10

let include_headers_for_types = ref false

type include_options =
    I_UNSPECIFIED | I_NO_INCLUDES | I_NORMAL_INCLUDES
  | I_ALL_INCLUDES | I_REALLY_ALL_INCLUDES

let include_options = ref I_UNSPECIFIED

let include_path = ref ([] : string list)
(* if true then when have a #include "../../xx.h", we look also for xx.h in
 * current directory. This is because of how works extract_c_and_res
 *)

let relax_include_path = ref false

let extra_includes = ref ([] : string list)
