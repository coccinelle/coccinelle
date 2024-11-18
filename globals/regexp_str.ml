(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)


type regexp = Str.regexp

let pcre_support = ref false

let regexp string =
  Str.regexp string

let string_match regexp string =
  try
    Str.search_forward regexp string 0 >= 0
  with _ -> false
