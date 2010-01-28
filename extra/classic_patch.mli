open Common

(* used by tools/split_patch *)

type patch = patchitem list
   and patchitem = File of filename * string (* header line *) * string list

val parse_patch : filename -> patch
val unparse_patch : patch -> filename (* outfile *) -> unit
