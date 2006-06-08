open Fullcommon

type patchinfo = (filename, fileinfo) oassoc
  and fileinfo = ((int * int) * string) list

val parse_patch : string list -> patchinfo

val relevant_part : filename * (int * int) -> patchinfo -> string

val filter_driver_sound : string list -> string list
