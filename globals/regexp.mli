type regexp = Regexp_pcre.regexp = Pcre of int | Str of Str.regexp
val pcre_table : (int, Pcre.regexp) Hashtbl.t
val pcre_ctr : int ref
val pcre_support : bool ref
val regexp : string -> regexp
val string_match : regexp -> string -> bool
