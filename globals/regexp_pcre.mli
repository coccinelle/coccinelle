type regexp = Pcre of int | Str of Str.regexp
val pcre_support : bool ref
val regexp : string -> regexp
val string_match : regexp -> string -> bool
