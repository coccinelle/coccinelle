
type regexp = Str.regexp

let pcre_support = ref false

let regexp string =
  Str.regexp string

let string_match regexp string =
  try
    Str.search_forward regexp string 0 >= 0
  with _ -> false
