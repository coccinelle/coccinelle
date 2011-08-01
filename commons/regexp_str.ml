
type regexp = Str.regexp

let support = "Str"

let regexp string =
  Str.regexp string

let string_match regexp string =
  Str.string_match regexp string 0
