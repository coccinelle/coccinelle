
type regexp = Pcre.regexp

let regexp string =
  Pcre.regexp string

let string_match regexp string =
  Pcre.pmatch ~rex:regexp string
