
type regexp =
    Pcre of Pcre.regexp
  | Str of Str.regexp

let pcre_support = ref true

let regexp string =
  if !pcre_support
  then Pcre (Pcre.regexp string)
  else Str (Str.regexp string)

let string_match regexp string =
  match regexp with
      Pcre regexp -> Pcre.pmatch ~rex:regexp string
    | Str regexp ->
      try
	ignore(Str.search_forward regexp string 0);
	true
      with _ -> false
