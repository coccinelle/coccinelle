
type regexp = Pcre.regexp option * Str.regexp option

let pcre_support = ref true

let regexp string =
  if !pcre_support
  then (Some (Pcre.regexp string), None)
  else (None, Some (Str.regexp string))

let string_match regexp string =
  match regexp with
    (Some regexp,_) -> Pcre.pmatch ~rex:regexp string
  | (_,Some regexp) ->
      try
	Str.search_forward regexp string 0;
	true
      with _ -> false
