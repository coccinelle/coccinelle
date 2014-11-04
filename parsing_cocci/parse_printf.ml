(* %[parameter][flags][width][.precision][length]type *)

exception Not_format_string

let suffix s n = String.sub s n (String.length s - n)

let string_of_char c = Printf.sprintf "%c" c

let safe_get s n = try String.get s n with _ -> raise Not_format_string

let check_parameter s =
  match Str.split (Str.regexp_string "$") s with
    a::rest ->
      (try
	let _ = int_of_string a in
	(Some a, String.concat "$" rest)
      with Failure "int_of_string" -> (None, s))
  | _ -> (None, s)

let check_flags s =
  let c1 = safe_get s 0 in
  match c1 with
    '+' | ' ' | '-' | '#' | '0' -> (Some (string_of_char c1), suffix s 1)
  | _ -> (None, s)

let check_width s =
  if safe_get s 0 = '*'
  then (Some "*", suffix s 1)
  else
    let re = Str.regexp "[0-9]+" in
    if Str.string_match re s 0
    then
      let front = Str.matched_string s in
      let front_len = String.length front in
      (Some front, suffix s front_len)
    else (None, s)

let check_precision s =
  match safe_get s 0 with
    '.' ->
      let s1 = suffix s 1 in
      (match check_width s1 with
	(Some n, s) -> (Some ("."^n), s)
      |	(None, s) -> raise Not_format_string)
  | _ -> (None, s)

(* PRI macros not supported *)
let check_length s =
  let c1 = safe_get s 0 in
  match c1 with
    'h' ->
      let s1 = suffix s 1 in
      let c2 = safe_get s1 0 in
      (match c2 with
	'h' -> (Some "hh", suffix s1 1)
      |	_ -> (Some "h", s1))
  | 'l' ->
      let s1 = suffix s 1 in
      let c2 = safe_get s1 0 in
      (match c2 with
	'l' -> (Some "ll", suffix s1 1)
      |	_ -> (Some "l", s1))
  | 'L' | 'z' | 'j' | 't' -> (Some (string_of_char c1), suffix s 1)
  | 'I' ->
      let s1 = suffix s 1 in
      if Str.string_match (Str.regexp "32") s1 0 ||
	Str.string_match (Str.regexp "64") s1 0
      then (Some ("I"^Str.matched_string s1), suffix s1 2)
      else (Some "I", suffix s 1)
  | 'q' (* BSD *) -> (Some "q", suffix s 1)
  | _ -> (None, s)

let check_type s =
  let c1 = safe_get s 0 in
  match c1 with
    'd' | 'i' | 'u' | 'f' | 'F' | 'e' | 'E' | 'g' | 'G' | 'x' | 'X' |
    'o' | 's' | 'c' | 'p' | 'a' | 'A' | 'n' -> (string_of_char c1, suffix s 1)
  | 'D' when !Flag.ibm ->
      (match safe_get s 1 with
	'(' ->
	  (try
	    let final = Str.search_forward (Str.regexp_string ")") s 2 in
	    let len = final + 1 in
	    (String.sub s 0 len, suffix s len)
	  with _ -> raise Not_format_string)
      |	_ -> raise Not_format_string)
  | _ -> raise Not_format_string

(* perhaps useful for ocaml scripts *)
let get_all_pieces s =
  let (p,s) = check_parameter s in
  let (f,s) = check_flags s in
  let (w,s) = check_width s in
  let (pr,s) = check_precision s in
  let (l,s) = check_length s in
  let (t,s) = check_type s in
  (p,f,w,pr,l,t,s)

let unsome = function Some s -> s | None -> ""

let get_format_string s =
  let (p,f,w,pr,l,t,s) = get_all_pieces s in
  (((unsome p) ^ (unsome f) ^ (unsome w) ^ (unsome pr) ^ (unsome l) ^ t), s)
