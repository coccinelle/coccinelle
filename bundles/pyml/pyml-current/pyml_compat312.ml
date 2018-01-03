type floatarray = float array

let floatarray_create length = Array.make length 0.

let floatarray_length : floatarray -> int = Array.length

let floatarray_get : floatarray -> int -> float = Array.get

let floatarray_set : floatarray -> int -> float -> unit = Array.set

let lowercase = String.lowercase

let rec mapi_from f i l =
  match l with
    [] -> []
  | hd :: tl -> f i hd :: mapi_from f (succ i) tl

let mapi f l =
  mapi_from f 0 l

let lazy_from_fun = Lazy.lazy_from_fun

let is_blank char = char = ' '

let trim s =
  let length = String.length s in
  let rec skip_blank_left index =
    if index < length && is_blank (String.unsafe_get s index) then
      skip_blank_left (succ index)
    else
      index in
  let left_index = skip_blank_left 0 in
  let rec skip_blank_right index =
    if index >= 0 && is_blank (String.unsafe_get s index) then
      skip_blank_right (pred index)
    else
      index in
  let right_index = skip_blank_right (pred length) in
  Pyutils.substring_between s left_index (succ right_index)

let index_opt string c = Pyutils.option_find (String.index string) c

let index_from_opt string from c =
  Pyutils.option_find (String.index_from string from) c

let rec split_on_char_aux string from accu separator =
  match index_from_opt string from separator with
    None ->
      let word =
        Pyutils.substring_between string from (String.length string) in
      List.rev_append accu [word]
  | Some position ->
      let word = Pyutils.substring_between string from position in
      split_on_char_aux string (succ position) (word :: accu) separator

let split_on_char separator string = split_on_char_aux string 0 [] separator

let list_find_opt p l = Pyutils.option_find (List.find p) l

let getenv_opt var = Pyutils.option_find Sys.getenv var
