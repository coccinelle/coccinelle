let lowercase = String.lowercase

let rec mapi_from f i l =
  match l with
    [] -> []
  | hd :: tl -> f i hd :: mapi_from f (succ i) tl

let mapi f l =
  mapi_from f 0 l
