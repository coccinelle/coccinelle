let member x y = List.mem x y

let rec is_set = function
    [] -> true
  | (x::xs) -> (not(member x xs)) && (is_set xs)

let rec make_set = function
    [] -> []
  | (x::xs) -> if member x xs then make_set xs else x :: (make_set xs)

let rec subset xs ys =
  match xs with
    [] -> true
  | x::xs -> (member x ys) && (subset xs ys)

let rec subset_fail xs ys =
  match xs with
    [] -> None
  | x::xs ->
      if member x ys
      then subset_fail xs ys
      else Some x

let set_equal s1 s2 = subset s1 s2 && subset s2 s1

let rec intersect xs ys =
    match xs with
    [] -> []
  | x::xs -> if member x ys then x :: (intersect xs ys) else intersect xs ys

let rec intersect_all = function
    [] -> []
  | [x] -> x
  | x::xs -> intersect x (intersect_all xs)

let rec union xs ys =
    match xs with
    [] -> ys
  | x::xs -> if member x ys then union xs ys else x :: (union xs ys)

let rec union_list = function
    [] -> []
  | x::xs -> if member x xs then union_list xs else x :: (union_list xs)

let sorted_union xs ys =
  List.sort compare (union xs ys)

let set2c s =
  let rec loop = function
      [] -> ""
    | [x] -> x
    | x::xs -> x ^", "^(loop xs)
  in "{" ^ (loop s) ^ "}"

let rec remove v = function
    [] -> []
  | x :: xs -> if x = v then remove v xs else x :: remove v xs

(* subtract second argument from the first *)
let subtract a b =
  let rec loop = function
      [] -> []
    | x :: xs -> if member x b then loop xs else x :: loop xs in
  loop a

let rec option_filter f = function
    [] -> []
  | x :: xs ->
      (match f x with
	Some y -> y :: (option_filter f xs)
      |	None -> option_filter f xs)

let option_for_all2 f l1 l2 =
  let res =
    List.fold_left2
      (function res ->
	function e1 ->
	  function e2 ->
	    match res with
	      None -> None
	    | Some(res) ->
		(match f e1 e2 with
		  None -> None
		| Some(x) -> Some(x::res)))
      (Some([])) l1 l2
  in match res with
    None -> None
  | Some(x) -> Some (List.rev x)

let rec split3 = function
    [] -> ([],[],[])
  | (a,b,c)::xs -> let (ar,br,cr) = split3 xs in (a::ar,b::br,c::cr)

let rec combine3 l1 l2 l3 =
  let rec loop l1 l2 = function
      [] -> []
    | x::xs ->
	(List.hd l1, List.hd l2, x) :: (loop (List.tl l1) (List.tl l2) xs) in
  let len = List.length l1 in
  if List.length l2 = len && List.length l3 = len
  then loop l1 l2 l3
  else failwith "illegal arguments to combine3"

let flat_map f l = List.flatten (List.map f l)

let map_union f l =
  let res =
    List.fold_left
      (function res -> function x -> union (f x) res)
      [] l in
  List.rev res

let app_option f = function
    None -> None
  | Some x -> Some (f x)

let non_empty_prefixes l =
  List.fold_right
    (function cur ->
      function rest ->
	[cur] :: (List.map (function p -> cur::p) rest))
    l []

let rec non_empty_suffixes = function
    [] -> []
  | (x::xs) as cur -> cur :: non_empty_suffixes xs

let split_last lst =
  let tmp = List.rev lst in
  let last = List.hd tmp in
  let prev = List.rev(List.tl tmp) in
  (prev,last)

let list_find str f l =
  try List.find f l
  with Not_found -> failwith (Printf.sprintf "failed find in %s" str)

(* like list.partition, but only gives the first thing that matches the
function *)
let partition_one f l =
    let rec loop = function
	[] -> raise Not_found
      |	(x::xs) ->
	  if f x
	  then (x,xs)
	  else
	    let (thing,rest) = loop xs in
	    (thing,x::rest) in
    try Some (loop l)
    with Not_found -> None

(* --------------------------- Partition --------------------------- *)

let rec elem_in_each_position elem = function
    [] -> []
  | (curpos::rest) ->
      ((elem::curpos)::rest) ::
      (List.map (function x -> curpos :: x) (elem_in_each_position elem rest))

let rec mkall = function
    ([],r) -> Some [r]
  | (_,[]) -> None
  | (elem::others,rest) ->
      (match
	option_filter (function result -> mkall (others,result))
	  (elem_in_each_position elem rest)
      with
	[] -> None
      |	l -> Some (List.concat l))

let partition l n =
  let rec loop = function
      0 -> []
    | n -> [] :: (loop (n-1)) in
  let start = loop n in
  match mkall (l, start) with
    None -> []
  | Some l -> l

(* ----------------------------- Stack ----------------------------- *)

let push x cell = cell := x::(!cell)
let pop cell = cell := (List.tl (!cell))
let top cell = List.hd (!cell)
let union_top l cell =
  cell := (union l (List.hd (!cell))) :: (List.tl (!cell))
