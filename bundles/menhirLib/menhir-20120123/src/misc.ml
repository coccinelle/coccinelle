(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

let ( $$ ) x f = f x

let unSome = function
   None -> assert false
  | Some x -> x

let o2s o f =
  match o with
  | None ->
      ""
  | Some x ->
      f x

let single = function
  | [ x ] ->
      x
  | _ ->
      assert false

let rec mapd f = function
  | [] ->
      []
  | x :: xs ->
      let y1, y2 = f x in
      y1 :: y2 :: mapd f xs

let tabulateb n f =
  let a = Array.init n f in
  Array.get a,
  Array.fold_left (fun count element ->
    if element then count + 1 else count
  ) 0 a

let tabulateo number fold n f =
  let a = Array.create n None in
  let c = ref 0 in
  let () = fold (fun () element ->
    let image = f element in
    a.(number element) <- image;
    match image with
    | Some _ ->
	incr c
    | None ->
	()
  ) () in
  let get element =
    a.(number element)
  in
  get, !c

let rec truncate k xs =
  match k, xs with
  | 0, _ ->
      []
  | _, [] ->
      assert false
  | _, x :: xs ->
      x :: truncate (k-1) xs

let truncate k xs =
  if List.length xs <= k then xs else truncate k xs

module IntSet = Set.Make (struct 
			    type t = int
			    let compare = ( - )
			  end)

let separated_list_to_string printer separator list = 

  let rec loop x = function
    | [] ->
        printer x
    | y :: xs ->
        printer x 
	^ separator 
	^ loop y xs
  in

  match list with
  | [] ->
      ""
  | x :: xs ->
      loop x xs


let index_map string_map = 
  let n = StringMap.cardinal string_map in
  let a = Array.create n None in
  let conv, _ = StringMap.fold 
    (fun k v (conv, idx) ->
       a.(idx) <- Some (k, v);
       StringMap.add k idx conv, idx + 1)
    string_map (StringMap.empty, 0) 
  in
    ((fun n -> snd (unSome a.(n))),
     (fun k -> StringMap.find k conv),
     (fun n -> fst (unSome a.(n))))
  
let support_assoc l x =
  try
    List.assoc x l
  with Not_found -> x

let index (strings : string list) : int * string array * int StringMap.t =
  let name = Array.of_list strings
  and n, map = List.fold_left (fun (n, map) s ->
    n+1, StringMap.add s n map
  ) (0, StringMap.empty) strings in
  n, name, map

(* Turning an implicit list, stored using pointers through a hash
   table, into an explicit list. The head of the implicit list is
   not included in the explicit list. *)

let materialize (table : ('a, 'a option) Hashtbl.t) (x : 'a) : 'a list =
  let rec loop x =
    match Hashtbl.find table x with
    | None ->
	[]
    | Some x ->
	x :: loop x
  in
  loop x

(* [iteri] implements a [for] loop over integers, from 0 to
   [n-1]. *)

let iteri n f =
  for i = 0 to n - 1 do
    f i
  done

(* [foldi] implements a [for] loop over integers, from 0 to [n-1],
   with an accumulator. [foldij] implements a [for] loop over
   integers, from [start] to [n-1], with an accumulator. *)

let foldij start n f accu =
  let rec loop i accu =
    if i = n then
      accu
    else
      loop (i+1) (f i accu)
  in
  loop start accu

let foldi n f accu =
  foldij 0 n f accu

(* [mapi n f] produces the list [ f 0; ... f (n-1) ]. *)

let mapi n f =
  List.rev (
    foldi n (fun i accu ->
      f i :: accu
    ) []
  )

(* [qfold f accu q] repeatedly takes an element [x] off the queue [q]
   and applies [f] to the accumulator and to [x], until [q] becomes
   empty. Of course, [f] can add elements to [q] as a side-effect.

   We allocate an option to ensure that [qfold] is tail-recursive. *)

let rec qfold f accu q =
  match
    try
      Some (Queue.take q)
    with Queue.Empty ->
      None
  with
  | Some x ->
      qfold f (f accu x) q
  | None ->
      accu

(* [qiter f q] repeatedly takes an element [x] off the queue [q] and
   applies [f] to [x], until [q] becomes empty. Of course, [f] can add
   elements to [q] as a side-effect. *)

let qiter f q =
  try
    while true do
      f (Queue.take q)
    done
  with Queue.Empty ->
    ()

let rec smap f = function
  | [] ->
      []
  | (x :: xs) as l ->
      let x' = f x
      and xs' = smap f xs in
      if x == x' && xs == xs' then
	l
      else
	x' :: xs'

let rec smapa f accu = function
  | [] ->
      accu, []
  | (x :: xs) as l ->
      let accu, x' = f accu x in
      let accu, xs' = smapa f accu xs in
      accu,
      if x == x' && xs == xs' then
	l
      else
	x' :: xs'

let normalize s =
  let s = String.copy s in
  let n = String.length s in
  for i = 0 to n - 1 do
    match s.[i] with
    | '('
    | ')'
    | ',' ->
	s.[i] <- '_'
    | _ ->
	()
  done;
  s

(* [postincrement r] increments [r] and returns its original value. *)

let postincrement r =
  let x = !r in
  r := x + 1;
  x

(* [gcp] returns the greatest common prefix of two strings. *)

let gcp s1 s2 =
  let n1 = String.length s1
  and n2 = String.length s2 in
  let rec loop i =
    if (i < n1) && (i < n2) && (s1.[i] = s2.[i]) then
      loop (i+1)
    else
      String.sub s1 0 i
  in
  loop 0

(* [gcps] returns the greatest common prefix of a nonempty list of strings. *)

let rec gcps = function
  | [] ->
      assert false
  | s :: ss ->
      let rec loop accu = function
	| [] ->
	    accu
	| s :: ss ->
	    loop (gcp s accu) ss
      in
      loop s ss

(* [array_forall p a] computes the conjunction of the predicate [p] over all
   elements of the array [a]. *)

exception ArrayForall

let _ArrayForall =
  ArrayForall

let array_forall (p : 'a -> bool) (a : 'a array) : bool =
  try
    for i = 0 to Array.length a - 1 do
      let x = Array.get a i in
      if not (p x) then
	raise _ArrayForall
    done;
    true
  with ArrayForall ->
    false

