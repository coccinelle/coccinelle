let () =
  assert (Stdcompat.hypot 3. 4. = 5.);
  assert (Stdcompat.copysign 1. 2. = 1.);
  assert (Stdcompat.copysign 1. (-. 2.) = -. 1.);
  assert (Stdcompat.copysign (-. 1.) 2. = 1.);
  assert (Stdcompat.copysign (-. 1.) (-. 2.) = -. 1.);
  assert (
    try
      ignore (Stdcompat.raise_notrace Exit);
      false
    with Exit -> true);
  assert (Stdcompat.bool_of_string_opt "true" = Some true);
  assert (Stdcompat.bool_of_string_opt "false" = Some false);
  assert (Stdcompat.bool_of_string_opt "foo" = None);
  assert (Stdcompat.int_of_string_opt "42" = Some 42);
  assert (Stdcompat.int_of_string_opt "foo" = None);
  assert (Stdcompat.float_of_string_opt "42." = Some 42.);
  assert (Stdcompat.float_of_string_opt "foo" = None);
  assert (Lazy.force (Stdcompat.Lazy.from_fun (fun () -> 42)) = 42);
  assert (Lazy.force (Stdcompat.Lazy.from_val 42) = 42);
  assert (Stdcompat.Char.lowercase_ascii 'A' = 'a');
  assert (Stdcompat.Char.uppercase_ascii 'a' = 'A');
  assert (Stdcompat.Char.equal 'a' 'a');
  assert (not (Stdcompat.Char.equal 'A' 'a'));
  assert (Stdcompat.String.init 2 (fun i -> char_of_int i) = "\000\001");
  assert
    (Stdcompat.String.mapi
       (fun i c -> char_of_int (i + int_of_char c)) "abc" = "ace");
  assert (
    let s = Stdcompat.Bytes.create 3 in
    Stdcompat.String.iteri (fun i c -> Stdcompat.Bytes.set s i c) "abc";
    s = Stdcompat.Bytes.of_string "abc");
  assert (Stdcompat.String.map Stdcompat.Char.uppercase_ascii "abc" = "ABC");
  assert (Stdcompat.String.trim " \t abc\n" = "abc");
  assert (Stdcompat.String.lowercase_ascii "AbcD" = "abcd");
  assert (Stdcompat.String.uppercase_ascii "AbcD" = "ABCD");
  assert (Stdcompat.String.capitalize_ascii "abcD" = "AbcD");
  assert (Stdcompat.String.uncapitalize_ascii "AbcD" = "abcD");
  assert (Stdcompat.String.equal "abc" "abc");
  assert (not (Stdcompat.String.equal "Abc" "abc"));
  assert (Stdcompat.String.split_on_char ' ' " abc  d ef  "
    = ["";"abc";"";"d";"ef";"";""]);
  assert (Stdcompat.String.index_opt "abaababa" 'a' = Some 0);
  assert (Stdcompat.String.index_opt "abaababa" 'c' = None);
  assert (Stdcompat.String.rindex_opt "abaababa" 'a' = Some 7);
  assert (Stdcompat.String.rindex_opt "abaababa" 'c' = None);
  assert (Stdcompat.String.index_from_opt "abaababa" 1 'a' = Some 2);
  assert (Stdcompat.String.index_from_opt "abaababa" 1 'c' = None);
  assert (Stdcompat.String.rindex_from_opt "abaababa" 4 'a' = Some 3);
  assert (Stdcompat.String.rindex_from_opt "abaababa" 4 'c' = None);
  assert (
    let s = Stack.create () in
    Stack.push 1 s;
    Stack.push 2 s;
    Stack.push 3 s;
    Stdcompat.Stack.fold ( + ) 0 s = 6 &&
    Stack.length s = 3);
  assert (
    let t = Hashtbl.create 17 in
    Hashtbl.add t 1 2;
    Hashtbl.add t 3 4;
    (Stdcompat.Hashtbl.stats t).Stdcompat.Hashtbl.num_bindings = 2);
  assert (
    let t = Hashtbl.create 17 in
    Hashtbl.add t 1 1;
    Hashtbl.add t 2 2;
    Hashtbl.add t 2 3;
    Hashtbl.add t 3 4;
    Stdcompat.Hashtbl.filter_map_inplace
        (fun k v -> if k = 3 then None else Some (pred v)) t;
    Hashtbl.find_all t 1 = [0] &&
    Hashtbl.find_all t 2 = [2; 1] &&
    Hashtbl.find_all t 3 = []);
  assert (
    let t = Hashtbl.create 17 in
    Hashtbl.add t 1 1;
    Hashtbl.add t 2 2;
    Stdcompat.Hashtbl.find_opt t 1 = Some 1 &&
    Stdcompat.Hashtbl.find_opt t 3 = None);
  assert (
    let module H = struct
      type t = int

      let equal : int -> int -> bool = ( = )

      let hash : int -> int = fun x -> x
    end in
    let module M = Hashtbl.Make (H) in
    let module M' = Stdcompat.Hashtbl.Make (H) in
    let t = M.create 17 in
    M.add t 1 1;
    M.add t 2 2;
    M'.find_opt t 1 = Some 1 &&
    M'.find_opt t 3 = None);
  assert (
    let module S = Set.Make (String) in
    let module S' = Stdcompat.Set.Make (String) in
    let s = S'.of_list ["a"; "b"; "c"] in
    S.compare
      (S'.map (fun s -> s ^ ".") s)
      (S'.of_list ["a."; "b."; "c."]) = 0 &&
    S'.find_opt "a" s = Some "a" &&
    S'.find_opt "d" s = None &&
    S'.find_first (fun s -> s >= "b") s = "b" &&
    S'.find_last (fun s -> s <= "b") s = "b");
  assert (
    let module M = Map.Make (String) in
    let module M' = Stdcompat.Map.Make (String) in
    let m = M.add "1" 2 M.empty in
    M'.compare compare (
      M'.update "1" (function None -> Some 0 | Some i -> Some (i + 1))
      (M'.update "2" (function None -> Some 0 | Some _ -> None)
         (M'.update "3" (function None -> None | Some i -> Some i) m)))
      (M.add "1" 3 (M.add "2" 0 M.empty)) = 0);
  assert (
    let b = Stdcompat.Bytes.of_string "hello" in
    let b = Stdcompat.Bytes.extend b (-1) 2 in
    Stdcompat.Bytes.sub_string b 0 4 = "ello" &&
    Stdcompat.Bytes.length b = 6);
  assert (
    let l = ref [] in
    let f a b =
      l := (a, b) :: !l in
    Stdcompat.List.iteri f [1; 2; 3];
    !l = [2, 3; 1, 2; 0, 1]);
  assert (
    let f a b =
      (a, b) in
    Stdcompat.List.mapi f  [1; 2; 3] = [0, 1; 1, 2; 2, 3]);
  assert (
    Stdcompat.List.sort_uniq compare  [2; 1; 3; 2; 1; 3]
    = [1; 2; 3]);
  assert (Stdcompat.List.cons 1 [2; 3] = [1; 2; 3]);
  assert (Stdcompat.List.compare_lengths [1] [2; 3] < 0);
  assert (Stdcompat.List.compare_lengths [1; 2] [2; 3] = 0);
  assert (Stdcompat.List.compare_lengths [1; 2; 3] [2; 3] > 0);
  assert (Stdcompat.List.compare_length_with [1] 2 < 0);
  assert (Stdcompat.List.compare_length_with [1; 2] 2 = 0);
  assert (Stdcompat.List.compare_length_with [1; 2; 3] 2 > 0);;
  assert (Stdcompat.List.nth_opt [1; 2; 3] 2 = Some 3);
  assert (Stdcompat.List.nth_opt [1; 2; 3] 3 = None);
  assert (
    try
      ignore (Stdcompat.List.nth_opt [1; 2; 3] (-1));
      false
    with Invalid_argument _ -> true);
  assert (Stdcompat.List.find_opt (fun i -> i mod 2 = 0) [1; 2; 3] = Some 2);
  assert (Stdcompat.List.find_opt (fun i -> i mod 4 = 0) [1; 2; 3] = None);
  assert (Stdcompat.List.assoc_opt 2 [1, 0; 2, 1; 3, 2] = Some 1);
  assert (Stdcompat.List.assoc_opt 4 [1, 0; 2, 1; 3, 2] = None);
  assert (Stdcompat.List.assq_opt 2 [1, 0; 2, 1; 3, 2] = Some 1);
  assert (Stdcompat.List.assq_opt "a" ["a", 1; "b", 2; "c", 3] = None);
  assert (Stdcompat.Filename.extension "a.b/c.de" = ".de");
  assert (Stdcompat.Filename.extension "a.b/cd" = "");
  assert (Stdcompat.Filename.remove_extension "a.b/c.de" = "a.b/c");
  assert (Stdcompat.Filename.remove_extension "a.b/cd" = "a.b/cd");
  assert (
    let array = Stdcompat.Array.Floatarray.create 2 in
    Stdcompat.Array.Floatarray.set array 0 1.;
    Stdcompat.Array.Floatarray.set array 1 2.;
    Stdcompat.Array.Floatarray.get array 0 = 1. &&
    Stdcompat.Array.Floatarray.get array 1 = 2.);
  assert (
    let l = ref [] in
    let f a b =
      l := (a, b) :: !l in
    Stdcompat.Array.iter2 f [| 0; 1 |] [| 2; 3 |];
    !l = [1, 3; 0, 2]);
  assert (
    let f a b =
      (a, b) in
    Stdcompat.Array.map2 f  [| 0; 1 |] [| 2; 3 |] = [| 0, 2; 1, 3 |]);
  assert (Stdcompat.Array.for_all (fun x -> x > 0) [| 1; 2; 3 |]);
  assert (not (Stdcompat.Array.for_all (fun x -> x > 0) [| 1; 2; 0; 3 |]));
  assert (Stdcompat.Array.exists (fun x -> x > 2) [| 1; 2; 3 |]);
  assert (not (Stdcompat.Array.exists (fun x -> x > 3) [| 1; 2; 3 |]));
  assert (Stdcompat.Array.mem "a" [| "a"; "b"; "c" |]);
  assert (not (Stdcompat.Array.mem "d" [| "a"; "b"; "c" |]));
  assert (Stdcompat.Array.memq 2 [| 1; 2; 3 |]);
  assert (not (Stdcompat.Array.memq "a" [| "a"; "b"; "c" |]));
  assert (
    let q = Stdcompat.Queue.create () in
    Stdcompat.Array.of_seq (Stdcompat.Queue.to_seq q) = [| |]);
  assert (
    let q = Stdcompat.Queue.create () in
    Stdcompat.Queue.add_seq q (Stdcompat.List.to_seq ["a"; "b"; "c"]);
    Stdcompat.Array.of_seq (Stdcompat.Queue.to_seq q) = [| "a"; "b"; "c" |]);
  assert (
    let l = Stdcompat.List.to_seq ["a", 1; "b", 2; "c", 3] in
    let module M = Stdcompat.Map.Make (String) in
    let q = Stdcompat.Hashtbl.of_seq (M.to_seq (M.of_seq l)) in
    let m = M.of_seq (Stdcompat.Hashtbl.to_seq q) in
    M.cardinal m = 3 && M.find "a" m = 1 && M.find "b" m = 2
      && M.find "c" m = 3)
