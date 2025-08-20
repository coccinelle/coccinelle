(* Dealing with compiler versions *)

type extra_prefix = Plus | Tilde

let string_of_prefix = function
  | Plus -> "+"
  | Tilde -> "~"

type extra_info = extra_prefix * string

let string_of_extra_info (p, s) = (string_of_prefix p) ^ s

let string_of_extra_info_opt = function
  | None -> ""
  | Some ei -> string_of_extra_info ei

let ocaml_of_prefix = function
  | Plus -> "Plus"
  | Tilde -> "Tilde"

let ocaml_of_extra_info (p, s) =
  Printf.sprintf "(%s, \"%s\")" (ocaml_of_prefix p) s

let ocaml_of_extra_info_opt = function
  | None -> "None"
  | Some ei ->
    Printf.sprintf "Some %s" (ocaml_of_extra_info ei)

type t = {
  major : int;
  minor : int;
  patch_level : int;
  extra_info : extra_info option;
}

let major v = v.major
let minor v = v.minor
let patch_level v= v.patch_level

let extra_info v = v.extra_info

let mk x y z =
  { major = x; minor = y; patch_level = z; extra_info = None; }

(* Known compiler versions *)

let v3_08_0 = mk 3 8 0
let v3_09_0 = mk 3 9 0
let v3_10_0 = mk 3 10 0
let v3_11_0 = mk 3 11 0
let v3_12_0 = mk 3 12 0
let v3_12_1 = mk 3 12 1
let v4_00_0 = mk 4 0 0
let v4_00_1 = mk 4 0 1
let v4_01_0 = mk 4 1 0
let v4_02_0 = mk 4 2 0
let v4_02_1 = mk 4 2 1
let v4_02_2 = mk 4 2 2
let v4_02_3 = mk 4 2 3
let v4_03_0 = mk 4 3 0
let v4_04_0 = mk 4 4 0
let v4_05_0 = mk 4 5 0
let v4_06_0 = mk 4 6 0
let v4_06_1 = mk 4 6 1
let v4_07_0 = mk 4 7 0
let v4_07_1 = mk 4 7 1
let v4_08_0 = mk 4 8 0
let v4_08_1 = mk 4 8 1
let v4_09_0 = mk 4 9 0
let v4_09_1 = mk 4 9 1
let v4_10_0 = mk 4 10 0
let v4_10_1 = mk 4 10 1
let v4_10_2 = mk 4 10 2
let v4_11_0 = mk 4 11 0
let v4_11_1 = mk 4 11 1
let v4_11_2 = mk 4 11 2
let v4_12_0 = mk 4 12 0
let v4_12_1 = mk 4 12 1
let v4_13_0 = mk 4 13 0
let v4_13_1 = mk 4 13 1
let v4_14_0 = mk 4 14 0
let v4_14_1 = mk 4 14 1
let v4_14_2 = mk 4 14 2
let v5_0_0 = mk 5 0 0
let v5_1_0 = mk 5 1 0
let v5_1_1 = mk 5 1 1
let v5_2_0 = mk 5 2 0
let v5_2_1 = mk 5 2 1
let v5_3_0 = mk 5 3 0

let known_versions =
[
  v3_08_0;
  v3_09_0;
  v3_10_0;
  v3_11_0;
  v3_12_0;
  v3_12_1;
  v4_00_0;
  v4_00_1;
  v4_01_0;
  v4_02_0;
  v4_02_1;
  v4_02_2;
  v4_02_3;
  v4_03_0;
  v4_04_0;
  v4_05_0;
  v4_06_0;
  v4_06_1;
  v4_07_0;
  v4_07_1;
  v4_08_0;
  v4_08_1;
  v4_09_0;
  v4_09_1;
  v4_10_0;
  v4_10_1;
  v4_10_2;
  v4_11_0;
  v4_11_1;
  v4_11_2;
  v4_12_0;
  v4_12_1;
  v4_13_0;
  v4_13_1;
  v4_14_0;
  v4_14_1;
  v4_14_2;
  v5_0_0;
  v5_1_0;
  v5_1_1;
  v5_2_0;
  v5_2_1;
  v5_3_0;
]

let is_known v = List.mem v known_versions

let is_development _ = false

let make x y z extra_info =
  let v = mk x y z in
  if is_known v then { v with extra_info } else raise Not_found

let to_string { major; minor; patch_level; extra_info } =
  let printer = Printf.sprintf in
  let printer =
    if major>=5
    then printer "%d.%d.%d"
    else printer "%d.%02d.%d"
  in
  (printer major minor patch_level) ^
    (string_of_extra_info_opt extra_info)

let of_string version_string =
  let f major minor patch_level s = 
    let extra_info_opt = 
      if s="" then None
      else begin
        let prefix = 
          match s.[0] with
          | '+' -> Plus
          | '~' -> Tilde
          | _ -> Printf.sprintf "The string '%s' is not a valid OCaml compiler version" s |> failwith
        in
        let l = String.length s in
        let extra = String.sub s 1 (l-1) in
        Some (prefix, extra)
      end
    in
    make major minor patch_level extra_info_opt
  in
  Scanf.sscanf version_string "%d.%d.%d%s" f

let compare v1 v2 =
  if v1.major < v2.major then -1
  else if v1.major = v2.major then begin
    if v1.minor < v2.minor then -1
    else if v1.minor = v2.minor then begin
      if v1.patch_level < v2.patch_level then -1
      else if v1.patch_level = v2.patch_level then 0
      else 1
    end else 1
  end else 1

let for_all_pairs pred l =
  let rec for_all_pairs' = function
    | [] -> true
    | x::xs ->
      let pred' = pred x in
      (List.for_all pred' xs) && (for_all_pairs' xs)
  in
  for_all_pairs' l

let test_reflexivity () =
  let cmp_is_refl v = (compare v v) = 0 in
  let compare_is_reflexive =
    List.for_all cmp_is_refl known_versions
  in
  let tmp = if compare_is_reflexive then "" else "NOT " in
  Printf.eprintf
    "The comparison of compiler versions is %sreflexive\n%!" tmp

let test_ordering () =
  let is_smaller v1 v2 = (compare v1 v2) < 0 in
  let sorted = for_all_pairs is_smaller known_versions in
  let tmp = if sorted then "" else "NOT " in
  Printf.eprintf 
    "The list of known compiler versions is %ssorted\n%!" tmp

let test_antisymetry () =
  let pred v1 v2 =
    let n1 = compare v1 v2 in
    let n2 = compare v2 v1 in
    (n1<0 && n2>0) || (n1>0 && n2<0)
  in
  let tmp =
    if for_all_pairs pred known_versions
    then ""
    else "NOT "
  in
  Printf.eprintf
    "The ordering of versions is %santisymetric\n%!" tmp

let test () =
  test_reflexivity ();
  test_ordering ();
  test_antisymetry ()
