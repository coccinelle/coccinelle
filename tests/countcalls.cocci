@initialize:ocaml@
@@

let tbl = Hashtbl.create 101
let local = ref []

let inc f =
  match
    try Some (Hashtbl.find tbl f)
    with Not_found -> None
  with
    None -> Hashtbl.add tbl f 1
  | Some count -> Hashtbl.replace tbl f (succ count)

let merge_hashtbls hashtbls op =
  let result = Hashtbl.create 101 in
  let add_item key value =
    match
      try Some (Hashtbl.find result key)
      with Not_found -> None
    with
      None -> Hashtbl.add result key value
    | Some value' -> Hashtbl.replace result key (op value value') in
  let add_hashtbl hashtbl = Hashtbl.iter add_item hashtbl in
  List.iter add_hashtbl hashtbls;
  result

@script:ocaml@
@@
local := []

@r@
identifier f;
@@

f(...) { ... }

@script:ocaml@
f << r.f;
@@

if not (List.mem f !local) then local := f :: !local

@r1@
identifier f;
type T;
@@

T f(...);

@script:ocaml@
f << r1.f;
@@

if not (List.mem f !local) then local := f :: !local

@s@
identifier f;
@@
f(...);

@script:ocaml@
f << s.f;
@@

if not (List.mem f !local) && String.lowercase f = f then inc f

@finalize:ocaml@
tbls << merge.tbl;
@@
let tbl = merge_hashtbls tbls ( + ) in
List.iter
  (function (v,f) -> Printf.printf "%s: %d\n" f v)
  (List.rev
    (List.sort compare
      (Hashtbl.fold (fun k v r -> (v,k)::r) tbl [])))
