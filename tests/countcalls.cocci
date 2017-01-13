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

let merge_hashtbls op target source =
  let add_item key value =
    match
      try Some (Hashtbl.find target key)
      with Not_found -> None
    with
      None -> Hashtbl.add target key value
    | Some value' -> Hashtbl.replace target key (op value value') in
  Hashtbl.iter add_item source

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
let tbl =
  match tbls with
    [] -> failwith "Unexpected empty table list"
  | tbl :: others ->
      List.iter (merge_hashtbls ( + ) tbl) others;
      tbl in
List.iter
  (function (v,f) -> Printf.printf "%s: %d\n" f v)
  (List.rev
    (List.sort compare
      (Hashtbl.fold (fun k v r -> (v,k)::r) tbl [])))
