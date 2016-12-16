@initialize:ocaml@
@@

let nm = Hashtbl.create 11

@script:ocaml@
@@

Printf.printf "file start: %d\n" (Hashtbl.length nm)

@r@
identifier f;
position p;
@@

f@p(...)

@script:ocaml@
p << r.p;
@@

let file = (List.hd p).file in
if not (Hashtbl.mem nm file) then Hashtbl.add nm file ()

@finalize:ocaml@
nms << merge.nm;
@@

List.iteri
  (fun i nm -> Hashtbl.iter (fun k v -> Printf.printf "%d: %s\n" i k) nm)
  nms
