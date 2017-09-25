@initialize:ocaml@
@@

let tbl = Hashtbl.create 101

let hashadd k v =
  let cell =
    try Hashtbl.find tbl k
    with Not_found ->
      let cell = ref [] in
      Hashtbl.add tbl k cell;
      cell in
  if not (List.mem v !cell) then cell := v :: !cell

@locker@
position p;
@@

mutex_lock@p (...);

@unlocker@
position p1;
@@

mutex_unlock@p1 (...);

// -------------------------------------------------------------------

@ref1 exists@
identifier lock,unlock;
position locker.p;
position any unlocker.p1;
expression a,e;
position pa;
@@

lock@p(e,...)
... when != unlock@p1(e,...)
a@pa

@ref1_1@
identifier lock;
position locker.p;
expression ref1.a,ref1.e;
global idexpression x : script:ocaml() {not(String.uppercase(x) = x)};
@@

 lock@p(e,...);
 (<+...x@a...+>);

@script:ocaml depends on ref1_1@
p << locker.p;
a << ref1.pa;
@@

hashadd p a

// -------------------------------------------------------------------

@ref2 exists@
identifier lock,unlock;
position any locker.p;
position unlocker.p1;
expression a,e;
position pa;
@@

a@pa
... when != lock@p
unlock@p1(e,...)

@ref2_1@
identifier unlock;
position unlocker.p1;
expression ref2.a,ref2.e;
global idexpression x : script:ocaml() {not(String.uppercase(x) = x)};
@@

 (<+...x@a...+>);
 unlock@p1(e,...);

@script:ocaml depends on ref2_1@
p << unlocker.p1;
a << ref2.pa;
@@

hashadd p a

// -------------------------------------------------------------------

@finalize:ocaml@
tbls << merge.tbl;
@@

List.iter
  (function t ->
    Hashtbl.iter (fun k v -> List.iter (fun v -> hashadd k v) !v) t)
  tbls;
let infos =
  Hashtbl.fold
    (fun k v r -> if List.length !v = 1 then (k,List.hd !v)::r else r)
    tbl [] in
let infos = List.sort compare infos in
List.iter
  (fun (k,v) ->
    Printf.printf "k: %s:%d\n" (List.hd k).file (List.hd k).line;
    List.iter
      (function k ->
	Printf.printf "v: %s:%d\n" k.file k.line)
      v)
  infos
