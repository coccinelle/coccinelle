@initialize:ocaml@
@@

let print p =
 Printf.printf "%d: %s\n" (List.hd p).line (List.hd p).current_element

@r@
position p;
identifier f;
@@

f@p(...) { ... }

@script:ocaml@
p << r.p;
@@

if (List.hd p).line <> 4
then Coccilib.include_match false

@@
position r.p;
identifier f;
@@

-f@p
+g
  (...) { ... }
