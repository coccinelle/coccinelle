@initialize:ocaml@
@@
let ctr = ref 0
let make_fresh _ =
  ctr := !ctr + 1;
  Printf.sprintf "fresh%d" !ctr

@r1@
fresh identifier id = script:ocaml() { make_fresh() };
expression x;
@@
-bar (x);
+bar2 (id);

@r2@
identifier r1.id;
expression r1.x;
identifier old_id;
@@
-bar2 (old_id);
+bar3 (x, old_id, id);
