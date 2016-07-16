@initialize:ocaml@
@@

let watched_ids = ["a"; "c"]

let is_watched id =
    List.mem id watched_ids

@@
identifier id : script:ocaml() { is_watched(id) };
@@

-id
+18