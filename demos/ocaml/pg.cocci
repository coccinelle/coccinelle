@initialize:ocaml@
@@
open Postgresql

(*
let conn =
    let conninfo = "" in
    new Postgresql.connection ~conninfo ()
*)

@r@
identifier f;
expression x,a;
@@

f(x,a)

@script:ocaml@
f << r.f;
y << r.x;
yy << r.a;
@@

if Str.string_match (Str.regexp "^foo") f 0
then Printf.eprintf "Fct '%s' matches \"^foo\"\n" f
else Printf.eprintf "Fct '%s' does not match \"^foo\"\n" f

@finalize:ocaml@
@@
()

(*
conn#finish
*)

