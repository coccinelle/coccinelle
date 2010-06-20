@initialize:ocaml@

let filename = "/tmp/pgtest"
let db =
    Dbm.opendbm
	filename
	[Dbm.Dbm_rdwr;Dbm.Dbm_create]
	600

let _ = Printf.eprintf "Applies to %s\n" (Coccilib.dir ())

(*
open Postgresql

  We must use a lib. not available in coccinelle.

  http://caml.inria.fr/pub/docs/manual-ocaml/manual039.html
	ocamlc other options graphics.cma other files

  http://caml.inria.fr/pub/docs/manual-ocaml/manual040.html
	ocamlc other options dbm.cma other files
	ocamlopt other options dbm.cmxa other files

  http://caml.inria.fr/pub/docs/manual-ocaml/manual042.html
        ocamlc other options -I +labltk labltk.cma other files
        ocamlopt other options -I +labltk labltk.cmxa other files
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

Dbm.add db y yy;
if Str.string_match (Str.regexp "^foo") f 0
then Printf.eprintf "Fct '%s' matchs \"^foo\"\n" f
else Printf.eprintf "Fct '%s' does not match \"^foo\"\n" f

@finalize:ocaml@

Dbm.iter (fun key data -> Printf.printf "'%s' goes with '%s'\n" key data) db;
Dbm.close db;
Sys.remove (filename^".dir");
Sys.remove (filename^".pag")

(*

()

*)
