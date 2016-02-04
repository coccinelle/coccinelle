/*
Given a file with data about the integer values that variables
x can take at certain positions, this patch identifies some
then-branches of if(x) ... statements.

Run this example from the coccinelle main directory using:

  ./scripts/spatch --sp-file demos/external_ana.cocci demos/external_ana.c \
                   --external-analysis-file demos/external_ana.data

The analysis input was produced with a custom-made plugin for
frama-c that produced the external_ana.data file using the
commandline:

  frama-c -load-module ValueExport.cmxs -value-export \
          -export-file ./external_ana.data external_ana.c

*/

@r@
idexpression x;
position p, q;
statement S;
@@

if@q (x@p) S


@script:ocaml@
x << r.x;
p << r.p;
@@

let p1 = Coccilib.basename_pos (List.hd p) in
Printf.printf "considering %s at position %s:(%d,%d)-(%d,%d):\n"
  x p1.Coccilib.file p1.Coccilib.line p1.Coccilib.col
  p1.Coccilib.line_end p1.Coccilib.col_end;

let rs = Coccilib.Ana.find p1 in
Printf.printf "  results: %d\n" (List.length rs);
List.iter (fun r -> Printf.printf "  value: %s\n" (Coccilib.Ana.show_result r)) rs;

let is_z = Coccilib.Ana.has_only_nul p1 in
Printf.printf "  Always zero: %B\n" is_z;

Coccilib.include_match is_z


@@
position r.q;
statement S;
@@

- if@q (...) S
