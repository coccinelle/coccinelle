@r@
expression x,a;
position p1, p2;
@@

f(x@p1,a@p2)

@script:ocaml@
y << r.x;
yy << r.a;
p1 << r.p1;
p2 << r.p2;
@@

let p1_ = List.hd p1 in
let file1 = p1_.Coccilib.file in
let cure1 = p1_.Coccilib.current_element in
let line1 = p1_.Coccilib.line in
let line_end1 = p1_.Coccilib.line_end in
let colb1 = p1_.Coccilib.col in
let cole1 = p1_.Coccilib.col_end in

Printf.printf "%s and %s\n" y yy;
Printf.printf
	"%s @ p1 (file:\"%s\" fct:\"%s\" line:%d-%d col:%d-%d)\n"
	y file1 cure1 line1 line_end1 colb1 cole1;
Printf.printf "%s @ p2 - p2 is not used in the SP. ocamlc should have reported a warning.\n" yy

@script:ocaml@
y << r.a;
zz << r.x;
@@

Printf.printf "%s again and %s again \n" zz y
