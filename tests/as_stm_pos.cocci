@r@
position p1,p2;
statement s;
@@

12@p1@s@p2

@script:ocaml@
p1 << r.p1;
p2 << r.p2;
@@

let p1 = List.hd p1 in
let p2 = List.hd p2 in
Printf.printf "%d %d %d\n" p1.line p1.col p1.col_end;
Printf.printf "%d %d %d\n" p2.line p2.col p2.col_end
