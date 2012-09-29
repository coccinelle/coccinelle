@r@
field list[n] fs;
identifier I,x;
@@

struct I { fs int x; ...};

@script:ocaml@
n << r.n;
x << r.x;
i << r.I;
@@

Printf.printf "%s at offset %d in %s\n" x n i
