@r disable braces0@
statement S;
position p;
@@

(
while (...) { ... }
|
while (...) S@p
)

@script:ocaml@
p << r.p;
//_s << r.S;
@@

Printf.printf "line1 %d\n" (List.hd p).line


@s disable braces0@
statement S;
position p;
@@

(
while (...) { ... }
|
while (...) S@p
)

@script:ocaml@
p << s.p;
_s << s.S;
@@

Printf.printf "line2 %d\n" (List.hd p).line
