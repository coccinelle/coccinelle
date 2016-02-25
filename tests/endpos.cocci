virtual lines

@r@
position p;
identifier f;
@@

f(...) {
  ...
}@p

@script:ocaml depends on lines@
p << r.p;
f << r.f;
@@

Printf.printf "%s: %s\n" f
  (String.concat " "
     (List.map (function x -> string_of_int (x.line)) p))

@s@
position p;
identifier f;
@@

f
 (...) {
+f();
  ...
}
+int f() { return 0; }
