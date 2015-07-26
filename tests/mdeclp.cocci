@script:ocaml r@
p;
@@

p := make_position "mdeclp.c" "one" 1 4 1 7

@@
position r.p;
identifier f;
@@

- f@p(...) { ... }

@s@
position p;
identifier f;
@@

 f@p(...) { ... }

@script:ocaml@
p << s.p;
@@

let p = List.hd p in
flush stdout;
Printf.printf "function name at %s:%s:%d:%d:%d:%d\n"
  p.file p.current_element p.line p.col p.line_end p.col_end;
flush stdout
