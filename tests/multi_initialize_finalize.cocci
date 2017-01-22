/* i3 comes before i2 since ocaml is initialized in one step. */

@initialize:ocaml@
@@

let x = 0
let () = print_endline "i1"

@initialize:python@
@@

import sys
x = 0
print("i2")
sys.stdout.flush()

@initialize:ocaml@
@@

let y = 1
let () = print_endline "i3"

@initialize:python@
@@

y = 1
print("i4")
sys.stdout.flush()

@@
@@
- return 0;
+ return 1;

@finalize:ocaml@
@@
assert (x = 0);
print_endline "f1"

@finalize:python@
@@
assert x == 0
print("f2")
sys.stdout.flush()

@finalize:ocaml@
@@
assert (y = 1);
print_endline "f3"

@finalize:python@
@@
assert y == 1
print("f4")
sys.stdout.flush()