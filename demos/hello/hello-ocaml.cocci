@main@
type T;
parameter list P;
symbol printf;
expression E;
position p;
@@

T main(P) {
  printf@p(E);
  ...
}

@script:ocaml@
p << main.p;
@@

Printf.printf "Hello at: %d" (List.hd p).Coccilib.line
