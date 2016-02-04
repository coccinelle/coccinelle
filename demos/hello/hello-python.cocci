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

@script:python@
p << main.p;
@@

print("Hello at: %s" % p[0].line);
