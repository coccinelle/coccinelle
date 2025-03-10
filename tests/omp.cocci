@initialize:ocaml@
@@

let ok s =
  List.hd(String.split_on_char ' ' s) = "TEST"

@initialize:python@
@@

def ok (s):
  return (s.split()[0] == "PTEST")

@@
pragmainfo p =~ "^SOMETHING ";
@@

- #pragma omp p

@@
pragmainfo p : script:ocaml() { ok p };
@@

- #pragma omp p

@@
pragmainfo p : script:python() { ok(p) };
@@

- #pragma omp p
