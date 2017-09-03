@initialize:ocaml@
@@

let check e1 e2 = e1 = e2

@r@
expression e1;
expression e2 : script:ocaml(e1) { check e1 e2 };
identifier f,g;
@@

f(...,e1,...,e2,...);
...
g(...,e1,...,e2,...);
++h(e1,e2);
