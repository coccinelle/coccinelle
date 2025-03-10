@r exists@
identifier x;
identifier f;
@@

f(...) { ... when any
- int x;
... when any
}

@@
identifier r.f,r.x;
@@

f(...) {
++ char x;
... when any
}
