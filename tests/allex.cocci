@exists@
expression e;
statement S;
@@

if (e) {
  ... when forall
  return ...;
}
...
-if (e) S

/* shows that the previous rule did not apply */
@@
@@

- 15
+ 200