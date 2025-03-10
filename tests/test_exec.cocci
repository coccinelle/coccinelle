@@
constant n, pr;
idexpression decimal(n,pr) i;
@@

+{
EXEC SQL ... :i ...;
+}

@r exists@
constant n, pr;
idexpression decimal(n,pr) i;
identifier f;
position p;
@@

f(...) {
  ... when any
  EXEC@p SQL ... :i ...;
  ... when any
}

@@
constant r.n, r.pr;
idexpression decimal(n,pr) r.i;
identifier r.f;
position r.p;
statement S;
fresh identifier tmp = "__exec__";
@@

f(...) {
  <... {
++ char tmp[64];
  ... when != S
++decToString(i, tmp);
  EXEC@p SQL ...;
++stringToDec(tmp, &i);
  }
  ...>
}

@@
identifier i,tmp;
@@

 decToString(i, tmp);
  ... when != stringToDec(...);
  EXEC SQL ... :
-i
+tmp
    ...;


/*
@@
@@

  {
- {
  ...
- }
  }
*/
