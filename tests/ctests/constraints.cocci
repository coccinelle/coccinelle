@r1@
constant c1, c2 !~ "^3";
binary operator b1 = {==,!=,&,|};
binary operator b2 =~ "=$";
@@
f(c1 b1 c2);
...
g(c1 b2 c2);

@@
constant c1, c2;
binary operator b = {r1.b1,r1.b2} && !~ "^!";
@@
-h(c1 b c2);
+h(c2 b c1);

@r2@
type T = { int, char, bool * };
identifier x;
@@
- T x;
+ T *x;

@@
type T = r2.T * || =~ "^char";
identifier x;
@@
- T *x[];
+ T x[];

@@
parameter list[n = {2 ... 6}] P;
identifier f;
@@

-f
+h
 (P) { ... }
