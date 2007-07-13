@@
expression x;
expression E;
constant c;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (
-     c == (x = E)
+     x == c
     )
  S1 else S2

@@
expression x;
expression E;
constant c;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (
-     (x = E) == c
+     x == c
     )
  S1 else S2

@@
expression x;
expression E;
constant c;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (
-     c != (x = E)
+     x != c
     )
  S1 else S2

@@
expression x;
expression E;
constant c;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (
-     (x = E) != c
+     x != c
     )
  S1 else S2

@@
expression x;
expression E, E1;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (
-     E1 == (x = E)
+     E1 == x
     )
  S1 else S2

@@
expression x;
expression E, E1;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (
-     (x = E) == E1
+     x == E1
     )
  S1 else S2

@@
expression x;
expression E,E1;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (
-     E1 != (x = E)
+     E1 != x
     )
  S1 else S2

@@
expression x;
expression E,E1;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (
-     (x = E) != E1
+     x != E1
     )
  S1 else S2

@@
expression x;
expression E;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (
-     x = E
+     x
     )
  S1 else S2

@@
expression x;
expression E;
statement S1, S2;
@@

+ x = E;
+ NOTHING_XXX;
  if (!
-     (x = E)
+     x
     )
  S1 else S2

@@
@@

- NOTHING_XXX;
