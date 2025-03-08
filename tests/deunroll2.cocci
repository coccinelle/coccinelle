#spatch --smpl-spacing
@p0@
identifier i;
statement A,B;
@@
for (...;...;...)
{
+ undo;A
+ undo;B
  \( A \& i+0 \)
  \( B \&
-    i+1
+    i+0
  \)
}

@r0@
statement p0.A,B;
@@
  for(...;...;...)
{
- undo;A
- undo;B
  A
- A
}

@u0@
statement p0.A,p0.B;
statement B1;
@@
  for (...;...;...)
  {
- undo;A
- undo;B
    A
-   B1
+   B
  }