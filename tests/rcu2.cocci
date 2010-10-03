// prepare for transformation

@@
idexpression struct list_head *I;
@@

- I
+ _Y(I)

@@
identifier I;
expression E;
iterator list_for_each_rcu;
statement S;
@@

 list_for_each_rcu(
-                  _Y(I)
+                  _X(I)
                   ,E)
  S

@@
identifier I;
expression E;
iterator list_for_each;
statement S;
@@

 list_for_each(
-              _Y(I)
+              _X(I)
               ,E)
  S

@@
identifier I;
expression E;
iterator list_for_each_prev;
statement S;
@@

 list_for_each_prev(
-              _Y(I)
+              _X(I)
               ,E)
  S

@@
type T;
identifier I;
expression E;
@@

 list_entry(
-           _Y(I)
+           _X(I)
            ,T,E)

// the case where the list element is just stored in a variable

@r@
type T,T1;
identifier I, x;
expression E, E1, E2;
iterator list_for_each_entry_rcu;
@@

  <... when != _Y(I)
- list_for_each_rcu(_X(I),E1)
+ list_for_each_entry_rcu(x,E1,E2)
  {
      ... when != \(_Y(I)\|_X(I)\|x=E\)
	  when != T1 x;
-     x = list_entry(_X(I),T,E2);
      ... when != \(_Y(I)\|_X(I)\|x=E\)
  }
  ...>

@ra@
type T,T1;
identifier I, x;
expression E, E1, E2;
iterator list_for_each_entry;
@@

  <... when != _Y(I)
- list_for_each(_X(I),E1)
+ list_for_each_entry(x,E1,E2)
  {
      ... when != \(_Y(I)\|_X(I)\|x=E\)
	  when != T1 x;
-     x = list_entry(_X(I),T,E2);
      ... when != \(_Y(I)\|_X(I)\|x=E\)
  }
  ...>

@rb@
type T,T1;
identifier I, x;
expression E, E1, E2;
iterator list_for_each_entry_reverse;
@@

  <... when != _Y(I)
- list_for_each_prev(_X(I),E1)
+ list_for_each_entry_reverse(x,E1,E2)
  {
      ... when != \(_Y(I)\|_X(I)\|x=E\)
	  when != T1 x;
-     x = list_entry(_X(I),T,E2);
      ... when != \(_Y(I)\|_X(I)\|x=E\)
  }
  ...>

// instances of the above that we can't treat because of the local variable
// problem.  seems better to do nothing.

@r1@
type T,T1;
identifier I, x;
expression E1, E2;
@@

  list_for_each_rcu(
-                   _X(I)
+                   _Y(I)
                    ,E1)
  {
      ...
(
      T1 x;
      ...
      x =
-         list_entry(_X(I),T,E2);
+         _LOCAL_DECL(list_entry(_X(I),T,E2));
|
      T1 x =
-            list_entry(_X(I),T,E2);
+            _LOCAL_DECL(list_entry(_X(I),T,E2));
)
      ...
  }

@r1a@
type T,T1;
identifier I, x;
expression E1, E2;
@@

  list_for_each(
-               _X(I)
+               _Y(I)
                ,E1)
  {
      ...
(
      T1 x;
      ...
      x =
-         list_entry(_X(I),T,E2);
+         _LOCAL_DECL(list_entry(_X(I),T,E2));
|
      T1 x =
-            list_entry(_X(I),T,E2);
+            _LOCAL_DECL(list_entry(_X(I),T,E2));
)
      ...
  }

@r1b@
type T,T1;
identifier I, x;
expression E1, E2;
@@

  list_for_each_prev(
-               _X(I)
+               _Y(I)
                ,E1)
  {
      ...
(
      T1 x;
      ...
      x =
-         list_entry(_X(I),T,E2);
+         _LOCAL_DECL(list_entry(_X(I),T,E2));
|
      T1 x =
-            list_entry(_X(I),T,E2);
+            _LOCAL_DECL(list_entry(_X(I),T,E2));
)
      ...
  }

// the case where the list element is used for something else (often a return)

@@
type T;
identifier I;
expression E1, E2;
@@

- struct list_head *I;
+ T *I;
  <+... when != _Y(I)
- list_for_each_rcu(_X(I),E1)
+ list_for_each_entry_rcu(I,E1,E2)
  {
      <+... when != _Y(I)
-     list_entry(_X(I),T,E2)
+     I
      ...+>
  }
  ...+>

@@
type T;
identifier I;
expression E1, E2;
@@

- struct list_head *I;
+ T *I;
  <+... when != _Y(I)
- list_for_each(_X(I),E1)
+ list_for_each_entry(I,E1,E2)
  {
      <+... when != _Y(I)
-     list_entry(_X(I),T,E2)
+     I
      ...+>
  }
  ...+>

@@
type T;
identifier I;
expression E1, E2;
@@

- struct list_head *I;
+ T *I;
  <+... when != _Y(I)
- list_for_each_prev(_X(I),E1)
+ list_for_each_entry_reverse(I,E1,E2)
  {
      <+... when != _Y(I)
-     list_entry(_X(I),T,E2)
+     I
      ...+>
  }
  ...+>

// clean up

@@
struct list_head *I;
@@

(
- _X(I)
+ I
|
- _Y(I)
+ I
)

@@
expression I;
@@

(
- _X(I)
+ _INCONSISTENT_MODIF(I)
|
- _Y(I)
+ _INCONSISTENT_MODIF(I)
)

@ depends on r || ra @
identifier I;
@@

- struct list_head *I;
  ... when != I
