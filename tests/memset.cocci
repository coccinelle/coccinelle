// have to duplicate a lot of rules because T E only matches if E has a known
// type, even if T is not used elsewhere.

@@
type T, T2;
expression x;
expression E1,E2,E;
@@

- x = (T)kmalloc(E1,E2)
  ...  when != x = E
- memset((T2)x,0,E1);

@@
type T, T2;
type T1;
T1 *x;
expression E2,E;
@@

- x = (T)kmalloc(sizeof(T1),E2)
  ...  when != x = E
- memset((T2)x,0,sizeof(*x));

@@
type T, T2;
type T1;
T1 *x;
expression E2,E;
@@

- x = (T)kmalloc(sizeof(*x),E2)
  ...  when != x = E
- memset((T2)x,0,sizeof(T1));

// ---------------------------------------------------------------------


@@
type T, T1, T2;
identifier x;
expression E1,E2,E;
@@

- T1 x = (T)kmalloc(E1,E2);
  ...  when != x = E
- memset((T2)x,0,E1);

@@
type T, T2;
type T1;
identifier x;
expression E2,E;
@@

- T1 x = (T)kmalloc(sizeof(T1),E2);
  ...  when != x = E
- memset((T2)x,0,sizeof(*x));

@@
type T, T2;
type T1;
identifier x;
expression E2,E;
@@

- T1 x = (T)kmalloc(sizeof(*x),E2);
  ...  when != x = E
- memset((T2)x,0,sizeof(T1));
