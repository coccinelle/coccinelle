// spatch -test voyager -sgrep2

@ r4 @
type T, T2;
expression x;
expression E1,E2,E;
@@

- x = (T)kmalloc(E1,E2)
  ...  when != x = E
- memset((T2)x,0,E1);

@ r18 @
type T, T2;
type T1;
T1 *x;
expression E1,E2;
@@

- x = (T)kmalloc(E1,E2)
  ...
- memset((T2)x,0,sizeof(T1));
