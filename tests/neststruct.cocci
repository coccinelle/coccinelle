@r@
type T1;
T1 *x;
expression E,E2;
@@

- x = kmalloc(sizeof(E),E2)
+ x = kzalloc(sizeof(E), E2)
