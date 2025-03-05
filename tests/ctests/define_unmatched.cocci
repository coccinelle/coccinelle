@@
expression x;
expression y;
@@
// we do not expect a change, we are just checking coccinelle does not crash

- DECLARE_SETMEM(x, y)
+ PASSVAR(x, y)
