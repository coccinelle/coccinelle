@identifier@
identifier i;
@@
// TODO - this test is known to fail, coccinelle does not properly interpret
// the `typedef struct a b` syntax

- i
+ xxx
