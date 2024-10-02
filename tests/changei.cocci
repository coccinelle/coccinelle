@identifier@
identifier i;
@@
// TODO - this test fails, coccinelle does not properly interpret the
`typedef struct a b` syntax

- i
+ xxx
