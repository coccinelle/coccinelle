@@
expression E;
@@

// TODO - this test fails. coccinelle does not consider 3 and 0x03 to be
// isomorphic

- f(E);
- g(E);
