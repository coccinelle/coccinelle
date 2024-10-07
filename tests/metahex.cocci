@@
expression E;
@@

// TODO - this test is known to fail. coccinelle does not consider 3 and 0x03
// to be isomorphic

- f(E);
- g(E);
