// to be used without isomorphisms

@@
expression *E;
@@

  E ==
- 0
+ NULL

@@
expression *E;
@@

- 0
+ NULL
  == E

@@
expression *E;
@@

  E !=
- 0
+ NULL

@@
expression *E;
@@

- 0
+ NULL
  != E

// assignments

@@
expression *E;
expression E1;
@@

  (E = E1) ==
- 0
+ NULL

@@
expression *E;
expression E1;
@@

- 0
+ NULL
  == (E = E1)

@@
expression *E;
expression E1;
@@

  (E = E1) !=
- 0
+ NULL

@@
expression *E;
expression E1;
@@

- 0
+ NULL
  != (E = E1)
