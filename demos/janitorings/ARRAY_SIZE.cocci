// empty.iso is used because there is an iso that converts sizeof(E) to
// sizeof E, which causes a double match in an expression, and thus a
// double modification

@ rule1 using "empty.iso" @
expression E;
@@

- (sizeof(E)/sizeof(*E))
+ ARRAY_SIZE(E)

@ rule2 using "empty.iso" @
expression E;
@@

- sizeof(E)/sizeof(*E)
+ ARRAY_SIZE(E)

@ rule3 using "empty.iso" @
expression E, E1;
@@

- (sizeof(E)/sizeof(E[E1]))
+ ARRAY_SIZE(E)

@ rule4 using "empty.iso" @
expression E, E1;
@@

- sizeof(E)/sizeof(E[E1])
+ ARRAY_SIZE(E)

@ rule5 using "empty.iso" @
type T;
T[] E;
@@

- (sizeof(E)/sizeof(T))
+ ARRAY_SIZE(E)

@ rule6 using "empty.iso" @
type T;
T[] E;
@@

- sizeof(E)/sizeof(T)
+ ARRAY_SIZE(E)

// ---------------------------------------------------------------------------
// some of the above rules with more parentheses
// this can't be done with an isomorphism, as described above

@ rule1p using "empty.iso" @
expression E;
@@

- (sizeof(E)/sizeof(*(E)))
+ ARRAY_SIZE(E)

@ rule2p using "empty.iso" @
expression E;
@@

- sizeof(E)/sizeof(*(E))
+ ARRAY_SIZE(E)

@ rule3p using "empty.iso" @
expression E, E1;
@@

- (sizeof(E)/sizeof((E)[E1]))
+ ARRAY_SIZE(E)

@ rule4p using "empty.iso" @
expression E, E1;
@@

- sizeof(E)/sizeof((E)[E1])
+ ARRAY_SIZE(E)

// ---------------------------------------------------------------------------
@@ expression E; @@

- NUM_ELEMENTS(E)
+ ARRAY_SIZE(E)

@ rule53 @
identifier NUM, x;
@@

- #define NUM(x)       ARRAY_SIZE(x)

@@
expression E;
identifier rule53.NUM;
@@

- NUM(E)
+ ARRAY_SIZE(E)

@@
expression E;
@@

- ((int)ARRAY_SIZE(E))
+ ARRAY_SIZE(E)
