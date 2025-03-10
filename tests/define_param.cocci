
@ rule2 disable all @
expression E;
@@

- (sizeof(E)/sizeof(*E))
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
