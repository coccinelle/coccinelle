@ rule1 using "empty.iso" @
expression E;
type T;
@@

- (sizeof(E)/sizeof(T))
+ ARRAY_SIZE(E)
