@ disable all @
type T;
const T[] E;
@@

- (sizeof(E)/sizeof(T))
+ ARRAY_SIZE(E)
