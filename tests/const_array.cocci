@@
type T;
T[] E;
@@

- sizeof(E)/sizeof(T)
+ ARRAY_SIZE(E)

@@
type T;
const T*[] E;
@@

- sizeof(E)/sizeof(T*)
+ ARRAY_SIZE(E)
