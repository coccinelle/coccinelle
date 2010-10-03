@@
type T;
T[] id;
@@

- sizeof(id) / sizeof(T)
+ ARRAY_SIZE(id)
