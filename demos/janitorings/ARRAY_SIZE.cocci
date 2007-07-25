@@
type T;
T *id;
@@

- sizeof(id) / sizeof(T)
+ ARRAY_SIZE(id)



@@ expression E; @@

- (sizeof(E)/sizeof(*E))
+ ARRAY_SIZE(E)

@@ expression E; @@

- sizeof(E)/sizeof(*E)
+ ARRAY_SIZE(E)

@@ expression E, E1; @@

- (sizeof(E)/sizeof(E[E1]))
+ ARRAY_SIZE(E)

@@ expression E, E1; @@

- sizeof(E)/sizeof(E[E1])
+ ARRAY_SIZE(E)

// @@
// type T;
// expression E;
// @@
// 
// - (sizeof(E)/sizeof(T))
// + ARRAY_SIZE(E)


@@
type T;
expression E;
@@

- sizeof(E)/sizeof(T)
+ ARRAY_SIZE(E)

@@ expression E; @@

- NUM_ELEMENTS(E)
+ ARRAY_SIZE(E)

@@ identifier NUM; @@

- #define NUM(x)       ARRAY_SIZE(x)

@@ expression E; @@

- NUM(E)
+ ARRAY_SIZE(E)