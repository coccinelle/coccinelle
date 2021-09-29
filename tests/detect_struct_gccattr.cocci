@@
@@

struct abcd {
	...
-	int b;
	...
} __attribute__((pack,aligned(16)));

@@
identifier i;
@@

struct abcd {
	...
-	int b;
	...
} __attribute__((pack)) i;

@@
@@

struct abcd {
	...
-	int b;
	...
} __attribute__((aligned(...)));

@@
identifier i;
@@

struct abcd {
	...
-	int b;
	...
} __attribute__((aligned(16))) i;
