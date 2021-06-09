@@
@@

struct abcd {
	...
-	int b;
	...
} __attribute__((pack));

@@
identifier i;
@@

struct abcd {
	...
-	int b;
	...
} __attribute__((pack)) i;
