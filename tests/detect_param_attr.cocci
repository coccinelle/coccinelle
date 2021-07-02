@@
type T1, T2;
attribute name __nocast;
identifier x, y;
@@

T1 x(
	...,
-	T2
+	char
	y __nocast,
	...
   ) {...}
@@
type T1, T2;
attribute name __nocast;
identifier x, y;
@@

T1 x(
	...,
-	T2
+	char
	__nocast *y,
	...
   ) {...}

@@
type T1, T2;
identifier x, y;
@@

T1 x(
	...,
-	T2
+	char
	y __attribute__((nocast)),
	...
   ) {...}

@@
type T1, T2;
identifier x, y;
@@

T1 x(
	...,
-	T2
+	long
	__attribute__((nocast)) *y,
	...
   ) {...}
