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
