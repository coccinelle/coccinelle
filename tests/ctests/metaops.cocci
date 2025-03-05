@@
expression e1, e2;
binary operator op1 = { -, *, /, %, |, &, ^, <<, >>, &&, ||, ==, !=, >, >=, <, <= } ;
binary operator op2 = + ;
@@

(
-  e1 op1 e2
+  e1 + e2
|
-  e1 op2 e2
+  e1 - e2
)

@@
expression e1, e2;
assignment operator aop1 = { +=, -=, *=, /=, %=, |=, &=, ^=, <<=, >>= };
assignment operator aop2 = = ;
@@

(
-  e1 aop1 e2
+  e1 = e2
|
-  e1 aop2 e2
+  e1 += e2
)
