@ rule1 @ expression E; @@
f(E);
...
g(E);
...
-h()
+h(E);
@ script:python @ x << rule1.E; @@
print x
