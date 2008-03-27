@ rule1 @ type T; identifier I; constant C; expression E; @@
T I[C];
<... 
-I[E]
+I[E]
...>
@ script:python @ @@
print "Hello"
@ rule2 @ constant rule1.C; @@
- C
