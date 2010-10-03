@ rule1 @ type T; identifier I; constant C; expression E; @@
T I[C];
<... 
-I[E]
+I[E]
...>
@ script:python @ t << rule1.T; i << rule1.I; x << rule1.C; y << rule1.E; @@
print t, i, "[", x, "]; ", i, "[", y, "];"
#print "Hello"
@ rule2 @ constant rule1.C; @@
- C
