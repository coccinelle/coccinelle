@ rule1 @ 
type T; identifier I; constant C; expression E; 
position p1, p2, p3, p4;
@@
T I@p2[C@p3];
<... 
I[E@p4]
...>
@ script:python @ 
x << rule1.p3;
y << rule1.p4;
@@
print "%s:%s:%s:%s" % (x.location.file, x.location.line, x.location.column, x)
print "%s[%s]" % (x,y)
