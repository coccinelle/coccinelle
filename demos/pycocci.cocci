@ rule1 @ 
type T; identifier I; expression C; expression E; 
position p1, p2, p3, p4;
@@
T I@p2[C@p3];
<... 
I[E@p4]
...>
@ script:python @ 
x_mv << rule1.C; xp << rule1.p3;
y_mv << rule1.E; yp << rule1.p4;
@@
x = cocci.combine(x_mv, xp)
y = cocci.combine(y_mv, yp)
cocci.register_match(True, [(x, 'Array match'), (y, 'Array use')])
