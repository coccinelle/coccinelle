@ rule1 @ 
expression C; 
position p1;
@@
*C@p1
@ script:python @ 
x << rule1.C;
xloc << rule1.p1;
@@
print "%s[%s]" % (x,xloc)
