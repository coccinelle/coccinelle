@r1@
identifier round_down,x,y;
@@
#define round_down(x,y) rounddown(x,y)

@r2@
identifier round_up,x,y;
@@
-#define round_up(x,y) roundup(x,y)

@@
identifier r1.round_down;
expression E1,E2;
@@
- round_down(E1, E2);
+ roundup(E1, E2);

@@
identifier r2.round_up;
expression E1,E2;
@@
- round_up(E1, E2);
+ roundup(E1, E2);
