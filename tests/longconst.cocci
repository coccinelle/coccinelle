@@
expression E;
long E1;
int E2;
@@
- E1 = E2 << E;
+ E1 = f(E2, "int");

@@
expression E;
long E1;
unsigned int E2;
@@
- E1 = E2 << E;
+ E1 = f(E2, "unsigned");

@@
expression E;
long E1;
long E2;
@@
- E1 = E2 << E;
+ E1 = f(E2, "long");

@@
expression E;
long E1;
unsigned long E2;
@@
- E1 = E2 << E;
+ E1 = f(E2, "unsigned long");
