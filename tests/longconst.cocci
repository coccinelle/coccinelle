@  @
expression E;
long E1;
{int, signed char, signed short} E2;
@@
- E1 = E2 << E;
+ E1 = (long)(E2) << E;

@  @
expression E;
long E1;
{long,unsigned} E2;
@@
- E1 = E2 << E;
+ E1 = 27;

@  @
expression E;
long E1;
{unsigned long} E2;
@@
- E1 = E2 << E;
+ E1 = 48;
