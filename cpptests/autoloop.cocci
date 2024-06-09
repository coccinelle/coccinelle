# spatch --c++=11
@rule1@
type T;
identifier i,a;
statement s;
@@
- for (T i=0; i<sizeof(a); ++i)
+ for (auto ae: a)
  {
    s
  }

@rule2@
expression e;
identifier rule1.a;
identifier rule1.i;
statement  rule1.s;
@@
(
- a[i]
+ ae
&
 e@s
)
