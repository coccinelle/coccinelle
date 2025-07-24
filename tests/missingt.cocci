@@
expression a;
@@

- f(a)
+ g(a)

@@
type T;
parameter list ps;
identifier F;
fresh identifier F_fn = F ## "_fn";
@@

- T F ( ps );
+ T ( * F_fn ) ( ps );
