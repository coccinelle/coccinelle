@@
identifier l,l1;
expression e;
statement S;
@@

-goto l;
...
l:
<... when != S
l1:
...>
return e;

@@
identifier l,l1;
expression e;
statement S;
@@

-l:
<... when != S
     when any // why is this needed?
l1:
...>
return e;