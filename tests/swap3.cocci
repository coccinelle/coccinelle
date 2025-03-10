@ok exists@
type t1;
identifier tmp;
expression i1,i2;
position p;
@@

t1 tmp@p;
...
swap(i1, i2, tmp);

@@
expression i1,i2;
identifier tmp;
type ok.t1;
position ok.p;
@@

-t1 tmp@p;
 <... when strict
      when != tmp
 swap(i1, i2, tmp);
 ...>
?t1 tmp;
