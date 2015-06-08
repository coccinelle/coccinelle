@@
expression i;
identifier tmp;
type t1;
position p;
@@

(
-t1 tmp@p = 0;
|
-t1 tmp@p;
)
 <... when strict
      when != tmp
 swap(i,tmp);
 ...>
?t1 tmp;
