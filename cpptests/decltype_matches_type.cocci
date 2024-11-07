# spatch --c++
@@
type T;
identifier i;
fresh identifier n = i ## "_";
@@

- T i;
+ T n;

