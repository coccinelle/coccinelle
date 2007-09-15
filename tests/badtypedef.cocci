// need the merge_val facility for typedef and struct  equivalence

@@
type T;
T E, E1;
@@

- xxx(E);
+ aaa(E);
  ...
- yyy(E1);
+ bbb(E1);
