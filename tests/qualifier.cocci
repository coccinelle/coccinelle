@ disable drop_cast @
type T;
T E;
@@
- (T)
     E

@ disable drop_cast @
type T;
typedef survived_r1;
expression e;
@@

- (T)
+ (survived_r1)
  e
