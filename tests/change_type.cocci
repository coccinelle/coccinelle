@@
identifier i1,i2;
struct i1 *e;
@@

(
  (struct i1 *)e
|
- (struct i2 *)e
+ 42
)
