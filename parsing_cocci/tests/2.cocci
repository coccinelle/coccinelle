@@
struct i2c_client x;
@@

- x.name
+ x.dev.name

@@
struct i2c_client *p;
expression E;
@@

(
- p->data = E
+ i2c_set_clientdata(p,E)
|
- p->data
+ i2c_get_clientdata(p)
)

@@
struct i2c_client *p;
expression E1, E2;
@@

  p = kmalloc(E1, E2);
? if (!p) { ... }
+ memcpy(p,0,E1);
  ...              WHEN != memcpy(p,...)
