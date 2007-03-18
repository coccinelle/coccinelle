@@
identifier I;
expression E;
@@

struct i2c_client I = {
- 	.name = E,
...
+	.dev = { .name = E, },
};

@@
identifier I;
expression E;
@@

struct i2c_adapter I = {
- 	.name = E,
...
+	.dev = { .name = E, },
};

@@
struct i2c_client *x;
struct i2c_client y;
expression E;
@@

(
- x->data = E
+ i2c_set_clientdata(x,E)
|
- y.data = E
+ i2c_set_clientdata(&y,E)
)

@@
{struct i2c_client *, struct i2c_adapter *} x;
{struct i2c_client, struct i2c_adapter} y;
expression E;
@@

(
- sprintf(x->name,
+ snprintf(x->name, DEVICE_NAME_SIZE,
          ...)
|
- sprintf(y.name,
+ snprintf(y.name, DEVICE_NAME_SIZE,
          ...)
|
- strcpy(x->name, E)
+ strncpy(x->name, E, DEVICE_NAME_SIZE)
|
- strcpy(y.name, E)
+ strncpy(y.name, E, DEVICE_NAME_SIZE)
)

@@
struct i2c_client *x;
struct i2c_client y;
struct i2c_adapter *a;
struct i2c_adapter b;
type T;
@@

(
- x->name
+ x->dev.name
|
- y.name
+ y.dev.name
|
- a->name
+ a->dev.name
|
- b.name
+ b.dev.name
|
- (T)x->data
+ i2c_get_clientdata(x)
|
- (T)y.data
+ i2c_get_clientdata(&y)
|
- (T)x->adapter->data
+ i2c_get_adapdata(x->adapter)
)

@@
struct i2c_client *x;
expression E1, E2, E3;
@@

  x = kmalloc(E1, E2)
  ...
  if (!x) { ... return ...; }
+ memset(x,0,E1);
  ... when != memcpy(x,E3,E1)
