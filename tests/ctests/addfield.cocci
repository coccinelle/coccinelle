@@
identifier I;
expression E;
@@

struct i2c_client I = {
-       .name = E,
...,
+       .dev = { .name = E, },
};
