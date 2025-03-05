@@
declarer name FOO;
@@

- struct foo my_foo[] = {
- .a = 1,
- .u.b = 42,
- };
+ FOO(1, 42);
