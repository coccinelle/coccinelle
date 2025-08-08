#spatch --c++=11
@@
@@
- struct B: public A { int f() override { return 0; } };
+ struct B: public A { int f() override { return 1; } };
