#spatch --c++
// in c++ mode, co_return in a semantic patch only matches co_return, whereas return can match co_return too
@@
@@
  int f()
  {
- return 0;
+ return 1;
  }

@@
@@
  int g()
  {
- co_return 0;
+ co_return 1;
  }

@@
@@
  int h()
  {
- return 0;
+ return 1;
  }

@@
@@
  int l()
  {
- co_return 0;
+ co_return 1;
  }

@@
@@
  int m()
  {
- return;
+ 1;
+ return;
  }

@@
@@
  int n()
  {
- co_return;
+ 1;
+ co_return;
  }

@@
@@
  int o()
  {
- return;
+ 1;
+ return;
  }

@@
@@
  int p()
  {
- co_return;
+ 1;
+ co_return;
  }

