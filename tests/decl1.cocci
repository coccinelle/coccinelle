@@
statement S;
@@

  ioctl(int cmd, void *arg) {
    ...
    int x;
+   foo();
    S
    ...
  }
