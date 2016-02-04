@@
statement S;
identifier ioctl;
@@

  ioctl(int cmd, void *arg) {
    ...
    int x;
+   foo();
    S
    ...
  }
