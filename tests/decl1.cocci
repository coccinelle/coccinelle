@@
statement S;
identifier ioctl, cmd, arg;
symbol x;
@@

  ioctl(int cmd, void *arg) {
    ...
    int x;
+   foo();
    S
    ...
  }
