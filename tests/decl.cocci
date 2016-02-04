// pb: foo doesn't get added
@@
statement S;
identifier ioctl;
symbol cmd,arg;
@@

  ioctl(int cmd, void *arg) {
+   foo();
    ...
  }
