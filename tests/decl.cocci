// pb: foo doesn't get added
@@
statement S;
identifier ioctl;
@@

  ioctl(int cmd, void *arg) {
+   foo();
    ...
  }
