// pb: foo doesn't get added
@@
statement S;
@@

  ioctl(int cmd, void *arg) {
+   foo();
    ...
  }
