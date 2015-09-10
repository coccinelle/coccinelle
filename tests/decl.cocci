// pb: foo doesn't get added
@@
identifier ioctl;
symbol cmd,arg;
@@

  ioctl(int cmd, void *arg) {
+   foo();
    ...
  }
