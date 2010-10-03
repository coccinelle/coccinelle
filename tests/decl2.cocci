// pb: foo doesn't get added
@@
identifier ioctl, cmd, arg;
@@

  ioctl(int cmd, void *arg) {
    ...
-   x
+   y
    ...
  }
