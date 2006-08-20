@@
type T;
identifier v,fld;
@@

// copy_from_user and possibly copy_to_user

  ioctl(...) {
    <...
-     f();
    ...
(
-   g();
|
    ...
)
    ...>
  }
