@disable add_signed@
identifier func;
symbol i;
@@
func (...) { 
- const int i;
+ float i;
  ...
- char const *i;
+ double *i;
  ...
}

@disable add_signed@
identifier func;
symbol i;
@@
func (...) {
  const volatile
-int
+long
  i;
}
