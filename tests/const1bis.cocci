@disable add_signed@ identifier func;@@
func (...) { 
- const int i;
+ float i;
  ...
- char const *i;
+ double *i;
  ...
}

@disable add_signed@ identifier func;@@
func (...) {
  const volatile
-int
+long
  i;
}
