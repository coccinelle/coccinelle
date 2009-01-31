@@
function get_type;
identifier this_info, this_type;
typedef GType, GTypeInfo;
expression E ;
@@
+ static GType this_type = 0;
+ static const GTypeInfo this_info = E ;
GType get_type (...) {
- static GType this_type = 0;
  if (...) {
-       static const GTypeInfo this_info = E ;
  ...
  }
...
}
