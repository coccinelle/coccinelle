#spatch --c++=23
@@
@@
+ return 1;
+ #else
+ #if defined Y
+ return 1;
+ #elifdef Y
  return 0;
+ #endif
