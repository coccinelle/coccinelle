#spatch --c++
@@
@@
  int & b = v;
+ // reference matched above
