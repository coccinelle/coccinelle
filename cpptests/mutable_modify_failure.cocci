#spatch --c++
@@
symbol x;
@@
- mutable int x = 1;
+ int x = 1;
