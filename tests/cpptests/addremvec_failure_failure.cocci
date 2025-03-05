# spatch --c++
@@
symbol d;
@@
- vector<f(1, 2)>
+ vector<f(12, 2)>
  d;
