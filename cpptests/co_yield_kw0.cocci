#spatch --c++
// in c++ mode, co_return in a semantic patch only matches co_return, whereas return can match co_return too
@@
@@
- 0
+ 1
