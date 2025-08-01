#spatch --c++
// as of now, co_return in the code would match the return here
@@
@@
- return 0;
+ return 1;
