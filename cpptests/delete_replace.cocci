#spatch --c++
@@
@@
- delete [] a;
+ delete [] b;

@@
@@
- delete a;
+ delete b;
