#spatch --c++=20
@@
@@
- constinit const int i = 0;
+ constinit const int i = 0x0;
