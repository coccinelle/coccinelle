#spatch --c++
@@
symbol i;
@@
 switch (i) {
-   [[likely]]
   case 0: ... break;
-  [[unlikely]]
   case 1: ... break;
 }
