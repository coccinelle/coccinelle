#spatch --c++=17
@@
symbol a;
@@
- int a [] = {0,1};
+ int d [] = {0,1};

@ identifier@
symbol a;
@@
- int & [b, c] = a;
+ int & [b, c] = d;
