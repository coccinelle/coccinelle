@@
expression X, Y, Z;
@@

f(X);
...
g(Y);
...
- h(Z);
+ h(1); 

// si c'est '+ h(X,Y,Z)' alors la il y'a probleme par contre
