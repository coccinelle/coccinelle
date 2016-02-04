// Deprecated min/max http://gcc.gnu.org/onlinedocs/gcc-4.0.1/gcc/Deprecated-Features.html
// Only works if "algorithm" is allready included
// This spatch is on hold until coccinelle is extended to support >?,<?,>?=,<?=
@@
expression x,y;
@@
- x <?= y;
+ x = min(x,y);
@@
expression x,y;
@@
- x <? y
+ min(x,y)
@@
expression x,y;
@@
- x >?= y;
+ x = max(x,y);
@@
expression x,y;
@@
- x >? y
+ max(x,y)

