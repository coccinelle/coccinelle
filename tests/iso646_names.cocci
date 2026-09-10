// In C, and, or, not, xor, compl, bitand, and bitor are ordinary names;
// they are only operators in C++, or with <iso646.h>.  The functions using
// them as names must parse.

@@
expression VAR, GFP;
@@

- VAR = kmalloc(sizeof(*VAR), GFP)
+ VAR = kmalloc_obj(*VAR, GFP)
