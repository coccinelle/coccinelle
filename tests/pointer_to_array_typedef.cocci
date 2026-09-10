// s16 is not declared in pointer_to_array_typedef.c, but it can only be a
// type in "const s16 (*x)[2]" and in "(s16 (*)[2])", so the functions using
// those forms must parse.  lookup in "lookup(*p)[1]" is a function.

@@
expression VAR, GFP;
@@

- VAR = kmalloc(sizeof(*VAR), GFP)
+ VAR = kmalloc_obj(*VAR, GFP)
