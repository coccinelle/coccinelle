// complex is a type specifier next to float, double, or long, and an
// ordinary name elsewhere.  The functions using it as a name must parse.

@@
expression VAR, GFP;
@@

- VAR = kmalloc(sizeof(*VAR), GFP)
+ VAR = kmalloc_obj(*VAR, GFP)
