// The body of to_item makes its parameter "type" look like a typedef name.
// That must not change how "type" is parsed after the #define.

@@
expression VAR, GFP;
@@

- VAR = kmalloc(sizeof(*VAR), GFP)
+ VAR = kmalloc_obj(*VAR, GFP)
