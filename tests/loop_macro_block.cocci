// A name followed by a parenthesized argument list and a block, at the
// start of a statement in a function body, can only be a loop macro, even
// when the name gives no hint of it.

@@
expression VAR, GFP;
@@

- VAR = kmalloc(sizeof(*VAR), GFP)
+ VAR = kmalloc_obj(*VAR, GFP)
