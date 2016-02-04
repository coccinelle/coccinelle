// before handling "depends on", when julia was generating
// from this rule the rule to modify the corresponding prototype,
// this rule was applied whatever happend an in particular 
// this SP will not match the .c because return x <> return 12
// but its prototype will still be modified :( 

// with "depends on" the prototype is changed only if the
// function is matched and transformed.


@@
@@

- f(int x)
+ f(int x, int y)
   { return x; }

