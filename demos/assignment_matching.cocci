// This only matches cases where the assignment is a complete statement
@@
expression x;
@@

-x = f(3);
+matches_little();

// This matches also cases where the assignment is a subterm of another
// statement, such as a conditional.
@@
expression x;
@@

-x = g(3)
+matches_more()

// An isomorphism will let this also match cases where the type is not there
@@
expression x;
type T;
@@

-x = (T)h(3)
+matches_even_more()

// This matches a variable declaration too.  There are constraints on
// the transformation performed in this case, as it must ensure that the result
// will also be a variable declaration.
@@
expression x;
type T;
@@

 x = 
-(T)i(3)
+matches_most()

