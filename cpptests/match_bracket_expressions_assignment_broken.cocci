# spatch --c++=11
// FIXME: TODO: this rule seemingly does not change the code because of the k = {1} presence
// It shall already match j = 1.
// And once {1} is recognized as an expression too, also k = {1}.
@@
identifier i;
expression e;
@@
+ // assign to value:
 i = e;
