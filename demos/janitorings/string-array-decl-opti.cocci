//@@
//identifier x;
//expression s;
//@@
// (
//  char *x = NULL;
// |
// - char *x = s;
// + char x[] = s;
// )


@@
identifier x;
constant char [] s;
@@


- char *x = s;
+ char x[] = s;
