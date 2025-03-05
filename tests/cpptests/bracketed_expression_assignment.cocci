# spatch --c++=11
// FIXME: expression e does not currently see {1} as an expression
@@
identifier i;
expression e;
@@
+ // specific identifier matched:
i = e;
