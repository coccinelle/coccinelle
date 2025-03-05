# spatch --c++=11
@ disable list_and_aggregate_initialization@
identifier i;
@@
- int i={0};
