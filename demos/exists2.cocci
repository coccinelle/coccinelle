// this rule lacks exists, without exists, all control flows possible
// must match the rule. In the case of exists2.c two possible control
// flows exists on main():
// 1. b() --> a > 5 --> c();
// 2. b() --> a <= 5 ---> no c();
// Not all control flows match, so no changes are made.
// To visualize the control graph use: spatch --control-flow exists2.c
@r@
@@

b();
...
-c();
