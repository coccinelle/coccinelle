// This rule lacks exists, without exists, all control flows possible
// must match the rule when + or - is used. In the case of exists1.c only
// one possible control flow exists, the flow is completely linear.
@r@
@@

b();
...
-c();
