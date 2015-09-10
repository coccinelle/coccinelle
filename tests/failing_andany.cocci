/* This test case shows that the andany optimization doesn't work.  This
optmization requires that the left argument of the conjunction have at most
one match.  This will always be true because every function has only one
starting open brace.  But the pattern inside can be matched according to
two environments, as illustrated in smc_probe1. That is, the rule should
somehow fail and go to the naive implementation of the rule, but it is not
clear if that can be done efficiently.  For the moment, the two matches from
the function body are not compatible, and the complete match fails. */

@@
identifier fn,i,c2!=__badcall__;
type T;
@@

fn(...,T i,...) { <...
-  c2(...,i,...);
  ...>
}
