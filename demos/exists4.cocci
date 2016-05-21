// This rule uses forall, with it, all control flows must match.
//
// The exists4.c was extended to add a c() in comparison to exists2.c
// this is done to show that using forall will still have an effect
// on bar() even though it does not match on main()
//
// This is the default behaviour when + or - is used as well.
@r forall@
@@

b();
...
-c();
