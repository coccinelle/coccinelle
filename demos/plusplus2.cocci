// This can support having a structure with more than two int variables,
// and adding a respective float for each. It can do this as order does
// not matter.
//
// This uses ++ to support the fact that the rule may be working
// with multiple variables that we need to modify and that order
// does not matter.
//
// If you don't use "++" you'll get "already tagged token" error since
// Coccinelle is concerned that the user has no way of specifying the order
// in which they should appear. By using "++" you are telling Coccinelle
//
//   "I know that a lot of things can collect here, and I'm OK
//    with that.  I'm also OK with things getting added out of order.

@plusplus@
identifier x,v;
fresh identifier xx = v ## "_float";
@@

struct x {
++	float xx;
	...
	int v;
	...
}
