// This adds a float for one int variable found.
// Doing this however is limited to one int in the data structure, if
// more one int variable in the structure we would not know where to
// place the new one safely order-wise. See plusplus2.cocci for an
// example of dealing with this issue.

@simpleplus@
identifier x,v;
fresh identifier xx = v ## "_float";
@@

struct x {
+	float xx;
	...
	int v;
	...
}
