// if the translation of ... doesn't search for the closing brace
// then the for loop in makes_a_loop.c causes an infinite loop in the CTL
// of length the number of nodes in the for loop
// the problem is the witnesses, one witness derived from the break in the
// for loop enters and then leaves and then reenters and then releaves
// the set of witnesses

@r@
identifier I;
identifier retval;
expression E1, E2;
@@

	if (retval) {
-		foo();
-		...
	}
