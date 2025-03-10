int two () { return 12; }

#define foo 12

#pragma inline(one)

int one ();

int one () { return 12; }

int three () { return 12; }

#pragma abc one def

#pragma abc ddd def

#pragma abc one def
