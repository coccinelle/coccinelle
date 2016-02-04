// Illustrate the use of virtual rules
//
// Confidence: High
// Copyright: (C) Gilles Muller, Julia Lawall, INRIA, DIKU.  GPLv2.
// Options: some permutation of -D/U p1 -D/U p2

virtual p1, p2

@script:python depends on p1@
@@

print "p1"

@script:python depends on p2@
@@

print "p2"

@script:python depends on p1 && !p2@
@@

print "only p1"

@script:python depends on p2 && !p1@
@@

print "only p2"
