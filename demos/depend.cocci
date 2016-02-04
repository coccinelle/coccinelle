// this illustrates various complex dependencies
@a@
position p;
@@

a@p();

@b@
position p;
@@

b@p();

@c@
position p;
@@

c@p();

@script:python depends on a@
@@

print "a is ok"

@script:python depends on !a@
@@

print "a is not ok"

@script:python depends on (a && b) || c@
@@

print "a and b or c"

@script:python depends on !(!a && !b) || c@
@@

print "a or b or c"
