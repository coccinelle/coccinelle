@r@
position p1, p2;
identifier f;
expression E;
@@

f@p1(E@p2)

@ script:python @
p1 << r.p1;
p2 << r.p2;
f  << r.f;
@@

c1 = cocci.combine(f,p1)
c2 = cocci.combine(f,p2)
print "1. function in column %s" % c1.location.column
print "1. argument in column %s" % c2.location.column
c1 = cocci.combine(f,p1)
print "2. function in column %s" % c1.location.column
c2 = cocci.combine(f,p2)
print "2. argument in column %s" % c2.location.column
print "3. function in column %s" % ','.join([p.column for p in p1])
print "3. argument in column %s" % ','.join([p.column for p in p2])
print "4. function in column %s" % p1[0].column
print "4. argument in column %s" % p2[0].column
