// problem with positions.  in check_unprotected, we start with a set of
// positions, but the binding that is deduced is only relevant to one of them
// but since it was deduced from both of them, both of them are printed out
// at the end

@unprotected exists@
expression x;
identifier fld;
position p,p1;
statement S;
expression E;
@@

  x@p1 = FN(...);
  ... when != x = E
(
  if (x == NULL) { ... return ...; } else S
|
  x@p->fld
)

@check_unprotected exists@ // ensure both are present
position unprotected.p, unprotected.p1;
expression x;
identifier fld;
@@

x@p1 = FN(...);
...
x@p->fld

@ script:python depends on check_unprotected @
p << unprotected.p; // position of ref
p1 << unprotected.p1; // position of call
fld << check_unprotected.fld; // identifier
@@

c = cocci.combine(fld,p1)
print "   call to FN on line %s column %s" % (c.location.line,c.location.column)
c1 = cocci.combine(fld,p)
print "   ref to field %s on line %s column %s" % (fld,c1.location.line,c1.location.column)
