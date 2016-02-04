// Options: -dir {directory}

@initialize:python@
@@
counter = 0

@x@
position p;
@@

kmalloc@p(...)

@script:python@
p<<x.p;
@@

counter = counter + 1

@finalize:python@
@@

print "counter %d" % (counter)
