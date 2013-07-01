// Options: -D alloc=kmalloc -D free=kfree

@r@
identifier virtual.alloc, virtual.free;
expression x;
position p1,p2;
@@

x = alloc@p1(...);
...
free@p2(x);

@script:python@
p1 << r.p1;
p2 << r.p2;
alloc << virtual.alloc;
@@
coccilib.xml_firehose.import_firehose()
coccilib.xml_firehose.print_issue(p1, ("alloc="+alloc));
