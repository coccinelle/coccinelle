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
cocci.print_main(alloc,p1);
cocci.print_secs("free",p2);