//
// kmalloc 7
//
//

virtual org,diff

@r exists@
local idexpression x;
statement S;
expression E;
identifier f,l;
position p1,p2,p3;
expression *ptr != NULL;
@@

(
if ((x@p1 = \(kmalloc\|kzalloc\|kcalloc\)(...)) == NULL) S
|
x@p1 = \(kmalloc\|kzalloc\|kcalloc\)(...);
...
if (x == NULL) S
)
<... when != x
     when != if (...) { <+...x...+> }
(
goto@p3 l;
|
x->f = E
)
...>
(
 return \(0\|<+...x...+>\|ptr\);
|
 return@p2 ...;
)

@script:python depends on org@
p1 << r.p1;
p2 << r.p2;
p3 << r.p3;
@@

cocci.print_main("",p1)
cocci.print_secs("", p2)
cocci.print_secs("goto", p3)
cocci.include_match(False)

@script:python depends on org@
p1 << r.p1;
p2 << r.p2;
@@

cocci.print_main("",p1)
cocci.print_secs("", p2)

@with_goto depends on diff@
expression x;
identifier l;
position r.p1, r.p2, r.p3;
@@

*x@p1
<...
*goto@p3 l;
...>
*return@p2 ...;
