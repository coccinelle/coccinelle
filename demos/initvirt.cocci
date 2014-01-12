@initialize:ocaml@
x << virtual.x;
@@

let _ = Printf.printf "ocaml start: x is %s\n" x

@initialize:python@
x << virtual.x;
@@

print "python start: x is %s" % (x)

@@
@@

foo();

@script:ocaml@
@@
()

@finalize:ocaml@
x << virtual.x;
y << virtual.y;
@@

Printf.printf "ocaml end: x is %s\n" x;
Printf.printf "ocaml end: y is %s\n" y

@finalize:python@
x << virtual.x;
y << virtual.y;
z << virtual.z;
@@

print "python end: x is %s" % (x)
print "python end: y is %s" % (y)
