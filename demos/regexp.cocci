@anyid@
type t;
identifier id;
@@

t id () {
...
}

@script:python@
x << anyid.id;
@@

print "Identifier: %s" % x

@contains@
type t;
identifier foo =~ "foo";
@@

t foo () {
...
}

@script:python@
x << contains.foo;
@@

print "Contains foo: %s" % x

@nocontain@
type t;
identifier foo !~ "foo";
@@

t foo () {
...
}

@script:python@
x << nocontain.foo;
@@

print "Does not contain foo: %s" % x

@endsby@
type t;
identifier foo =~ "foo$";
@@

t foo () {
...
}

@script:python@
x << endsby.foo;
@@

print "Ends by foo: %s" % x

@beginsby@
type t;
identifier foo =~ "^foo";
@@

t foo () {
...
}

@script:python@
x << beginsby.foo;
@@

print "Begins by foo: %s" % x
