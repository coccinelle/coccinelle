// foobar(x) comes out to the right of foo(x)
// if bar is alone in a branch, as in julia7.c, the if ends up with no branch

@@
@@

    foo();
    ...
?-  bar();
    after();
