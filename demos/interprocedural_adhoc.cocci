// Even if our analysis does not go through nodes containing 
// funcall, we can still do some (limited) interprocedural modification.

@ rule1 @
identifier foo;
@@


ioctl (...) {
 ...
 foo(3);
 ...
}



@@
identifier rule1.foo;
@@


foo(...)
{

- bar(1);
+ bar(2);

}