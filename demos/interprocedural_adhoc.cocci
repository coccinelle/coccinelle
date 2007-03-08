// Even if our analysis does not go through nodes containing 
// funcall, we can still do some (limited) interprocedural modification.

@@
identifier foo;
@@


ioctl (...) {
 ...
 foo(3);
 ...
}



@@
@@


foo(...)
{

- bar(1);
+ bar(2);

}