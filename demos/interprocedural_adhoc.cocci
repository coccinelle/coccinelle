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