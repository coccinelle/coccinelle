// A metavariable of a const-qualified type is compared with the types of
// all the expressions in the file, including pointers to types with an
// attribute such as __iomem, which must not abort processing of the file.

@@
const int *CP;
expression E;
@@

- CP = E;
+ CP = NULL;
