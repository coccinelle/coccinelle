#spatch
// this test concerns the parsing of the C file
// note that having  #spatch --macro-file-builtins /dev/null  would be a workaround.
@@
@@
 int
- __attribute__ ((...))
 foo() { ... }
