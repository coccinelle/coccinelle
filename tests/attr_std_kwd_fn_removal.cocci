#spatch
// this test probably concerns the lexing phase of the C file (where the 'noinline' token is special)
// we observe that having  #spatch --macro-file-builtins /dev/null  would be a workaround.
@@
@@
- int
- __attribute__ ((...))
- foo() { ... }
