@@
constant char [] c;
identifier f;
@@

f(...,
- c,
+ "KERN_ERR %s: %s: redirecting sector %llu to another IO path\n", __func__,
...)
