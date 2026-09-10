#spatch --include tests/define_param_header_inc.h

// The body of to_item in define_param_header.h makes its parameter "type"
// look like a typedef name.  That must not change how "type" is parsed in
// define_param_header.c, which includes another header afterwards.  The
// --include gives all of these parses a common table of typedef names to
// start from, as it does in the Linux kernel's coccicheck.

@@
struct holder *h;
expression GFP;
@@

- h->buf = kmalloc(sizeof(*h->buf), GFP)
+ h->buf = kmalloc_obj(*h->buf, GFP)
