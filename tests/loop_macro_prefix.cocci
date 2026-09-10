// with_lock_held, scoped_lock_held, and scoped_cond_lock are unknown macros
// used as loops, with and without braces, and scoped_cond_lock with a
// statement among its arguments.  Their names start with with_ and scoped_,
// so they must be recognized as loops, or the functions using them do not
// parse.  scoped_guard is a keyword, and must keep working with a single
// statement and inside such a loop.  with_console is an ordinary function
// call, which must stay one.

@@
expression VAR, GFP;
@@

- VAR = kmalloc(sizeof(*VAR), GFP)
+ VAR = kmalloc_obj(*VAR, GFP)
