// run --parse-cocci on this.  Should give some filtering tokens.
// No execution test.

@r@
identifier f = {g_malloc,g_malloc0,g_new,g_new0};
expression x;
expression list es;
statement S1,S2;
@@

(
+ x = f(es);
-if ((x = f(es)) == NULL)
-S1 else
 S2
|
+ x = f(es);
-if ((x = f(es)) != NULL)
 S1
-else S2
|
+ x = f(es);
-if ((x = f(es)) == NULL)
-S1
|
+ x = f(es);
-if ((x = f(es)) != NULL)
 S1
)