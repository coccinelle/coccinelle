@r@
expression E;
identifier func;
@@
func(E);

@script:python s@
func << r.func;
prefix_func;
@@

prefix_func = "one_argument_function_%s" % func

@@
expression E;
identifier r.func,s.prefix_func;
@@
-func(E);
+prefix_func(E);
