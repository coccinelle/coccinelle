@r0@
declarer name DECLARER;
attribute name __macro_attr;
@@

DECLARER(
	...,
-	a
+	c
	,
	...
  ) __macro_attr;

@initialize:ocaml@
@@
let check_attr s attrname =
    try
      Str.search_forward (Str.regexp_string attrname) s 0; true
    with Not_found -> false

@r1@
declarer name DECLARER2;
attribute name __macro_attr2;
declaration s : script:ocaml () { check_attr s "__macro_attr2" };
@@
-s
