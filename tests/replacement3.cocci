@replacement3@
expression e;
expression list el;
identifier i;
type t;
@@
-t *i;
 {
-   i = e(el)
+   t *i = e(el)
 ;
 }
