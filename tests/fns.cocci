@@
rule1:
expression req8_reg_arg2;
expression E;
identifier fn1;
expression req8_reg_arg1, req8_reg_arg3;
identifier fn2;
@@

fn1(...) {
 ...
 for(...; ...; ...) {
    ...
   if (check_region(E, req8_reg_arg2))
      { ... when = \( printk(...); \| dbg(...); \)
	continue; }
 ...
 }
...
}

fn2(...) {
-   request_region(req8_reg_arg1, req8_reg_arg2, req8_reg_arg3);
}

@@
rule2 extends rule1:
statement S;
@@

-   if (check_region(req8_reg_arg2)) S
+   if (!request_region(req8_reg_arg2, req8_reg_arg3)) S
