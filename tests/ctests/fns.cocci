// we were using some minirules before where fn1 and fn2 were in same rule

// pad: il manque un E dans check_region dans rule3 car il a 2 args
// mais je sais pas si faut le propager dans request_region

@ rule1 @
expression req8_reg_arg2;
expression E;
identifier fn1;
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

@ rule2 depends on rule1 @
expression rule1.req8_reg_arg2;
expression req8_reg_arg1;
expression req8_reg_arg3;
identifier fn2;
@@

fn2(...) {
-   request_region(req8_reg_arg1, req8_reg_arg2, req8_reg_arg3);
}

@ rule3 depends on rule1 @
expression rule1.req8_reg_arg2;
expression rule2.req8_reg_arg3;
statement S;
@@

-   if (check_region(req8_reg_arg2)) S
+   if (!request_region(req8_reg_arg2, req8_reg_arg3)) S
