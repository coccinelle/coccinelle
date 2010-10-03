@ rule1 @
@@

 foo(1);
 bar(2);


@ rule2 depends on rule1 @
@@

- bar(3);