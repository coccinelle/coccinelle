@ rule1 @
@@

- foo();

@ rule2 @
@@

- bar();

@ rule3 depends on rule1 && rule2 @
@@

- xxx();
