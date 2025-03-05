@ rule1 @
expression E;
@@

- xxx(E);
- yyy();
+ bar();

@ rule2 extends rule1 @
@@

- goo(E);
+ har();
