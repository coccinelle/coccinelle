@ rule1 @
expression E, E1;
@@

(
foo(E1)
|
bar(E)
)

@ rule2 extends rule1 @
@@

- xxx(E1)
+ yyy(E)
