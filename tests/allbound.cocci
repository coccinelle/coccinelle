@@
Name: rule1;
expression E, E1;
@@

(
foo(E1)
|
bar(E)
)

@@
Extends: rule1;
@@

- xxx(E1)
+ yyy(E)
