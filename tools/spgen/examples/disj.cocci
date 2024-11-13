// This file is part of Coccinelle, lincensed under the terms of the GPL v2.
// See copyright.txt in the Coccinelle source code for more information.
// The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/

@disj@
expression E1, E2, E3;
identifier x;
@@

if (E2 <
- E3
+ E1
 ) { ...
(
some_function(0);
x = 30;
|
some_function(1);
x = 30;
|
- this(E1);
+ that(E2);
|
- some_function(E1)
+ another_function(E2)
;
- x = 30;
)
... }
