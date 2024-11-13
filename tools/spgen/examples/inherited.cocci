// This file is part of Coccinelle, lincensed under the terms of the GPL v2.
// See copyright.txt in the Coccinelle source code for more information.
// The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/

@r@
expression x;
@@

(
 f(x);
|
 g(1);
)

@@
expression r.x;
@@

- h(x);
+ hh(x);


@@
expression r.x;
@@
- h2(x);
+ hh22(x);

@@
@@

- foo(1);
+ bar(1);
