// This file is part of Coccinelle, lincensed under the terms of the GPL v2.
// See copyright.txt in the Coccinelle source code for more information.
// The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

@@
local function f;
identifier x, y, m;
@@

  f(
*   int x,
    int y
  ) {
  int m = x * y;
  ...
  return m;
}
