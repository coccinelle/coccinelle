@@
expression e;
identifier i;
@@

-#define i e

@r@
identifier fn;
@@

- #pragma inline (fn)

@@
identifier r.fn;
type T;
@@

T fn(...)
+ __attribute__((always_inline))
;

@@
identifier r.fn;
@@

- #pragma abc fn def