@display@
attribute name __free;
identifier release, var;
initialiser init;
type t;
@@
(
*t * var __free(release);
|
*t * var __free(release) = init;
)
