@@
identifier FN;
type T;
identifier x;
expression y;
@@

(
- T x = <+... FN(...) ...+>;
|
- y = <+... FN(...) ...+>;
|
- FN(...);
|
- return <+... FN(...) ...+>;
)
