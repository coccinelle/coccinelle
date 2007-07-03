@@
expression E;
identifier y;
constant c;
identifier x;
@@

(
#define x (E)
|
#define x y
|
#define x c
|
- #define x E
)
