@@
expression E;
identifier y;
constant c;
identifier x,fld;
expression f;
@@

(
#define x (E)
|
#define x y
|
#define x c
|
#define x f(...)
|
#define x sizeof(...)
|
#define x E.fld
|
#define x E->fld
|
- #define x E
)
