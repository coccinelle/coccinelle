@ rule1 @
identifier f, x;
@@

f(...,size_t x,...) { ... }

@@
expression E;
type T;
size_t E1;
identifier rule1.f;
@@

(
f(...,sizeof(E),...)
|
f(...,sizeof(T),...)
|
f(...,E1,...)
|
* f(...)
)

@ rule2 @
identifier f, x;
type T;
@@

T f(...,size_t x,...);

@@
expression E;
type T;
size_t E1;
identifier rule2.f;
@@

(
f(...,sizeof(E),...)
|
f(...,sizeof(T),...)
|
f(...,E1,...)
|
* f(...)
)
