@ rule1 @
identifier f, x;
@@

f(...,size_t x,...) { ... }

@ rule1_start @
identifier f_start, x;
@@

f_start(size_t x,...) { ... }

@ rule1_end @
identifier f_end, x;
@@

f_end(...,size_t x) { ... }

@@
expression E;
type T, T_start, T_end;
size_t E1;
T_start E_start;
T_end E_end;
identifier rule1.f;
identifier rule1_start.f_start;
identifier rule1_end.f_end;
@@

(
kcalloc(...)
|
f_start(sizeof(E),...)
|
f_start(sizeof(T),...)
|
f_start(E1,...)
|
- f_start(E_start,...)
|
f_start(...)
|
f_end(...,sizeof(E))
|
f_end(...,sizeof(T))
|
f_end(...,E1)
|
- f_end(...,E_end)
|
f_end(...)
|
f(...,sizeof(E),...)
|
f(...,sizeof(T),...)
|
f(...,E1,...)
|
- f(...)
)

@ rule2 @
identifier f, x;
type T;
@@

T f(...,size_t x,...);

@ rule2_start @
identifier f_start, x;
type T;
@@

T f_start(size_t x,...);

@ rule2_end @
identifier f_end, x;
type T;
@@

T f_end(...,size_t x);

@@
expression E;
type T, T_start, T_end;
size_t E1;
T_start E_start;
T_end E_end;
identifier rule2.f;
identifier rule2_start.f_start;
identifier rule2_end.f_end;
@@

(
kcalloc(...)
|
f_start(sizeof(E),...)
|
f_start(sizeof(T),...)
|
f_start(E1,...)
|
- f_start(E_start,...)
|
f_start(...)
|
f_end(...,sizeof(E))
|
f_end(...,sizeof(T))
|
f_end(...,E1)
|
- f_end(...,E_end)
|
f_end(...)
|
f(...,sizeof(E),...)
|
f(...,sizeof(T),...)
|
f(...,E1,...)
|
- f(...)
)
