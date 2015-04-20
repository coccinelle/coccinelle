/* match all explicit boolean functions */
@boolean_function@
identifier fbool;
typedef bool;
@@

bool fbool(...) {
...
}

/* match variables eligible for boolean conversion */
@eligible_var exists@
identifier f, boolean_function.fbool;
typedef u1, u2, u4, u8, u16, u32;
local idexpression {int, u8, u1, u2, u4, u16, u32, char} x;
identifier xname;
expression e1, e2;
position p;
binary operator bop = { &&, ||, ==, !=, <, <=, >, >= };
@@


f@p(...) {
...when any
(
  x@xname = 1;
|
  x@xname = 0;
|
  x@xname = (e1) ? 0 : 1;
|
  x@xname = (e2) ? 1 : 0;
|
  x@xname = fbool(...);
|
  x@xname = e1 bop e2
)
...when any
}

/* match all acceptable complex assignement */
@valid_assign exists@
identifier eligible_var.f, boolean_function.fbool;
local idexpression {int, u8, u1, u2, u4, u16, u32, char} eligible_var.x;
expression e1, e2;
position p;
binary operator bop = { &&, ||, ==, !=, <, <=, >, >= };
@@

f(...) {
...when any
(
  x@p = (e1) ? 0 : 1;
|
  x@p = (e1) ? 1 : 0;
|
  x@p = fbool(...);
|
  x@p = e1 bop e2
)
...when any
}

/* match any expression where x is used as an int */
@badvar1 exists@
identifier eligible_var.f;
local idexpression {int, u8, u1, u2, u4, u16, u32, char} eligible_var.x;
expression e1 != {0, 1}, e2;
position p != {valid_assign.p};
binary operator bop = { +, -, *, /, &, |, %, ^, <<, >> };
assignment operator aop = { +=, -=, *=, /=, %=, |=, &=, ^=, <<=, >>= };
@@

f(...) {
...when any
(
  x@p = e1;
|
  x aop e2
|
  e2 aop x
|
  x++
|
  ++x
|
  x--
|
  --x
|
  x bop e2
|
  e2 bop x
|
  ~x
|
  return x;
)
...when any
}



@depends on !badvar1@
identifier eligible_var.f;
local idexpression {int, u8, u1, u2, u4, u16, u32, char} eligible_var.x;
identifier eligible_var.xname;
type t;
expression e;
@@


f(...) {
...
(
++ bool xname = false;
- t xname = 0;
|
++ bool xname = true;
- t xname = 1;
|
++ bool xname;
- t xname;
)
<...
(
  x =
- 1
+ true
|
  x =
- 0
+ false
|
- x = (e) ? 1 : 0
+ x = (e) ? true : false
|
- x = (e) ? 0 : 1
+ x = (e) ? false : true
)
...>

}
