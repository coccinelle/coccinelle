@display@
expression action;
expression* pointer, target, x;
identifier member, var;
statement is;
type t;
@@
(
*t* var = pointer->member;
|
*pointer->member;
|
*target = pointer->member;
|
*t* var = pointer->member(...);
|
*pointer->member(...);
|
*target = pointer->member(...);
)
 ... when any
     when != pointer = \( action(...) \| x \)
*if (
(    !pointer
|    pointer == NULL
)
    )
  is
