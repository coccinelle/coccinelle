//does not work
//@@
//struct BCState *b;
//struct IsdnCardState *i;
//identifier f;
//identifier g;
//@@
//
//(
//- b->BC_SetStack = f;
//|
//- b->BC_Close = g;
//|
//- i->bcs->BC_SetStack = f;  // have to help the type checker along
//|
//- i->bcs->BC_Close = g;
//)

@@
struct BCState *b;
struct IsdnCardState *i;
identifier f,g;
@@

<... // needed to get f and g bound at the same time
(
- b->BC_SetStack = f;
|
- i->bcs->BC_SetStack = f;  // have to help the type checker along
|
- b->BC_Close = g;
|
- i->bcs->BC_Close = g;
)
...>

@@
identifier str;
struct BCState *b;
struct IsdnCardState *i;
@@

(
i->bc_l1_ops = &str;
|
b->cs->bc_l1_ops = &str;
)

@@
@@

struct bc_l1_ops str = {
  ...
+ .open = f,
+ .close = g,
};
