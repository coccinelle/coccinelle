@@
struct BCState *b;
struct IsdnCardState *i;
identifier f;
identifier g;
@@

(
- b->BC_SetStack = f;
|
- b->BC_Close = g;
|
- i->bcs->BC_SetStack = f;  // have to help the type checker along
|
- i->bcs->BC_Close = g;
)

@@
identifier b;
struct IsdnCardState *e;
@@

e->bc_l1_ops = &b;

@@
@@

struct bc_l1_ops b = {
+ .open = f,
+ .close = g,
};
