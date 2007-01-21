@@
struct BCState *e;
identifier f;
identifier g;
@@

(
- e->BC_SetStack = f;
|
- e->BC_Close = g;
)

@@
identifier b;
IsdnCardState *e;
@@

e->bc_l1_ops = b;

@@
@@

struct bc_l1_ops b = {
+ .open = f,
+ .close = g
};
