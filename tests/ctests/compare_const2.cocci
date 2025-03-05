@allconst@
position p;
binary operator b;
constant c1,c2;
idexpression i;
@@

(
c1 b <+...i...+>
|
c1 b@p <+...c2...+>
)

@disable bitor_comm, neg_if_exp@
constant c;
local idexpression i;
expression e,e1,e2;
binary operator b = {==,!=,&,|};
type t;
position p != allconst.p;
@@

(
sizeof(t) b e1
|
sizeof e b e1
|
i b e1
|
c | e1 | e2 | ...
|
c | (e ? e1 : e2)
|
-c b@p
 e
+ b c
)
