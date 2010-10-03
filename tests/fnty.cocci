@structure@
identifier idtype, y;
type t;
position p;
@@
struct idtype {
  ...
        t (*y)(...);@p
  ...
};

@bad@
identifier structure.idtype, y;
type t;
position p != structure.p;
@@

struct idtype {
  ...
        t y;@p
  ...
};

@depends on !bad@
identifier structure.idtype;
@@
struct
- idtype
+ newname
{
  ...
};
