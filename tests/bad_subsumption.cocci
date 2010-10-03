@loc exists@
type T;
identifier E;
identifier f != {kmalloc,kcalloc,kzalloc};
position pl;
@@

  T *E;
  ... when any
  E = f@pl(...)

@probe forall@
identifier E;
expression E1,E2;
identifier loc.f,g;
int ret;
statement S;
position loc.pl;
@@

(
  E = f@pl(...);
  ... when != E
  if (<+... E == NULL ...+>) S
|
  if (<+...(E = f@pl(...)) == NULL...+>) S
)
  ... when strict
      when any
      when != E2 = E
      when != E = E2
(
  E1 = E;
|
  E = E1;// this could seem bad: the value is being overwritten before being
         // saved, but in bd_claim_by_kobject in fs/block_dev, the value is
         // first passed to a function that saves it.  anyway, we will see if
         // this leads to false positives.
|
  g(...,E,...);
|
  return;
|
  return ret;
)

// might be a different function than the one matched above
@exists@
identifier E;
identifier loc.f, probe.g,x;
expression E1;
position loc.pl;
int ret;
@@

  E = f@pl(...)
  ... when strict
      when any
      when != E1 = E
  if (...) {
    ... when any
        when != E1 = E
        when != g(...,E,...)
    g(E
+     ,"detected allocator",f,g
    );
    ... when != E
(
    return;
|
    return ret;
)
  }
