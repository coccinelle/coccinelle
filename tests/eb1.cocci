@rule1@
identifier p;
identifier func;
identifier fdl;
@@
func(...) {
<+...
Packet p;
...
(
- p.fdl
+ p->fdl
|
- &p
+ p
)
...+>
}


@rule2@
identifier p;
identifier func;
statement S;
@@
func(...) {
  ...
  Packet
- p
+ *p = SCMalloc(SIZE_OF_PACKET)
  ;
  ...
++if (p == NULL) return 0;
  S
  ...
++SCFree(p);
  return ...;

}
