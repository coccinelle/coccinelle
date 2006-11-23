//----------------------------------------------------------------------
// Rule 17
//----------------------------------------------------------------------

@@
struct BCState *bcs;
@@

-   bcs->tqueue
+   bcs->work

@@
struct BCState bcs;
@@

-   bcs.tqueue
+   bcs.work


//----------------------------------------------------------------------
@@
struct IsdnCardState *cs;
identifier i;
identifier bcs; // needed, was reused in rule17old.cocci
@@

(
-   cs->tqueue
+   cs->work
|
-   cs->bcs[i].tqueue
+   cs->bcs[i].work
)


@@
struct IsdnCardState cs;
@@

-   cs.tqueue
+   cs.work




