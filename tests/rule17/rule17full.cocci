//----------------------------------------------------------------------
// Rule 17
//----------------------------------------------------------------------

//----------------------------------------------------------------------
@@
identifier x;
@@
<...
  INIT_WORK(...,
- (void *) (void *)
  x,...);
...>



//----------------------------------------------------------------------
// Renaming "tqueue" to "work" in certain contexts
//----------------------------------------------------------------------


//----------------------------------------------------------------------
@@
//struct BCState *bcs;
identifier bcs;
@@

    struct BCState *bcs; // feeble form of type checking
    <...
(
-   bcs->tqueue
+   bcs->work
|
-   bcs.tqueue
+   bcs.work
)
    ...>

//----------------------------------------------------------------------
@@
identifier bcs;
identifier fn;
@@

  fn(...,struct BCState *bcs,...) { // feeble form of type checking
    <...
(
-   bcs->tqueue
+   bcs->work
|
-   bcs.tqueue
+   bcs.work
)
    ...>
  }

//----------------------------------------------------------------------
@@
//struct IsdnCardState *cs;
identifier cs;
identifier i;
@@

    struct IsdnCardState *cs; // feeble form of type checking
    <...
(
-   cs->tqueue
+   cs->work
|
-   cs.tqueue
+   cs.work
|
-   cs->bcs[i].tqueue
+   cs->bcs[i].work
)
    ...>

//----------------------------------------------------------------------
@@
//struct IsdnCardState *cs;
//int ev;
identifier cs;
identifier fn;
identifier i;
@@

  fn(...,struct IsdnCardState *cs,...) { // feeble form of type checking
    <...
(
-   cs->tqueue
+   cs->work
|
-   cs.tqueue
+   cs.work
|
-   cs->bcs[i].tqueue
+   cs->bcs[i].work
)
    ...>
  }



//----------------------------------------------------------------------
// Final NULL argument of INIT_WORK should be changed
//----------------------------------------------------------------------

//----------------------------------------------------------------------
@@
identifier bcs;
@@

    struct BCState *bcs; // feeble form of type checking
    <...
   INIT_WORK(bcs->work,...,
-  NULL
+  bcs
   )
    ...>

//----------------------------------------------------------------------
@@
identifier bcs;
identifier fn;
@@

  fn(...,struct BCState *bcs,...) { // feeble form of type checking
    <...
   INIT_WORK(&bcs->work,...,
-  NULL
+  bcs
   )
    ...>
  }

//----------------------------------------------------------------------
@@
identifier cs;
identifier i;
@@

    struct IsdnCardState *cs; // feeble form of type checking
    <...
(
   INIT_WORK(&cs->work,...,
-  NULL
+  cs
   )
|
   INIT_WORK(&cs->bcs[i].work,...,
-  NULL
+  &cs->bcs[i]
   )
)
    ...>


//----------------------------------------------------------------------
@@
identifier cs;
identifier i;
identifier fn;
@@

  fn(...,struct IsdnCardState *cs,...) { // feeble form of type checking
    <...
(
   INIT_WORK(&cs->work,...,
-  NULL
+  cs
   )
|
   INIT_WORK(&cs->bcs[i].work,...,
-  NULL
+  &cs->bcs[i]
   )
)
    ...>
  }
