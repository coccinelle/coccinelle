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
identifier bcs;
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

//----------------------------------------------------------------------

@@
// local function f;
identifier f;
expression E1, E2;
type T;
@@

// occurrences of T should be (void (*)(void *)) but no fn ptrs in SmPL
(
- INIT_WORK(E1, (T)f, E2)
+ INIT_WORK(E1, f, E2)
|
- INIT_WORK(E1, (void *)(void *)f, E2)
+ INIT_WORK(E1, f, E2)
|
- INIT_WORK(E1, (T)&f, E2)
+ INIT_WORK(E1, f, E2)
|
- INIT_WORK(E1, (void *)(void *)&f, E2)
+ INIT_WORK(E1, f, E2)
)

@@
type T;
identifier x;
fresh identifier data;
@@
- void f(T x)
+ void f(void *data)
   {
+    T x = data;
     ...
   }
