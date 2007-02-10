//----------------------------------------------------------------------
// Rule 17
//----------------------------------------------------------------------

@@
struct BCState *bcs_id;
@@

-   bcs_id->tqueue
+   bcs_id->work

@@
struct BCState bcs_id;
@@

-   bcs_id.tqueue
+   bcs_id.work


//----------------------------------------------------------------------
@@
struct IsdnCardState *cs;
@@

- cs->tqueue.data = cs;  // what if RHS were something else?


@@
struct IsdnCardState *cs;
expression E;
@@

(
-   cs->tqueue
+   cs->work
|
-   cs->bcs[E].tqueue
+   cs->bcs[E].work
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
- INIT_WORK(&E1->work, (T)\(f\|&f\), NULL)
+ INIT_WORK(&E1->work, f, E1)
|
- INIT_WORK(&E1->work, (void *)(void *)\(f\|&f\), NULL)
+ INIT_WORK(&E1->work, f, E1)
|
- INIT_WORK(E1, (T)\(f\|&f\), (void*)E2)
+ INIT_WORK(E1, f, E2)
|
- INIT_WORK(E1, (void *)(void *)\(f\|&f\), (void *)E2)
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
