@@ expression E; @@

(
- (skb_queue_len(E) > 0)
+ !skb_queue_empty(E)
|
- (!skb_queue_len(E))
+ skb_queue_empty(E)
|
- (skb_queue_len(E) == 0)
+ skb_queue_empty(E)
)

@@ expression E; @@

(
- skb_queue_len(E) > 0
+ !skb_queue_empty(E)
|
- !skb_queue_len(E)
+ skb_queue_empty(E)
|
- skb_queue_len(E) == 0
+ skb_queue_empty(E)
)

@@
expression E;
statement S1, S2;
@@

(
  if (
-     skb_queue_len(E)
+     !skb_queue_empty(E)
     )
  S1 else S2
|
  if (
-     skb_queue_len(E)
+     !skb_queue_empty(E)
     )
  S1
|
  while (
-     skb_queue_len(E)
+     !skb_queue_empty(E)
     )
  S1
)

@@
expression E1, E2;
@@

(
- skb_queue_len(E1)
+ !skb_queue_empty(E1)
  || E2
|
- skb_queue_len(E1)
+ !skb_queue_empty(E1)
  && E2
|
- (skb_queue_len(E1))
+ !skb_queue_empty(E1)
  || E2
|
- (skb_queue_len(E1))
+ !skb_queue_empty(E1)
  && E2
)

@@
expression E1, E2;
@@

(
  E2 ||
- skb_queue_len(E1)
+ !skb_queue_empty(E1)
|
  E2 &&
- skb_queue_len(E1)
+ !skb_queue_empty(E1)
|
  E2 ||
- (skb_queue_len(E1))
+ !skb_queue_empty(E1)
|
  E2 &&
- (skb_queue_len(E1))
+ !skb_queue_empty(E1)
)
