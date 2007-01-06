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
)
