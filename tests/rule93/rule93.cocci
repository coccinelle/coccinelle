@@ expression current; @@

(
- if (unlikely(current->flags & PF_FREEZE)) refrigerator(PF_FREEZE);
+ try_to_freeze();
|
- if (unlikely(current->flags & PF_FREEZE)) refrigerator(0);
+ try_to_freeze();
)

@@ expression current; statement S; @@

  if (unlikely(
-     current->flags & PF_FREEZE
+     try_to_freeze()
     ))
- {
(
-   refrigerator(PF_FREEZE);
|
-   refrigerator(0);
)
    S
- }

@@ expression current; @@

  if (unlikely(
-     current->flags & PF_FREEZE
+     try_to_freeze()
     )) {
(
-   refrigerator(PF_FREEZE);
|
-   refrigerator(0);
)
    ...
  }

@@ expression current; @@

  if (unlikely(
-     current->flags & PF_FREEZE
+     freezing(current)
     )) {
    ...
(
-   refrigerator(PF_FREEZE);
+   refrigerator();
|
-   refrigerator(0);
+   refrigerator();
)
    ...
  }

@@
@@
- try_to_freeze(PF_FREEZE)
+ try_to_freeze()
