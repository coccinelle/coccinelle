@@
identifier interrupt;
statement S;
@@

  interrupt(...) {
    ...
(
+   spin_unlock(&cs->lock);
    return;
|
    S
+   spin_unlock(&cs->lock);
)
  }
