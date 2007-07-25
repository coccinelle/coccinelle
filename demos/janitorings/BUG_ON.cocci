// is also mega10.cocci 


@@ expression E; @@

// super unreadable, but we don't want the isomorphism to apply, because then
// two bindings of E drift up to the top

- if (unlikely(
+ BUG_ON(
  E
+ );
- )) { BUG(); }

@@ expression E; @@

- if (E) { BUG(); }
+ BUG_ON(E);
