# spatch --c++=11
@@
identifier i, IA;
statement S;
@@

- for (auto i: IA)
+ for (auto x: IA)
  S
