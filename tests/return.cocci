@@
statement S;
@@

foo(...) {
  ...
(
+ xxx();
  return;
|
  ccc();
+ yyy();
)
}
