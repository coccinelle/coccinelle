@@
statement S;
@@

foo(...) {
  ...
(
+ xxx();
  return;
|
  S
+ xxx();
)
}
