@@
statement S;
@@

foo(...) {
  ...
(
+ before_return();
  return;
|
  S
+ before_return();
)
}
