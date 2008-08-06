@one disable all@
identifier foo;
statement S;
@@

foo(...) {
  <+...
  xxx();
  ...+>
}

@two depends on one@
@@

- yyy();
