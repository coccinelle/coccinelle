@one disable all@
identifier foo;
statement S;
@@

foo(...) {
  <+...
  {
  ...
  return;
  }
  ...+>
}

@two depends on one@
@@

- yyy();
