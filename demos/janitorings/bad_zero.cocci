
@ disable is_zero @
expression *E;
@@

  E ==
- 0
+ NULL

@ disable is_zero @
expression *E;
@@

- 0
+ NULL
  == E

@ disable isnt_zero @
expression *E;
@@

  E !=
- 0
+ NULL

@ disable isnt_zero @
expression *E;
@@

- 0
+ NULL
  != E


@@
idexpression *X;
statement S;
@@

- if(X == NULL)
+ if(!X)
  S

@@
idexpression *X;
statement S;
@@

- if(X != NULL)
+ if(X)
  S
