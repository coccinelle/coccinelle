// shows how a single position variable can get more than one value

@a@
position p;
identifier g;
@@

f(...)
...
g@p(1,...)

@@
position a.p;
identifier a.g;
@@

- g@p(...);
