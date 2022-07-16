@nested@
position p;
statement S;
@@

do@p{
    S
    <+...
-    do {...} while(...);
    ...+>
  } while (...);
