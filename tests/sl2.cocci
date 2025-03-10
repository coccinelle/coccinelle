@r@
statement list ss;
@@

main(...) {
- ss
+ something();
+ others();
+ more();
 }

@@
statement list r.ss;
@@

other(...) {
+ ss
  ...
 }
