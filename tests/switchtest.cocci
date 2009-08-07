@ switch_1
@
statement S_1,S_2;
position p1,p2;
@@
switch (...)
   {
   case ...:@p1 S_1
   case ...:@p2 S_2
   }

@
script:python @ stmt_1 << switch_1.S_1;stmt_2 << switch_1.S_2;
                p1 << switch_1.p1;p2 << switch_1.p2;
              @@
print "--- switch"
print stmt_1
print stmt_2
