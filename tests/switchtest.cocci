@ switch_1
@
statement S_1;
@@
switch (...)
   {
   case ...: S_1
   case ...: ...
   }

@
script:python @ stmt_1 << switch_1.S_1;
              @@
print "--- switch"
print stmt_1
