@ switch_1
@
statement S_1;
position p_1;
@@
switch (...)
   {
   case ...:@p_1 S_1
//   case ...: S_1

   }

@
script:python @ stmt_1 << switch_1.S_1;
              @@
print "--- switch"
#print loc_1[0].line, " ", loc_1[0].column
print stmt_1
