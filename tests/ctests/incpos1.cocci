@initialize:python@
@@

first = 0
second = 0

@first_hdr@
position p;
@@

#include <...>@p

@script:python@
p << first_hdr.p;
@@

if first == 0:
   print("keeping first hdr %s" % (p[0].line))
   first = int(p[0].line)
else:
   print("dropping first hdr")
   cocci.include_match(False)

@second_hdr@
position p;
@@

#include "..."@p

@script:python@
p << second_hdr.p;
@@

if int(p[0].line) > first and first != 0:
   print("dropping second hdr")
   cocci.include_match(False)
else:
   if second == 0:
      print("keeping second hdr %s because of %d" % (p[0].line,first))
      second = int(p[0].line)
   else:
      print("dropping second hdr")
      cocci.include_match(False)

@done@
position second_hdr.p;
@@

+#include <foo.h>
#include "..."@p

@depends on never done@
@@

+#include <foo.h>
#include <...>
