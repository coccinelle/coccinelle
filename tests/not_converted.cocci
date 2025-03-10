// A variable is used between two affectations. 
//
// Confidence: High
// Copyright: (C) Nicolas Palix, Julia Lawall, DIKU.  GPLv2.
// URL: 
// Options: -ifdef_to_if

@filter@
local idexpression E;
expression Er;
@@

E = Er;

@r @
local idexpression filter.E;
position b;
position e,f;
expression E1;
expression E2;
@@

E@b = E1;
... when != E
    when strict
(
E@f = <+...E...+>;
|
E@e = E2;
)

@script:python@
p1 << r.b;
p2 << r.e;
p3 << r.f;
@@

cocci.include_match(False)

@script:python@
p1 << r.b;
p2 << r.e;
@@

cocci.print_main("",p1)
cocci.print_secs("",p2)
