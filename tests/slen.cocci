@rr@
statement list [3] ss;
@@

{ ss }

@@
statement list [3] ss;
@@

{
- ss
+ was3();
}

@r@
statement list [n] ss;
@@

{ ss }

@script:ocaml@
n << r.n;
@@

if not(n=5) then include_match false

@@
statement list r.ss;
@@

{
- ss
+ was5();
}
